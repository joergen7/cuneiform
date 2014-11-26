/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.huberlin.wbi.cuneiform.core.repl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;
import de.huberlin.wbi.cuneiform.core.preprocess.ChannelListener;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;
import de.huberlin.wbi.cuneiform.core.preprocess.PreListener;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SingleExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.ReplTicketSrc;

public abstract class BaseRepl {

	public static final int CTL_NIL = 0;
	public static final int CTL_QUIT = 1;
	public static final int CTL_STATE = 2;
	public static final int CTL_QUERYSET = 4;
	public static final int CTL_TICKETSET = 8;

	public static final String LABEL_VERSION = "2.0";
	public static final String LABEL_BUILD = "2014-11-26";

	private final CfSemanticModelVisitor state;
	private final Map<UUID,DynamicNodeVisitor> runningMap;
	private final ReplTicketSrc ticketSrc;
	private final Log log;
	private CompoundExpr ans;
	private final Log statLog;



	public BaseRepl( ReplTicketSrc ticketSrc, Log statLog ) {

		if( ticketSrc == null )
			throw new NullPointerException( "Ticket source actor must not be null." );
		
		this.ticketSrc = ticketSrc;
		
		runningMap = new HashMap<>();
		state = new CfSemanticModelVisitor();
		log = LogFactory.getLog( BaseRepl.class );
		this.statLog = statLog;
	}
	
	public CompoundExpr getAns() {
		return ans;
	}
	
	public Set<UUID> getRunningSet() {
		return runningMap.keySet();
	}
	
	public Set<Ticket> getTicketSet() {
		
		Set<Ticket> set;
		
		set = new HashSet<>();
		
		for( UUID queryId : runningMap.keySet() )
			set.addAll( ticketSrc.getTicketSet( queryId ) );
		
		return set;
	}
	
	public TopLevelContext getState() {
		return state.getTopLevelContext();
	}
	
	public boolean hasAns() {
		return ans != null;
	}
	
	public int interpret( String input ) {
		
		String afterPre, afterChannel;
		CuneiformLexer lexer;
		CuneiformParser parser;
		TopLevelContext tlc;
		
		if( input == null )
			throw new NullPointerException( "Input string must not be null." );
				
		// pre-step
		afterPre = PreListener.process( input );
		
		// channel-step
		afterChannel = ChannelListener.process( afterPre );
				
		// create lexer
		lexer = new CuneiformLexer( new ANTLRInputStream( afterChannel ) );
		lexer.removeErrorListeners();
		lexer.addErrorListener( state );

		// create parser
		parser = new CuneiformParser( new CommonTokenStream( lexer ) );
		parser.removeErrorListeners();		
		parser.addErrorListener( state );
		
		// visit parsed script
		tlc = ( TopLevelContext )state.visit( parser.script() );
		
		return interpret( tlc );
	}
	
	public int interpret( TopLevelContext tlc ) {
		
		int ctl;
		DynamicNodeVisitor reducer;
		UUID queryId;

		// remove control targets
		ctl = fetchCtl( tlc );
			
		if( !tlc.isTargetListEmpty() ) {
			
			reducer = new DynamicNodeVisitor( ticketSrc, this, tlc );			
			queryId = reducer.getQueryId();
			runningMap.put( queryId, reducer );
			
			try {
				
				queryStarted( queryId );
				
				// trigger reduction
				reducer.step();
			
			}
			catch( ParseException e ) {
				queryFailed( queryId, null, e, null, null, null );
			}
		}
		
		return ctl;
	}
	
	public boolean isBusy() {
		return !runningMap.isEmpty();
	}
	
	public boolean isRunning( UUID queryId ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query id must not be null." );
		
		return runningMap.containsKey( queryId );
	}
	
	public void queryFailed( UUID queryId, Long ticketId, Exception e, String script, String stdOut, String stdErr ) {
		
		String s;
		
		if( queryId == null )
			throw new NullPointerException( "Query id must not be null." );
		
		runningMap.remove( queryId );
		
		s = "";
		
		if( e != null ) {
			
			s += " "+e.getClass().getName();
			
			if( e.getMessage() != null )
				s += ": \""+e.getMessage()+"\"";
		}
		
		if( stdOut != null )
			s += " Output channel: \""+stdOut.replace( '\n', ' ' )+"\"";

		if( stdErr != null )
			s += " Error channel: \""+stdErr.replace( '\n', ' ' )+"\"";
		
		if( log.isErrorEnabled() )
			log.error( "Query "+queryId+" failed while executing ticket "+ticketId+"."+s );
		
		queryFailedPost( queryId, ticketId, e, script, stdOut, stdErr );
	}

	public abstract void queryFailedPost( UUID queryId, Long ticketId, Exception e, String script, String stdOut, String stdErr );

	public void queryFinished( UUID queryId, CompoundExpr result ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query id must not be null." );
		
		if( result == null )
			throw new NullPointerException( "Result compound expression must not be null." );
		
		runningMap.remove( queryId );
		ans = result;

		if( log.isInfoEnabled() )
			log.info( "Query "+queryId+" finished: "+result );

		queryFinishedPost( queryId, result );
	}
	
	public abstract void queryFinishedPost( UUID queryId, CompoundExpr result );
	
	public void queryStarted( UUID runId ) {

		if( log.isInfoEnabled() )
			log.info( "Query "+runId+" started." );

		queryStartedPost( runId );
	}
	
	public abstract void queryStartedPost( UUID runId );
	
	public void ticketFinished( UUID queryId, long ticketId, Set<JsonReportEntry> reportEntrySet ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query ID must not be null." );
		
		if( reportEntrySet == null )
			throw new NullPointerException( "Report entry set must not be null." );
		
		runningMap.get( queryId ).step();
		
		if( log.isDebugEnabled() )
			log.debug( "Ticket finished: "+ticketId+" part of query "+queryId );
		
		flushStatLog( reportEntrySet );
			
	}
	
	protected void flushStatLog( Set<JsonReportEntry> reportEntrySet ) {
		
		if( statLog == null )
			return;
		
		if( statLog.isDebugEnabled() )
			for( JsonReportEntry entry : reportEntrySet )
				statLog.debug( entry );
	}
	
	public static String getLogo() {
		
		return "                              _\n"
			+"                         _gg@@@L\n"
			+"                       _g@@@@@@k\n"
			+"                       g@@@@BBBA\n"
			+"                      gB@@@P\n"
			+"                     _@@@@B\n"
			+"                   __@@@@@gg@@BBBB@@gggp\n"
			+"              __gg@@@@@@@@@@@@@@@@@@@@BF\n"
			+"           _gg@@@@@@@@@@@BBBBBBBBBBBB@BL\n"
			+"         _g@@@@@B@@BMB@@@F\n"
			+"       _@B@@@@@#P^   @@@@F\n"
			+"     _gB@@@B#F      q@@@@L\n"
			+"    _@@@@@#\"        q@@@BL\n"
			+"    g@@@BF          q@@@BL\n"
			+"   _@@@@F           q@@@BL\n"
			+"   4@@@@L           q@@@BL\n"
			+"   ^@@@@gg_____ggp  q@@@BL\n"
			+"    MB@@@@@@@@@@@g  q@@@BL\n"
			+"     ^WB@@@@@@@@BB  q@@@BL\n"
			+"        \"MM##MP\"\"   q@BB@L\n"
			+"                     \"\"\"`\n"
			+"\nCUNEIFORM - A Functional Workflow Language\nVersion "
			+LABEL_VERSION+" build "+LABEL_BUILD+"\n\nJörgen Brandt    Marc Bux    Ulf Leser\n";
	}

	public static int fetchCtl( TopLevelContext tlc ) {
		
		int ctl;
		List<CompoundExpr> removeCandidateList;
		CompoundExpr cp;
		
		if( tlc == null )
			throw new NullPointerException( "Top level context must not be null." );
		
		ctl = 0;
		removeCandidateList = new ArrayList<>();
		
		try {
		
			for( CompoundExpr ce : tlc.getTargetList() ) {
				
				cp = ce.clone();
				
				for( SingleExpr se : cp.getSingleExprList() )
					if( se instanceof NameExpr ) {
						
						
						if( ( ( NameExpr )se ).getId().equals( "state" ) ) {
							ctl += CTL_STATE;
							ce.remove( se );
							removeCandidateList.add( ce );
							continue;
						}
	
						if( ( ( NameExpr )se ).getId().equals( "quit" ) ) {
							ctl += CTL_QUIT;
							ce.remove( se );
							removeCandidateList.add( ce );
							continue;
						}
	
						if( ( ( NameExpr )se ).getId().equals( "queries" ) ) {
							ctl += CTL_QUERYSET;
							ce.remove( se );
							removeCandidateList.add( ce );
							continue;
						}
						
						if( ( ( NameExpr )se ).getId().equals( "tickets" ) ) {
							ctl += CTL_TICKETSET;
							ce.remove( se );
							removeCandidateList.add( ce );
							continue;
						}
					}
			}
		}
		catch( CloneNotSupportedException e ) {
			throw new RuntimeException( e );
		}
		
		
		for( CompoundExpr ce : removeCandidateList )
			if( ce.getNumSingleExpr() == 0 )
				tlc.removeTarget( ce );
		
		return ctl;
	}
}
