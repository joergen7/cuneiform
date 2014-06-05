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
import de.huberlin.wbi.cuneiform.core.preprocess.PreListener;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SingleExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public abstract class BaseRepl {

	public static final int CTL_NIL = 0;
	public static final int CTL_QUIT = 1;
	public static final int CTL_STATE = 2;
	public static final int CTL_QUERYSET = 4;
	public static final int CTL_TICKETSET = 8;

	public static final String LABEL_VERSION = "2.0";
	public static final String LABEL_BUILD = "2014-06-04";

	private final CfSemanticModelVisitor state;
	private final Map<UUID,DynamicNodeVisitor> runningMap;
	private final TicketSrcActor ticketSrc;
	private final Log log;
	private final Log statLog;
	private CompoundExpr ans;


	public BaseRepl( TicketSrcActor ticketSrc ) {

		if( ticketSrc == null )
			throw new NullPointerException( "Ticket source actor must not be null." );
		
		this.ticketSrc = ticketSrc;
		
		runningMap = new HashMap<>();
		state = new CfSemanticModelVisitor();
		log = LogFactory.getLog( BaseRepl.class );
		statLog = LogFactory.getLog( "statLogger" );
	}
	
	public synchronized CompoundExpr getAns() {
		return ans;
	}
	
	public synchronized Set<UUID> getRunningSet() {
		return runningMap.keySet();
	}
	
	public synchronized Set<Ticket> getTicketSet() {
		
		Set<Ticket> set;
		
		set = new HashSet<>();
		
		for( UUID queryId : runningMap.keySet() )
			set.addAll( ticketSrc.getTicketSet( queryId ) );
		
		return set;
	}
	
	public synchronized TopLevelContext getState() {
		return state.getTopLevelContext();
	}
	
	public synchronized boolean hasAns() {
		return ans != null;
	}
	
	public synchronized int interpret( String input ) {
		
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
	
	public synchronized int interpret( TopLevelContext tlc ) {
		
		int ctl;
		DynamicNodeVisitor reducer;
		UUID queryId;

		// remove control targets
		ctl = fetchCtl( tlc );
		
		if( !tlc.isTargetListEmpty() ) {
		
			
			reducer = new DynamicNodeVisitor( ticketSrc, this, tlc );
			
			queryId = reducer.getQueryId();
			
			runningMap.put( queryId, reducer );
			queryStarted( queryId );
			
			// trigger reduction
			reducer.step();
		}
		
		return ctl;
	}
	
	public synchronized boolean isBusy() {
		return !runningMap.isEmpty();
	}
	
	public synchronized boolean isRunning( UUID queryId ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query id must not be null." );
		
		return runningMap.containsKey( queryId );
	}
	
	public synchronized void queryFailed( UUID queryId, long ticketId, Exception e, String script, String stdOut, String stdErr ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query id must not be null." );
		
		if( script == null )
			throw new NullPointerException( "Script string must not be null." );		

		runningMap.remove( queryId );
		
		if( log.isErrorEnabled() )
			if( e == null )
				log.error( "Query "+queryId+" failed while executing ticket "+ticketId+". Error channel: "+stdErr.replace( '\n', ' ' ) );
			else
				log.error( "Query "+queryId+" failed while executing ticket "+ticketId+". Message: "+e.getMessage()+" Error channel: "+stdErr.replace( '\n', ' ' ) );
		
		queryFailedPost( queryId, ticketId, e, script, stdOut, stdErr );
	}

	public abstract void queryFailedPost( UUID queryId, long ticketId, Exception e, String script, String stdOut, String stdErr );

	public synchronized void queryFinished( UUID queryId, CompoundExpr result ) {
		
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
	
	public synchronized void queryStarted( UUID runId ) {

		if( log.isInfoEnabled() )
			log.info( "Query "+runId+" started." );

		queryStartedPost( runId );
	}
	
	public abstract void queryStartedPost( UUID runId );
	
	public synchronized void ticketFinished( UUID queryId, long ticketId, Set<JsonReportEntry> reportEntrySet ) {
		
		if( queryId == null )
			throw new NullPointerException( "Query ID must not be null." );
		
		if( reportEntrySet == null )
			throw new NullPointerException( "Report entry set must not be null." );
		
		runningMap.get( queryId ).step();
		
		if( log.isDebugEnabled() )
			log.debug( "Ticket finished: "+ticketId+" part of query "+queryId );
		
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
		
		for( CompoundExpr ce : removeCandidateList )
			if( ce.getNumSingleExpr() == 0 )
				tlc.removeTarget( ce );
		
		return ctl;
	}
}
