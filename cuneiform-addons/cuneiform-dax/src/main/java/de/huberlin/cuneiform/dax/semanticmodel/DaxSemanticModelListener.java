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

package de.huberlin.cuneiform.dax.semanticmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.huberlin.cuneiform.libdax.parser.DaxBaseListener;
import de.huberlin.cuneiform.libdax.parser.DaxParser;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;

public class DaxSemanticModelListener extends DaxBaseListener implements ANTLRErrorListener {

	private final Map<String,DaxJob> idJobMap;
	private final Map<String,DaxJob> fileJobMap;
	private final List<DaxFilename> fileList;
	private final List<DaxFilename> outputList;
	private DaxFilename filename;
	private DaxJobUses jobUses;
	private DaxJob job;
	private final Log log;
	
	public DaxSemanticModelListener() {
		idJobMap = new HashMap<>();
		fileJobMap = new HashMap<>();
		fileList = new ArrayList<>();
		outputList = new ArrayList<>();
		log = LogFactory.getLog( DaxSemanticModelListener.class );
	}
	
	@Override
	public void enterFilename( @NotNull DaxParser.FilenameContext ctx ) {
		filename = new DaxFilename();		
	}
	
	@Override
	public void exitFilename( @NotNull DaxParser.FilenameContext ctx ) {

		if( job == null ) {

			fileList.add( filename );
			if( filename.isLinkOutput() )
				outputList.add( filename );
		}
		else
			job.addFilenameArg( filename );

		filename = null;
	}
	
	@Override
	public void enterFilenamePropFile( @NotNull DaxParser.FilenamePropFileContext ctx ) {
		filename.setFile( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterFilenamePropLinkInput( @NotNull DaxParser.FilenamePropLinkInputContext ctx ) {
		filename.setLinkInput();
	}
	
	@Override
	public void enterFilenamePropLinkOutput( @NotNull DaxParser.FilenamePropLinkOutputContext ctx ) {
		filename.setLinkOutput();
	}

	@Override
	public void enterFilenamePropLinkInout( @NotNull DaxParser.FilenamePropLinkInoutContext ctx ) {
		filename.setLinkInout();
	}

	@Override
	public void enterJob( @NotNull DaxParser.JobContext ctx ) {
		job = new DaxJob();
	}

	@Override
	public void exitJob( @NotNull DaxParser.JobContext ctx ) {
		job = null;
	}
	
	@Override
	public void exitJobPropId( @NotNull DaxParser.JobPropIdContext ctx ) {
		
		String id;
		
		id = getString( ctx.STRING() );
		
		idJobMap.put( id, job );
	}

	@Override
	public void enterJobPropName( @NotNull DaxParser.JobPropNameContext ctx ) {
		job.setName( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterArgumentElPlain( @NotNull DaxParser.ArgumentElPlainContext ctx ) {
		job.addPlainArg( ctx.ARG().getText() );
	}
	
	@Override
	public void enterJobElUses( @NotNull DaxParser.JobElUsesContext ctx ) {
		jobUses = new DaxJobUses();
		job.addJobUses( jobUses );
	}
	
	@Override
	public void exitJobElUses( @NotNull DaxParser.JobElUsesContext ctx ) {
		
		if( jobUses.isLinkOutput() )
			fileJobMap.put( jobUses.getFile(), job );
		
		jobUses = null;
	}
	
	@Override
	public void enterJobUsesPropFile( @NotNull DaxParser.JobUsesPropFileContext ctx ) {
		jobUses.setFile( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterJobUsesPropLinkInput( @NotNull DaxParser.JobUsesPropLinkInputContext ctx ) {
		jobUses.setLinkInput();
	}

	@Override
	public void enterJobUsesPropLinkOutput( @NotNull DaxParser.JobUsesPropLinkOutputContext ctx ) {
		jobUses.setLinkOutput();
	}
	
	@Override
	public void enterJobUsesPropExecutable( @NotNull DaxParser.JobUsesPropExecutableContext ctx ) {
		jobUses.setExecutable( true );
	}

	@Override
	public void enterJobUsesPropOptionalTrue( @NotNull DaxParser.JobUsesPropOptionalTrueContext ctx ) {
		jobUses.setOptional( true );
	}

	
	@Override
	public void enterChild( @NotNull DaxParser.ChildContext ctx ) {
		
		String id;
		
		id = getString( ctx.STRING() );
		
		job = idJobMap.get( id );
		if( job == null )
			throw new NullPointerException( "Could not retrieve referenced child job." );
	}
	
	@Override
	public void exitChild( @NotNull DaxParser.ChildContext ctx ) {
		job = null;
	}
	
	@Override
	public void enterParent( @NotNull DaxParser.ParentContext ctx ) {
		
		DaxJob parent;
		String id;
		
		id = getString( ctx.STRING() );
		
		parent = idJobMap.get( id );
		if( job == null )
			throw new NullPointerException( "Could not retrieve referenced parent job." );
		
		parent.addChild( job );
		job.addParent( parent );
	}

	public TopLevelContext toTopLevelContext() {
		
		TopLevelContext tlc;
		CompoundExpr ce;
		ApplyExpr applyExpr;
		
		tlc = new TopLevelContext();
		ce = new CompoundExpr();
		tlc.addTarget( ce );
		
		
		// gather input contract
		for( DaxFilename input : fileList )
			tlc.putAssign( input.getNameExpr(), new CompoundExpr( new StringExpr( input.getFile() ) ) );
		
		// gather jobs
		for( DaxJob j : idJobMap.values() ) {
			
			for( DaxFilename f : j.getOutputJobUsesSet() ) {
				
				applyExpr = j.getApplyExpr( f, fileList );
				tlc.putAssign( f.getNameExpr(), new CompoundExpr( applyExpr ) );
			}
		}
		
		// gather output contract
		for( DaxFilename output : outputList )
			ce.addSingleExpr( output.getNameExpr() );
		
		return tlc;
	}
	
	public static int getInt( TerminalNode node ) {
		return Integer.valueOf( getString( node ) );
	}

	public static String getString( TerminalNode node ) {
		
		String s;
		
		s = node.getText();
		s = s.substring( 1, s.length()-1 );
		
		return s;
	}
	
	public static URL getUrl( TerminalNode node ) {
		
		try {
			return new URL( getString( node ) );
		}
		catch( MalformedURLException e ) {
			throw new RuntimeException( e );
		}
	}

	@Override
	public void reportAmbiguity( Parser arg0, DFA arg1, int arg2, int arg3,
			boolean arg4, BitSet arg5, ATNConfigSet arg6 ) {
		
		if( log.isDebugEnabled() )
			log.debug( "Ambiguity detected." );

	}

	@Override
	public void reportAttemptingFullContext( Parser arg0, DFA arg1, int arg2,
			int arg3, BitSet arg4, ATNConfigSet arg5 ) {
		
		if( log.isDebugEnabled() )
			log.debug( "Attempting full context." );

	}

	@Override
	public void reportContextSensitivity( Parser arg0, DFA arg1, int arg2,
			int arg3, int arg4, ATNConfigSet arg5) {
		
		if( log.isDebugEnabled() )
			log.debug( "Context sensitivity detected." );
	}

	@Override
	public void syntaxError( Recognizer<?, ?> arg0, Object offendingSymbol, int line,
			int charPositionInLine, String msg, RecognitionException arg5 ) {
		
		String near;
		
		near = null;
		if( offendingSymbol != null )
			near = ( ( Token )offendingSymbol ).getText();
		
		throw new ParseException( line, charPositionInLine, near, msg );
	}

}
