package de.huberlin.cuneiform.dax.repl;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.UUID;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import de.huberlin.cuneiform.dax.semanticmodel.DaxSemanticModelListener;
import de.huberlin.cuneiform.libdax.parser.DaxLexer;
import de.huberlin.cuneiform.libdax.parser.DaxParser;
import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class DaxRepl extends BaseRepl {

	public DaxRepl( TicketSrcActor ticketSrc ) {
		super( ticketSrc );
	}
	
	@Override
	public synchronized int interpret( String input ) {
		
		DaxSemanticModelListener adag;
		TopLevelContext tlc;
		
		adag = process( input );
		tlc = adag.toTopLevelContext();
		
		try( BufferedWriter writer = new BufferedWriter( new FileWriter( new File( "/home/jorgen/montage2.cf" ) ) ) ) {
			writer.write( tlc.toString() );
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return interpret( tlc );
		
	}

	@Override
	public void queryFailedPost( UUID queryId, long ticketId, Exception e,
			String script, String stdOut, String stdErr ) {}

	@Override
	public void queryFinishedPost( UUID queryId, CompoundExpr result ) {}

	@Override
	public void queryStartedPost( UUID runId ) {}

	public static DaxSemanticModelListener process( String input ) {
		
		ANTLRInputStream instream;
		DaxLexer lexer;
		CommonTokenStream tokenStream;
		DaxParser parser;
		ParseTree tree;
		ParseTreeWalker walker;
		DaxSemanticModelListener adag;
		
		walker = new ParseTreeWalker();
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new DaxLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new DaxParser( tokenStream );
		parser.removeErrorListeners();
		
		adag = new DaxSemanticModelListener();
		lexer.addErrorListener( adag );
		parser.addErrorListener( adag );

		
		tree = parser.adag();
		
		walker.walk( adag, tree );
		
		return adag;
	}
	

}
