package de.huberlin.cuneiform.dax.app;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import de.huberlin.cuneiform.dax.compiler.DaxCfCompiler;
import de.huberlin.cuneiform.libdax.parser.DaxLexer;
import de.huberlin.cuneiform.libdax.parser.DaxParser;

public class App {

	public static void main( String[] args ) throws ParseException, FileNotFoundException, IOException {
		
		Options opt;
		GnuParser parser;
		CommandLine cmdline;
		StringBuffer inbuf;
		String line;
		ANTLRInputStream instream;
		DaxLexer lexer;
		TokenStream tokenStream;
		DaxParser dparser;
		ParseTree tree;
		ParseTreeWalker walker;
		DaxCfCompiler daxCfCompiler;
		
		opt = new Options();
		
		opt.addOption( "o", "output", true,
			"The location of the output Cuneiform file. If not specified, the "
			+"compilation result will be printed to standard out." );
		
		opt.addOption( "i", "input", true,
			"The location of the input DAX file. If not specified, the DAX "
			+"input will be read from standard in." );
		
		parser = new GnuParser();
		cmdline = parser.parse( opt, args );
		
		
		// read input
		
		inbuf = new StringBuffer();

		if( cmdline.hasOption( 'i' ) )
			
			try(
				BufferedReader reader =
					new BufferedReader(
						new FileReader( cmdline.getOptionValue( 'i' ) ) ) ) {
				
				while( ( line = reader.readLine() ) != null )
					inbuf.append( line ).append( '\n' );
				
			}
		
		else
			
			try(
				BufferedReader reader =
					new BufferedReader( new InputStreamReader( System.in ) ) ) {
				
				while( ( line = reader.readLine() ) != null )
					inbuf.append( line ).append( '\n' );
			}
		
		// compile

		walker = new ParseTreeWalker();
				
		instream = new ANTLRInputStream( inbuf.toString() );
		
		lexer = new DaxLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		dparser = new DaxParser( tokenStream );

		daxCfCompiler = new DaxCfCompiler();
		
		tree = dparser.adag();
		
		walker.walk( daxCfCompiler, tree );	
		
		// write output
		
		if( cmdline.hasOption( 'o' ) )
			
			try( FileWriter writer = new FileWriter( cmdline.getOptionValue( 'o' ) ) ) {
				
				writer.append( daxCfCompiler.toCuneiform() );
			}
		
		else
			
			try( OutputStreamWriter writer = new OutputStreamWriter( System.out ) ) {
				
				writer.append( daxCfCompiler.toCuneiform() );
			}
	}
}
