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

package de.huberlin.wbi.cuneiform.cmdline.main;

import de.huberlin.cuneiform.dax.repl.DaxRepl;
import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.cre.LocalCreActor;
import de.huberlin.wbi.cuneiform.core.cre.LocalThread;
import de.huberlin.wbi.cuneiform.core.invoc.Invocation;
import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.repl.CmdlineRepl;
import de.huberlin.wbi.cuneiform.core.repl.InteractiveRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;
import de.huberlin.wbi.cuneiform.htcondorcre.CondorCreActor;
import org.apache.commons.cli.*;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
	
	private static final String FORMAT_CF = "cf";
	private static final String FORMAT_DAX = "dax";
	private static final String PLATFORM_LOCAL = "local";
	private static final String PLATFORM_HTCONDOR = "htcondor";


	public static final int MAX_TRANSFER = 1073741824; // no more than 1G of total transfer

	private static String platform;
	private static String format;
	private static Path[] inputFileVector;
	

	public static void main( String[] args ) throws IOException, ParseException, InterruptedException, NotDerivableException {
		
		CommandLine cmd;
		Options opt;
		BaseRepl repl;
		BaseCreActor cre;
		Path sandbox;
		ExecutorService executor;
		TicketSrcActor ticketSrc;
		JsonSummary summary;
		Path summaryPath;
		Log statLog;
		int nthread;
		Path workDir;
		
		statLog = LogFactory.getLog( "statLogger" );
		
		executor = Executors.newCachedThreadPool();
		try {
		
			opt = getOptions();
			cmd = parse( args, opt );
			config( cmd );
			
			if( cmd.hasOption( 'h' ) ) {
				
				System.out.println(
					"CUNEIFORM - A Functional Workflow Language\nversion "
					+BaseRepl.LABEL_VERSION+" build "+BaseRepl.LABEL_BUILD );
				new HelpFormatter().printHelp(
					"java -jar cuneiform.jar [OPTION]*", opt );
				
				return;
			}
			
			if( cmd.hasOption( 'r' ) )
				Invocation.putLibPath( ForeignLambdaExpr.LANGID_R, cmd.getOptionValue( 'r' ) );
			
			if( cmd.hasOption( 'y' ) )
				Invocation.putLibPath(  ForeignLambdaExpr.LANGID_PYTHON, cmd.getOptionValue( 'y' ) );
			
			if( cmd.hasOption( 'l' ) )
				sandbox = Paths.get( cmd.getOptionValue( "l" ) );
			else
				sandbox = Paths.get( System.getProperty( "user.home" ) ).resolve( ".cuneiform" );
			
			sandbox = sandbox.toAbsolutePath();
			
			if( cmd.hasOption( 'c' ) )
				LocalThread.deleteIfExists( sandbox );
			
			if( cmd.hasOption( 't' ) )
				nthread = Integer.valueOf( cmd.getOptionValue( 't' ) );
			else
				nthread = Runtime.getRuntime().availableProcessors();
			
			if( cmd.hasOption( 'w' ) )
				workDir = Paths.get( cmd.getOptionValue( 'w' ) );
			else
				workDir = Paths.get( System.getProperty( "user.dir" ) );
			
			workDir = workDir.toAbsolutePath();
			
			

						
			switch( platform ) {
			
				case PLATFORM_LOCAL :
					
					if( !Files.exists( sandbox ) )
						Files.createDirectories( sandbox );
					
					
					cre = new LocalCreActor( sandbox, workDir, nthread );
					break;
					
				case PLATFORM_HTCONDOR :
					
					if( !Files.exists( sandbox ) )
						Files.createDirectories( sandbox );

					if (cmd.hasOption('m')) {  // MAX_TRANSFER SIZE
						String maxTransferSize = cmd.getOptionValue('m');
						try {
							cre = new CondorCreActor(sandbox, maxTransferSize);
						} catch (Exception e) {
							System.out.println(
									"INVALID '-m' option value: " + maxTransferSize +
									"\n\nCUNEIFORM - A Functional Workflow Language\nversion "
											+ BaseRepl.LABEL_VERSION + " build " + BaseRepl.LABEL_BUILD);
							new HelpFormatter().printHelp(
									"java -jar cuneiform.jar [OPTION]*", opt);

							return;
						}
					}
					else{
						cre = new CondorCreActor(sandbox);
					}
					
					break;
					
				default : throw new RuntimeException( "Platform not recognized." );
			}
	
			executor.submit( cre );
			ticketSrc = new TicketSrcActor( cre );
			executor.submit( ticketSrc );
			executor.shutdown();
			
			switch( format ) {
			
				case FORMAT_CF :
					
					if( cmd.hasOption( "i" ) )
						repl = new InteractiveRepl( ticketSrc, statLog );
					else
						repl = new CmdlineRepl( ticketSrc, statLog );
					break;
					
				case FORMAT_DAX :
					repl = new DaxRepl( ticketSrc, statLog );
					break;
					
				default :
					throw new RuntimeException( "Format not recognized." );
			}
			
			if( cmd.hasOption( "i" ) ) {
				
				// run in interactive mode
				BaseRepl.run( repl );
				
				return;
			}

			// run in quiet mode
			
			if( inputFileVector.length > 0 )
				
				for( Path f : inputFileVector )
					repl.interpret( readFile( f ) );
			
			else
				repl.interpret( readStdIn() );
			
				
			Thread.sleep( 3*Actor.DELAY );
			while( repl.isBusy() )
				Thread.sleep( Actor.DELAY );

			if( cmd.hasOption( "s" ) ) {
				
				summary = new JsonSummary( ticketSrc.getRunId(), sandbox, repl.getAns() );
				summaryPath = Paths.get( cmd.getOptionValue( "s" ) );
				summaryPath = summaryPath.toAbsolutePath();
				try( BufferedWriter writer = Files.newBufferedWriter( summaryPath, Charset.forName( "UTF-8" ) ) ) {
					
					writer.write( summary.toString() );
				}
				
			}
				
		
		}
		finally {
			executor.shutdownNow();
		}

	}
	
	public static void config( CommandLine cmd ) {
		
		String[] fileVector;
		Path f;
		String s;
		int i, n;
		
		fileVector = cmd.getArgs();
		n = fileVector.length;
		inputFileVector = new Path[ n ];
		
		for( i = 0; i < n; i++ ) {
			
			s = fileVector[ i ];
			f = Paths.get( s );
			
			inputFileVector[ i ] = f;
		}
		
		if( cmd.hasOption( 'p' ) )
			platform = cmd.getOptionValue( 'p' );
		else
			platform = PLATFORM_LOCAL;
		
		if( cmd.hasOption( 'f' ) )
			format = cmd.getOptionValue( 'f' );
		else
			format = FORMAT_CF;
	}
	
	public static CommandLine parse( String[] args, Options opt ) throws ParseException {
		
		GnuParser parser;
		CommandLine cmd;
		
		parser = new GnuParser();		
		cmd = parser.parse( opt, args );
		return cmd;
	}
	
	public static Options getOptions() {
		
		Options opt;
		
		opt = new Options();
		
		opt.addOption( "c", "clean", false, "Clear local cache before start." );
		
		opt.addOption( "f", "format", true,
				"The format of the input file. Must be either '"+FORMAT_CF+"' for Cuneiform or '"+FORMAT_DAX+"' for Pegasus DAX. Default is '"+FORMAT_CF+"'." );
			
		opt.addOption( "h", "help", false, "Output help." );
		
		opt.addOption( "i", "interactive", false, "Start an interactive REPL." );
		
		opt.addOption( "l", "localcache", true, "Path to the local cache. Defaults to '~/.cuneiform'." );
		
		opt.addOption( "p", "platform", true,
				"The platform to run. Currently available platforms are '"+PLATFORM_LOCAL+"' and '"+PLATFORM_HTCONDOR+"'. Default is '"+PLATFORM_LOCAL+"'." );

		opt.addOption("m", "maxtransfer", true,
				"Optional. Takes effect only if the platform is '"+PLATFORM_HTCONDOR+"'. Default is 1G. Controls max input file transfer.  If total input files exceed this limit, condor job is run in the local universe. Takes a number in bytes by default or specified units [G|M|K], i.e. 250000000 or 2G or 1000M or 234K");

		opt.addOption( "r", "rlib", true, "Optional. The directory in which custom R libraries are installed. If set, the directory is added to R's libPath list." );
		
		opt.addOption( "s", "summary", true,
				"The name of a JSON summary file. No file is created if this parameter is not specified." );
			
		opt.addOption( "t", "threads", true, "The number of threads to use. Defaults to the number of CPU cores available on this machine. Takes effect only if the platform is '"+PLATFORM_LOCAL+"'." );
		
		opt.addOption( "w", "workdir", true, "Optional. The working directory from which to look for local data. The default is the current working directory from which Cuneiform was called." );
		
		opt.addOption( "y", "pylib", true, "Optional. The directory in which custom Python libraries are installed. If set, the directory is added to Python's system path list." );
		
		
		return opt;
		
	}
	
	public static String readFile( Path f ) throws FileNotFoundException, IOException {
		
		String line;
		StringBuffer buf;
		
		buf = new StringBuffer();
		try( BufferedReader reader = Files.newBufferedReader( f, Charset.forName( "UTF-8" ) ) ) {
			
			while( ( line = reader.readLine() ) != null )
				buf.append( line ).append( '\n' );
		}
		
		return buf.toString();
		
	}
	
	public static String readStdIn() throws IOException {
		
		StringBuffer buf;
		String line;
		
		buf = new StringBuffer();
		try( BufferedReader reader = new BufferedReader( new InputStreamReader( System.in ) ) ) {
			
			while( ( line = reader.readLine() ) != null )
				buf.append( line ).append( '\n' );
		}
		
		return buf.toString();
	}
}
