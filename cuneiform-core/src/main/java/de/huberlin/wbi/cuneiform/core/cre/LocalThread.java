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

package de.huberlin.wbi.cuneiform.core.cre;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.huberlin.wbi.cuneiform.core.invoc.Invocation;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketFailedMsg;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketFinishedMsg;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class LocalThread implements Runnable {

	private final Invocation invoc;
	private final Log log;
	private final File buildDir;
	private final TicketSrcActor ticketSrc;
	private final BaseCreActor cre;
	
	public LocalThread( TicketSrcActor ticketSrc, BaseCreActor cre, Ticket ticket, File buildDir ) {
		
		if( buildDir == null )
			throw new NullPointerException( "Build directory must not be null." );
		
		if( !buildDir.exists() )
			throw new RuntimeException( "Build directory does not exist." );
		
		if( !buildDir.isDirectory() )
			throw new RuntimeException( "Directory expected." );
		
		if( cre == null )
			throw new NullPointerException( "CRE actor must not be null." );
		
		if( ticketSrc == null )
			throw new NullPointerException( "Ticket source must not be null." );
		
		this.ticketSrc = ticketSrc;
		this.cre = cre;
		this.buildDir = buildDir;

		invoc = Invocation.createInvocation( ticket );
		log = LogFactory.getLog( LocalThread.class );
	}
	
	@Override
	public void run() {
		
		File scriptFile;
		Process process;
		int exitValue;
		Set<JsonReportEntry> report;
		String line;
		String[] arg;
		String value;
		StringBuffer buf;
		File location;
		File reportFile;
		File stdErrFile;
		File stdOutFile;
		String signature;
		Path srcPath, destPath;
		File successMarker;
		File lockMarker;
		ProcessBuilder processBuilder;
		Ticket ticket;
		String script, stdOut, stdErr;
		
		if( log.isDebugEnabled() )
			log.debug( "Starting up local thread for ticket "+invoc.getTicketId()+"." );
		
		
		
		process = null;
		lockMarker = null;
		successMarker = null;
		script = null;
		stdOut = null;
		stdErr = null;
		try {
					
			if( invoc == null )
				throw new NullPointerException( "Invocation must not be null." );

			
			location = new File( buildDir.getAbsolutePath()+"/"+invoc.getTicketId() );
			lockMarker = new File( location.getAbsolutePath()+"/"+Invocation.LOCK_FILENAME );
			successMarker = new File( location.getAbsolutePath()+"/"+Invocation.SUCCESS_FILENAME );
			reportFile = new File( location.getAbsolutePath()+"/"+Invocation.REPORT_FILENAME );
			
			if( lockMarker.exists() )
				throw new IOException( "Lock held on ticket "+invoc.getTicketId() );
			
			if( !successMarker.exists() ) {
				

				if( location.exists() )
					FileUtils.deleteDirectory( location );
				
				FileUtils.forceMkdir( location );
				
				if( !lockMarker.createNewFile() )
					throw new IOException( "Could not create lock on ticket "+invoc.getTicketId() );
							
				scriptFile = new File( location.getAbsolutePath()+"/"+Invocation.SCRIPT_FILENAME );
				
				try( BufferedWriter writer = new BufferedWriter( new FileWriter( scriptFile, false ) ) ) {
					
					// write away script
					writer.write( invoc.toScript() );
				}
				
				scriptFile.setExecutable( true );
				
				
				// write executable log entry
				
				ticket = invoc.getTicket();
				try( BufferedWriter writer = new BufferedWriter( new FileWriter( reportFile, false ) ) ) {
					
					writer.write( ticket.getExecutableLogEntry().toString() );
					writer.write( '\n' );
				}
				
				for( String filename : invoc.getStageInList() ) {
					
					
	
					if( filename.charAt( 0 ) != '/' && filename.indexOf( '_' ) >= 0 ) {
	
						signature = filename.substring( 0, filename.indexOf( '_' ) );
						
						srcPath = FileSystems.getDefault().getPath( buildDir.getAbsolutePath()+"/"+signature+"/"+filename );				
						destPath = FileSystems.getDefault().getPath( buildDir.getAbsolutePath()+"/"+invoc.getTicketId()+"/"+filename );
						Files.createSymbolicLink( destPath, srcPath );
					}
				}
				
				buf = new StringBuffer();
				
				buf.append( "{" )
				.append( JsonReportEntry.ATT_TIMESTAMP ).append( ":" ).append( System.currentTimeMillis() ).append( "," )
				.append( JsonReportEntry.ATT_RUNID ).append( ":\"" ).append( invoc.getRunId() ).append( "\"," )
				.append( JsonReportEntry.ATT_TASKID ).append( ":" ).append( invoc.getTaskId() ).append( "," );
				
				if( invoc.hasTaskName() )
					buf.append( JsonReportEntry.ATT_TASKNAME ).append( ":\"" ).append( invoc.getTaskName() ).append( "\"," );
				
				buf.append( JsonReportEntry.ATT_LANG ).append( ":\"" ).append( invoc.getLangLabel() ).append( "\"," )
				.append( JsonReportEntry.ATT_INVOCID ).append( ":" ).append( invoc.getTicketId() ).append( "," )
				.append( JsonReportEntry.ATT_KEY ).append( ":\"" ).append( JsonReportEntry.KEY_INVOC_TIME ).append( "\"," )
				.append( JsonReportEntry.ATT_VALUE ).append( ":" )
				.append( "{\"realTime\":%e,\"userTime\":%U,\"sysTime\":%S," )
				.append( "\"maxResidentSetSize\":%M,\"avgResidentSetSize\":%t," )
				.append( "\"avgDataSize\":%D,\"avgStackSize\":%p,\"avgTextSize\":%X," )
				.append( "\"nMajPageFault\":%F,\"nMinPageFault\":%R," )
				.append( "\"nSwapOutMainMem\":%W,\"nForcedContextSwitch\":%c," )
				.append( "\"nWaitContextSwitch\":%w,\"nIoRead\":%I,\"nIoWrite\":%O," )
				.append( "\"nSocketRead\":%r,\"nSocketWrite\":%s,\"nSignal\":%k}}" );
	
				arg = new String[] {
						"/usr/bin/time",
						"--quiet",
						"-a",
						"-o",
						location.getAbsolutePath()+"/"+Invocation.REPORT_FILENAME,
						"-f",
						buf.toString(),
						scriptFile.getAbsolutePath() };
				
				
				
				// run script
				processBuilder = new ProcessBuilder( arg );
				processBuilder.directory( location );
				
				stdOutFile = new File( location.getAbsolutePath()+"/"+Invocation.STDOUT_FILENAME );
				stdErrFile = new File( location.getAbsolutePath()+"/"+Invocation.STDERR_FILENAME );

				processBuilder.redirectOutput( stdOutFile );
				processBuilder.redirectError( stdErrFile );
				
				
				process = processBuilder.start();
								
				exitValue = process.waitFor();

				
				
				try( BufferedWriter reportWriter = new BufferedWriter( new FileWriter( reportFile, true ) ) ) {
							
					try( BufferedReader reader = new BufferedReader( new FileReader( stdOutFile ) ) ) {
						
						buf = new StringBuffer();
						while( ( line = reader.readLine() ) != null )
							buf.append( line.replaceAll( "\\\\", "\\\\\\\\" ).replaceAll( "\"", "\\\"" ) ).append( '\n' );
						
						
						value = buf.toString();
						if( !value.isEmpty() )
						
						reportWriter.write( new JsonReportEntry( invoc, null, JsonReportEntry.KEY_INVOC_STDOUT, value ).toString() );
					}
					try( BufferedReader reader = new BufferedReader( new FileReader( stdErrFile ) ) ) {
						
						buf = new StringBuffer();
						while( ( line = reader.readLine() ) != null )
							buf.append( line.replaceAll( "\\\\", "\\\\\\\\" ).replaceAll( "\"", "\\\"" ) ).append( '\n' );
						
						value = buf.toString();
						if( !value.isEmpty() )
						
						reportWriter.write( new JsonReportEntry( invoc, null, JsonReportEntry.KEY_INVOC_STDERR, value ).toString() );
					}
					
					if( exitValue == 0 ) {
						
						if( !successMarker.createNewFile() )
							throw new IOException( "Could not create success marker." );
					}
					
					if( exitValue != 0 ) {
						
						script = invoc.toScript();
						
						buf = new StringBuffer();
						try( BufferedReader reader = new BufferedReader( new FileReader( stdOutFile ) ) ) {
							
							while( ( line = reader.readLine() ) != null )
								buf.append( line ).append( '\n' );
						}
						stdOut = buf.toString();

					
						buf = new StringBuffer();
						try( BufferedReader reader = new BufferedReader( new FileReader( stdErrFile ) ) ) {
							
							while( ( line = reader.readLine() ) != null )
								buf.append( line ).append( '\n' );
						}
						stdErr = buf.toString();
						
						
						ticketSrc.sendMsg( new TicketFailedMsg( cre, ticket, null, script, stdOut, stdErr ) );

						lockMarker.delete();
						return;
						
					}

			
				}
				
			}
			
			// gather report
			report = new HashSet<>();
			try(
					
				BufferedReader reader =
					new BufferedReader( new FileReader( reportFile ) ) ) {
				
				while( ( line = reader.readLine() ) != null ) {
					
					line = line.trim();
					
					if( line.isEmpty() )
						continue;
					
					report.add( new JsonReportEntry( line ) );
				}
				
			}
			
			invoc.evalReport( report );
	
			ticketSrc.sendMsg( new TicketFinishedMsg( cre, invoc.getTicket(), report ) );
			
		}
		catch( InterruptedException e ) {}
		catch( Exception e ) {
			
			if( successMarker != null )
				if( successMarker.exists() )
					successMarker.delete();
			
			ticketSrc.sendMsg( new TicketFailedMsg( cre, invoc.getTicket(), e, script, stdOut, stdErr ) );
		}
		finally {
			
			if( lockMarker != null )
				if( lockMarker.exists() )
					lockMarker.delete();
			
			if( log.isDebugEnabled() )
				log.debug( "Stopping local thread for ticket "+invoc.getTicketId()+"." );

			if( process != null )
				process.destroy();
		}
	}		
}
