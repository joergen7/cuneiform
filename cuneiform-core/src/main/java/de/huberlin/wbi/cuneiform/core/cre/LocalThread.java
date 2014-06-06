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
import org.json.JSONObject;

import de.huberlin.wbi.cuneiform.core.actormodel.Message;
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
		JsonReportEntry entry;
		String line;
		StringBuffer buf;
		File location;
		File callLocation;
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
		long tic, toc;
		JSONObject obj;
		Message msg;
		
		if( log.isDebugEnabled() )
			log.debug( "Starting up local thread for ticket "+invoc.getTicketId()+"." );
		
		
		ticket = invoc.getTicket();
		process = null;
		lockMarker = null;
		successMarker = null;
		script = null;
		stdOut = null;
		stdErr = null;
		try {
					
			if( invoc == null )
				throw new NullPointerException( "Invocation must not be null." );

			callLocation = new File( "." );
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
				
				try( BufferedWriter writer = new BufferedWriter( new FileWriter( reportFile, false ) ) ) {
					
					writer.write( ticket.getExecutableLogEntry().toString() );
					writer.write( '\n' );
				}
				
				
					for( String filename : invoc.getStageInList() ) {
						
							
						destPath = FileSystems.getDefault().getPath( buildDir.getAbsolutePath()+"/"+invoc.getTicketId()+"/"+filename );
	
						if( filename.matches( "([^/].+/)?\\d+_\\d+_[^/]+$" ) ) {
							
							signature = filename.substring( filename.lastIndexOf( '/' )+1, filename.indexOf( '_' ) );
							srcPath = FileSystems.getDefault().getPath( buildDir.getAbsolutePath()+"/"+signature+"/"+filename );				
						}
						else
							
							srcPath = FileSystems.getDefault().getPath( callLocation.getAbsolutePath()+"/"+filename );
						
						Files.createSymbolicLink( destPath, srcPath );
					}
				
				// run script
				processBuilder = new ProcessBuilder( scriptFile.getAbsolutePath() );
				processBuilder.directory( location );
				
				stdOutFile = new File( location.getAbsolutePath()+"/"+Invocation.STDOUT_FILENAME );
				stdErrFile = new File( location.getAbsolutePath()+"/"+Invocation.STDERR_FILENAME );

				processBuilder.redirectOutput( stdOutFile );
				processBuilder.redirectError( stdErrFile );
				
				tic = System.currentTimeMillis();
				process = processBuilder.start();
				exitValue = process.waitFor();
				toc = System.currentTimeMillis();
				
				
				try( BufferedWriter reportWriter = new BufferedWriter( new FileWriter( reportFile, true ) ) ) {
					
					obj = new JSONObject();
					obj.put( JsonReportEntry.LABEL_REALTIME, toc-tic );
					entry = new JsonReportEntry( tic, invoc, null, JsonReportEntry.KEY_INVOC_TIME, obj );
					
					reportWriter.write( entry.toString() );
					reportWriter.write( '\n' );
					
					try( BufferedReader reader = new BufferedReader( new FileReader( stdOutFile ) ) ) {
						
						buf = new StringBuffer();
						while( ( line = reader.readLine() ) != null )
							buf.append( line ).append( '\n' );
						
						
						stdOut = buf.toString();
						
						if( !stdOut.isEmpty() ) {
							entry = new JsonReportEntry( invoc, null, JsonReportEntry.KEY_INVOC_STDOUT, stdOut );
							reportWriter.write( entry.toString() );
							reportWriter.write( '\n' );
						}
					}
					try( BufferedReader reader = new BufferedReader( new FileReader( stdErrFile ) ) ) {
						
						buf = new StringBuffer();
						while( ( line = reader.readLine() ) != null )
							buf.append( line ).append( '\n' );
						
						stdErr = buf.toString();
						if( !stdErr.isEmpty() ) {
						
							entry = new JsonReportEntry( invoc, null, JsonReportEntry.KEY_INVOC_STDERR, stdErr );
							reportWriter.write( entry.toString() );
							reportWriter.write( '\n' );
						}
					}
					
					if( exitValue == 0 ) {
						
						if( !successMarker.createNewFile() )
							throw new IOException( "Could not create success marker." );
					}
					
					if( exitValue != 0 ) {
						
						script = invoc.toScript();
						
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
			
			if( log.isTraceEnabled() )
				log.trace( "Local thread ran through without exception." );
			
		}
		catch( InterruptedException e ) {
			
			if( log.isTraceEnabled() )
				log.trace( "Local thread has been interrupted." );
		}
		catch( Exception e ) {
			
			if( log.isTraceEnabled() )
				log.trace( "Something went wrong. Deleting success marker if present." );
			
			if( successMarker != null )
				if( successMarker.exists() )
					successMarker.delete();
			
			msg = new TicketFailedMsg( cre, ticket, e, script, stdOut, stdErr );
			
			ticketSrc.sendMsg( msg );
			
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
