/*******************************************************************************
 * The HTCondorCRE provides a execution base for the functional 
 * workflow language Cuneiform, using the HTCondor software
 * (formerly known as 'Condor').
 *
 * List of Contributors:
 *
 * Björn Groß (HU Berlin)
 * Jörgen Brandt (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project.
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

package de.huberlin.wbi.cuneiform.htcondorcre;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.cre.TicketReadyMsg;
import de.huberlin.wbi.cuneiform.core.invoc.Invocation;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketFailedMsg;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketFinishedMsg;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class CondorCreActor extends BaseCreActor {
	public static final String VERSION = "2015-03-05-4";

	private CondorWatcher watcher;

	private static final String PATH_CENTRALREPO = "repo";
	private static final int WAIT_INTERVAL = 100;
	private static final int MAX_TRIALS = 4;

	private static String verbose = null;
	private static String unused = null;

	private final Path buildDir;
	private final Path centralRepo;

	public CondorCreActor(Path buildDir) throws IOException {
		// creates an actor for default jobs
		if (log.isDebugEnabled()) {
			log.debug("Condor CRE actor created. Version " + VERSION);
		}

		if (buildDir == null) {
			throw new NullPointerException("Build directory must not be null.");
		}
		if (!Files.exists(buildDir)) {
			throw new RuntimeException("Build directory does not exist.");
		}
		if (!Files.isDirectory(buildDir)) {
			throw new RuntimeException("Directory type expected.");
		}
		this.buildDir = buildDir;
		centralRepo = buildDir.resolve(PATH_CENTRALREPO);
		if (!Files.exists(centralRepo)) {
			Files.createDirectories(centralRepo);
		}

		ExecutorService executor;
		executor = Executors.newCachedThreadPool();
		// create a new condor watcher to watch for job status changes
		watcher = new CondorWatcher(this);

		executor.submit(watcher);
		executor.shutdown();
	}

	/**
	 * Decides whether the condor job should be submitted using the verbose
	 * flag. The default option is false.
	 * 
	 * @param value
	 *            Adds the verbose flag if true, removes the flag if false
	 * @see Verbose output - display the created job ClassAd
	 */
	public void setVerbose(boolean value) {
		if (value) {
			verbose = "-verbose";
		} else {
			verbose = null;
		}
	}

	/**
	 * Sets the unused flag settings for the condor job. As a default, causes no
	 * warnings to be issued about user-defined macros not being used within the
	 * submit description file. The meaning reverses (toggles) when the
	 * configuration variable WARN_ON_UNUSED_SUBMIT_FILE_MACROS is set to the
	 * non default value of False. Printing the warnings can help identify
	 * spelling errors of submit description file commands. The warnings are
	 * sent to stderr.
	 * 
	 * @param value
	 *            True to activate the flag, false to delete the flag.
	 */
	public void setUnused(boolean value) {
		if (value) {
			unused = "-unused";
		} else {
			unused = null;
		}
	}

	/**
	 * Used for status messages received from the watcher
	 * 
	 * @param sm
	 */
	protected void processMsg(StatusMessage msg) {

		Actor sender;
		TicketSrcActor ticketSrc;
		Ticket ticket;

		if (log.isDebugEnabled()) {
			log.debug("CondorCRE received status message with status code "
					+ msg.getStatusCode());
		}

		sender = msg.getSender();
		if (sender != watcher) {
			throw new RuntimeException(
					"Status message source different from designated watcher.");
		}

		ticket = msg.getTicket();
		ticketSrc = (TicketSrcActor) msg.getOriginalSender();
		Set<JsonReportEntry> entry = this.gatherReport(msg);

		if (msg.getStatusCode() == StatusMessage.CODEJobTerminated) {
			// create link in central data repository
			Invocation invoc = Invocation.createInvocation(ticket);
			Path location = buildDir.resolve(String.valueOf(invoc.getTicketId()));
			
			try{
				invoc.evalReport(entry);
				if (log.isDebugEnabled()) {
					// for testing purpose only, show the first staged file
					for( NameExpr nameExpr : ticket.getOutputList()){
						log.debug("Ticket "+ticket.getTicketId()+" has at least one output file: "+nameExpr.getId());
						//only display the first file
						log.debug("Total number of output files: "+ticket.getOutputList().size());
						break;
					}
				}
 
				for( String f : invoc.getStageOutList() ) {
					
					Path srcPath = location.resolve( f );
					Path destPath = centralRepo.resolve( f );
					
					if( Files.exists( destPath ) )
						continue;
					
					if( log.isTraceEnabled() )
						log.trace( "Creating link from "+srcPath+" to "+destPath+"." );
					
					Files.createSymbolicLink( destPath, srcPath );
				} 
			}catch (Exception e){
				if (log.isTraceEnabled()) {
					log.trace("Something went wrong.");
				}
				if (log.isDebugEnabled()) {
					// Show full stacktrace
					log.debug(e.getMessage());
				}
				ticketSrc.sendMsg(new TicketFailedMsg(this, ticket, e, null, null, null));
			}
			
			//notify of finished job
			ticketSrc.sendMsg(new TicketFinishedMsg(this, ticket, entry));

			return;
		}

		if (msg.getStatusCode() == StatusMessage.CODEJobAborted) {
			ticketSrc.sendMsg(new TicketFailedMsg(this, ticket, null, null,
					null, null));

			return;
		}

	}

	@Override
	protected void processMsg(TicketReadyMsg msg) {

		Actor sender;
		TicketSrcActor ticketSrc;
		Ticket ticket;

		sender = msg.getSender();
		if (!(sender instanceof TicketSrcActor)) {
			throw new RuntimeException("Ticket source actor expected.");
		}

		ticketSrc = (TicketSrcActor) sender;

		ticket = msg.getTicket();

		if (!ticket.isNormal())
			throw new RuntimeException("Ticket " + ticket.getTicketId()
					+ ": Trying to evaluate ticket that is not ready.");

		if (ticket.isEvaluated())
			throw new RuntimeException(
					"Ticket "
							+ ticket.getTicketId()
							+ ": Trying to evaluate ticket that has already been evaluated.");

		try {
			submitJob(ticketSrc, ticket);
		} catch (InterruptedException e) {

			if (log.isTraceEnabled())
				log.trace("CondorCRE has been interrupted.");
		} catch (Exception e) {
			Exception ex = e;
			if (log.isTraceEnabled()) {
				log.trace("Something went wrong.");
			}
			if (log.isDebugEnabled()) {
				// Show full stacktrace
				log.debug(ex.getMessage());
			}

			ticketSrc.sendMsg(new TicketFailedMsg(this, ticket, ex, null, null,
					null));

		}

	}

	@Override
	protected void shutdown() {
		// nothing
	}

	private Set<JsonReportEntry> gatherReport(StatusMessage msg) {
		Set<JsonReportEntry> report;
		JsonReportEntry entry;
		Charset cs;
		String line;
		Invocation invoc = Invocation.createInvocation(msg.getTicket());
		Path location = buildDir.resolve(String.valueOf(invoc.getTicketId()));
		Path reportFile = location.resolve(Invocation.REPORT_FILENAME);

		report = new HashSet<>();
		cs = Charset.forName( "UTF-8" );
		try (BufferedReader reader = Files.newBufferedReader(reportFile, cs)) {

			while ((line = reader.readLine()) != null) {

				line = line.trim();

				if (line.isEmpty())
					continue;

				entry = new JsonReportEntry(line);

				// If the report comes from the hard cache then the run id
				// is different from the run id of this invocation. This is
				// corrected here.
				entry.setRunId(invoc.getRunId());

				report.add(entry);
			}
			invoc.evalReport(report);

		} catch (Exception e) {
			if (log.isDebugEnabled()) {
				log.debug("Exception while gathering json report data: " + e.getMessage() + ".");
			}
		}

		return report;
		
	}

	/**
	 * Submits a job to condor
	 */
	private void submitJob(TicketSrcActor ticketSrc, Ticket ticket)
			throws Exception {

		if (ticketSrc == null) {
			throw new NullPointerException("Ticket source must not be null.");
		}
		if (ticket == null) {
			throw new NullPointerException("Ticket must not be null.");
		}

		// Setup
		Invocation invoc = Invocation.createInvocation(ticket);
		Path location = buildDir.resolve(String.valueOf(invoc.getTicketId()));
		Files.createDirectories( location );
		if (log.isDebugEnabled()) {
			log.debug("Build directory for ticket: " + location.toString());
		}
		Path submitFile = location.resolve("cfsubmitfile");
		String script = invoc.toScript();
		Charset cs = Charset.forName("UTF-8");
		Path scriptFile = invoc.getExecutablePath( location );
		Path reportFile = location.resolve(Invocation.REPORT_FILENAME);
		
		/**
		 * Log created by the condor job, used to monitor the job. Each job
		 * should have it's own log file for monitoring  */
		java.util.Date date = new java.util.Date();
		Path cjLogFile = location.resolve((date.getTime()) + "cjlog.txt");

		if (log.isDebugEnabled()) {
			log.debug("Starting up condor_submit for ticket "
					+ invoc.getTicketId() + ".");
		}

		// add input files
		Set<String> inputs = new HashSet<>();
		for (String filename : invoc.getStageInList()) {
			if( filename.charAt( 0 ) == '/' ){						
				throw new UnsupportedOperationException( "Absolute path encountered '"+filename+"'." );
			}	
			Path srcPath = centralRepo.resolve( filename );
			Path destPath = location.resolve( filename );
			Path callLocation = Paths.get( System.getProperty( "user.dir" ) );
			
			if( !Files.exists( srcPath ) ) {
				
				srcPath = callLocation.resolve( filename );
				if( log.isTraceEnabled() )
					log.trace( "Resolving relative path '"+srcPath+"'." );
			}
			else

				if( log.isTraceEnabled() )
					log.trace( "Resolving path to central repository '"+srcPath+"'." );
			
			if( log.isTraceEnabled() )
				log.trace( "Trying to create symbolic link from '"+srcPath+"' to '"+destPath+"'." );

			if( !Files.exists( destPath.getParent() ) )
				Files.createDirectories( destPath.getParent() );
			
			Files.createSymbolicLink( destPath, srcPath );
			//add the path to the set to insert it in the submitfile later on
			inputs.add(destPath.toString());
		}

		try {
			Files.createFile(scriptFile, 
					PosixFilePermissions.asFileAttribute(
							PosixFilePermissions.fromString("rwxr-xrwx" ) ) );
			if (log.isDebugEnabled()) {
				log.debug("Scriptfile for ticket " + invoc.getTicketId() + " has successfully been created at "+scriptFile.toString());
			}
			scriptFile.toFile().setWritable(true, false);
		} catch (FileAlreadyExistsException faee) {
			// if file already exists do nothing
			if (log.isDebugEnabled()) {
				log.debug("Scriptfile for ticket " + invoc.getTicketId() + " already exists.");
			}
		}

		// write executable script
		try (BufferedWriter writer = Files.newBufferedWriter(scriptFile, cs,
				StandardOpenOption.CREATE)) {
			writer.write(script);
		}

		// write executable log entry
		try (BufferedWriter writer = Files.newBufferedWriter(reportFile, cs,
				StandardOpenOption.CREATE)) {
			writer.write(ticket.getExecutableLogEntry().toString());
			writer.write('\n');
		}

		try {
			Files.createFile(submitFile, PosixFilePermissions
					.asFileAttribute(PosixFilePermissions
							.fromString("rwxr-x---")));
		} catch (FileAlreadyExistsException faee) {
			if (log.isDebugEnabled()) {
				log.debug("submitFile " + submitFile.toString()
						+ " already exists.");
			}
		}
		
		Path condorError = location.resolve("condor_stderr.txt");
		Path condorOutput = location.resolve("condor_stdout.txt");
		try {
			condorError = Files.createFile(condorError, PosixFilePermissions
					.asFileAttribute(PosixFilePermissions
							.fromString("rwxr-xrw-")));
			condorError.toFile().setWritable(true, false);
			condorOutput = Files.createFile(condorOutput, PosixFilePermissions
					.asFileAttribute(PosixFilePermissions
							.fromString("rwxr-xrw-")));
			condorOutput.toFile().setWritable(true, false);
		} catch (FileAlreadyExistsException faee) {
			if (log.isDebugEnabled()) {
				log.debug("condorError or condorOutput for "+ ticket.getTicketId() +" already exists.");
			}
		}
		
		
		try (BufferedWriter writer = Files.newBufferedWriter(submitFile, cs,
				StandardOpenOption.CREATE)) {
			// name of the executable script
			writer.write("executable = " + scriptFile.toString());
			writer.write('\n');
			writer.write("universe = vanilla");
			writer.write('\n');
			writer.write("run_as_owner = True");
			writer.write('\n');
			writer.write("log = " + cjLogFile.toString());
			writer.write('\n');
			writer.write("output = " + condorOutput.toString());
			writer.write('\n');
			writer.write("error = " + condorError.toString());
			writer.write('\n');
			//TODO: Transfer files or not?
			writer.write("should_transfer_files = YES \n");
			writer.write("when_to_transfer_output = ON_EXIT \n");
			// inputfiles
			if (!inputs.isEmpty()) {
				writer.write("transfer_input_files = ");
				boolean successor = false;
				for (String file : inputs) {
					if(successor){
						writer.write("," );
					}else{
						successor = true;
					}
					writer.write(file);
				}
				// TODO: is the ',' causing problems?
				writer.write('\n');
			}
			// at last add the job to the queue
			writer.write('\n');
			writer.write("initialdir = "+location.toString());
			writer.write('\n');
			writer.write("queue");
		}
		// run script
		// TODO: Add command line arguments here
		ArrayList<String> command = new ArrayList<String>();
		command.add("condor_submit");
		command.add(submitFile.toString());
		if (verbose != null) {
			command.add(verbose);
		}
		ProcessBuilder processBuilder = new ProcessBuilder(
				command.toArray(new String[command.size()]));
		/*
		 * Sets this process builder's working directory. 
		 * Subprocesses subsequently started by this object's start() method will 
		 * use this as their working directory. The argument may be null -- this 
		 * means to use the working directory of the current Java process, usually the 
		 * directory named by the system property user.dir, as the working directory 
		 * of the child process. */
		processBuilder.directory(location.toFile());

		Path stdOutFile = location.resolve( Invocation.STDOUT_FILENAME );
		Path stdErrFile = location.resolve( Invocation.STDERR_FILENAME );

		processBuilder.redirectOutput( stdOutFile.toFile() );
		processBuilder.redirectError( stdErrFile.toFile() );

		int trial = 1;
		boolean suc = false;
		Exception ex = null;

		do {
			try {
				processBuilder.start();
				suc = true;
			} catch (IOException e) {
				ex = e;
				if (log.isWarnEnabled())
					log.warn("Unable to start process on trial " + (trial++)
							+ " Waiting " + WAIT_INTERVAL + "ms: "
							+ e.getMessage());
				Thread.sleep(WAIT_INTERVAL);
			}
		} while (suc == false && trial <= MAX_TRIALS);

		if (suc == false) {
			// Couldn't find condor_submit or another error occured - end
			// execution
			ticketSrc.sendMsg(new TicketFailedMsg(this, ticket, ex, script,
					null, null));
			return;
		}

		// worked, add job to the watchlist
		StatusMessage sm = new StatusMessage(this, ticket.getRunId(),
				cjLogFile, ticket, ticketSrc);
		watcher.processMsg(sm);

	}

}
