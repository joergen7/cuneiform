package de.huberlin.wbi.cuneiform.htcondorcre;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.actormodel.Message;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;


public class CondorWatcher extends Actor {
	public static final String VERSION = "2015-02-17-1";
	
	private CondorCreActor caller = null;
	private ArrayList<StatusMessage> messages = new ArrayList<StatusMessage>();
	
	private static final int WAIT_INTERVAL = 100;
	private static final int MAX_TRIALS = 4;
	
	//polled in intervall condor_wait für alle angegebenen jobs und sendet nachricht
	//an cre, sobald ein job finished oder failed ist
	public CondorWatcher(CondorCreActor cre){
		if( log.isDebugEnabled() ){
			log.debug( "Condor Watcher actor created, version "+VERSION );
		}
		caller = cre;
	}
	
	
	
	@Override
	protected synchronized void processMsg( Message msg ){
		if( msg instanceof StatusMessage ) {
			StatusMessage sm = (StatusMessage) msg;
			messages.add(sm);
			
			debug( "Condor Watcher received a new status message with job path "+sm.getJobpath().toString() );
			
			return;
		}
		
		throw new RuntimeException( "Message type not recognized." );
	}
	
	//executed before processQueue (contains processMsg) call
	@Override
	protected void preRec(){
		int result;
		if(!messages.isEmpty()){
			//will hold all messages that are removable after the loop
			ArrayList<StatusMessage> removable = new ArrayList<StatusMessage>();
			//check all messages in waitline
			for(StatusMessage sm : messages){
				result = checkJobStatus(sm);
				if(result == 5 || result == 2 || result == 9){
					debug("Condor Watcher found a finished or aborted job: "+sm.getJobpath());
					StatusMessage answer = new StatusMessage(this, sm.getQueryId(), sm.getJobpath(), sm.getTicket(), sm.getOriginalSender());
					answer.setStatusCode(result);
					caller.processMsg(answer);
					removable.add(sm);
				}
			}
			//remove finished jobs
			for(StatusMessage sm : removable){
				messages.remove(sm);
			}
			removable.clear();
		}
	}

	@Override
	protected void shutdown() {
		System.exit(0);
	}
	
	private int checkJobStatus(StatusMessage sm){		
		String jobfile = sm.getJobpath().toString();
		
		int trial = 1;
		boolean suc = false;
		Exception ex = null;
		
		do {
			try {
				FileInputStream in = new FileInputStream(jobfile);
		        BufferedReader br = new BufferedReader(new InputStreamReader(in));
		        String tmp;
		        int tempId = -1;
		        int lastIdLine = -2;
		        /* The following will search for the last state id of the job.
		         * This assumes that only one job uses the log file, so log files
		         * for multiple jobs will behave unexpected.
		         * The last id is the current state id.
		         */
		        while ((tmp = br.readLine()) != null) {
		        	try{
		        		tempId = Integer.parseInt(tmp.substring(0, 3));
		        	}catch(NumberFormatException nfe){
		        		continue;
		        	}
		        	//debug(tmp.substring(0, 3) + " and integer "+tempId);
		        		lastIdLine = tempId;
		        }
		        in.close();   
				
		        int shellExitStatus = lastIdLine;
		        suc = true;
		        
		        //debug( "CondorWatcher checked job '"+jobfile+"' and got return status "+shellExitStatus+"." );
				return shellExitStatus;
			}
			catch( IOException e ) {
				ex = e;
				if( log.isWarnEnabled() && trial > 1) {
					log.warn( "Retrying "+( ++trial )+"th time. Waiting "+WAIT_INTERVAL+"ms: "+ex.getMessage() );
				}
				try {
					Thread.sleep( WAIT_INTERVAL );
				}catch(Exception exept){
					log.warn( "Wait time canceled due to exception "+exept.getMessage() );
				}
			}
		} while( suc == false && trial <= MAX_TRIALS );
		
		return -1;
	}
	
	private void debug(String message){
		if( log.isDebugEnabled() ){
			log.debug( message );
		}
	}
	
}
