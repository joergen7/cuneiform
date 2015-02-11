package de.huberlin.wbi.cuneiform.htcondorcre;

import java.nio.file.Path;
import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class StatusMessage extends Message {
	public static final int CODESubmit = 0;
	public static final int CODEExecute = 1;
	public static final int CODEExecutableError = 2;
	public static final int CODECheckpointed = 3;
	public static final int CODEJobEvicted = 4;
	public static final int CODEJobTerminated = 5;
	public static final int CODEImageSize = 6;
	public static final int CODEShadowException = 7;
	public static final int CODEGeneric = 8;
	public static final int CODEJobAborted = 9;
	public static final int CODEJobSuspended = 10;
	public static final int CODEJobUnsuspended = 11;
	public static final int CODEJobHeld = 12;
	public static final int CODEJobReleased = 13;
	public static final int CODENodeExecute = 14;
	public static final int CODENodeTerminated = 15;
	public static final int CODEPostScriptTerminated = 16;
	public static final int CODEGlobusSubmit = 17;
	public static final int CODEGlobusSubmitFailed = 18;
	public static final int CODEGlobusResourceUp = 19;
	public static final int CODEGlobusResourceDown = 20;
	public static final int CODERemoteError = 21;
	
	private final Path jobpath;
	private final Actor originalSender;
	private final UUID queryId;
	private final Ticket ticket;
	private int status = 0;
		
	public StatusMessage(Actor sender, UUID queryId, Path jobpath, Ticket ticket, Actor originalSender) {
		super(sender);
		
		if( jobpath == null ){
			throw new NullPointerException( "Jobpath must not be null." );
		}
				
		if( queryId == null ){
			throw new NullPointerException( "Run ID must not be null." );
		}
		
		if( ticket == null ){
			throw new NullPointerException( "Ticket must not be null." );
		}
		
		if( originalSender == null ){
			throw new NullPointerException( "Original sender must not be null." );
		}
		
		this.jobpath = jobpath;
		this.queryId = queryId;
		this.ticket = ticket;
		this.originalSender = originalSender;
	}
	
	public Path getJobpath(){
		return this.jobpath;
	}
	
	public Actor getOriginalSender(){
		return this.originalSender;
	}
	
	public int getStatusCode(){
		return this.status;
	}
	
	public void setStatusCode(int status){
		if(status >= 0 && status <= 21){
			this.status = status;
		}
	}

	public UUID getQueryId() {
		return this.queryId;
	}
	
	public Ticket getTicket() {
		return this.ticket;
	}
	
	@Override
	public String toString() {
		return "{ statusMessage, \""+queryId+"\", "+jobpath+"\" }";
	}

}
