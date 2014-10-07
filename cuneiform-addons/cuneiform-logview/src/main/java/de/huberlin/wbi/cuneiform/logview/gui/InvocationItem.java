package de.huberlin.wbi.cuneiform.logview.gui;

import javax.swing.tree.DefaultMutableTreeNode;

public class InvocationItem extends DefaultMutableTreeNode {

	private static final long serialVersionUID = -381630579332316845L;
	
	private final long invocId;
	private final String taskName;
	private String stdErr;
	private String stdOut;
	
	public InvocationItem( long invocId, String taskName ) {
		super( String.valueOf( invocId ) );
		
		this.invocId = invocId;
		this.taskName = taskName;
	}
	
	public long getInvocId() {
		return invocId;
	}
	
	public String getStdOut() {
		return stdOut;
	}
	
	public String getStdErr() {
		return stdErr;
	}
	
	public String getTaskName() {
		return taskName;
	}
	
	public void setStdOut( String stdOut ) {
		this.stdOut = stdOut;
	}

	public void setStdErr( String stdErr ) {
		this.stdErr = stdErr;
	}
}
