package de.huberlin.wbi.cuneiform.logview.gantt;

public class ExecEntry extends GanttEntry {

	private final String taskName;
	
	public ExecEntry( long startTime, double duration, String taskName ) {
		super( startTime, duration );
		
		if( taskName == null )
			throw new NullPointerException( "Task name must not be null." );
		
		if( taskName.isEmpty() )
			throw new RuntimeException( "Task name must not be empty." );
		
		this.taskName = taskName;
	}
	
	public String getTaskName() {
		return taskName;
	}
}
