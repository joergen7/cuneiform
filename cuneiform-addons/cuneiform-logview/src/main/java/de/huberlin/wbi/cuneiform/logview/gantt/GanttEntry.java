package de.huberlin.wbi.cuneiform.logview.gantt;

public abstract class GanttEntry {

	private final long startTime;
	private final double duration;
	
	public GanttEntry( long startTime, double duration ) {
		
		if( duration < 0 )
			throw new RuntimeException( "Duration time must not be smaller than 0." );
		
		this.startTime = startTime;
		this.duration = duration;
	}
	
	public double getStart() {
		return startTime/60000.;
	}
	
	public double getEnd() {
		return getStart()+getDuration();
	}
	
	public double getDuration() {
		return duration/60.;
	}
}
