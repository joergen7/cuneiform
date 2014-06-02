package de.huberlin.wbi.cuneiform.logview.gantt;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.JSONException;
import org.json.JSONObject;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.op.ReportEntryOp;

public class GanttOp implements ReportEntryOp {

	private Map<Long,Set<GanttEntry>> ganttMap;
	
	public GanttOp() {
		ganttMap = new HashMap<>();
	}

	@Override
	public void process( JsonReportEntry entry ) {
		
		long invocId, timestamp;
		double realTime;
		JSONObject obj;
		GanttEntry ganttEntry;
		Set<GanttEntry> ganttSet;
		String key;
		String taskName;
		
		try {
		
			if( !entry.hasInvocId() )
				return;
			
			if( !entry.isValueJson() )
				return;

			invocId = entry.getInvocId();
			key = entry.getKey();

			obj = entry.getValueJsonObj();
			timestamp = entry.getTimestamp();
			taskName = entry.getTaskName();

			ganttSet = ganttMap.get( invocId );
			if( ganttSet == null ) {
				ganttSet = new HashSet<>();
				ganttMap.put( invocId, ganttSet );
			}

			if( key.equals( JsonReportEntry.KEY_INVOC_TIME ) ) {
				
				// invoc-time entry found
				
				
				realTime = obj.getDouble( "realTime" );
				ganttEntry = new ExecEntry( timestamp, realTime, taskName );
				ganttSet.add( ganttEntry );
			}
			
			// TODO: evaluate staging messages
		
		}
		catch( JSONException e ) {
			throw new RuntimeException( e );
		}
		
	}
	
	public String getChart() {
		
		ResourcePool pool;
		Integer res;
		Set<GanttEntry> ganttSet;
		double start, duration, origin, finish, end;
		StringBuffer buf;
		ExecEntry execEntry;
		List<Double> idSet;
		Map<Double,Set<Long>> startMap;
		Set<Long> startSet;
				
		buf = new StringBuffer();
		
		buf.append( "#!/usr/bin/env octave\n\nfigure\nhold on\n" );
		buf.append( "xlabel( 'Time [min]' )\nylabel( 'Resource' )\n" );
		
		pool = new ResourcePool();
		
		origin = Double.MAX_VALUE;
		finish = Double.MIN_VALUE;
		startMap = new HashMap<>();
		for( long ticketId : ganttMap.keySet() ) {
			
			ganttSet = ganttMap.get( ticketId );
			start = getStart( ganttSet );
			duration = getDuration( ganttSet, start );
			
			if( start < origin )
				origin = start;
			
			end = start+duration;
			if( end > finish )
				finish = end;
			
			startSet = startMap.get( start );
			if( startSet == null ) {
				startSet = new HashSet<>();
				startMap.put( start, startSet );
			}
			
			startSet.add( ticketId );
			
		}
		finish -= origin;
		
		idSet = new ArrayList<>();
		idSet.addAll( startMap.keySet() );
		Collections.sort( idSet );
		for( double st : idSet ) {
			
			for( long ticketId : startMap.get( st ) ) {
			
				ganttSet = ganttMap.get( ticketId );
	
				if( ganttSet.isEmpty() )
					continue;
				
				duration = getDuration( ganttSet, st );
				
				res = pool.getResource( st, duration );
				
				for( GanttEntry ganttEntry : ganttSet ) {
					
					if( ganttEntry instanceof ExecEntry ) {
						
						execEntry = ( ExecEntry )ganttEntry;
						
						buf.append( getPlot( origin, execEntry.getStart(), execEntry.getEnd(), res, 'g' ) );
						buf.append( getText( origin, execEntry.getStart(), res, execEntry.getTaskName() ) );
						continue;
					}
					
					throw new RuntimeException( "Gantt entry type not recognized." );
				}
			}
		}
		
		buf.append( "axis( [0 "+finish+" -.5 "+( pool.size()-.5 )+"] )\n" );
		
		return buf.toString();
	}
	
	private static String getPlot( double origin, double start, double end, int res, char color ) {
		
		double s, e;
		
		s = start;
		s -= origin;
		
		e = end;
		e -= origin;
		
		return "plot( ["+s+" "+e+"], ["+res+" "+res+"], '+-"+color+"' )\n";
	}
	
	private static String getText( double origin, double start, int res, String txt ) {
		return "text( "+( start-origin )+", "+res+", '"+txt+"' )\n";
	}
	
	private static double getStart( Set<GanttEntry> ganttSet ) {
		
		double t, t1;
				
		t = Double.MAX_VALUE;
		for( GanttEntry entry : ganttSet ) {
			
			t1 = entry.getStart();
			if( t1 < t )
				t = t1;
		}
		
		return t;
	}
	
	private static double getDuration( Set<GanttEntry> ganttSet, double start ) {
		
		double t, t1;
		
		t = Double.MIN_VALUE;
		for( GanttEntry entry : ganttSet ) {
			
			t1 = entry.getEnd();
			if( t1 > t )
				t = t1;
		}
		
		return t-start;
	}
}
