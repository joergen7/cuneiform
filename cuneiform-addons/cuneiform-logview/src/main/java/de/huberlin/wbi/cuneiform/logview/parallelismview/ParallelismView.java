package de.huberlin.wbi.cuneiform.logview.parallelismview;

import java.awt.BorderLayout;
import java.util.HashMap;
import java.util.Map;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.DefaultTableXYDataset;
import org.jfree.data.xy.XYSeries;
import org.json.JSONException;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.common.Visualizable;

public class ParallelismView extends Visualizable {

	private static final long serialVersionUID = -5167316615121741821L;
	private static final int NSAMPLE = 1024;
	
	private long firstBegin;
	private long lastBegin;
	private long lastDur;
	private final Map<Long,Long> beginMap;
	private final Map<Long,Long> durMap;
	private final Map<Long,String> taskNameMap;
	
	public ParallelismView() {
		
		beginMap = new HashMap<>();
		durMap = new HashMap<>();
		taskNameMap = new HashMap<>();
		clear();
		
		setLayout( new BorderLayout() );
	}
	
	@Override
	public void clear() {

		firstBegin = Long.MAX_VALUE;
		lastDur = Long.MIN_VALUE;

		beginMap.clear();
		durMap.clear();
		taskNameMap.clear();
	}
	
	@Override
	public void register( JsonReportEntry entry ) throws JSONException {

		long begin, dur;
		long invocId;
		String taskName;
		
		if( entry == null )
			throw new NullPointerException( "JSON report entry must not be null." );
		
		if( !entry.isKeyInvocTime() )
			return;
		
		invocId = entry.getInvocId();
		taskName = entry.getTaskName();
		
		dur = entry.getValueJsonObj().getLong( JsonReportEntry.LABEL_REALTIME );
		begin = entry.getTimestamp();
		if( begin < firstBegin )
			firstBegin = begin;
		else
			if( begin+dur > lastBegin+lastDur ) {
				lastBegin = begin;
				lastDur = dur;
			}
				
		beginMap.put( invocId, begin );
		durMap.put( invocId, dur );
		taskNameMap.put( invocId, taskName );
	}
	
	@Override
	public void updateView() {
		
		JFreeChart chart;
		DefaultTableXYDataset dataset;
		Map<String,int[]> seriesMap;
		String taskName;
		int[] cvec;
		int i;
		long begin, stop, t;
		long[] tvec;
		long delta;
		XYSeries series;
		long dur;
		String uname;
		int udiv;
		
		seriesMap = new HashMap<>();
		tvec = new long[ NSAMPLE ];
		dur = lastBegin+lastDur-firstBegin;
		delta = dur/( NSAMPLE-1 )+1;
		
		t = firstBegin;
		for( i = 0; i < NSAMPLE; i++ ) {
			
			tvec[ i ] = t;
			
			for( Long invocId : beginMap.keySet() ) {
				
				begin = beginMap.get( invocId );
				if( begin > t )
					continue;
				
				stop = begin+durMap.get( invocId );
				if( stop < t )
					continue;
				
				taskName = taskNameMap.get( invocId );
				if( taskName == null )
					taskName = "[lambda]";
				
				cvec = seriesMap.get( taskName );
				if( cvec == null ) {
					cvec = new int[ NSAMPLE ];
					seriesMap.put( taskName, cvec );
				}
				
				cvec[ i ]++;
				
			}
			
			t += delta;
		}
		
		udiv = 3600000*24;
		uname = "d";
		
		if( dur < 2*3600000*24 ) {

			udiv = 3600000;
			uname = "h";
		}

		if( dur < 2*3600000 ) {
			
			udiv = 60000;
			uname = "min";
		}
		
		if( dur < 2*60000 ) {
			
			udiv = 1000;
			uname = "s";
		}
		
		if( dur < 2000 ) {
			
			udiv = 1;
			uname = "ms";
		}
		
		dataset = new DefaultTableXYDataset();
		for( String n : seriesMap.keySet() ) {
			
			cvec = seriesMap.get( n );
			series = new XYSeries( n, true, false );
			for( i = 0; i < NSAMPLE; i++ )
				series.add( ( ( double )( tvec[ i ]-firstBegin ) )/udiv, cvec[ i ] );
			
			dataset.addSeries( series );
		}
		
		chart = ChartFactory.createStackedXYAreaChart(
			"Parallelism",				// title
			"Time ["+uname+"]",					// xAxisLabel
			"N invocations",			// yAxisLabel
			dataset,					// dataset
			PlotOrientation.VERTICAL,	// orientation
			true,						// legend
			true,						// tooltips
			true						// urls
		);
		
		add( new ChartPanel( chart ), BorderLayout.CENTER );
		
	}
	
}