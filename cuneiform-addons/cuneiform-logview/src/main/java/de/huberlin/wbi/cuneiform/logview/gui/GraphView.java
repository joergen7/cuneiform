package de.huberlin.wbi.cuneiform.logview.gui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.JSONObject;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.op.Visualizable;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.SparseMultigraph;

public class GraphView extends Visualizable {

	private static final long serialVersionUID = 9059116172334909768L;
	
	private final Map<String,CfEdge> edgeMap;
	private final Set<Long> vertexSet;
	
	public GraphView() {
		
		edgeMap = new HashMap<>();
		vertexSet = new HashSet<>();
	}

	@Override
	public void register( JsonReportEntry entry ) throws Exception {

		long invocId;
		String filename;
		CfEdge edge;

		if( !entry.hasInvocId() )
			return;
		
		invocId = entry.getInvocId();
		vertexSet.add( invocId );
		
		if( entry.isKeyFileSizeStageIn() ) {
			
			filename = entry.getFile();
			
			edge = edgeMap.get( filename );
			if( edge == null ) {
				edge = new CfEdge( filename );
				edgeMap.put( filename, edge );
			}
			
			edge.addConsumer( invocId );
		}
		else if( entry.isKeyFileSizeStageOut() ) {
			
			filename = entry.getFile();
			edgeMap.put( filename, new CfEdge( filename, invocId ) );
		}
	}

	@Override
	public void clear() {
		vertexSet.clear();
		edgeMap.clear();
	}

	@Override
	public void updateView() {

		Graph<Long,String> g;
		Long src;
		CfEdge edge;
		long producer;
		
		g = new SparseMultigraph<>();
		
		// add vertices
		for( Long vertex : vertexSet )
			g.addVertex( vertex );
		
		// add edges
		for( String key : edgeMap.keySet() ) {
			
			edge = edgeMap.get( key );
			if( edge.hasProducer() )
				producer = edge.getProducer();
			else {
				producer = key.hashCode();
				if( !vertexSet.contains( producer ) )
					vertexSet.add( producer );
			}
		}
	}

}
