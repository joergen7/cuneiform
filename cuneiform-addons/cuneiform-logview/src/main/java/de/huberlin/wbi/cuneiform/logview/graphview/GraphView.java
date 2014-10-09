package de.huberlin.wbi.cuneiform.logview.graphview;

import java.awt.BorderLayout;
import java.awt.Font;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.main.Visualizable;

public class GraphView extends Visualizable {

	private static final long serialVersionUID = 9059116172334909768L;
	private static final String COLOR_FILE = "darkgoldenrod";
	private static final String COLOR_INVOC = "blue";
	private static final double WIDTH_VERTEX = .01;
	private static final double NODESEP = .03;
	
	private final Map<String,CfEdge> edgeMap;
	private final Map<String,Set<Long>> vertexMap;
	private final JTextArea dotArea;
	
	public GraphView() {
		
		edgeMap = new HashMap<>();
		vertexMap = new HashMap<>();
		
		setLayout( new BorderLayout() );
		
		dotArea = new JTextArea();
		dotArea.setEditable( false );
		dotArea.setFont( new Font( Font.MONOSPACED, Font.PLAIN, 11 ) );
		add( new JScrollPane( dotArea ), BorderLayout.CENTER );
	}

	@Override
	public void register( JsonReportEntry entry ) throws Exception {

		long invocId;
		String filename;
		CfEdge edge;
		String taskName;
		Set<Long> vertexSet;

		if( !entry.hasInvocId() )
			return;
		
		invocId = entry.getInvocId();
		
		taskName = entry.getTaskName();
		if( taskName == null )
			taskName = "[lambda]";
		
		vertexSet = vertexMap.get( taskName );
		if( vertexSet == null ) {
			vertexSet = new HashSet<>();
			vertexMap.put( taskName, vertexSet );
		}
		
		vertexSet.add( invocId );
		
		if( entry.isKeyFileSizeStageIn() ) {
			
			filename = entry.getFile();
			
			edge = edgeMap.get( filename );
			if( edge == null ) {
				edge = new CfEdge( filename );
				edgeMap.put( filename, edge );
			}
			
			edge.addConsumerId( invocId );
		}
		else if( entry.isKeyFileSizeStageOut() ) {
			
			filename = entry.getFile();
			edgeMap.put( filename, new CfEdge( filename, invocId ) );
		}
	}

	@Override
	public void clear() {
		vertexMap.clear();
		edgeMap.clear();
	}

	@Override
	public void updateView() {

		CfEdge edge;
		StringBuffer buf;
		int sg;
		long producerId;
		int hc;
		
		buf = new StringBuffer();
		buf.append( "digraph G {\n  nodesep=" ).append( NODESEP ).append( ";\n" );
		
		
		// add vertices to subgraphs
		sg = 0;
		for( String taskName : vertexMap.keySet() ) {
			
			buf.append( "  subgraph cluster" ).append( sg++ ).append( " {\n" );
			buf.append( "    label=\"" ).append( taskName ).append( "\";\n" );
			
			for( Long invocId : vertexMap.get( taskName ) )
				buf.append( "    node_invoc" ).append( invocId )
					.append( " [shape=box,color=" ).append( COLOR_INVOC )
					.append( ",width=" ).append( WIDTH_VERTEX ).append( ",label=\"\"];\n" );
			
			buf.append( "  }\n" );
		}
		
		for( String filename : edgeMap.keySet() ) {
			
			edge = edgeMap.get( filename );
			hc = Math.abs( filename.hashCode() );
			
			
			if( !edge.hasProducer() ) {
				
				// edge is an input file
				buf.append( "  node_infile" ).append( hc )
					.append( " [shape=box,color=" ).append( COLOR_FILE )
					.append( ",width=" ).append( WIDTH_VERTEX ).append( ",label=\"\"];\n" );
				
				for( long consumerId : edge.getConsumerIdSet() )
					
					buf.append( "  node_infile" ).append( hc ).append( ":s -> " )
						.append( "node_invoc" ).append( consumerId ).append( ":n;\n" );
			}
			else {
				
				// edge has a producer
				producerId = edge.getProducerId();
				
				if( edge.isConsumerIdSetEmpty() )
					buf.append( "  node_outfile" ).append( hc ).append( " [shape=box,color=" ).append( COLOR_FILE )
						.append( ",width=" ).append( WIDTH_VERTEX ).append( ",label=\"\"];\n" )
						.append( "  node_invoc" ).append( producerId )
						.append( ":s -> node_outfile" ).append( hc ).append( ":n;\n" );
				
				else
					for( long consumerId : edge.getConsumerIdSet() )
						
						buf.append( "  node_invoc" ).append( producerId ).append( ":s -> " )
							.append( "node_invoc" ).append( consumerId ).append( ":n;\n" );
				
			}
		}
		
		buf.append( "}\n" );
		
		dotArea.setText( buf.toString() );
	}

}
