package de.huberlin.wbi.cuneiform.starlinger;

public class StarlingerWorkflow extends JsonMap {
	
	private static final String LABEL_FILENAME = "filename";
	private static final String LABEL_RESOURCE_URI = "resource_uri";
	private static final String LABEL_TITLE = "title";
	private static final String LABEL_FILEPATH = "filepath";
	private static final String LABEL_DESCRIPTION = "description";
	private static final String LABEL_NODES = "nodes";
	private static final String LABEL_EDGES = "edges";
	
	private StarlingerNodeSet nodeSet;
	private StarlingerEdgeSet edgeSet;

	public StarlingerWorkflow() {
		nodeSet = new StarlingerNodeSet();
		edgeSet = new StarlingerEdgeSet();
	}
	
	public void addEdge( String src, String sink ) {
		edgeSet.addEdge( src, sink );
	}
	
	public void addNode( StarlingerNode node ) {
		nodeSet.addNode( node );
	}
	
	public void setFilename( String filename ) {
		putAttribute( LABEL_FILENAME, "'"+filename+"'" );
	}
	
	public void setResourceUri( String resourceUri ) {
		putAttribute( LABEL_RESOURCE_URI, "'"+resourceUri+"'" );
	}
	
	public void setTitle( String title ) {
		putAttribute( LABEL_TITLE, "'"+title+"'" );
	}
	
	public void setFilepath( String filepath ) {
		putAttribute( LABEL_FILEPATH, "'"+filepath+"'" );
	}
	
	public void setDescription( String description ) {
		putAttribute( LABEL_DESCRIPTION, "'"+description+"'" );
	}
	
	@Override
	public String toString() {
		
		putAttribute( LABEL_EDGES, edgeSet.toString() );
		putAttribute( LABEL_NODES, nodeSet.toString() );
		
		return super.toString();
	}
		
}
