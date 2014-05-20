package de.huberlin.wbi.cuneiform.starlinger;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class StarlingerEdgeSet {

	private Map<String,List<String>> edgeMap;
	
	public StarlingerEdgeSet() {
		
		edgeMap = new HashMap<>();
	}
	
	public void addEdge( String src, String sink ) {
		
		List<String> sinkList;
		
		sinkList = edgeMap.get( src );
		
		if( sinkList == null ) {
			
			sinkList = new LinkedList<>();
			edgeMap.put( src, sinkList );
		}
		
		sinkList.add( sink );
	}
	
	@Override
	public String toString() {
		
		JsonMap jsonMap;
		StringBuffer buf;
		boolean comma;
		
		jsonMap = new JsonMap();
		
		for( String key : edgeMap.keySet() ) {
			
			
			buf = new StringBuffer();
			
			buf.append( '[' );
			
			comma = false;
			for( String value : edgeMap.get( key ) ) {
				
				if( comma )
					buf.append( ',' );
				comma = true;
				
				buf.append( '\'' ).append( value ).append( '\'' );
			}
			
			buf.append( ']' );
			
			jsonMap.putAttribute( key, buf.toString() );
		}
		
		return jsonMap.toString();
	}
}
