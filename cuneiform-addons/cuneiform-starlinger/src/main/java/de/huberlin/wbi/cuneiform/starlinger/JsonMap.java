package de.huberlin.wbi.cuneiform.starlinger;

import java.util.HashMap;
import java.util.Map;

public class JsonMap {

	private Map<String,String> attributeMap;

	public JsonMap() {
		attributeMap = new HashMap<>();
	}
	
	public String getAttribute( String key ) {
		return attributeMap.get( key );
	}
	
	public void putAttribute( String key, String value ) {
		
		if( key == null )
			throw new NullPointerException( "Key must not be null." );
		
		attributeMap.put( key, value );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "{\n" );
		
		comma = false;
		for( String key : attributeMap.keySet() ) {
			
			if( comma )
				buf.append( ",\n" );
			comma = true;
			
			buf.append( '\'' ).append( key ).append( "'=>" )
				.append( attributeMap.get( key ) );
		}
		
		buf.append( "\n}" );
		
		return buf.toString();
	}


}
