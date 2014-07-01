/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

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
