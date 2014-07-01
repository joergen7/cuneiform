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

package de.huberlin.cuneiform.dax.semanticmodel;

import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;

public class DaxFilename {

	public static final int LINK_INPUT = 1;
	public static final int LINK_OUTPUT = 2;
	public static final int LINK_INOUT = 3;
	private static final String PREFIX_LINK = "link";

	private String file;
	private Integer link;

	@Override
	public boolean equals( Object obj ) {
		
		DaxFilename other;
		
		if( !( obj instanceof DaxFilename ) )
			return false;
		
		other = ( DaxFilename )obj;
		if( file == null )
			throw new NullPointerException(
				"Trying to compare filenames when file member is not set." );
		
		return file.equals( other.file );
	}
	
	public String getFile() {
		return file;
	}
	
	@Override
	public int hashCode() {
		return file.hashCode();
	}
	
	public boolean isLinkInput() {
		
		if( link == null )
			throw new NullPointerException( "Link direction not set." );
		
		return link == LINK_INPUT;
	}
	
	public boolean isLinkOutput() {
		
		if( link == null )
			throw new NullPointerException( "Link direction not set." );
		
		return link == LINK_OUTPUT;
	}
	
	public void setFile( String file ) {
		
		if( file == null )
			throw new NullPointerException( "File string must not be null." );
		
		if( file.isEmpty() )
			throw new RuntimeException( "File string must not be empty." );
		
		this.file = file;
	}
	
	public void setLinkInput() {
		link = LINK_INPUT;
	}
	
	public void setLinkOutput() {
		link = LINK_OUTPUT;
	}
	
	public void setLinkInout() {
		link = LINK_INOUT;
	}
	
	public NameExpr getNameExpr() {
		return new NameExpr( PREFIX_LINK+file.hashCode() );
	}
	
	@Override
	public String toString() {
		return file;
	}

}
