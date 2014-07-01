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
