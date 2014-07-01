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

package de.huberlin.wbi.cuneiform.logview.gantt;

import java.util.ArrayList;
import java.util.List;

public class Resource {

	private List<double[]> intervalList;
	
	public Resource() {
		intervalList = new ArrayList<>();
	}
	
	public boolean canHost( double start, double duration ) {
				
		for( double[] candidate : intervalList )
			
			if( candidate[ 0 ] > start ) {
				
				// candidate starts after original starts
				
				if( candidate[ 0 ] < start+duration )
										
					// candidate starts before original ends
					return false;
			}	
			else
				
				// candidate starts before original starts
				
				if( candidate[ 0 ]+candidate[ 1 ] > start )
					
					// candidate ends after original starts
					return false;
		
		
		return true;
	}
	
	public void host( double start, double duration ) {
		
		if( duration < 0 )
			throw new RuntimeException( "Duration must not be negative." );
		
		intervalList.add( new double[] { start, duration } );
	}
}
