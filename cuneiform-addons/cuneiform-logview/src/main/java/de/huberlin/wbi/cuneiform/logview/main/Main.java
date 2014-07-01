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

package de.huberlin.wbi.cuneiform.logview.main;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.json.JSONException;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.gantt.GanttOp;

public class Main {

	public static void main( String[] args ) throws FileNotFoundException, IOException, JSONException {

		String filename = "/home/jorgen/data/14-05-28_DBIS_Experimente/logs/e11_23_2048_variant-call-09";
		String line;
		JsonReportEntry reportEntry;
		GanttOp op;
		
		
		op = new GanttOp();
		
		
		try( BufferedReader reader = new BufferedReader( new FileReader( new File( filename ) ) ) ) {
						
			while( ( line = reader.readLine() ) != null ) {
				
				reportEntry = new JsonReportEntry( line );
				op.process( reportEntry );
			}
			
			reader.close();
			
			
			try( BufferedWriter writer = new BufferedWriter( new FileWriter( new File( "/home/jorgen/main.m" ) ) ) ) {
				writer.write( op.getChart() );
			}
			
		}
		
		
		
    }
}
