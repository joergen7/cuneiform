package de.huberlin.wbi.cuneiform.logview.main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;


import org.json.JSONException;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.gantt.GanttOp;

public class Main {

	public static void main( String[] args ) throws FileNotFoundException, IOException, JSONException {

		String filename = "/home/jorgen/statlog.txt";
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
			
			
			System.out.println( op.getChart() );
			
		}
		
		
		
    }
}
