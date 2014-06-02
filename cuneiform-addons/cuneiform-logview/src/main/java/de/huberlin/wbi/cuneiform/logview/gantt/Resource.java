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
