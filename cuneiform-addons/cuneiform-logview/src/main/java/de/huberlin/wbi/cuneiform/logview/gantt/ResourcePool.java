package de.huberlin.wbi.cuneiform.logview.gantt;

import java.util.ArrayList;

public class ResourcePool {

	private ArrayList<Resource> resourceList;
	
	public ResourcePool() {
		resourceList = new ArrayList<>();
	}
	
	public int size() {
		return resourceList.size();
	}
	
	public int getResource( double start, double duration ) {
		
		int i;
		Resource res;
		
		for( i = 0; i < resourceList.size(); i++ ) {
			
			res = resourceList.get( i );
			
			if( res.canHost( start, duration ) ) {
				
				res.host( start, duration );
				return i;
			}
		}
		
		res = new Resource();
		res.host( start, duration );
		resourceList.add( res );
		
		return resourceList.size()-1;
	}
}
