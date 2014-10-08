package de.huberlin.wbi.cuneiform.logview.gui;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class CfEdge {

	private final String title;
	private Long producer;
	private final Set<Long> consumerIdSet;

	public CfEdge( String title ) {
		this( title, null );
	}
	
	public CfEdge( String title, Long producer ) {
		
		if( title == null )
			throw new NullPointerException( "Vertex title must not be null." );
		
		if( title.isEmpty() )
			throw new RuntimeException( "Vertex title must not be empty." );
		
		this.title = title;
		this.producer = producer;
		consumerIdSet = new HashSet<>();
	}
	
	public void addConsumerId( long invocId ) {
		consumerIdSet.add( invocId );
	}
	
	public boolean isConsumerIdSetEmpty() {
		return consumerIdSet.isEmpty();
	}
	
	public Set<Long> getConsumerIdSet() {
		return Collections.unmodifiableSet( consumerIdSet );
	}
	
	public String getTitle() {
		return title;
	}
	
	public boolean hasProducer() {
		return producer != null;
	}
	
	public Long getProducerId() {
		return producer;
	}	
}
