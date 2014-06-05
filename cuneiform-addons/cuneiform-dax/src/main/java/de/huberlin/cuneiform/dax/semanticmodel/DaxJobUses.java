package de.huberlin.cuneiform.dax.semanticmodel;

import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;

public class DaxJobUses {
	
	public static final int LINK_INPUT = 1;
	public static final int LINK_OUTPUT = 2;
	
	private static final String PREFIX_LINK = "link";
	private static int nextId;
	
	private String file;
	private Integer link;
	private final int id;
	
	public DaxJobUses() {
		id = popId();
	}
	
	@Override
	public boolean equals( Object obj ) {
		
		DaxJobUses other;
		
		if( !( obj instanceof DaxJobUses ) )
			return false;
		
		other = ( DaxJobUses )obj;
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
	
	public NameExpr getNameExpr() {
		return new NameExpr( PREFIX_LINK+id );
	}
	
	@Override
	public String toString() {
		return file;
	}

	private static synchronized int popId() {
		return nextId++;
	}
}
