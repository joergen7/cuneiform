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
