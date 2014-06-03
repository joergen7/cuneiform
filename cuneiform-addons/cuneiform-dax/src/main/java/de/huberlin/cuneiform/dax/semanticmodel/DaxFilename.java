package de.huberlin.cuneiform.dax.semanticmodel;

public class DaxFilename {
	
	public static final int LINK_INPUT = 1;
	public static final int LINK_OUTPUT = 2;
	public static final int LINK_INOUT = 3;
	
	private String file;
	private Integer link;
	
	public boolean isLinkOutput() {
		
		if( link == null )
			return false;
		
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
}
