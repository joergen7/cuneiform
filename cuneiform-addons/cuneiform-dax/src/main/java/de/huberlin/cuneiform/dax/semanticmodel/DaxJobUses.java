package de.huberlin.cuneiform.dax.semanticmodel;


public class DaxJobUses extends DaxFilename {
	
	private boolean executable;
	private boolean optional;
	
	public DaxJobUses() {
		executable = false;
		optional = false;
	}

	public boolean isExecutable() {
		return executable;
	}
	
	public boolean isOptional() {
		return optional;
	}
	
	public void setExecutable( boolean e ) {
		executable = e;
	}
	
	public void setOptional( boolean o ) {
		optional = o;
	}

}
