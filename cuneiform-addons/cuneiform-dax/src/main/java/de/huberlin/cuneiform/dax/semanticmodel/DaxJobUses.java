package de.huberlin.cuneiform.dax.semanticmodel;

public class DaxJobUses extends DaxFilename {

	private final int TYPE_EXECUTABLE = 1;
	
	private Boolean register;
	private Boolean transfer;
	private Integer type;
	private Boolean optional;
	
	public void setRegister( Boolean register ) {
		this.register = register;
	}
	
	public void setTransfer( Boolean transfer ) {
		this.transfer = transfer;
	}
	
	public void setTypeExecutable() {
		type = TYPE_EXECUTABLE;
	}
	
	public void setOptional( Boolean optional ) {
		this.optional = optional;
	}
}
