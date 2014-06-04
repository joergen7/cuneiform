package de.huberlin.wbi.cuneiform.core.semanticmodel;

public class DrawHelper {

	private final CompoundExpr taskExpr;
	private final BaseBlock bindingBlock;
	private final Prototype prototype;
	
	public DrawHelper( ApplyExpr applyExpr ) {
		this( applyExpr.getTaskExpr(), applyExpr );
	}
	
	public DrawHelper( CompoundExpr taskExpr, BaseBlock bindingBlock ) {
		
		if( taskExpr == null )
			throw new NullPointerException( "Task expression must not be null." );
		
		if( bindingBlock == null )
			throw new NullPointerException( "Binding block must not be null." );
		
		this.taskExpr = taskExpr;
		this.prototype = ( ( LambdaExpr )taskExpr.getSingleExpr( 0 ) ).getPrototype();
		this.bindingBlock = bindingBlock;
	}
	
	public CompoundExpr reduceComb() {
		
		CompoundExpr ce;
		
		ce = new CompoundExpr();
		// TODO
		return ce;
	}
}
