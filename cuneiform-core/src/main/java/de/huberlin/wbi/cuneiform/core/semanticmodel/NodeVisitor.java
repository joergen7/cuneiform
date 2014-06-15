/**
 * 
 */
package de.huberlin.wbi.cuneiform.core.semanticmodel;


/**
 * @author jorgen
 *
 */
public interface NodeVisitor<T> {
	
	public T accept( StringExpr stringExpr );
	public T accept( QualifiedTicket qualifiedTicket );
	public T accept( NativeLambdaExpr nativeLambdaExpr );
	public T accept( NameExpr nameExpr ) throws HasFailedException;
	public T accept( LambdaType lambdaType );
	public T accept( DrawParam drawParam );
	public T accept( DataType dataType );
	public T accept( CondExpr condExpr ) throws HasFailedException;
	public T accept( Block block );
	public T accept( ApplyExpr applyExpr ) throws HasFailedException;
	public T accept( CompoundExpr compoundExpr ) throws HasFailedException;
	public T accept( CorrelParam correlParam );
	public T accept( ForeignLambdaExpr foreignLambdaExpr );
	public T accept( CurryExpr curryExpr ) throws HasFailedException;
	public T accept( Prototype prototype );
	public T accept( ReduceVar reduceVar );
	public T accept(TopLevelContext topLevelContext) throws HasFailedException;
	
}
