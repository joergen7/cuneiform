package de.huberlin.wbi.cuneiform.core.semanticmodel;

public abstract class BaseNodeVisitor implements NodeVisitor<CompoundExpr> {

	@Override
	public CompoundExpr accept( Block block ) {
		throw new RuntimeException( "Block is not an expression." );
	}
	
	@Override
	public CompoundExpr accept( ForeignLambdaExpr foreignLambdaExpr ) {
		return new CompoundExpr( foreignLambdaExpr );
	}
		
	@Override
	public CompoundExpr accept( NativeLambdaExpr nativeLambdaExpr ) {
		return new CompoundExpr( nativeLambdaExpr );
	}
	
	@Override
	public CompoundExpr accept( StringExpr stringExpr ) {
		return new CompoundExpr( stringExpr );
	}
	
	@Override
	public CompoundExpr accept( QualifiedTicket qualifiedTicket ) {
		return new CompoundExpr( qualifiedTicket );
	}
	
	@Override
	public CompoundExpr accept( LambdaType lambdaType ) {
		throw new RuntimeException( "Lambda type is not an expression." );
	}

	@Override
	public CompoundExpr accept( DrawParam drawParam ) {
		throw new RuntimeException( "Draw parameter is not an expression." );
	}

	@Override
	public CompoundExpr accept( DataType dataType ) {
		throw new RuntimeException( "Data type is not an expression." );
	}
	
	@Override
	public CompoundExpr accept( CorrelParam correlParam ) {
		throw new RuntimeException( "Correlated parameter is not an expression." );
	}

	@Override
	public CompoundExpr accept( Prototype prototype ) {
		throw new RuntimeException( "Prototype is not an expression." );
	}

	@Override
	public CompoundExpr accept( ReduceVar reduceVar ) {
		throw new RuntimeException( "Reduce variable is not an expression." );
	}



}
