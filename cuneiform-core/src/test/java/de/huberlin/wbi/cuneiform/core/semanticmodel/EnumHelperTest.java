package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EnumHelperTest {

	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected = IllegalArgumentException.class )
	public void ConstructorShouldThrowIaeOnNullTaskExpr() {
		
		EnumHelper eh;
		BaseBlock bindingBlock;
		
		bindingBlock = mock( BaseBlock.class );
		
		eh = new EnumHelper( null, bindingBlock );
	}
	
	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected = IllegalArgumentException.class )
	public void ConstructorShouldThrowIaeOnNullBindingBlock() {
		
		EnumHelper eh;
		CompoundExpr ce;
		
		ce = mock( CompoundExpr.class );
		
		eh = new EnumHelper( ce, null );
	}
	
	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected = IllegalArgumentException.class )
	public void ConstructorShouldThrowIaeOnNil() {
		
		EnumHelper eh;
		CompoundExpr ce;
		BaseBlock bindingBlock;
		
		bindingBlock = mock( BaseBlock.class );
		ce = new CompoundExpr();
		
		eh = new EnumHelper( ce, bindingBlock );
	}
	
	@SuppressWarnings({ "unused", "static-method" })
	@Test( expected = IllegalArgumentException.class )
	public void ConstructorShouldThrowIaeOnNonLambdaExpr() {
		
		EnumHelper eh;
		CompoundExpr ce;
		BaseBlock bindingBlock;
		
		bindingBlock = mock( BaseBlock.class );
		ce = new CompoundExpr( mock( NameExpr.class ) );
		
		eh = new EnumHelper( ce, bindingBlock );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void getCardinalityShouldWorkWithTwoCorrelatedParameters() throws NotDerivableException {
		
		Prototype prototype;
		CorrelParam cp;
		CompoundExpr taskExpr;
		LambdaExpr le;
		Block bindingBlock;
		NameExpr a, b, c, d;
		CompoundExpr ce;
		EnumHelper eh;
		
		// create name expressions for four parameters
		a = new NameExpr( "a" );
		b = new NameExpr( "b" );
		c = new NameExpr( "c" );
		d = new NameExpr( "d" );
		
		// generate prototype
		prototype = new Prototype();
		
		cp = new CorrelParam();
		cp.addName( a );
		cp.addName( b );
		prototype.addParam( cp );
		
		cp = new CorrelParam();
		cp.addName( c );
		cp.addName( d );
		prototype.addParam( cp );
		
		// create lambda expression
		le = mock( LambdaExpr.class );
		when( le.getPrototype() ).thenReturn( prototype );
		
		// create task expression
		taskExpr = new CompoundExpr( le );
		
		// bind parameter values
		bindingBlock = new Block();
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "a1" ) );
		ce.addSingleExpr( new StringExpr( "a2" ) );
		ce.addSingleExpr( new StringExpr( "a3" ) );
		bindingBlock.putAssign( a, ce );
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "b1" ) );
		ce.addSingleExpr( new StringExpr( "b2" ) );
		ce.addSingleExpr( new StringExpr( "b3" ) );
		bindingBlock.putAssign( b, ce );
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "c1" ) );
		ce.addSingleExpr( new StringExpr( "c2" ) );
		ce.addSingleExpr( new StringExpr( "c3" ) );
		ce.addSingleExpr( new StringExpr( "c4" ) );
		ce.addSingleExpr( new StringExpr( "c5" ) );
		bindingBlock.putAssign( c, ce );
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "d1" ) );
		ce.addSingleExpr( new StringExpr( "d2" ) );
		ce.addSingleExpr( new StringExpr( "d3" ) );
		ce.addSingleExpr( new StringExpr( "d4" ) );
		ce.addSingleExpr( new StringExpr( "d5" ) );
		bindingBlock.putAssign( d, ce );
		
		eh = new EnumHelper( taskExpr, bindingBlock );
		assertEquals( 15, eh.getCardinality() );
	}

	@SuppressWarnings("static-method")
	@Test
	public void getCardinalityShouldWorkWithTaskCorrelation() throws NotDerivableException {
		
		Prototype prototype;
		CorrelParam cp;
		CompoundExpr taskExpr;
		LambdaExpr le1, le2;
		Block bindingBlock;
		NameExpr task, b, c, d;
		CompoundExpr ce;
		EnumHelper eh;
		
		// create name expressions for four parameters
		task = new NameExpr( "task" );
		b = new NameExpr( "a" );
		c = new NameExpr( "b" );
		d = new NameExpr( "c" );
		
		// generate prototype
		prototype = new Prototype();
		
		cp = new CorrelParam();
		cp.addName( task );
		cp.addName( b );
		prototype.addParam( cp );
		
		cp = new CorrelParam();
		cp.addName( c );
		cp.addName( d );
		prototype.addParam( cp );
		
		// create lambda expression
		le1 = mock( LambdaExpr.class );
		when( le1.getPrototype() ).thenReturn( prototype );
		when( le1.getNumAtom() ).thenReturn( 1 );
		
		le2 = mock( LambdaExpr.class );
		when( le2.getPrototype() ).thenReturn( prototype );
		when( le2.getNumAtom() ).thenReturn( 1 );
		
		// create task expression
		taskExpr = new CompoundExpr( le1 );
		taskExpr.addSingleExpr( le2 );
		
		// bind parameter values
		bindingBlock = new Block();
				
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "b1" ) );
		ce.addSingleExpr( new StringExpr( "b2" ) );
		bindingBlock.putAssign( b, ce );
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "c1" ) );
		ce.addSingleExpr( new StringExpr( "c2" ) );
		ce.addSingleExpr( new StringExpr( "c3" ) );
		ce.addSingleExpr( new StringExpr( "c4" ) );
		ce.addSingleExpr( new StringExpr( "c5" ) );
		bindingBlock.putAssign( c, ce );
		
		ce = new CompoundExpr();
		ce.addSingleExpr( new StringExpr( "d1" ) );
		ce.addSingleExpr( new StringExpr( "d2" ) );
		ce.addSingleExpr( new StringExpr( "d3" ) );
		ce.addSingleExpr( new StringExpr( "d4" ) );
		ce.addSingleExpr( new StringExpr( "d5" ) );
		bindingBlock.putAssign( d, ce );
		
		eh = new EnumHelper( taskExpr, bindingBlock );
		assertEquals( 10, eh.getCardinality() );
	}

}
