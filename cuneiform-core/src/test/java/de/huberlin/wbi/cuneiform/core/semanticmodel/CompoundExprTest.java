package de.huberlin.wbi.cuneiform.core.semanticmodel;

import static org.junit.Assert.*;

import org.junit.Test;

public class CompoundExprTest {

	@SuppressWarnings("static-method")
	@Test
	public void equalsReturnsTrueOnCompoundExprWithEqualContent() {
		
		CompoundExpr a, b;
		
		a = new CompoundExpr( new StringExpr( "A" ) );
		b = new CompoundExpr( new StringExpr( "A" ) );
		
		assertEquals( a, b );
		assertEquals( b, a );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void equalsReturnsFalseOnCompoundExprWithNonEqualContent() {
		
		CompoundExpr a, b;
		
		a = new CompoundExpr( new StringExpr( "A" ) );
		b = new CompoundExpr( new StringExpr( "B" ) );
		
		assertNotEquals( a, b );
		assertNotEquals( b, a );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void equalsReturnsFalseOnNull() {
		
		CompoundExpr a;
		
		a = new CompoundExpr();
		assertNotEquals( a, null );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void equalsReturnsFalseOnCompoundExprWithDifferingSize() {
		
		CompoundExpr a, b;
		
		a = new CompoundExpr();
		b = new CompoundExpr( new StringExpr( "B" ) );
		
		assertNotEquals( a, b );
		assertNotEquals( b, a );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void equalsReturnsFalseOnNonCompoundExpr() {
		
		CompoundExpr a;
		
		a = new CompoundExpr();
		assertNotEquals( a, "blub" );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void condIsNonNormal() {
		
		CondExpr cnd;
		CompoundExpr ce;
		
		cnd = new CondExpr( new CompoundExpr(), new CompoundExpr(), new CompoundExpr() );		
		ce = new CompoundExpr( cnd );
		
		assertFalse( ce.isNormal() );
		
	}
}
