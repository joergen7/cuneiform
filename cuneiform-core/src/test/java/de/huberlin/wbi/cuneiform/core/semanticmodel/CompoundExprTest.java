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
	
	@SuppressWarnings("static-method")
	@Test
	public void copyApplyExprShouldWork() {
		
		ApplyExpr ae;
		NativeLambdaExpr lam;
		Prototype sign;
		Block body;
		CompoundExpr ce1, ce2;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "inp" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "inp" ) ) );
		
		lam = new NativeLambdaExpr( sign, body );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( new CompoundExpr( lam ) );
		ae.putAssign( new NameExpr( "inp" ), new CompoundExpr( new StringExpr( "A" ) ) );
		
		ce1 = new CompoundExpr( ae );
		ce2 = new CompoundExpr( ce1 );
		
		assertEquals( ce1, ce2 );
		assertNotSame( ce1, ce2 );
	}
}
