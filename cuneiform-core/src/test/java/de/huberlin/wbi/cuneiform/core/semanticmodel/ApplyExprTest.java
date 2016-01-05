package de.huberlin.wbi.cuneiform.core.semanticmodel;

import static org.junit.Assert.*;

import org.junit.Test;

public class ApplyExprTest {

	@SuppressWarnings("static-method")
	@Test
	public void copyConstructorShouldWork() {
		
		ApplyExpr ae1, ae2;
		NativeLambdaExpr lam;
		Prototype sign;
		Block body;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "inp" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "inp" ) ) );
		
		lam = new NativeLambdaExpr( sign, body );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( new CompoundExpr( lam ) );
		ae1.putAssign( new NameExpr( "inp" ), new CompoundExpr( new StringExpr( "A" ) ) );
		
		ae2 = new ApplyExpr( ae1 );
		
		assertEquals( ae1, ae2 );
		assertNotSame( ae1, ae2 );
	}
}
