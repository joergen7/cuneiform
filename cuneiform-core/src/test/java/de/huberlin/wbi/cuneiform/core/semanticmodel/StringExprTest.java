package de.huberlin.wbi.cuneiform.core.semanticmodel;

import static org.junit.Assert.*;

import org.junit.Test;

public class StringExprTest {

	@SuppressWarnings("static-method")
	@Test
	public void EqualReturnsTrueOnStringExprWithEqualContent() {
		
		StringExpr a, b;
		
		a = new StringExpr( "A" );
		b = new StringExpr( "A" );
		
		assertEquals( a, b );
		assertEquals( b, a );
	}	
}
