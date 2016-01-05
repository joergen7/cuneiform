package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Test;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class CondExprTest {
	
	@SuppressWarnings("static-method")
	@Test
	public void constructorSetsThreeExpr() {
		
		CondExpr condExpr;
		CompoundExpr ifExpr, thenExpr, elseExpr;
		
		ifExpr = mock( CompoundExpr.class );
		thenExpr = mock( CompoundExpr.class );
		elseExpr = mock( CompoundExpr.class );
		
		condExpr = new CondExpr( ifExpr, thenExpr, elseExpr );
		assertEquals( ifExpr, condExpr.getIfExpr() );
		assertEquals( thenExpr, condExpr.getThenExpr() );
		assertEquals( elseExpr, condExpr.getElseExpr() );
	}
	
	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected=IllegalArgumentException.class )
	public void constructorThrowsIaeOnNullIfExpr() {
		
		CondExpr condExpr;
		CompoundExpr thenExpr, elseExpr;
		
		thenExpr = mock( CompoundExpr.class );
		elseExpr = mock( CompoundExpr.class );
		
		condExpr = new CondExpr( null, thenExpr, elseExpr );
	}

	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected=IllegalArgumentException.class )
	public void constructorThrowsIaeOnNullThenExpr() {
		
		CondExpr condExpr;
		CompoundExpr ifExpr, elseExpr;
		
		ifExpr = mock( CompoundExpr.class );
		elseExpr = mock( CompoundExpr.class );
		
		condExpr = new CondExpr( ifExpr, null, elseExpr );
	}

	@SuppressWarnings({ "static-method", "unused" })
	@Test( expected=IllegalArgumentException.class )
	public void constructorThrowsIaeOnNullElseExpr() {
		
		CondExpr condExpr;
		CompoundExpr ifExpr, thenExpr;
		
		ifExpr = mock( CompoundExpr.class );
		thenExpr = mock( CompoundExpr.class );
		
		condExpr = new CondExpr( ifExpr, thenExpr, null );
	}

}
