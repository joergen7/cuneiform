package de.huberlin.wbi.cuneiform.core.repl;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Block;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NativeLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SemanticModelException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.NodeVisitorTicketSrc;

public class ReferenceTest {

	private DynamicNodeVisitor dnv;
	private TopLevelContext tlc;

	@Before
	public void setUp() throws HasFailedException, NotDerivableException {
		
		QualifiedTicket qt;
		NodeVisitorTicketSrc ticketSrc;
		BaseRepl repl;
		
		
		qt = mock( QualifiedTicket.class );
		when( qt.getOutputValue() ).thenThrow( new NotDerivableException( "blub" ) );
		
		ticketSrc = mock( NodeVisitorTicketSrc.class );
		when( ticketSrc.requestTicket( any( BaseRepl.class ), any( UUID.class ), any( ApplyExpr.class ) ) ).
		thenReturn( qt );
		
		repl = mock( BaseRepl.class );
		tlc = new TopLevelContext();
		
		dnv = new DynamicNodeVisitor( ticketSrc, repl, tlc );
	}
	
	@Test
	public void nilShouldEvalItself() throws HasFailedException, NotBoundException {
		
		CompoundExpr nil, result;
		
		nil = new CompoundExpr();
		result = dnv.accept( nil );
		assertEquals( nil, result );
	}
	
	@Test
	public void strShouldEvalItself() throws HasFailedException, NotBoundException {
		
		CompoundExpr str, result;
		
		str = new CompoundExpr( new StringExpr( "bla" ) );
		result = dnv.accept( str );
		assertEquals( str, result );
	}
	
	@Test( expected=NotBoundException.class )
	public void undefVarShouldFail() throws HasFailedException, NotBoundException {
		
		CompoundExpr freeVar;
		
		freeVar = new CompoundExpr( new NameExpr( "x" ) );
		dnv.accept( freeVar );
	}
	
	@Test
	public void defVarShouldEvalToBoundValue() throws HasFailedException, NotBoundException {
		
		CompoundExpr boundVar, result, content;
		
		content = new CompoundExpr( new StringExpr( "blub" ) );
		tlc.putAssign( new NameExpr( "x" ), content );
		
		boundVar = new CompoundExpr( new NameExpr( "x" ) );
		result = dnv.accept( boundVar );
		
		assertEquals( content, result );
	}
	
	@Test
	public void defVarShouldCascadeBinding() throws HasFailedException, NotBoundException {
		
		CompoundExpr result, str;
		
		str = new CompoundExpr( new StringExpr( "blub" ) );
		
		tlc.putAssign( new NameExpr( "x" ), new CompoundExpr( new NameExpr( "y" ) ) );
		tlc.putAssign( new NameExpr( "y" ), str );
		
		result = dnv.accept( new CompoundExpr( new NameExpr( "x" ) ) );
		assertEquals( str, result );
	}
	
	@Test
	public void unfinishedTicketShouldEvalToItself() throws HasFailedException, NotBoundException {

		CompoundExpr ce;
		QualifiedTicket qt;
		
		qt =  mock( QualifiedTicket.class );
		when( qt.visit( dnv ) ).thenReturn( dnv.accept( qt ) );
		
		ce = new CompoundExpr( );
		
		assertEquals( ce, dnv.accept( ce ) );
	}
	
	@Test
	public void identityFnShouldEvalArg() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, f, lamList;
		Prototype sign;
		Block body;
		ApplyExpr ae;
		
		
		e = new CompoundExpr( new StringExpr( "bla" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "inp" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "inp" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "inp" ), e );
		
		f = new CompoundExpr( ae );
		
		assertEquals( e, dnv.accept( f ) );
	}
	
	@Test
	public void lamShouldEvalItself() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "blub" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		assertEquals( lamList, dnv.accept( lamList ) );
	}
	
	@Test
	public void multipleOutputShouldBeBindable() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr e1, e2, lamList;
		ApplyExpr f1, f2;
		Block body;
		
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out1" ) );
		sign.addOutput( new NameExpr( "out2" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		e1 = new CompoundExpr( new StringExpr( "bla" ) );
		e2 = new CompoundExpr( new StringExpr( "blub" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out1" ), e1 );
		body.putAssign( new NameExpr( "out2" ), e2 );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		f1 = new ApplyExpr( 1, false );
		f1.setTaskExpr( lamList );
		
		f2 = new ApplyExpr( 2, false );
		f2.setTaskExpr( lamList );
		
		assertEquals( e1, dnv.accept( f1 ) );
		assertEquals( e2, dnv.accept( f2 ) );
	}
	
	@Test( expected=SemanticModelException.class )
	public void applicationShouldIgnoreCallingContext() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "x" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		tlc.putAssign( new NameExpr( "x" ), new CompoundExpr( new StringExpr( "blub" ) ) );
		dnv.accept( new CompoundExpr( ae ) );
	}
	
	@Test
	public void bindingShouldOverrideBody() throws HasFailedException, NotBoundException {
		
		CompoundExpr f, g, h, lamList;
		Prototype sign;
		Block body;
		ApplyExpr ae;
		
		f = new CompoundExpr( new StringExpr( "blub" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "x" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "x" ), new CompoundExpr( new StringExpr( "bla" ) ) );
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "x" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "x" ), f );
		
		g = new CompoundExpr( ae );
		h = dnv.accept( g );
		
		assertEquals( f, h );
	}
	
	@Test( expected=NullPointerException.class )
	public void appWithEmptyTaskListShouldFail() throws HasFailedException, NotBoundException {
		
		ApplyExpr ae;
		
		ae = new ApplyExpr( 1, false );
		dnv.accept( new CompoundExpr( ae ) );
	}
	
	@Test
	public void crossProductShouldBeDerivableFromSignature() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr e1, e2, lamList, f1, f2;
		Block body;
		ApplyExpr ae1, ae2;
		
		e1 = new CompoundExpr();
		e1.addSingleExpr( new StringExpr( "A" ) );
		e1.addSingleExpr( new StringExpr( "B" ) );
		
		e2 = new CompoundExpr();
		e2.addSingleExpr( new StringExpr( "1" ) );
		e2.addSingleExpr( new StringExpr( "2" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out1" ) );
		sign.addOutput( new NameExpr( "out2" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "p1" ) );
		sign.addParam( new NameExpr( "p2" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out1" ), new CompoundExpr( new NameExpr( "p1" ) ) );
		body.putAssign( new NameExpr( "out2" ), new CompoundExpr( new NameExpr( "p2" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lamList );
		ae1.putAssign( new NameExpr( "p1" ), e1 );
		ae1.putAssign( new NameExpr( "p2" ), e2 );
		
		ae2 = new ApplyExpr( 2, false );
		ae2.setTaskExpr( lamList );
		ae2.putAssign( new NameExpr( "p1" ), e1 );
		ae2.putAssign( new NameExpr( "p2" ), e2 );
		
		f1 = new CompoundExpr();
		f1.addSingleExpr( new StringExpr( "A" ) );
		f1.addSingleExpr( new StringExpr( "A" ) );
		f1.addSingleExpr( new StringExpr( "B" ) );
		f1.addSingleExpr( new StringExpr( "B" ) );
		
		f2 = new CompoundExpr();
		f2.addSingleExpr( new StringExpr( "1" ) );
		f2.addSingleExpr( new StringExpr( "2" ) );
		f2.addSingleExpr( new StringExpr( "1" ) );
		f2.addSingleExpr( new StringExpr( "2" ) );
		
		assertEquals( f1, dnv.accept( new CompoundExpr( ae1 ) ) );
		assertEquals( f2, dnv.accept( new CompoundExpr( ae2 ) ) );
	}
}
