package de.huberlin.wbi.cuneiform.core.repl;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.NodeVisitorTicketSrc;

public class ReferenceTest {

	private DynamicNodeVisitor dnv;
	private TopLevelContext tlc;

	@Before
	public void setUp() throws HasFailedException, NotDerivableException {
		
		NodeVisitorTicketSrc ticketSrc;
		BaseRepl repl;
		QualifiedTicket qt;
		
		
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
}
