/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.huberlin.wbi.cuneiform.core.semanticmodel;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.cre.LocalCreActor;
import de.huberlin.wbi.cuneiform.core.repl.CmdlineRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class BaseSemanticTest {

	private TicketSrcActor ticketSrc;
	private CmdlineRepl repl;
	private StringBuffer buf;
	private File sandbox;
	
	
	public BaseSemanticTest() {
		
		ExecutorService executor;
		LocalCreActor creActor;
		
		sandbox = new File( "/tmp/cuneiform" );
		sandbox.mkdir();

		
		executor = Executors.newCachedThreadPool();
		
		creActor = new LocalCreActor( sandbox );
		executor.submit( creActor );
		
		ticketSrc = new TicketSrcActor( creActor );
		executor.submit( ticketSrc );
		executor.shutdown();
		
	}
	
	@Before
	public void before() {

		for( File f : sandbox.listFiles() )
			FileUtils.deleteQuietly( f );

		repl = new CmdlineRepl( ticketSrc );
		buf = new StringBuffer();
	}
	
	@After
	public void after() {
		for( File f : sandbox.listFiles() )
			FileUtils.deleteQuietly( f );
	}
		
	public void appendLine( String line ) {
		buf.append( line ).append( '\n' );
	}
	
	public void interpret() {
		
		String script;
		
		script = buf.toString();
		
		System.out.println( "[script]" );
		System.out.println( script.trim() );
		System.out.println( "[run]" );

		
		repl.interpret( script );
		
		while( !repl.hasAns() )
			
			try {
				Thread.sleep( Actor.DELAY );
			}
			catch( InterruptedException e ) {
				e.printStackTrace();
			}		
	}
	
	public void assertLengthEquals( int len ) {
		
		CompoundExpr ce;
		
		if( len < 0 )
			throw new RuntimeException( "Length must not be smaller than 0." );
		
		System.out.println( "[test] length equals "+len );
		
		ce = repl.getAns();
		
		if( ce.getNumSingleExpr() != len ) {
			
			System.out.println( "FAILED" );
			Assert.fail( ce+" had not length "+len+"." );
		}
		
	}
	
	public void assertContainsString( String value ) {
		assertContainsExpr( "'"+value+"'" );
	}
	
	public void assertContainsExpr( String value ) {
		
		CompoundExpr ce;
		int i, n;
		
		
		ce = repl.getAns();

		System.out.println( "[test] contains "+value );
		
		n = ce.getNumSingleExpr();
		
		for( i = 0; i < n; i++ )
			if( ce.getSingleExpr( i ).toString().equals( value ) )
				return;
		
		System.out.println( "FAILED" );
		Assert.fail( ce+" did not contain "+value+"." );
	}
}
