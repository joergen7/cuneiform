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

package de.huberlin.wbi.cuneiform.core.actormodel;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class Actor implements Runnable {
	
	public static final int DELAY = 1000;
	
	protected final Log log;
	private final List<Message> inbox;
	
	public Actor() {
		inbox = new ArrayList<>();
		log = LogFactory.getLog( Actor.class );
	}
	
	public synchronized void sendMsg( Message msg ) {
		
		if( msg == null )
			throw new NullPointerException( "Message must not be null." );
		
		if( log.isDebugEnabled() )
			log.debug( "Sending message. "+msg.getSender().getClass().getSimpleName()+" -> "+getClass().getSimpleName()+" "+msg );


		inbox.add( msg );

	}
	
	private synchronized void processQueue() {

		if( inbox == null )
			throw new NullPointerException( "Inbox must not be null." );
		
		for( Message msg : inbox ) {
			
			if( msg == null )
				throw new NullPointerException( "Message must not be null." );
			
			if( log.isDebugEnabled() )
				log.debug( "Receiving message. "+msg.getSender().getClass().getSimpleName()+" -> "+getClass().getSimpleName()+" "+msg );
			
			processMsg( msg );
		}
		inbox.clear();

	}
	
	protected abstract void processMsg( Message msg );

	@Override
	public void run() {
	
		try {
			
			if( log.isDebugEnabled() )
				log.debug( "Starting actor: "+getClass().getSimpleName() );
			
			while( true ) {
				
					
				Thread.sleep( DELAY );
				
				if( log.isDebugEnabled() )
					log.debug( getClass()+" is alive." );

				preRec();
				processQueue();
				postRec();
				
			}			
		}
		catch( InterruptedException e ) {
			
			if( log.isDebugEnabled() )
				log.debug( getClass()+" caught InterruptedException. Shutting down." );
			
		}
		catch( Throwable e ) {
			
			if( log.isDebugEnabled() )
				log.debug(  getClass()+" caught unexpected throwable: "+e.getMessage() );
			
			e.printStackTrace();
		}
		finally {
			
			if( log.isDebugEnabled() )
				log.debug( "Downing "+getClass()+"." );
			
			shutdown();
		}
	}
	
	protected abstract void shutdown();
	protected void preRec() {
		// override if necessary
	}
	protected void postRec() {
		// override if necessary
	}
	
}
