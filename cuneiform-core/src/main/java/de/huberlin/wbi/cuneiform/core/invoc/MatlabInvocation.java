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

package de.huberlin.wbi.cuneiform.core.invoc;

import java.nio.file.Path;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class MatlabInvocation extends OctaveInvocation {
	
	public MatlabInvocation( Ticket ticket ) {
		super( ticket );
	}

	@Override
	protected String getShebang() {
		return "";
	}
	
	@Override
	public Path getExecutablePath( Path location ) {
		return location.resolve( SCRIPT_NAME+".m" );
	}
	
	@Override
	public String[] getCmd() {
		
		return new String[] {
			"matlab",
			"-nodisplay",
			"-nosplash",
			"-r",
			SCRIPT_NAME };
	}
	
	@SuppressWarnings("static-method")
	public String getScriptHead() {
		return "function "+SCRIPT_NAME+"\ntry\n";
	}
	
	@SuppressWarnings("static-method")
	public String getScriptFoot() {
		return "catch ex\nexit( -1 )\nend\nexit\nend\n";
	}
	
	@Override
	public String toScript() throws NotBoundException, NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		// get script header
		buf.append( comment( "script header" ) );
		buf.append( getScriptHead() );

		// bind single output variables to default values
		buf.append( comment( "bind single output variables to default values" ) );
		for( String outputName : getSingleOutputNameSet() )
			buf.append(
				varDef(
					outputName,	
					quote( outputName ) ) );
		buf.append( '\n' );
		
		// bind input parameters
		buf.append( comment( "bind input parameters" ) );
		for( String paramName : getSingleParamNameSet() ) {
			
			if( paramName.equals( CfSemanticModelVisitor.LABEL_TASK ) )
				continue;
			
			buf.append( varDef( paramName, quote( getResolveableBoundToSingleParam( paramName ) ) ) );
		}
		for( String paramName : getReduceParamNameSet() )
			buf.append( varDef( paramName, getReduceParam( paramName ) ) );		
		buf.append( '\n' );
		
		// report stage in file sizes and report error when something is missing
		buf.append( comment( "report stage in file sizes and report error when something is missing" ) );
		buf.append( getStageInCollect() ).append( '\n' );
				
		// insert function body
		buf.append( comment( "insert function body" ) );
		buf.append( getTicket().getBody() ).append( '\n' );
		
		// rename output files
		buf.append( comment( "rename output files" ) );
		buf.append( getOutputRename() ).append( '\n' );
		
		// collect output variables
		buf.append( comment( "collect output variables" ) );
		buf.append( getOutputCollect() ).append( '\n' );
		
		// collect stage out information
		buf.append( comment( "collect stage out information" ) );
		buf.append( getStageOutCollect() ).append( '\n' );
		
		// script footer
		buf.append( comment( "script footer" ) );
		buf.append( getScriptFoot() );
		
		// define necessary functions
		buf.append( comment( "define necessary functions" ) );
		buf.append( getFunDef() ).append( '\n' );
		
		return buf.toString();
	}

}
