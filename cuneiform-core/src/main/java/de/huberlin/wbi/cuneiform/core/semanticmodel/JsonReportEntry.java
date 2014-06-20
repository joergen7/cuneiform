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

import java.util.UUID;

import org.json.JSONException;
import org.json.JSONObject;

import de.huberlin.wbi.cuneiform.core.invoc.Invocation;

public class JsonReportEntry {
	
	public static final String KEY_INVOC_TIME = "invoc-time";
	public static final String KEY_FILE_SIZE_STAGEIN = "file-size-stagein";
	public static final String KEY_FILE_SIZE_STAGEOUT = "file-size-stageout";
	public static final String KEY_INVOC_OUTPUT = "invoc-output";
	public static final String KEY_INVOC_STDOUT = "invoc-stdout";
	public static final String KEY_INVOC_STDERR = "invoc-stderr";
	public static final String KEY_INVOC_USER = "invoc-user";
	public static final String KEY_INVOC_EXEC = "invoc-exec";
	public static final String KEY_REDUCTION_TIME = "reduction-time";

	public static final String ATT_INVOCID = "invocId";
	public static final String ATT_KEY = "key";
	public static final String ATT_RUNID = "runId";
	public static final String ATT_VALUE = "value";
	public static final String ATT_TASKNAME = "taskname";
	public static final String ATT_TASKID = "taskId";
	public static final String ATT_TIMESTAMP = "timestamp";
	public static final String ATT_LANG = "lang";
	public static final String ATT_FILE = "file";
	
	public static final String LABEL_REALTIME = "realTime";
	
	private UUID runId;
	private Long invocId;
	private String key;
	private String value;
	private String taskname;
	private Long taskId;
	private long timestamp;
	private String lang;
	private String file;
	
	public JsonReportEntry(
		long timestamp, UUID runId, Long taskId, String taskname,
		String lang, Long invocId, String file,  String key, String rawValue ) {
		
		setTimestamp( timestamp );
		setRunId( runId );
		setTaskId( taskId );
		setTaskname( taskname );
		setInvocId( invocId );
		setLang( lang );
		setFile( file );
		setKey( key );
		setValueFromRawString( rawValue );
	}
	
	public JsonReportEntry(
		UUID runId, Long taskId, String taskname,
		String lang, Long invocId, String file, String key, String rawValue ) {
		this( System.currentTimeMillis(), runId, taskId, taskname, lang, invocId, file, key, rawValue );
	}
		
	public JsonReportEntry(
		long timestamp, UUID runId, Long taskId, String taskname,
		String lang, Long invocId, String file, String key, JSONObject obj ) {
			
		setTimestamp( timestamp );
		setRunId( runId );
		setTaskId( taskId );
		setTaskname( taskname );
		setInvocId( invocId );
		setLang( lang );
		setFile( file );
		setKey( key );
		setValueFromJsonObj( obj );
	}
	
	public JsonReportEntry(
		UUID runId, Long taskId, String taskname,
		String lang, Long invocId, String file, String key, JSONObject obj ) {
		this( System.currentTimeMillis(), runId, taskId, taskname, lang, invocId, file, key, obj );
	}
	
	public JsonReportEntry( String raw ) throws JSONException {
		
		JSONObject obj, valueObj;
		String valueString;
		
		try {
			obj = new JSONObject( raw.replace( "\0", "" ) );
			
			
			setTimestamp( obj.getLong( ATT_TIMESTAMP ) );
			setRunId( UUID.fromString( obj.getString( ATT_RUNID ) ) );
			
			if( obj.has( ATT_TASKID ) )
				if( !obj.isNull( ATT_TASKID ) )
					setTaskId( obj.getLong( ATT_TASKID ) );
			
			if( obj.has( ATT_TASKNAME ) )
				if( !obj.isNull( ATT_TASKNAME ) )
					setTaskname( obj.getString( ATT_TASKNAME ) );
			
			if( obj.has( ATT_LANG ) )
				if( !obj.isNull( ATT_LANG ) )
					setLang( obj.getString( ATT_LANG ) );
			
			if( obj.has( ATT_INVOCID ) )
				if( !obj.isNull( ATT_INVOCID ) )
					setInvocId( obj.getLong( ATT_INVOCID ) );
			
			if( obj.has( ATT_FILE ) )
				if( !obj.isNull( ATT_FILE ) )
					setFile( obj.getString( ATT_FILE ) );
			
			setKey( obj.getString( ATT_KEY ) );
			
			try {
				valueObj = obj.getJSONObject( ATT_VALUE );
				setValueFromJsonObj( valueObj );
			}
			catch( JSONException e ) {
				valueString = obj.getString( ATT_VALUE );
				setValueFromRawString( valueString );
			}
		}
		catch( JSONException e ) {
			System.err.println( "[raw]" );
			System.err.println( raw );
			throw e;
		}
	}
	
	public JsonReportEntry( Invocation invoc, String file, String key, String value ) {		
		this(
			invoc.getRunId(),
			invoc.getTaskId(),
			invoc.getTaskName(),
			invoc.getLangLabel(),
			invoc.getTicketId(),
			file,
			key,
			value );
	}
	
	public JsonReportEntry( long timestamp, Invocation invoc, String file, String key, String value ) {		
		this(
			timestamp,
			invoc.getRunId(),
			invoc.getTaskId(),
			invoc.getTaskName(),
			invoc.getLangLabel(),
			invoc.getTicketId(),
			file,
			key,
			value );
	}
	
	public JsonReportEntry( long timestamp, Invocation invoc, String file, String key, JSONObject value ) {		
		this(
			timestamp,
			invoc.getRunId(),
			invoc.getTaskId(),
			invoc.getTaskName(),
			invoc.getLangLabel(),
			invoc.getTicketId(),
			file,
			key,
			value );
	}
	
	public String getFile() {
		return file;
	}

	public Long getInvocId() {
		return invocId;
	}
	
	public String getKey() {
		return key.replace( "\\n", "\n" );
	}
	
	public String getLang() {
		return lang;
	}
	
	public UUID getRunId() {
		return runId;
	}
	
	public JSONObject getValueJsonObj() throws JSONException {
		
		if( !isValueJson() )
			throw new RuntimeException( "Value is not a JSON object, but a string." );

		return new JSONObject( value );
	}
	
	public String getValueRawString() {
		
		String s;
		
		if( !isValueString() )
			throw new RuntimeException( "Value is not a string, but a JSON object." );
		
		s = value.substring( 1, value.length()-1 );
		s = s.replace( "\\n", "\n" );
		s = s.replace( "\\\"", "\"" );
		s = s.replace( "\\\\", "\\" );
		
		return s;
	}
	
	public String getTaskName() {
		return taskname;
	}
	
	public Long getTaskId() {
		return taskId;
	}
	
	public long getTimestamp() {
		return timestamp;
	}
	
	public boolean isValueString() {
		
		if( value.startsWith( "\"" ) )
			return true;
		
		return false;
	}
	
	public boolean hasFile() {
		return file != null;
	}
	
	public boolean hasInvocId() {
		return invocId != null;
	}
	
	public boolean hasLang() {
		return lang != null;
	}
	
	public boolean hasTaskId() {
		return taskId != null;
	}
	
	public boolean hasTaskname() {
		return taskname != null;
	}
	
	public boolean isValueJson() {
		return !isValueString();
	}
	
	public void setFile( String file ) {
		
		if( file == null ) {
			this.file = null;
			return;
		}
		
		if( file.isEmpty() )
			throw new RuntimeException( "File name must not be empty." );
		
		this.file = file;
	}
	
	public void setInvocId( Long invocId ) {		
		this.invocId = invocId;
	}
	
	public void setKey( String key ) {
		
		if( key == null )
			throw new NullPointerException( "Key must not be null." );
		
		if( key.isEmpty() )
			throw new RuntimeException( "Key must not be empty." );
		
		if( key.contains( "\n" ) )
			throw new RuntimeException( "Key must not contain newline character." );
		
		if( key.contains( "\"" ) )
			throw new RuntimeException( "Key must not contain double quote character." );
		
		if( key.contains( "\\" ) )
			throw new RuntimeException( "Key must not contain backslash character." );
		
		this.key = key;
	}
	
	public void setRunId( UUID runId ) {
		
		if( runId == null )
			throw new NullPointerException( "DAG id must not be null." );
		
		this.runId = runId;
	}
	
	public void setValueFromRawString( String value ) {
		
		if( value == null )
			throw new NullPointerException( "Payload must not be null." );
		
		if( value.isEmpty() )
			throw new RuntimeException( "Payload must not be empty." );
		
		this.value = "\""+value.replace( "\\", "\\\\" ).replace( "\n", "\\n" ).replace( "\"", "\\\"" )+"\"";
	}
	
	public void setValueFromJsonObj( JSONObject obj ) {
		
		if( obj == null )
			throw new NullPointerException( "Value JSON object must not be null." );
		
		value = obj.toString();
	}
	
	public void setLang( String lang ) {
		
		if( lang == null ) {
			this.lang = null;
			return;
		}
		
		if( lang.isEmpty() )
			throw new RuntimeException( "Language string must not be empty." );
				
		this.lang = lang;
	}
	
	public void setTaskId( Long taskId ) {		
		this.taskId = taskId;
	}
	
	public void setTaskname( String taskname ) {
		
		if( taskname == null ) {
			this.taskname = null;
			return;
		}

		if( taskname.isEmpty() )
			throw new RuntimeException( "Taskname must not be empty." );
		
		if( taskname.contains( "\n" ) )
			throw new RuntimeException( "Taskname id must not contain newline character." );
		
		if( taskname.contains( "\"" ) )
			throw new RuntimeException( "Taskname id must not contain double quote character." );
		
		if( taskname.contains( "\\" ) )
			throw new RuntimeException( "Taskname must not contain backslash character." );
		
		this.taskname = taskname;
	}
	
	public void setTimestamp( long timestamp ) {		
		this.timestamp = timestamp;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( '{' );
		buf.append( ATT_TIMESTAMP ).append( ':' ).append( timestamp ).append( ',' );
		buf.append( ATT_RUNID ).append( ":\"" ).append( runId ).append( "\"," );

		if( hasTaskId() )
			buf.append( ATT_TASKID ).append( ':' ).append( taskId ).append( ',' );
		
		if( hasTaskname() )
			buf.append( ATT_TASKNAME ).append( ':' ).append( "\"" ).append( taskname ).append( "\"," );
		
		if( hasLang() )
			buf.append( ATT_LANG ).append( ':' ).append( "\"" ).append( lang ).append( "\"," );

		if( hasInvocId() )
			buf.append( ATT_INVOCID ).append( ':' ).append( invocId ).append( ',' );
		
		if( hasFile() )
			buf.append( ATT_FILE ).append( ":\"" ).append( file ).append( "\"," );
		
		buf.append( ATT_KEY ).append( ":\"" ).append( key ).append( "\"," );
		buf.append( ATT_VALUE ).append( ':' ).append( value );
		buf.append( '}' );
		
		return buf.toString();
	}
}
