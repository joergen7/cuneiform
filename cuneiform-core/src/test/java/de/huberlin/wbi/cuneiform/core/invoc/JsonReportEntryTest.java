package de.huberlin.wbi.cuneiform.core.invoc;

import java.util.UUID;

import org.json.JSONException;
import org.junit.Assert;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;

public class JsonReportEntryTest {

	@SuppressWarnings("static-method")
	@Test
	public void testRunIdIsParsed() throws JSONException {
		
		String line;
		JsonReportEntry entry;
		
		line = "{timestamp:1396610901698,runId:\"ae113134-5b4f-4ab1-8da7-81507f9668c9\",taskId:3122,taskname:null,lang:null,invocId:null,key:\"wf-name\",value:\"word-count.cf\"}";
		
		entry = new JsonReportEntry( line );
		Assert.assertEquals(
			"Invalid run ID.",
			UUID.fromString( "ae113134-5b4f-4ab1-8da7-81507f9668c9" ),
			entry.getRunId() );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void testTaskIdCanBeNull() throws JSONException {
		
		String line;
		JsonReportEntry entry;
		
		line = "{timestamp:1396615800633,runId:\"2281e887-5b0a-40d5-8b00-d55b482de1ef\",taskId:null,taskname:null,lang:null,invocId:null,key:\"wf-name\",value:\"montage_0\"}";
		
		entry = new JsonReportEntry( line );
		Assert.assertFalse( "Entry should not have a task id.", entry.hasTaskId() );
		Assert.assertNull( "Task id should be null.", entry.getTaskId() );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void testTaskIdCanBeMissing1() throws JSONException {
		
		String line;
		JsonReportEntry entry;
		
		line = "{timestamp:1398874529654,runId:\"7dbb006c-2b9b-41ce-9dde-9433d1416de0\",key:\"wf-name\",value:\"variant-call-09-setup.cf\"}";
		
		entry = new JsonReportEntry( line );
		Assert.assertFalse( "Entry should not have a task id.", entry.hasTaskId() );
		Assert.assertNull( "Task id should be null.", entry.getTaskId() );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void testTaskIdCanBeMissing2() throws JSONException {
		
		String line;
		JsonReportEntry entry;
		
		line = "{timestamp:1398874529676,runId:\"7dbb006c-2b9b-41ce-9dde-9433d1416de0\",key:\"hiway-event\",value:{\"priority\":\"0\",\"nodes\":[],\"type\":\"container-requested\",\"vcores\":1,\"memory\":1536}}";
		
		entry = new JsonReportEntry( line );
		Assert.assertFalse( "Entry should not have a task id.", entry.hasTaskId() );
		Assert.assertNull( "Task id should be null.", entry.getTaskId() );
	}
}
