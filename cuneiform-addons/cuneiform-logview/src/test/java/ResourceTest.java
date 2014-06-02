import org.junit.Assert;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.logview.gantt.Resource;


public class ResourceTest {

	@Test
	public void partialOverlapMustSignal() {
		
		Resource res;
		
		res = new Resource();
		
		res.host( 4, 2 );
		
		Assert.assertFalse( res.canHost( 3, 2 ) );
		Assert.assertFalse( res.canHost( 5, 2 ) );
	}
	
	@Test
	public void fullOverlapMustSignal() {
		
		Resource res;
		
		res = new Resource();
		
		res.host( 4, 3 );
		
		Assert.assertFalse( res.canHost( 5, 1 ) );
		Assert.assertFalse( res.canHost( 3, 5 ) );
	}
	
	@Test
	public void noOverlapMustNotSignal() {
		
		Resource res;
		
		res = new Resource();
		
		res.host( 4, 2 );
		
		Assert.assertTrue( res.canHost( 2, 1 ) );
		Assert.assertTrue( res.canHost( 7, 1 ) );
	}
}
