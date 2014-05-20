package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Ignore;

public class CombiHelperTest extends BaseSemanticTest {

	@Ignore
	public void crossProduct() {
		
		appendLine( "idx = 'a' 'b' 'c';" );
		appendLine( "read = '0' '1' '2';" );
		appendLine( "deftask align( out : read idx )in bash *{ out=\"$read$idx\" }*" );
		appendLine( "align( read: read idx: idx );" );
		
		interpret();

		assertLengthEquals( 9 );
		
		assertContainsString( "0a" );
		assertContainsString( "0b" );
		assertContainsString( "0c" );
		
		assertContainsString( "1a" );
		assertContainsString( "1b" );
		assertContainsString( "1c" );
		
		assertContainsString( "2a" );
		assertContainsString( "2b" );
		assertContainsString( "2c" );
		
	}
	
	@Ignore
	public void correlation() {
		
		appendLine( "deftask align( sam : idx [fastq1 fastq2] )in bash *{ sam=$idx$fastq1$fastq2 }*" );
		appendLine( "fastq1 = '1a' '1b' '1c';" );
		appendLine( "fastq2 = '2a' '2b' '2c';" );
		appendLine( "idx = 'ia' 'ib' 'ic' 'id';" );
		appendLine( "align( idx: idx fastq1: fastq1 fastq2: fastq2 );" );
		
		interpret();
				
		assertLengthEquals( 12 );

		assertContainsString( "ia1a2a" );
		assertContainsString( "ia1b2b" );
		assertContainsString( "ia1c2c" );
		
		assertContainsString( "ib1a2a" );
		assertContainsString( "ib1b2b" );
		assertContainsString( "ib1c2c" );
		
		assertContainsString( "ic1a2a" );
		assertContainsString( "ic1b2b" );
		assertContainsString( "ic1c2c" );
		
		assertContainsString( "id1a2a" );
		assertContainsString( "id1b2b" );
		assertContainsString( "id1c2c" );

	}

}
