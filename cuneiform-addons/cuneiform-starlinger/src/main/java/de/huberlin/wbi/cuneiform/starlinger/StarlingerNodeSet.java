package de.huberlin.wbi.cuneiform.starlinger;

public class StarlingerNodeSet extends JsonMap {

	public void addNode( StarlingerNode node ) {
		putAttribute( node.getId(), node.toString() );
	}
}
