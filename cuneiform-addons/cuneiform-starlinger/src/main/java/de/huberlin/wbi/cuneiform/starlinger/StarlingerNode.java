package de.huberlin.wbi.cuneiform.starlinger;

public class StarlingerNode extends JsonMap {

	public static final String ATT_TYPE = "type";
	public static final String ATT_ID = "id";
	public static final String ATT_NAME = "name";
	public static final String ATT_FOREIGN_SRC = "foreign-src";
	public static final String ATT_STRING_CONTENT = "string-content";
	
	public static final String LABEL_NIL = "nil";
	public static final String LABEL_APPEND = "append";
	public static final String LABEL_NATIVE_LAMBDA = "\\\\{}";
	public static final String LABEL_CURRY = "curry";
	public static final String LABEL_FOREIGN_LAMBDA = "\\\\*{}*";
	public static final String LABEL_COND = "cond";
	public static final String LABEL_APPLY = "apply";
	
	public static final String TYPE_APPEND = "Append";
	public static final String TYPE_NIL = "Nil";
	public static final String TYPE_STRING = "StringExpr";
	public static final String TYPE_APPLY = "ApplyExpr";
	public static final String TYPE_CURRY = "CurryExpr";
	public static final String TYPE_FOREIGN_LAMBDA = "ForeignLambdaExpr";
	public static final String TYPE_NATIVE_LAMBDA = "NativeLambdaExpr";
	public static final String TYPE_COND = "CondExpr";
	public static final String TYPE_NAME = "NameExpr";
	
	public static StarlingerNode createAppendNode( String id ) {
		return new StarlingerNode( id, LABEL_APPEND, TYPE_APPEND );
	}	

	public static StarlingerNode createNilNode( String id ) {
		return new StarlingerNode( id, LABEL_NIL, TYPE_NIL );
	}
	
	public static StarlingerNode createStringNode( String id, String label ) {
		
		StarlingerNode node;
		
		node = new StarlingerNode( id, label, TYPE_STRING );
		node.setStringContent( label );
		
		return node;
	}
	
	public static StarlingerNode createCurryNode( String id ) {
		return new StarlingerNode( id, LABEL_CURRY, TYPE_CURRY );
	}
	
	public static StarlingerNode createForeignLambdaNode( String id ) {
		return new StarlingerNode( id, LABEL_FOREIGN_LAMBDA, TYPE_FOREIGN_LAMBDA );
	}
	
	public static StarlingerNode createNativeLambdaNode( String id ) {
		return new StarlingerNode( id, LABEL_NATIVE_LAMBDA, TYPE_NATIVE_LAMBDA );
	}
	
	public static StarlingerNode createCondNode( String id ) {
		return new StarlingerNode( id, LABEL_COND, TYPE_COND );
	}
	
	public static StarlingerNode createApplyNode( String id ) {
		return new StarlingerNode( id, LABEL_APPLY, TYPE_APPLY );
	}
	
	public static StarlingerNode createApplyNode( String id, String label ) {
		return new StarlingerNode( id, label, TYPE_APPLY );
	}
	
	public static StarlingerNode createNameNode( String id, String label ) {
		return new StarlingerNode( id, label, TYPE_NAME );
	}
	
	
	

	public StarlingerNode( String id, String label, String type ) {
		setId( id );
		setLabel( label );
		setType( type );
	}
	
	public String getId() {
		
		String s;
		
		s = getAttribute( ATT_ID );
		
		return s.substring( 1, s.length()-1 );
	}
	
	public void setType( String type ) {		
		putAttribute( ATT_TYPE, "'"+type+"'" );
	}

	public void setLabel( String label ) {
		putAttribute( ATT_NAME, "'"+label+"'" );
	}
	
	public void setId( String id ) {
		putAttribute( ATT_ID, "'"+id+"'" );
	}
	
	public void setStringContent( String content ) {
		putAttribute( ATT_STRING_CONTENT, "'"+content+"'" );
	}
	
}
