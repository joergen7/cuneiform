package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import javax.swing.text.StyledDocument;

public class StyleConf {
	
	public static final Color COL_BACKGROUND = new Color( 0, 10, 30 );

	public static final String KEY_KEYWORD = "keyword";
	public static final String KEY_VARNAME = "varname";
	public static final String KEY_COMMENT = "comment";
	public static final String KEY_CALL = "call";
	public static final String KEY_APPLY = "apply";
	public static final String KEY_STAT = "stat";
	public static final String KEY_DATA = "data";
	public static final String KEY_TYPE = "type";
	public static final String KEY_FOREIGN = "foreign";
	
	
	private Map<String,SimpleStyle> styleMap;
	
	public StyleConf() {
		styleMap = new HashMap<>();
	}
	
	public void setDoc( StyledDocument doc ) {

		SimpleStyle s;
		
		if( doc == null )
			throw new NullPointerException( "Document must not be null." );

		for( String key : styleMap.keySet() ) {
			
			s = styleMap.get( key );
			if( s == null )
				throw new NullPointerException( "Style for '"+key+"' not registered." );
			
			s.addStyleToDoc( doc, key );			
		}
	}
	
	private void setStyle( String key, SimpleStyle s ) {
		
		if( key == null )
			throw new NullPointerException( "Key must not be null." );
		
		if( key.isEmpty() )
			throw new RuntimeException( "Key must not be empty." );
		
		if( s == null )
			throw new NullPointerException( "Style object must not be null." );
		
		styleMap.put( key, s );
	}
	
	public void setApplyStyle( SimpleStyle s ) {
		setStyle( KEY_APPLY, s );
	}
	
	public void setCallStyle( SimpleStyle s ) {
		setStyle( KEY_CALL, s );
	}
	
	public void setKeywordStyle( SimpleStyle s ) {
		setStyle( KEY_KEYWORD, s );
	}
	
	public void setVarnameStyle( SimpleStyle s ) {
		setStyle( KEY_VARNAME, s );
	}
	
	public void setCommentStyle( SimpleStyle s ) {
		setStyle( KEY_COMMENT, s );
	}
	
	public void setStatStyle( SimpleStyle s ) {
		setStyle( KEY_STAT, s );
	}
	
	public void setDataStyle( SimpleStyle s ) {
		setStyle( KEY_DATA, s );
	}
	
	public void setTypeStyle( SimpleStyle s ) {
		setStyle( KEY_TYPE, s );
	}
	
	public void setForeignStyle( SimpleStyle s ) {
		setStyle( KEY_FOREIGN, s );
	}
	
	public static StyleConf createDefaultStyleConf() {
		
		StyleConf sc;
		
		sc = new StyleConf();
		
		sc.setCommentStyle( SimpleStyle.createPlainStyle( new Color( 100, 200, 255 ) ) );
		sc.setCallStyle( SimpleStyle.createItStyle() );
		sc.setApplyStyle( SimpleStyle.createUlStyle() );
		sc.setVarnameStyle( SimpleStyle.createPlainStyle( new Color( 100, 180, 140 ) ) );
		sc.setStatStyle( SimpleStyle.createPlainStyle( Color.WHITE ) );
		sc.setKeywordStyle( SimpleStyle.createBfStyle() );
		sc.setDataStyle( SimpleStyle.createPlainStyle( new Color( 255, 255, 150 ) ) );
		sc.setTypeStyle( SimpleStyle.createPlainStyle( new Color( 200, 255, 200 ) ) );
		sc.setForeignStyle( SimpleStyle.createPlainStyle( Color.LIGHT_GRAY ) );
		
		return sc;
	}

}
