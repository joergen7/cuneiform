package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.Color;

public class DarkStyleConf extends StyleConf {

	public DarkStyleConf() {
		setCommentStyle( SimpleStyle.createPlainStyle( new Color( 100, 200, 255 ) ) );
		setCallStyle( SimpleStyle.createItStyle() );
		setApplyStyle( SimpleStyle.createUlStyle() );
		setVarnameStyle( SimpleStyle.createPlainStyle( new Color( 100, 180, 140 ) ) );
		setStatStyle( SimpleStyle.createPlainStyle( Color.WHITE ) );
		setKeywordStyle( SimpleStyle.createBfStyle() );
		setDataStyle( SimpleStyle.createPlainStyle( new Color( 255, 255, 150 ) ) );
		setTypeStyle( SimpleStyle.createPlainStyle( new Color( 200, 255, 200 ) ) );
		setForeignStyle( SimpleStyle.createPlainStyle( Color.LIGHT_GRAY ) );
	}
	
	@Override
	public Color getBackgroundColor() {
		return new Color( 0, 10, 30 );
	}

}
