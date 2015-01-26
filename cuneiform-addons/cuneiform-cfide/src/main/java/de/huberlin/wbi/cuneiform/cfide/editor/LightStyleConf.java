package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.Color;

public class LightStyleConf extends StyleConf {

	public LightStyleConf() {
		setCommentStyle( SimpleStyle.createPlainStyle( new Color( 0, 100, 155 ) ) );
		setCallStyle( SimpleStyle.createItStyle() );
		setApplyStyle( SimpleStyle.createUlStyle() );
		setVarnameStyle( SimpleStyle.createPlainStyle( new Color( 0, 80, 40 ) ) );
		setStatStyle( SimpleStyle.createPlainStyle( Color.BLACK ) );
		setKeywordStyle( SimpleStyle.createBfStyle() );
		setDataStyle( SimpleStyle.createPlainStyle( new Color( 100, 100, 0 ) ) );
		setTypeStyle( SimpleStyle.createPlainStyle( new Color( 70, 125, 70 ) ) );
		setForeignStyle( SimpleStyle.createPlainStyle( Color.DARK_GRAY ) );
	}
	
	@Override
	public Color getBackgroundColor() {
		return Color.WHITE;
	}
}
