<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:m="http://www.w3.org/1998/Math/MathML"
		xmlns:x="http://www.w3.org/1999/xhtml"
                version='1.0'>

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<xsl:strip-space elements="m:*"/>

<xsl:template match="x:a">
	<xsl:value-of select="@href"/>
</xsl:template>

<xsl:template match="m:math">
	<xsl:apply-templates/>
</xsl:template>

<xsl:template match="m:true">
	<xsl:text>true</xsl:text>
</xsl:template>


<xsl:template match="m:false">
	<xsl:text>false</xsl:text>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:and]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>&amp;</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:or]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>|</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:xor]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>^</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:not]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:text>!</xsl:text>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:implies]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>=&gt;</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:cn">
	<xsl:value-of select="text()"/>
</xsl:template>

<xsl:template match="m:cn[@type='rational']">
	<xsl:param name="p" select="0"/>
	<xsl:if test="1 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:value-of select="text()[1]"/>
	<xsl:text>/</xsl:text>
	<xsl:value-of select="text()[2]"/>
	<xsl:if test="1 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:cn[@type='complex']">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:value-of select="text()[1]"/>
	<xsl:text>+</xsl:text>
	<xsl:value-of select="text()[2]"/>
	<xsl:text>*sqrt(-1)</xsl:text>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:ci | m:mi">
	<xsl:choose>
		<xsl:when test="text() = '&#x00391;'"><xsl:text>Alpha</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00392;'"><xsl:text>Beta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00393;'"><xsl:text>Gamma</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00394;'"><xsl:text>Delta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00395;'"><xsl:text>Epsilon</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00396;'"><xsl:text>Zeta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00397;'"><xsl:text>Eta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00398;'"><xsl:text>Theta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x00399;'"><xsl:text>Iota</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x0039A;'"><xsl:text>Kappa</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x0039B;'"><xsl:text>Lambda</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x0039C;'"><xsl:text>Mu</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x0039D;'"><xsl:text>Nu</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x0039E;'"><xsl:text>Xi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A0;'"><xsl:text>Pi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A1;'"><xsl:text>Rho</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A3;'"><xsl:text>Sigma</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A4;'"><xsl:text>Tau</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A5;'"><xsl:text>Upsilon</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A6;'"><xsl:text>Phi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A7;'"><xsl:text>Chi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A8;'"><xsl:text>Psi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003A9;'"><xsl:text>Omega</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B1;'"><xsl:text>alpha</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B2;'"><xsl:text>beta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B3;'"><xsl:text>gamma</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B4;'"><xsl:text>delta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B5;'"><xsl:text>epsilon</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B6;'"><xsl:text>zeta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B7;'"><xsl:text>eta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B8;'"><xsl:text>theta</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003B9;'"><xsl:text>iota</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003BA;'"><xsl:text>kappa</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003BB;'"><xsl:text>lambda</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003BC;'"><xsl:text>mu</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003BD;'"><xsl:text>nu</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003BE;'"><xsl:text>xi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C0;'"><xsl:text>pi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C1;'"><xsl:text>rho</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C3;'"><xsl:text>sigma</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C4;'"><xsl:text>tau</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C5;'"><xsl:text>upsilon</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C6;'"><xsl:text>phi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C7;'"><xsl:text>chi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C8;'"><xsl:text>psi</xsl:text></xsl:when>
		<xsl:when test="text() = '&#x003C9;'"><xsl:text>omega</xsl:text></xsl:when>
		<xsl:otherwise><xsl:apply-templates/></xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msub[*[1][self::m:mi] and *[2][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>[</xsl:text>
		<xsl:apply-templates/>
		<xsl:text>]</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msubsup[*[1][self::m:mi] and *[2][self::m:mrow] and *[3][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[3]/*">
		<xsl:text>'</xsl:text>
	</xsl:for-each>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>[</xsl:text>
		<xsl:apply-templates/>
		<xsl:text>]</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msup[*[1][self::m:mi] and *[2][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>'</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:minus] and count(*) = 2]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:text>-</xsl:text>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:minus] and count(*) &gt; 2]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>-</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:plus]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="0 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:text>+</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="0"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:times]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="1 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:text>*</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="1 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:divide]]">
	<xsl:param name="p" select="0"/>
	<xsl:if test="1 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:text>/</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="2"/>
	</xsl:apply-templates>
	<xsl:if test="1 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:power]]">
	<xsl:apply-templates select="*[2]">
	    	<xsl:with-param name="p" select="2"/>
	</xsl:apply-templates>
	<xsl:text>^</xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="2"/>
	</xsl:apply-templates>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:root]]">
	<xsl:text>sqrt(</xsl:text>
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:ci]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>(</xsl:text>
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:vector | m:matrix | m:matrixrow">
	<xsl:text>{</xsl:text>
	<xsl:for-each select="*">
		<xsl:apply-templates/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>}</xsl:text>
</xsl:template>

</xsl:stylesheet>