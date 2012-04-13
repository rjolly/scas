<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:m="http://www.w3.org/1998/Math/MathML"
                version='1.0'>
                
<xsl:output method="text" indent="no" encoding="UTF-8"/>

<xsl:strip-space elements="m:*"/>

<xsl:template match="m:math">
	<xsl:apply-templates/>
</xsl:template>

<xsl:template match="m:cn">
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()"/>
	</xsl:call-template>
</xsl:template>

<xsl:template match="m:cn[@type='rational']">
	<xsl:text>frac(</xsl:text>
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()[1]"/>
	</xsl:call-template>
	<xsl:text>, </xsl:text>
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()[2]"/>
	</xsl:call-template>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:ci | m:mi">
	<xsl:choose>
		<xsl:when test="text() = '&#x003B1;'"><xsl:text>alpha</xsl:text></xsl:when>
		<xsl:otherwise><xsl:apply-templates/></xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msub[*[1][self::m:mi] and *[2][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="text()"/>
		<xsl:text>)</xsl:text>
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
	<xsl:text>pow(</xsl:text>
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>, </xsl:text>
	<xsl:apply-templates select="*[3]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:vector | m:matrix | m:matrixrow">
	<xsl:text>Array(</xsl:text>
	<xsl:for-each select="*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position()!=last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:mfenced">
	<xsl:text>(</xsl:text>
	<xsl:for-each select="*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position()!=last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:integers">
	<xsl:text>ZZ</xsl:text>
</xsl:template>

<xsl:template match="m:rationals">
	<xsl:text>QQ</xsl:text>
</xsl:template>

<xsl:template match="m:complexes">
	<xsl:text>CC</xsl:text>
</xsl:template>

<xsl:template name="integer">
	<xsl:param name="value"/>
	<xsl:variable name="b" select="number($value) &gt; 2147483647 or number($value) &lt;= -2147483648"/>
	<xsl:if test="$b"><xsl:text>BigInteger(&#x00022;</xsl:text></xsl:if>
	<xsl:value-of select="$value"/>
	<xsl:if test="$b"><xsl:text>&#x00022;)</xsl:text></xsl:if>
</xsl:template>

</xsl:stylesheet>