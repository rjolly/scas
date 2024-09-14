<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:m="http://www.w3.org/1998/Math/MathML"
		xmlns:x="http://www.w3.org/1999/xhtml"
		xmlns:s="http://www.w3.org/2000/svg"
                version='1.0'>

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<xsl:strip-space elements="m:*"/>

<xsl:template match="x:a">
	<xsl:choose>
		<xsl:when test="starts-with(@href,'mvn')"><xsl:value-of select="text()"/></xsl:when>
		<xsl:when test="starts-with(@href,'mailto')"><xsl:value-of select="substring-after(@href, ':')"/></xsl:when>
		<xsl:otherwise><xsl:value-of select="@href"/></xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="s:svg"/>

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
	<xsl:text> &amp;&amp; </xsl:text>
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
	<xsl:text> || </xsl:text>
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
	<xsl:text> ^ </xsl:text>
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
	<xsl:text> &gt;&gt; </xsl:text>
	<xsl:apply-templates select="*[3]">
	    	<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="0 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:cn">
	<xsl:param name="p" select="-1"/>
	<xsl:variable name="content">
		<xsl:value-of select="number(text())"/>
	</xsl:variable>
	<xsl:if test="-1 &lt; $p and $content &lt; 0"><xsl:text>(</xsl:text></xsl:if>
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()"/>
	</xsl:call-template>
	<xsl:if test="-1 &lt; $p and $content &lt; 0"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:cn[@type='rational']">
	<xsl:param name="p" select="-1"/>
	<xsl:variable name="numerator">
		<xsl:value-of select="number(text()[1])"/>
	</xsl:variable>
	<xsl:if test="1 &lt; $p or (-1 &lt; $p and $numerator &lt; 0)"><xsl:text>(</xsl:text></xsl:if>
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()[1]"/>
	</xsl:call-template>
	<xsl:text>%%</xsl:text>
	<xsl:call-template name="integer">
		<xsl:with-param name="value" select="text()[2]"/>
	</xsl:call-template>
	<xsl:if test="1 &lt; $p or (-1 &lt; $p and $numerator &lt; 0)"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="m:cn[@type='complex-cartesian']">
	<xsl:text>Complex(</xsl:text>
	<xsl:value-of select="text()[1]"/>
	<xsl:text>, </xsl:text>
	<xsl:value-of select="text()[2]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:ci | m:mi">
	<xsl:call-template name="greek">
		<xsl:with-param name="value" select="text()"/>
	</xsl:call-template>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msub[*[1][self::m:mi] and *[2][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="text()"/>
		<xsl:text>)</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msubsup[*[1][self::m:mi] and *[2][self::m:mrow] and *[3][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[3]/*">
		<xsl:text>_</xsl:text>
	</xsl:for-each>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="text()"/>
		<xsl:text>)</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:ci[*[1][self::m:msup[*[1][self::m:mi] and *[2][self::m:mrow]]]]">
	<xsl:apply-templates select="*[1]/*[1]"/>
	<xsl:for-each select="*[1]/*[2]/*">
		<xsl:text>_</xsl:text>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:minus] and count(*) = 2]">
	<xsl:param name="p" select="-1"/>
	<xsl:if test="-1 &lt; $p"><xsl:text>(</xsl:text></xsl:if>
	<xsl:text>-</xsl:text>
	<xsl:apply-templates select="*[2]">
		<xsl:with-param name="p" select="1"/>
	</xsl:apply-templates>
	<xsl:if test="-1 &lt; $p"><xsl:text>)</xsl:text></xsl:if>
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
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>\</xsl:text>
	<xsl:apply-templates select="*[3]"/>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:root]]">
	<xsl:text>sqrt(</xsl:text>
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:ci]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>(</xsl:text>
	<xsl:for-each select="*[position() &gt; 1]">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:list">
	<xsl:text>List(</xsl:text>
	<xsl:for-each select="*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:vector | m:matrix">
	<xsl:text>Array(</xsl:text>
	<xsl:for-each select="*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:matrixrow">
	<xsl:for-each select="*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
</xsl:template>

<xsl:template match="m:apply[*[1][self::m:cartesianproduct]]">
	<xsl:text>Product(</xsl:text>
	<xsl:apply-templates select="*[2]"/>
	<xsl:text>, </xsl:text>
	<xsl:apply-templates select="*[3]"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:mrow[*[2][self::m:mfenced]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>(</xsl:text>
	<xsl:for-each select="*[2]/*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>).quotient()</xsl:text>
</xsl:template>

<xsl:template match="m:mrow[*[2][self::m:mfenced[@open='[' and @close=']']]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>(</xsl:text>
	<xsl:for-each select="*[2]/*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:msub[*[2][self::m:mfenced]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>(</xsl:text>
	<xsl:for-each select="*[2]/*">
		<xsl:apply-templates select="."/>
		<xsl:if test="position() &lt; last()"><xsl:text>, </xsl:text></xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:msub[*[1][self::m:integers] and *[2][self::m:cn]]">
	<xsl:text>ModInteger(</xsl:text>
	<xsl:text>&#x00022;</xsl:text>
	<xsl:value-of select="*[2]/text()"/>
	<xsl:text>&#x00022;</xsl:text>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:msup[*[2][self::m:cn]]">
	<xsl:apply-templates select="*[1]"/>
	<xsl:text>.pow(</xsl:text>
	<xsl:value-of select="*[2]/text()"/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="m:integers">
	<xsl:text>BigInteger</xsl:text>
</xsl:template>

<xsl:template match="m:rationals">
	<xsl:text>Rational</xsl:text>
</xsl:template>

<xsl:template match="m:complexes">
	<xsl:text>Complex</xsl:text>
</xsl:template>

<xsl:template name="integer">
	<xsl:param name="value"/>
	<xsl:value-of select="$value"/>
	<xsl:if test="number($value) &gt; 2147483647 or number($value) &lt;= -2147483648"><xsl:text>l</xsl:text></xsl:if>
</xsl:template>

<xsl:template name="greek">
	<xsl:param name="value"/>
	<xsl:choose>
		<xsl:when test="$value = '&#x00391;'"><xsl:text>Alpha</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00392;'"><xsl:text>Beta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00393;'"><xsl:text>Gamma</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00394;'"><xsl:text>Delta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00395;'"><xsl:text>Epsilon</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00396;'"><xsl:text>Zeta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00397;'"><xsl:text>Eta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00398;'"><xsl:text>Theta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x00399;'"><xsl:text>Iota</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x0039A;'"><xsl:text>Kappa</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x0039B;'"><xsl:text>Lambda</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x0039C;'"><xsl:text>Mu</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x0039D;'"><xsl:text>Nu</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x0039E;'"><xsl:text>Xi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A0;'"><xsl:text>Pi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A1;'"><xsl:text>Rho</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A3;'"><xsl:text>Sigma</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A4;'"><xsl:text>Tau</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A5;'"><xsl:text>Upsilon</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A6;'"><xsl:text>Phi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A7;'"><xsl:text>Chi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A8;'"><xsl:text>Psi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003A9;'"><xsl:text>Omega</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B1;'"><xsl:text>alpha</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B2;'"><xsl:text>beta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B3;'"><xsl:text>gamma</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B4;'"><xsl:text>delta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B5;'"><xsl:text>epsilon</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B6;'"><xsl:text>zeta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B7;'"><xsl:text>eta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B8;'"><xsl:text>theta</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003B9;'"><xsl:text>iota</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003BA;'"><xsl:text>kappa</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003BB;'"><xsl:text>lambda</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003BC;'"><xsl:text>mu</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003BD;'"><xsl:text>nu</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003BE;'"><xsl:text>xi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C0;'"><xsl:text>pi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C1;'"><xsl:text>rho</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C3;'"><xsl:text>sigma</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C4;'"><xsl:text>tau</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C5;'"><xsl:text>upsilon</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C6;'"><xsl:text>phi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C7;'"><xsl:text>chi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C8;'"><xsl:text>psi</xsl:text></xsl:when>
		<xsl:when test="$value = '&#x003C9;'"><xsl:text>omega</xsl:text></xsl:when>
		<xsl:otherwise><xsl:value-of select="$value"/></xsl:otherwise>
	</xsl:choose>
</xsl:template>

</xsl:stylesheet>