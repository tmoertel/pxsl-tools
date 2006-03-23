<xsl:stylesheet version="1.0" 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:output method = "text"/>

    <xsl:template match = "/">
        <xsl:apply-templates/>
    </xsl:template>
    

    <xsl:template match = "*">

        <xsl:if test = "self::xsl:template">
            <xsl:text>&#xA;</xsl:text>
        </xsl:if>

        <xsl:call-template name = "output-indent"/>
        
        <xsl:value-of select = "name()"/>
        <xsl:for-each select = "@*">
            <xsl:text> -</xsl:text>
            <xsl:value-of select = "name()"/>
            <xsl:text>=</xsl:text>
            <xsl:choose><xsl:when test = "contains(.,' ') or contains(.,')>')">
                <xsl:value-of select = "concat('&lt;{',string(.),'}>')"/>
            </xsl:when><xsl:otherwise>
                <xsl:value-of select = "string(.)"/>
            </xsl:otherwise></xsl:choose>
        </xsl:for-each>
        
        <xsl:if test = "self::xsl:stylesheet">
            <xsl:for-each select = "namespace::*">
                <xsl:text> -xmlns:</xsl:text>
                <xsl:value-of select = "name()"/>
                <xsl:text>=</xsl:text>
                <xsl:value-of select = "string(.)"/>
            </xsl:for-each>
        </xsl:if>
            
        <xsl:text>&#xA;</xsl:text>
        <xsl:apply-templates/>
    </xsl:template>
    
    
    <xsl:template match = "text()">
        <xsl:if test = "normalize-space(.) != ''">
            <xsl:call-template name = "output-indent"/>
            <xsl:text disable-output-escaping = 'yes'><![CDATA[<<]]></xsl:text>
            <xsl:value-of select = "."/>
            <xsl:text disable-output-escaping = 'yes'><![CDATA[>>]]>&#xA;</xsl:text>
        </xsl:if>
    </xsl:template>
    
    <xsl:template name = "output-indent">
        <xsl:for-each select = "ancestor::*">
            <xsl:text>  </xsl:text>
        </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>
