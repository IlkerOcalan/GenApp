
          EXEC SQL
               DECLARE HOUSE TABLE
                       ( POLICYNUMBER	BIGINT  	Not Null,
					     PROPERTYTYPE	CHAR(15) NOT NULL,
                         BEDROOMS	    SMALLINT NOT NULL,
                         VALUE	        INTEGER  NOT NULL,
                         HOUSENAME	    CHAR(20) NOT NULL,
                         HOUSENUMBER	CHAR(04) NOT NULL,
                         POSTCODE	    CHAR(08) NOT NULL
                       )
          END-EXEC.