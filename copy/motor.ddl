
          EXEC SQL
               DECLARE MOTOR TABLE
                       ( POLICYNUMBER	    BIGINT  	Not Null,
						 MAKE			    CHAR(15)  	Not Null,
						 MODEL		        CHAR(15)  	Not Null,
						 VALUE		        INTEGER  	Not Null,
						 REGNUMBER	        CHAR(07)  	Not Null,
						 COLOUR		        CHAR(08)  	Not Null,
						 CC			        SMALLINT  	Not Null,
						 yearOfManufacture	DATE  	    Not Null,
						 PREMIUM		    INTEGER  	Not Null,
						 ACCIDENTS	        INTEGER  	Not Null
                       )
          END-EXEC.