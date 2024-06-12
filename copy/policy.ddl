         EXEC SQL
               DECLARE POLICY TABLE
					(    policyNumber   INTEGER    NOT NULL,
                         CUSTOMERNUMBER CHAR(10)   NOT NULL,
                         ISSUEDATE      CHAR(10)   NOT NULL,
                         EXPIRYDATE     CHAR(10)   NOT NULL,
					     POLICYTYPE	    CHAR(01)   NOT NULL,
                         LASTCHANGED	CHAR(26)   NOT NULL,
                         BROKERID	    BIGINT     NOT NULL,
                         brokersReference CHAR(10) NOT NULL,
                         PAYMENT	    INTEGER    NOT NULL,
                         COMMISSION     SMALLINT   NOT NULL
       )
         END-EXEC.