          EXEC SQL
               DECLARE ENDOWMENT TABLE
                       ( POLICYNUMBER    BIGINT         NOT NULL,
                         EQUITIES        CHAR(01)       NOT NULL,
					     WITHPROFITS	 CHAR(01)       NOT NULL,
                         MANAGEDFUND	 CHAR(01)       NOT NULL,
                         FUNDNAME	     CHAR(10)       NOT NULL,
                         TERM	         SMALLINT       NOT NULL,
                         SUMASSURED	     INTEGER        NOT NULL,
                         LIFEASSURED	 CHAR(31)       NOT NULL,
                         PADDINGDATA	 VARCHAR(32611)
					   )
          END-EXEC.