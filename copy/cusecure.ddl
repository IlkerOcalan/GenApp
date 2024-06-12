
          EXEC SQL
               DECLARE CUSTOMER_SECURE TABLE
                       ( customerNumber   CHAR(10) NOT NULL,
                         customerPass     CHAR(32) Not Null,
                         state_indicator  CHAR(01) Not Null,
                         pass_changes     INTEGER  Not Null
                       )
          END-EXEC.
