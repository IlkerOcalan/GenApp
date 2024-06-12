         EXEC SQL
               DECLARE CLAIM TABLE
                    (    ClaimNumber    INTEGER    NOT NULL,
                         PolicyNumber   INTEGER    NOT NULL,
                         ClaimDate      DATE       NOT NULL,
                         Paid           INTEGER    NOT NULL,
                         Value          INTEGER    NOT NULL,
                         Cause          CHAR(256)  NOT NULL,
                         Observations   CHAR(256)  NOT NULL
       )
         END-EXEC.