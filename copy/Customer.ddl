          EXEC SQL
               DECLARE CUSTOMER TABLE
                       ( customerNumber INTEGER   NOT NULL,
                         firstName      CHAR(10)  NOT NULL,
                         lastName       CHAR(20)  NOT NULL,
                         dateOfBirth    DATE      NOT NULL,
                         houseName      CHAR(20)  NOT NULL,
                         houseNumber    CHAR(4)   NOT NULL,
                         postcode       CHAR(8)   NOT NULL,
                         phonehome      Char(20)  NOT NULL,
                         phonemobile    Char(20)  NOT NULL,
                         emailaddress   Char(100) NOT NULL
                       )
          END-EXEC.