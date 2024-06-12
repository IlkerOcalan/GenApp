       ID DIVISION.
       PROGRAM-ID. ORPHAN01.
      *    THIS DEMONSTRATES AN ORPHAN PROGRAM 
      *    WHICH HAS A DEAD CODE SECTION.
      *
      *    (C) 2023 IBM - ILKER OCALAN RESERVED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-FLEX-ES.
       OBJECT-COMPUTER. IBM-FLEX-ES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
           EXEC SQL DECLARE SYSIBM.CUSTOMERS TABLE
           ( IBMREQD                        CHAR(1) NOT NULL
           ) END-EXEC.
      *
       01 IBMREQD                           PIC X(1).
      *


       LINKAGE SECTION.


       PROCEDURE DIVISION.

       ORPHAN01-MAINLINE.
      * Call procedure to do SQL call
           PERFORM A805-DUMMY-SQL-CALL
		   IF SQLCODE NOT = 0
		      MOVE 8 to RETURN-CODE
		   END-IF.
		   CALL "ORPHAN03".
		   CALL "ORPHAN02".
       ORPHAN01-MAINLINE-EXIT.
		   GOBACK.
      
       A100-PROCESS-MAP SECTION.
           DISPLAY "THIS IS A DEAD CODE SECTION".
		   DISPLAY "A100 is never called from anywhere".
       A100-PROCESS-MAP-EXIT.
		   EXIT.
               
       A805-DUMMY-SQL-CALL SECTION.
           EXEC SQL
               SELECT IBMREQD
                    INTO :IBMREQD
                    FROM SYSIBM.CUSTOMERS
           END-EXEC.
       A805-DUMMY-SQL-CALL-EXIT.
		   EXIT.
     *
