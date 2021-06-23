         PRINT NOGEN
         BEGIN NAME=A7XB,VERSION=&VV,SHR=NONE,XCL=NONE
.* Endevor ALCS241 merge - force assembly                        DL0070
*---------------------------------------------------------------------*
*                                                                     *
*   TTTTTTTTTTTTTTTTT    222222222        SSSSSSSSS       EEEEEEEEEEE *
*  TTTTTTTTTTTTTTTTT    22222222222     SSSSSSSSSSSS     EEEEEEEEEEE  *
*         TTTT         22      22222   SSSS     SSSS     EEE          *
*        TTTT                 22222    SSSSS           EEE            *
*       TTTT                 2222       SSSSSSS       EEEEEEEE        *
*       TTTT               22222           SSSSSSS    EEEEEEE         *
*      TTTT              2222               SSSSSS   EEE              *
*      TTT           22222     2     SS     SSSSS   EEE               *
*     TTTT         2222222222222    SSSS  SSSSSS    EEEEEEEEEEEE      *
*    TTTT          2222222222222      SSSSSSSS     EEEEEEEEEEEE       *
*                                                                     *
*---------------------------------------------------------------------*
*        This segment is part of the T2SE package, and is marketed    *
*        and sold by Datalex Netherlands BV in cooperation with KLM   *
*        Royal Dutch Airlines.                                        *
*        This Product contains "Restricted materials of Datalex       *
*        Netherlands and KLM Royal Dutch Airlines".                   *
*        c COPYRIGHT Datalex/KLM  2003                                *
*        LICENSED MATERIAL   Program property of Datalex/KLM          *
*---------------------------------------------------------------------*
*        Description:                                                 *
*        This program handles the ZUTXR entry. The entry is used      *
*        to build and search the cross-reference file.                *
*                                                                     *
*        ZUTXR HELP          - display help file for ZUTXR            *
*        ZUTXR BUILD         - build xref database                    *
*        ZUTXR SEARCH TEXT-xx TYP-x VER-x STA-x                       *
*                      :       :     :     +- start                   *
*                      :       :     +------- version                 *
*                      :       +------------- type                    *
*                      +--------------------- search text             *
*                                                                     *
*        Program description:                                         *
*        -Logical function #1                                         *
*        -Logical function #2                                         *
*        -                                                            *
*        -                                                            *
*        -                                                            *
*        -                                                            *
*        etc.                                                         *
*                                                                     *
*  Input description                                                  *
*  -----------------                                                  *
*                                                                     *
*  Data level usage                                                   *
*  -----------------                                                  *
*                                                                     *
*  Work area usage                                                    *
*  -----------------                                                  *
*                                                                     *
*  Return conditions                                                  *
*  ----------------                                                   *
*---------------------------------------------------------------------*
         EJECT ,
*---------------------------------------------------------------------*
*        Data macros                                                  *
*---------------------------------------------------------------------*
         SPACE  ,
         T2SEEQ ,
         COPY  T2SESET                                          @CONSTA
         QR0YKL REG=R2             XREF DATABASE
         MI0MI  REG=R14            D0 - IMG
         AP4WK  REG=R4             D7 - WORK AREA
         PRINT GEN
         SW01SR REG=R5             TO CREATE DYNAMIC KEYS
         PRINT NOGEN
A7XAWKA  DSECT
         DS    F                   USED BY ALASC
SUBR1SAV DS    F                   SUBROUTINE SAVE REGISTER
SUBR2SAV DS    F                   SUBROUTINE SAVE REGISTER
IERROR   DS    CL1                 ERROR INDICATION
REQUEST  DS    CL8                 INPUT REQUEST
TEXT     DS    CL50                SEARCH TEXT STRING
START    DS    CL1                 SEARCH FROM START INDICATION
TYPE     DS    CL1                 COMMAND TYPE
VERSION  DS    CL1                 SCRIPT VERSION
OUTIND   DS    CL1                 INDICATOR FOR OUTPUT LINE
*                                  X'80' - HEADER LINE
*                                  X'40' - DETAIL LINE
*                                  X'20' - LAST LINE
*                                  X'10' - INCOMPLETE LINE
SELECT   DS    CL1                 INDICATION FOR SELECTED ITEM FOUND
NBRHITS  DS    H                   NUMBER OF ITEMS FOUND IN SEARCH
KEYNBR   DS    H                   NUMBER OF KEYS
         ORG   KEYNBR
KEYAREA  DS    CL(L'KEYNBR+4*L'SW01KIT)  SPACE FOR 4 KEYS
QRTXT    DS    CL50                WORKAREA FOR TEXT FIELD
OUTLIN   DS    CL60                BUILD UP OUTPUT AREA
         AIF   (&TALCS).BYP0                                    @CONSTA
#IS#     CSECT ,
         AGO   .BYP1                                            @CONSTA
.BYP0    ANOP                                                   @CONSTA
         RSECT ,                                                @CONSTA
.BYP1    ANOP                                                   @CONSTA
         USING A7XAWKA,R7          WORK AREA
         EJECT ,
******
* OBTAIN WORK BLOCK FOR AP4WK
******
         #IF   LEVTA,D7,NOTUSED    THEN NO WORKBLOCK YET
          GETCC D7,L2,FILL=00
         #EIF
         L     R4,CE1CR7           BASE WORKBLOCK
******
* OPEN XREF FILE
******
         DBOPN REF=QR0YKL,REG=R2,DETAC,NOHOLD
         SPACE ,
******
* SET UP KEYS FOR DBRED BASED UPON INPUT
******
         XC    KEYAREA,KEYAREA     CLEAR AREA TO SET UP KEYS
         LA    R5,KEYAREA          ADDRESS KEYAREA
         SPACE ,
* PRIME KEY
         LH    R14,KEYNBR
         LA    R14,1(R14)
         STH   R14,KEYNBR
         MVC   SW01DIS,=Y(QR0YKEY-QR0YREC)   DISPLACEMENT
         MVI   SW01CON,#DF_EQ                MUST BE EQUAL
         MVI   SW01MSK,#QR0YK80              KEY
         MVI   SW01ID1,#DF_UP+#DF_CONST
         SPACE ,
* TEXT
         LH   R14,KEYNBR
         LA   R14,1(R14)
         STH  R14,KEYNBR
         LA   R5,L'SW01KIT(R5)               NEXT KEY ITEM
         MVC  SW01DIS,=Y(QR0YCMD-QR0YREC)    DISPLACEMENT
         MVI  SW01ID1,#DF_UP+#DF_CHAR
*
         #IF   CLI,START,EQ,C'Y',AND,        THEN SEARCH FROM START
         #     TEXT,NONZERO                  AND TEXT ENTERED
          LA   R0,L'QR0YCMD                  SIZE OF TEXT
          LA   R15,TEXT                      TEXT FIELD
          #DO  TIMES=(R0)                    CALCULATE SIZE OF INPUT
          #DOEX CLI,0(R15),EQ,X'00'          END OF TEXT ?
           TR   0(1,R15),TRTTAB
           LA   R15,1(R15)                   NEXT CHARACTER
          #EDO
          LA   R14,TEXT
          ST   R14,SW01SEA                   SEARCH ARGUMENT
          SR   R15,R14                       GIVES SIZE OF TEXT
          STCM R15,3,SW01LEN                 LENGTH IN SEARCH
          MVI  SW01CON,#DF_EQ                MUST BE EQUAL TO
         #ELSE
          LA   R14,CMDCMP
          ST   R14,SW01SEA                   SEARCH ARGUMENT
          MVI  SW01CON,#DF_GT                MUST BE GREATER THAN
          MVC  SW01LEN,=Y(L'QR0YCMD)         LENGTH
         #EIF
         SPACE ,
* VERSION
         LH   R14,KEYNBR
         LA   R14,1(R14)
         STH  R14,KEYNBR
         LA   R5,L'SW01KIT(R5)               NEXT KEY ITEM
         MVC  SW01DIS,=Y(QR0YVER-QR0YREC)    DISPLACEMENT
         MVC  SW01MSK,VERSION                KEY
         MVI  SW01ID1,#DF_NOORG+#DF_CONST
         #IF   VERSION,NONZERO     THEN VERSION PART OF SEARCH
          MVI  SW01CON,#DF_EQ                MUST BE EQUAL
         #ELSE
          MVI  SW01CON,#DF_GT                MUST BE GREATER THAN
         #EIF
         SPACE ,
* TYPE
         LH   R14,KEYNBR
         LA   R14,1(R14)
         STH  R14,KEYNBR
         LA   R5,L'SW01KIT(R5)               NEXT KEY ITEM
         MVC  SW01DIS,=Y(QR0YTYP-QR0YREC)    DISPLACEMENT
         MVC  SW01MSK,TYPE                   KEY
         MVI  SW01ID1,#DF_NOORG+#DF_CONST
         #IF   TYPE,NONZERO        THEN TYPE PART OF SEARCH
          MVI  SW01CON,#DF_EQ                MUST BE EQUAL
         #ELSE
          MVI  SW01CON,#DF_GT                MUST BE GREATER THAN
         #EIF
         SPACE ,
* ADD KEYS FOR DBRED
         DBKEY REF=QR0YKL,KEYLIST=KEYAREA
         SPACE ,
******
* LOOP THROUGH THE XREF FILE
******
         XC    NBRHITS,NBRHITS
         DBRED BEGIN,REF=QR0YKL,REG=R2,ERRORA=QRERROR
         #IF   DBFOUND,YES         THEN AT LEAST ONE ITEM
          MVI  OUTIND,X'80'        HEADERLINE
         #PERF R14,SendOutputLine
         #ELSE                     THEN NO ITEMS
          MVI  OUTIND,X'08'        NO-ITEMS MESSAGE
         #PERF R14,SendOutputLine
         #EIF
         #DO   WHILE=(DBFOUND,YES)
         SPACE ,
* CHECK IF TEXT MATCH
          MVI   SELECT,C'Y'        INDICATE ITEM FOUND
          #IF   CLI,START,NE,C'Y',AND,
          #     TEXT,NONZERO       THEN TEXT SEARCH
           SR   R14,R14            CALCULATE SIZE OF INPUT
           LA   R15,TEXT           TEXT FIELD
           LA   R0,L'TEXT          MAX SIZE
           #DO   TIMES=(R0)         LOOP THROUGH TEXT
           #DOEX CLI,0(R15),EQ,X'00'  THEN END OF INPUT
            TR   0(1,R15),TRTTAB
            LA   R14,1(R14)         ADD 1 TO COUNT
            LA   R15,1(R15)         NEXT FIELD IN TEXT
           #EDO
           SR    R0,R0
           ICM   R0,3,QR0YSIZ       SIZE OF LREC
           SH    R0,=Y(QR0YTXT-QR0YREC)  GIVES LENGTH OF TEXT
           #IF   CR,R0,NL,R14       THEN SIZE ENOUGH
            SR    R0,R14            GIVES TIMES TO CHECK
            AH    R0,=H'1'           ADD 1 FOR LOOP CONTROL
            LA    R15,QR0YTXT        COMMAND FIELD IN XREF
            BCTR  R14,0              LENGTH MINUS 1 FOR EXECUTE
            #DO   TIMES=(R0)         LOOP THROUGH XREF FIELD
TRANS        TR   0(0,R15),TRTTAB
             EX   R14,TRANS          TO UPPER CASE
CHECK        CLC  TEXT(0),0(R15)
             EX   R14,CHECK          ITEM FOUND?
            #DOEX EQ                 EXIT LOOP WHEN EQUAL
             LA   R15,1(R15)         NEXT IN XREF FIELD
            #EDO
            #IF   R0,ZERO            THEN ITEM NOT FOUND
             MVI   SELECT,C'N'       INDICATE ITEM NOT FOUND
            #EIF
           #ELSE                     THEN SIZE NOT ENOUGH
            MVI    SELECT,C'N'       SHOW ITEM NOT FOUND
           #EIF
          #EIF
          #IF   CLI,SELECT,EQ,C'Y'  THEN ITEM FOUND
           LH    R14,NBRHITS
           LA    R14,1(R14)
           STH   R14,NBRHITS
           MVI   OUTIND,X'40'       DETAILED RESPONSE LINE
          #PERF  R14,SendOutputLine
         #EIF
         #DOEX  CLC,NBRHITS,NL,=H'500'  MAXIMUM # OF HITS
          DBRED REF=QR0YKL,REG=R2,ERRORA=QRERROR
         #EDO
******
* CLOSE XREF FILE
******
         DBCLS REF=QR0YKL,RELEASE
         #IF    CLC,NBRHITS,GT,=H'500'  THEN MAXIMUM REACHED
          MVI   OUTIND,X'10'
         #PERF  R14,SendOutputLine
         #EIF
         MVI   OUTIND,X'20'        INDICATE LAST LINE
         #PERF R14,SendOutputLine
         BACKC ,
         EJECT ,
*---------------------------------------------------------------------*
*   S U B R O U T I N E S - LEVEL 1                                   *
*---------------------------------------------------------------------*
         SPACE ,
*---------------------------------------------------------------------*
*   Send output to 'normal' terminal using WTOPC or to T2SE           *
*   application using A700.                                           *
*---------------------------------------------------------------------*
 #SUBR SendOutputLine,R14,SUBR1SAV
         L     R14,CE1CR0              BASE 06-IMG
         #IF   MI0PGM,NE,=C'DART'      THEN TERMINAL ACTIVATION
          #IF   CLI,OUTIND,EQ,X'80'    THEN HEADER LINE
           WTOPC TEXTA=A7XBHDR,HEADER=NO,CHAIN=YES,ENDOFM=NO,          >
               PREFIX=UTXR,NUM=1,LET=I
          #ELIF CLI,OUTIND,EQ,X'40'    THEN DETAIL LINE
         PRINT GEN
           XC   QRTXT,QRTXT
           LH   R14,QR0YSIZ             SIZE
           SH   R14,=Y(QR0YTXT-QR0YSIZ) MINUS FIXED FIELDS
           #IF   CH,R14,GT,=Y(L'QRTXT)  EXCEEDING MAX SIZE ?
            LA   R14,L'QRTXT            USE MAX SIZE
           #EIF
           BCTR  R14,0                  MINUS 1 FOR EXECUTE
MOVE1      MVC   QRTXT(0),QR0YTXT       MOVE TEXT
           EX    R14,MOVE1              MOVE TEXT
           WTOPC TEXTA=A7XBLIN,                                        >
               SUB=(CHARA,QR0YNAM,                                     >
               CHARA,QR0YGRP,                                          >
               CHARA,QR0YVER,                                          >
               CHARA,QRTXT),                                           >
               HEADER=NO,CHAIN=YES,ENDOFM=NO
         PRINT NOGEN
          #ELIF CLI,OUTIND,EQ,X'20'    THEN LAST LINE
           WTOPC TEXT='End of ZUTXR display',                          >
               CHAIN=YES,ENDOFM=YES,HEADER=NO
          #ELIF CLI,OUTIND,EQ,X'10'    THEN MAX COMMANDS LINE
           WTOPC TEXT='Too many hits - change search arguments',       >
               CHAIN=YES,ENDOFM=NO,HEADER=NO
          #ELIF CLI,OUTIND,EQ,X'08'    THEN NO ITEMS LINE
           WTOPC TEXT='No match found',                                >
               CHAIN=YES,ENDOFM=NO,HEADER=NO
          #EIF
         SPACE ,
*
* DIRECT ACTIVATION
*
         #ELSE                         THEN DIRECT ACTIVATION
          L     R4,CE1CR7              BASE AP4WK
          XC    A700INT,A700INT        CLEAR INTERFACE AREA
          #IF   CLI,OUTIND,EQ,X'80'    THEN HEADER LINE
           MVC   OUTLIN(22),A7XBHDR     HEADERLINE
           MVI   A700ACT,T2SE_ADD      INDICATE 'ADD LINE'
           LA    R14,22                SIZE OF OUTPUT
           ST    R14,A700SIZ           STORE FOR A700
          #ELIF CLI,OUTIND,EQ,X'40'    THEN DETAIL LINE
           MVC   OUTLIN(10),QR0YNAM    NAME OF SCRIPT
           MVI   OUTLIN+10,C' '
           MVC   OUTLIN+11(3),QR0YGRP  GROUP OF SCRIPT
           MVI   OUTLIN+14,C' '
           MVC   OUTLIN+15(1),QR0YVER  VERSION OF SCRIPT
           MVC   OUTLIN+16(3),=C' - '
           LH    R14,QR0YSIZ             SIZE
           SH    R14,=Y(QR0YTXT-QR0YSIZ) MINUS FIXED FIELDS
           #IF    CH,R14,GT,=Y(L'QRTXT)  EXCEEDING MAX SIZE ?
            LA    R14,L'QRTXT            USE MAX SIZE
           #EIF
           BCTR  R14,0                  MINUS 1 FOR EXECUTE
MOVE2      MVC   OUTLIN+19(0),QR0YTXT   MOVE TEXT
           EX    R14,MOVE2              MOVE TEXT
           LA    R15,OUTLIN+20(R14)
           MVI   0(R15),#CAR
           LA    R14,21(R14)            GIVES CORRECT LENGTH
           ST    R14,A700SIZ            STORE FOR A700
           MVI   A700ACT,T2SE_ADD      INDICATE 'ADD LINE'
          #ELIF CLI,OUTIND,EQ,X'20'    THEN LAST LINE
           MVC   OUTLIN(20),=C'End of ZUTXR display'
           LA    R14,20                LENGTH OF LINE
           ST    R14,A700SIZ           STORE FOR A700
           MVI   A700ACT,T2SE_ADD+T2SE_SEND
          #ELIF CLI,OUTIND,EQ,X'10'    THEN MAX COMMANDS LINE
           MVC   OUTLIN(40),=C'Too many hits - change search arguments'
           MVI   OUTLIN+40,#CAR
           LA    R14,41                LENGTH OF LINE
           ST    R14,A700SIZ           STORE FOR A700
           MVI   A700ACT,T2SE_ADD      INDICATE 'ADD LINE'
          #ELIF CLI,OUTIND,EQ,X'08'    THEN NO ITEMS LINE
           MVC   OUTLIN(14),=C'No match found'
           MVI   OUTLIN+14,#CAR
           LA    R14,15                LENGTH OF LINE
           ST    R14,A700SIZ           STORE FOR A700
           MVI   A700ACT,T2SE_ADD      INDICATE 'ADD LINE'
          #EIF
          LA     R14,OUTLIN
          ST     R14,A700ADR
          ENTRC  A700                  ADD LINE TO OUTPUT
         #EIF
 #ESUB
         EJECT ,
*---------------------------------------------------------------------*
*   S U B R O U T I N E S - LEVEL 2                                   *
*---------------------------------------------------------------------*
         SPACE ,
         EJECT ,
*---------------------------------------------------------------------*
*   E R R O R S                                                       *
*---------------------------------------------------------------------*
QRERROR  DS    0H
         WTOPC TEXT='A7XB_DBERROR_READING_QR0YKL',                     >
               NUM=1,LET=I,PREFIX=UTXR
         BACKC ,
         EJECT ,
*---------------------------------------------------------------------*
*   C O N S T A N T S                                                 *
*---------------------------------------------------------------------*
         SPACE ,
A7XBHDR  DS    0H
         DC    AL1(HDRLEN-1)
         DC    C'T2SE Cross Reference',AL1(#CAR)
HDRLEN   EQU   *-A7XBHDR
A7XBLIN  DS    0H
         DC    AL1(LINLEN-1)
         DC    C'.......... ... .. - ................................'
         DC    C'..................'
         DC    AL1(#CAR)
LINLEN   EQU   *-A7XBLIN
CMDCMP   DC    XL(L'QR0YCMD)'00' FOR TPFDF KEY1 IF NOT TEXT SEARCH
         SPACE ,
TRTTAB   DC    256AL1(#SP)
         ORG   TRTTAB+X'7D'
         DC    X'7D'
         ORG   TRTTAB+X'B0'
         DC    X'B0'
         ORG   TRTTAB+C'.'
         DC    C'.'
         ORG   TRTTAB+C'#'
         DC    C'#'
         ORG   TRTTAB+C'_'
         DC    C'_'
         ORG   TRTTAB+C'/'
         DC    C'/'
         ORG   TRTTAB+C'*'
         DC    C'*'
         ORG   TRTTAB+C'@'
         DC    C'@'
         ORG   TRTTAB+C'#'
         DC    C'#'
         ORG   TRTTAB+C'-'
         DC    C'-'
         ORG   TRTTAB+C'A'
         DC    C'ABCDEFGHI'
         ORG   TRTTAB+C'a'
         DC    C'ABCDEFGHI'
         ORG   TRTTAB+C'J'
         DC    C'JKLMNOPQR'
         ORG   TRTTAB+C'j'
         DC    C'JKLMNOPQR'
         ORG   TRTTAB+C'S'
         DC    C'STUVWXYZ'
         ORG   TRTTAB+C's'
         DC    C'STUVWXYZ'
         ORG   TRTTAB+C'0'
         DC    C'0123456789'
         SPACE ,
*---------------------------------------------------------------------*
*   T H E   E N D                                                     *
*---------------------------------------------------------------------*
         SPACE ,
         LTORG
         FINIS
         END
