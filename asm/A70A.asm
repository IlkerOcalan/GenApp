         PRINT NOGEN
         BEGIN NAME=A70A,VERSION=&VV,SHR=NONE,XCL=NONE       @T2SE08002
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
*        Parse incoming SOAP/XML message                              *
*                                                                     *
*        Program description:                                         *
*        This program performs the following functions:               *
*        -Validate the incoming SOAP/XML message                      *
*        -When the XML message is for the upload of SYS scripts,      *
*         program handles this in A70S.                               *
*        -Extract the header information and save for the response    *
*         message                                                     *
*        -Set up the internal T2SE script call format (message type   *
*         0003 with EXECute parameters)                               *
*        -Copy relevant data into the simulated message               *
*          Relevant data being:                                       *
*           Script name       (mandatory)                             *
*           Sript group       (mandatory)                             *
*           Script version    (optional, default=0)                   *
*           Parameter list    (optional                               *
*                                                                     *
*  Level usage                                                        *
*  D0       Input message translated into AMSG format                 *
*  D1       MQ message descriptor                                     *
*  D3       T2SE AMSG                                                 *
*  D5       Save area for SOAP header data                            *
*  D7       T2SE workblock                                            *
*---------------------------------------------------------------------*
         SPACE 1
***********************************************************************
*  PROGRAM NAME : A70A01                                              *
*    PROGRAMMER :                                                     *
*          DATE : 17FEB04                 CHANGE NUMBER : @T2SE04003_ *
*                                                                     *
*  PURPOSE :   REPLACE ALCS SPECIFIC GETCC'S (VARIABLE LENGTH AND LX) *
*              BY STANDARD TPF CALLS (MALOCS)                         *
*                                                                     *
*   METHOD :   FOR DATA EXCHANGE OF LARGE DATA RECORDS BETWEEN ECB'S  *
*              USE A SHORT TERM CORE CHAIN, THIS WORKS ON ALCS AS WELL*
*              AS TPF                                                 *
***********************************************************************
         SPACE 1
***********************************************************************
*  PROGRAM NAME : A70A01                                              *
*    PROGRAMMER : Ruud Schelvis                                       *
*          DATE : 24FEB04                 CHANGE NUMBER : @T2SER2003  *
*                                                                     *
*  PURPOSE : Routine to parse XML input did not work correctly        *
*            if the same XML name was used more than once.            *
*            The routine is enhanced to now parse the whole XML       *
*            message to search for the correct ENDTAG.                *
*                                                                     *
*   METHOD : The parsing of the XML to search for input variables     *
*            is now performed in A70G as this segment was becoming    *
*            too big.                                                 *
*            Additional: Routine for translate tables moved from      *
*            A714 to A744.                                            *
*                                                                     *
***********************************************************************
         SPACE 1
***********************************************************************
*  PROGRAM NAME : A70A02                                              *
*    PROGRAMMER : Ruud Schelvis                                       *
*          DATE : 08JUN04                 CHANGE NUMBER : @T2SETPF    *
*                                                                     *
*  PURPOSE : TPF and ALCS are not fully compatible. The relation      *
*            between CRI and CRN does not exist in TPF. For this      *
*            purpose an in-core database is created. Access to this   *
*            database is through a TPF version of the COMIC.          *
*                                                                     *
*   METHOD : Add conditional code based upon a setting in T2SESET     *
*            indication if the installation is for ALCS or TPF.       *
*            Some of the existing COMIC/COMCC routines have been      *
*            enhanced to make the ALCS and TPF version identical.     *
*            This means that no changes are required.                 *
*                                                                     *
***********************************************************************
*  PROGRAM NAME : A70A03                                              *
*    PROGRAMMER : Ruud Schelvis                                       *
*          DATE : 22FEB05                 CHANGE NUMBER : @T2SE05001  *
*                                                                     *
*  PURPOSE : Check on CRI active was hard-coded.                      *
*                                                                     *
*   METHOD : Change instruction to use L'CMACTV.                      *
*                                                                     *
***********************************************************************
*  PROGRAM NAME : A70AR2                                              *
*    PROGRAMMER : Geoff Lowry, included in common base by Jan Willem  *
*          DATE : 22SEP05                 CHANGE NUMBER : @T2SE06002  *
*                                                                     *
*  PURPOSE :   Implement AAA Initialisation enhancement               *
*                                                                     *
***********************************************************************
*    PROGRAMMER : Geoff Lowry                                         *
*          DATE : 15JUL2013               CHANGE NUMBER : DL0013      *
*                                                                     *
*  PURPOSE :   FIX RLCH POOL USAGE ERRORS - MOVE SOAP ENVELOPE        *
*                                                                     *
***********************************************************************
         SPACE 1
         EJECT ,
*---------------------------------------------------------------------*
*        Main Program flow - call subroutine for each segment         *
*---------------------------------------------------------------------*
         SPACE 1
         T2SEEQ ,
         COPY  T2SESET     Conditional coding settings         @T2SETPF
         AP4WK REG=R15     WORK BLOCK
         AM0SG REG=R2
         AP0MS REG=R1,SUFFIX=X
         AP0MS REG=R4      T2SE INTERNAL MESSAGE LAY-OUT
         KCPL  REG=R3
         AIF   (NOT &TALCS).TPFMAC                             @T2SETPF
*-*-* ALCS SPECIFIC *-*-*
         CMUSR REG=R6      USER AREA OF COMMS TABLE
         CO0IC REG=R14     COMIC AREA
         AGO   .COM1                                           @T2SETPF
.TPFMAC  ANOP                                                  @T2SETPF
*-*-* TPF SPECIFIC *-*-*
         TR0TC REG=R6      OVERLAY OF COMICAREA                @T2SETPF
.COM1    ANOP                                                  @T2SETPF
         CI0CO REG=R5      RCB
         SPACE ,
         AIF (&TALCS).GETEQ0       GET WITH EQUATE              @CONSTA
         GETCC D7,L4,FILL=00 OBTAIN WORKBLOCK
         GETCC D5,L4,FILL=00 SAVE AREA FOR SOAP INFO
         AGO .GETEQ1                                            @CONSTA
.GETEQ0  ANOP                                                   @CONSTA
         GETCC D7,#T2SE4K,FILL=00 OBTAIN WORKBLOCK              @CONSTA
         GETCC D5,#T2SE4K,FILL=00 SAVE AREA FOR SOAP INFO       @CONSTA
.GETEQ1  ANOP                                                   @CONSTA
         SPACE 1
         L     R15,CE1CR7  BASE WORKBLOCK
         SR    R14,R14     PREPARE FOR INSERT
         ICM   R14,B'0011',EBCCC0 BLKLENGTH (AS MAXIMUM SCAN VALUE)
         ST    R14,TOTLEN  STORE TOTAL MESSAGE LENGTH
         MVC   VARNR,PARMNUM INITIALISE VARIABLE NUMBER
         SPACE 1
******** CHECK IF MESSAGE IS FOR UPLOAD OF 'SYS' SCRIPTS
         BAS   R3,A70AB000   ROUTINE WILL NOT RETURN IF SPECIAL UPLOAD
         SPACE 1
******** SET UP FIXED PART OF T2SE EMULATION MESSAGE
         SPACE 1
         BAS   R3,A70A6000 INITIALISE T2SE SCRIPT EXECUTE TEXT
         SPACE 1
         L     R1,CE1CR0   BASE (START OF) INPUT DATA (XML MESSAGE)
         SPACE 1
******** SCAN / COPY ENVELOPE
         SPACE 1
         BAS   R3,A70A2000 SCAN/COPY ENVELOPE
         SPACE 1
         B     A70AERR1    ERROR RETURN - ENVELOPE ERROR
         SPACE 1
******** SCAN/VALIDATE T2SE SPECIFIC SECTION
         SPACE 1
******** TRANSFORM SCRIPT NAME AND GROUP INTO INTERNAL FORMAT
         SPACE 1
         BAS   R3,A70A4000 COPY / VALIDATE SCRIPT SPECIFICS
         SPACE 1
         B     A70AERR2    ERROR RETURN - ISSUE ERROR RETURN MSG
         SPACE 1
******** LOCATE PARAMETERS AND TRANSFORM INTO INTERNAL FORMAT
         SPACE 1
*@T2SER2003 LA R0,25             COPY TRANSLATE TABLES
*2T2SER2003 ENTRC A714
         ENTRC A744                                          @T2SER2003
*                                                            @T2SER2003
         ENTRC A70G              COPY / VALIDATE PARAMETERS  @T2SER2003
* R3 CONTAINS AN ERROR NUMBER OR CONTAINS 0 IF MESSAGE OK    @T2SER2003
         CH    R3,=H'3'          ERROR 3 RETURN?             @T2SER2003
         BE    A70AERR3          YES - BRANCH                @T2SER2003
         CH    R3,=H'6'          ERROR 6 RETURN?             @T2SER2003
         BE    A70AERR6          YES - BRANCH                @T2SER2003
         CH    R3,=H'7'          ERROR 7 RETURN?             @T2SER2003
         BE    A70AERR7          YES - BRANCH                @T2SER2003
*@T2SER2003 BAS   R3,A70A7000 COPY / VALIDATE PARAMETERS
         SPACE 1
*@T2SER2003 B     A70AERR3
         RELCC DA                RELEASE TRANSLATE TABLES
         SPACE 1
******** SAVE CLOSING SEQUNCE IN BLOCK IN LEVEL 5
         SPACE 1
         BAS   R3,A70A9000 SAVE SOAP TRAILER
         SPACE 1
         B     A70AERR3
         EJECT ,
*---------------------------------------------------------------------*
*        Message is transformed. Finalise and call next segment       *
*---------------------------------------------------------------------*
         SPACE 1
         L     R15,CE1CR7           BASE WORKBLOCK
         L     R14,TEXTADD          TEXT POINTER
         L     R2,CE1CR3            BASE PARAMETER CONTROL BLOCK
         L     R4,CE1CR3            APP MESSAGE LAY-OUT
         SPACE 1
******** SET NON-RELEVANT HEADER DATA TO EBCDIC ZEROES
         SPACE 1
         MVC   AP0IN1(12),=C'000000000000'
         LA    R2,AM0LIT            START OF AMSG TEXT AREA
         SR    R14,R2               LENGTH OF TEXT
         L     R2,CE1CR3            BLOCK CONTAINING PARAMETER LIST
         STCM  R14,B'0011',AM0CCT   BLOCK COUNT
         SH    R14,=AL2(AM0TXT-AM0LIT) SUBTRACT CONTROLS
         STCM  R14,B'0011',AP0TOT   TOTAL MESSAGE LENGTH
         SPACE 1
         L     R14,TEXTADD          TEXT POINTER
         SR    R14,R4               LENGTH OF TEXT
         SH    R14,=AL2(AP0CNT-AP0BID) APPLICATION LENGTH
         STCM  R14,B'0011',AP0CNT   MESSAGE LENGTH
         SPACE 1
******** TEST FOR T2SE USERID PARAMETER - IF FOUND, VALIDATE CRN
******** AND COPY INTO MESSAGE
******** SET KCPL INDICATOR TO 'FIXED'
         SPACE 1
         CLC   USERID#,=F'1'         DID WE LOCATE A USERID TAG
         BNE   A70A0100              NO, PROCEED TO A70B
         SPACE 1
*---------------------------------------------------------------------*
*        Validate the input CRN                                       *
*---------------------------------------------------------------------*
         SPACE 1
******** ISSUE COMIC USER AREA TO TEST T2SE ACTIVE BIT
         SPACE 1
         LA    R6,COMICAREA           ADDRESS FOR COMIC RESULT @T2SETPF
         COMIC CRN=EBX000,            RETRIEVE INPUT CRN               -
               DATA=USER,                                              -
               AREA=((R6),CMUSLEN)                             @T2SETPF
*@T2SETPF      AREA=(0,CMUSLEN)
         BC    B'0100',A70AERR8       INVALID USER ID
         SPACE 1
         TM    CMACTV,L'CMACTV        RESOURCE IN USE        @T2SE05001
         BZ    A70AERR9               NO, ISSUE ERROR MESSAGE
         SPACE 1
******** ISSUE COMIC SYSTEM TO PICK UP CRI
         SPACE 1
         COMIC CRN=EBX000,            RETRIEVE INPUT CRN               -
               DATA=SYS,                                               -
               AREA=((R6),ICELN2)                              @T2SETPF
*@T2SETPF      AREA=(0,ICELN2)
         BC    B'0100',A70AERR8       INVALID USER ID
         SPACE 1
         L     R15,CE1CR7             RELOAD BASE OF WORKBLOCK @T2SETPF
         LA    R14,COMICAREA                                   @T2SETPF
         MVC   EBROUT,ICECRI+1        COPY CRI OF RESOURCE
         EJECT ,
*---------------------------------------------------------------------*
*        Validate token and insert CRN in T2SE header                 *
*---------------------------------------------------------------------*
         SPACE 1
*
* RETRIEVE 06-RCB
*
         MVC   EBX040,EBW040          SAVE CURRENT CONTENTS
         MVI   EBW040,0               CLEAR CONTENT
         FLIPC D3,D6                  NEW MESSAGE TO LEVEL 6
         MVI   EBCM01,B'01100100'     INDICATOR FOR A7RB
*                                     DON'T RETRIEVE 06-AAA
         ENTRC A7RB                   RETRIEVE 06-RCB WITH HOLD
         SPACE 1
         TM    EBW040,X'80'           ERROR RETURN ?
         MVC   EBW040,EBX040
         BO    A70AERRA               YES - UNABLE
         SPACE 1
         L     R5,CE1CR3              RCB BASE
         CLC   CI0TOK,EBX008          DOES TOKEN MATCH INPUT
         BNE   A70AERRB               NO, REJECT
         SPACE 1
         RELCC D3                     RELEASE RCB
         FLIPC D3,D6                  FLIP T2SE MESSAGE TO LEVEL 3
         LA    R3,EBW000              ADDRESS KCPL
         NI    KCPLCTL1,255-KCPL1DYN  SWITCH OFF DYNAMIC CRI INDICATOR
         L     R4,CE1CR3              BASE T2SE MESSAGE
         MVC   AP0CRNA,EBX000         COPY CRN NAME
         SPACE 1
A70A0100 DS    0H
         ENTDC A70B        OFFER SCRIPT TO T2SE KERNEL
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - validate/copy header data                       *
*---------------------------------------------------------------------*
         SPACE 1
A70A2000 DS    0H
         LA    R14,STARTTAG POINTER FOR SEARCH ROUTINE
         LA    R6,L'STARTTAG LENGTH FOR SEARCH ROUTINE
         SPACE 1
         BAS   R5,A70A5000 LOCATE START OF HEADER
         SPACE 1
         B     A70AERR1    UNABLE TO FIND START TAG
         SPACE 1
         ST    R2,STRTPNTR  START OF ENVELOPE
         SPACE 1
         LA    R14,T2SESCRIPT      FOR SEARCH ROUTINE POINTER
         LA    R6,L'T2SESCRIPT     SEARCH ROUTINE LENGTH
         SPACE 1
         BAS   R5,A70A5000 LOCATE END OF HEADER
         SPACE 1
         B     A70AERR1    UNABLE TO FIND END TAG
         SPACE 1
******** SOAP ENVELOPE HEADER LOCATED -  COPY TO LEVEL 5
         SPACE 1
         LR    R1,R2       START OF T2SE SPECIFIC SECTION
         L     R2,CE1CR5   BASE SOAP AREA
         L     R14,STRTPNTR  RELOAD START OF TEXT
         LR    R0,R3       RETURN REGISTER
         SR    R1,R14      LENGTH TO BE COPIED
         LR    R3,R1       COPY LENGTH
         LR    R15,R3      COPY LENGTH
         SPACE 1
         USING T2SE_ENV,R2                                       DL0013
         ST    R3,T2SE_ENVCCT  STORE ENVELOPE COUNT              DL0013
         LA    R2,T2SE_ENVBEG  START OF DATA AREA                DL0013
         DROP  R2                                                DL0013
         USING AM0SG,R2                                          DL0013
         MVCL  R2,R14      COPY ENVELOPE TO LEVEL 5
         L     R15,CE1CR7  BASE WORK BLOCK
         ST    R14,STRTPNTR  NEW START OF DATA
         SPACE 1
         LR    R3,R0       RETURN REGISTER
         B     4(,R3)      OK RETURN
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - save closing sequnce on level 5                 *
*---------------------------------------------------------------------*
         SPACE 1
A70A9000 DS    0H
         L     R15,CE1CR7               BASE AP4WK            @T2SESNCF
         LA    R14,SOAPBODY    LOCATE BODY
         LA    R6,L'SOAPBODY
         L     R15,CE1CR7
         L     R1,STRTPNTR     CURRENT TEXT POINTER
         SPACE 1
         BAS   R5,A70A5000     LOCATE SOAP BODY
         SPACE 1
         B     A70A9100        DOES NOT MATTER IF NOT FOUND
         SPACE 1
         ST    R1,STRTPNTR     POINT BEYOND
         SPACE 1
A70A9100 DS    0H
         LA    R14,TRAILER1    START OF SOAP TRAILER
         LA    R6,L'TRAILER1   AND LENGTH
         L     R15,CE1CR7      WORK AREA BASE
         L     R1,STRTPNTR     CURRENT TEXT POINTER
         SPACE 1
         BAS   R5,A70A5000     LOCATE INPUT STRING
         SPACE 1
******** IF NOT FOUND, PROCEED - TRAILER DATA MAY NOT BE REQUIRED
         SPACE 1
         B     0(R3)           ERROR RETURN
         ST    R2,STRTPNTR     START OF FIRST TRAILER
         SPACE 1
         LA    R14,TRAILER2    SECOND PART OF SOAP TRAILER
         LA    R6,L'TRAILER2   AND LENGTH
         SPACE 1
         BAS   R5,A70A5000     LOCATE START OF USER ID
         SPACE 1
         B     0(R3)           ERROR - TRAILER NOT FOUND
         SPACE 1
         S     R1,STRTPNTR     UPDATED POINTER
         L     R5,CE1CR5       SAVE AREA FOR SOAP DATA
         AH    R5,=AL2(TRAILER) ADDRESS TRAILER AREA
         STH   R1,0(,R5)       LENGTH OF TRAILER
         BCTR  R1,0            SUBTRACT ONE FOR EXECUTE
         L     R15,STRTPNTR    START OF TRAILER TEXT
         MVC   2(0,R5),0(R15)
         EX    R1,*-6          COPY TRAILER
         B     4(,R3)          AND RETURN
         SPACE 1
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - validate/copy scripting info (name and parmlist)*
*---------------------------------------------------------------------*
         SPACE 1
A70A4000 DS    0H
         L     R15,CE1CR7      RELOAD WORK AREA BASE
         L     R1,STRTPNTR     CURRENT TEXT POINTER
         SPACE 1
         LA    R14,T2SESCRIPT  BEGINNING OF SCRIPT AREA
         LA    R6,L'T2SESCRIPT LENGTH OF SEARCH TAG
         SPACE 1
         BAS   R5,A70A5000     LOCATE START OF SCRIPT AREA
         SPACE 1
         B     A70AERR2        ERROR RETURN
         SPACE 1
         LA    R14,BLANK       LOCATE STRING (FIRST BLANK AFTER SCRIPT
*                              NAME)
         LA    R6,L'BLANK      LENGTH OF SEARCH STRING
         SPACE 1
         BAS   R5,A70A5000     LOCATE END OF SCRIPT NAME
         SPACE 1
         B     A70AERR2
         ST    R1,STRTPNTR     NEW TEXT POINTER
         SR    R2,R0           LENGTH OF SCRIPT NAME
         LR    R1,R0           START OF SCRIPT NAME
         L     R14,TEXTADD     POINTER IN T2SE FORMAT MESSAGE
         BCTR  R2,0            PREPARE FOR EXECUTE
         MVC   0(0,R14),0(R1)
         EX    R2,*-6          COPY SCRIPT NAME TO OUTPUT
         SPACE 1
         L     R5,CE1CR5       BASE SOAP SAVE AREA
         LA    R5,T2SE_ENVSNAM(,R5) ADDRESS NAME AREA            DL0013
         MVC   2(0,R5),0(R1)   COPY SCRIPT NAME
         EX    R2,*-6          COPY SCRIPT NAME TO SAVE AREA
         LA    R2,1(,R2)
         STH   R2,0(R5)        AND SCRIPTNAME LENGTH
         AR    R14,R2          INCREMENT TEXT POINTER
         SPACE 1
******** ADD 'GROUP=' TO COMMAND BLOCK
         SPACE 1
         MVC   0(L'GROUP,R14),GROUP
         LA    R14,L'GROUP(,R14) INCREMENT TEXT POINTER
         ST    R14,TEXTADD     NEW END OF TEXT
         SPACE 1
******** LOCATE GROUP NAME IN SOAP MESSAGE
         SPACE 1
         L     R1,STRTPNTR      PICK POINTER IN SOAP MESSAGE
         LA    R14,SOAPGRP      SEARCH STRING
         LA    R6,L'SOAPGRP     LENGTH OF SEARCH STRING
         SPACE 1
         BAS   R5,A70A5000      LOCATE START OF GROUP NAME FIELD
         SPACE 1
         B     A70AERR4
         LA    R14,DBQUOTE      SEARCH FOR GROUPNAME
         LA    R6,L'DBQUOTE
         SPACE 1
         BAS   R5,A70A5000      LOCATE START OF GROUPNAME
         B     A70AERR4
         SPACE 1
         L     R14,TEXTADD      T2SE AMSG TEXT POINTER
         MVC   0(3,R14),0(R1)   COPY GROUP NAME INTO COMMAND
         MVC   SCGROUP,0(R1)    COPY GROUP NAME TO WORK BLOCK
         LA    R14,3(,R14)      INCREMENT TEXT POINTER
         LA    R1,4(,R1)        INCREMENT SOAP POINTER      @T2SE06002
         SPACE 1
         MVC   0(L'VERSION,R14),VERSION COPY ++VERSION=
         LA    R14,L'VERSION(,R14)
         SPACE 1
         ST    R14,TEXTADD      AND STORE
         ST    R1,STRTPNTR      SAVE SOAP POINTER
         SPACE 1
******** CHECK IF VERSION IS SPECIFIED (DEFAULTS TO 0)
         SPACE 1
         LA    R14,XMLVERSION
         LA    R6,L'XMLVERSION
         SPACE 1
         BAS   R5,A70A5000      LOCATE VERSION IN XML DOC
         SPACE 1
         B     A70A4200         VERSION NOT FOUND
         SPACE 1
******** VERSION FOUND - LOCATE DOUBLE QUOTE
         SPACE 1
         LA    R14,DBQUOTE      SEARCH FOR VERSION NUMBER
         LA    R6,L'DBQUOTE
         SPACE 1
         BAS   R5,A70A5000      LOCATE START OF VERSION NUMBER
         SPACE 1
         B     A70AERR5         INVALID VERSION NOTATION
         SPACE 1
         L     R14,TEXTADD
         MVC   0(1,R14),0(R1)   COPY VERSION NUMBER
         LA    R1,2(,R1)        INCREMENT SOAP POINTER      @T2SE06002
         ST    R1,STRTPNTR      SAVE SOAP POINTER
         SPACE 1
A70A4100 DS    0H
         LA    R14,1(,R14)      STEP PAST VERSION NUMBER
         MVC   0(L'CLSCMD,R14),CLSCMD COPY CLOSE BRACKETS
         LA    R14,L'CLSCMD(,R14)
         ST    R14,TEXTADD      POINTER IN T2SE COMMAND BLOCK
         B     A70A4300         LOCATE ADDITIONAL ATTRIBUTES
         SPACE 1
A70A4200 DS    0H
         L     R14,TEXTADD      RELOAD TEXT POINTER
         MVI   0(R14),C'0'      DEFAULT VERSION NUMBER
         B     A70A4100         AND JOIN MAIN LINE
         EJECT ,
*------------------------------------------------------------@T2SE06002
*        AAA Initialisation Enhancement                               *
*                                                                     *
*        The AAA initialisation parameters follow after the Version=  *
*        tag and can contain as many as 6 parameters.                 *
*        They will appear as:                                         *
*        City="QSC" Carrier="BR" Office="042" ... >                   *
*                                                                     *
*        The logic in A70A will store the start and end address of    *
*        these AAA initialisation parameters for A711 to build the    *
*        AAA initialisation entry.                                    *
*                                                                     *
*------------------------------------------------------------@T2SE06002
         SPACE 1
A70A4300 DS    0H
         SPACE 1
******** DETERMINE TOTAL REMAINING SIZE OF T2SEscript TAG
         SPACE 1
         MVC   CPYTOTAL,TOTLEN  SAVE TOTAL REMAINING LENGTH
         ST    R1,AAAPARMBEG            SAVE START AAA PARMS @T2SE06002
         ST    R1,AAAPARMEND            INITALISE END PARMS  @T2SE06002
         SPACE 1
         LA    R14,CLOSEBRACKET         SEARCH PARAMETER
         LA    R6,L'CLOSEBRACKET        SEARCH PARAMETER LENGTH
         SPACE 1
         BAS   R5,A70A5000              SEARCH END OF TAG
         SPACE 1
         B     A70AERR2                 NO END OF TAG FOUND - ERROR
         SPACE 1
         ST    R1,AAAPARMEND            SAVE END AAA PARMS   @T2SE06002
*------------------------------------------------------------@T2SE06002
*        Copy AAA initialisation parms into the Soap Envelope save    *
*        block on level D5 for use by A711 to build the               *
*        AAA initialisation entry.                                    *
*        The AAA parameters are placed immediately after the SOAP     *
*        envelope.                                                    *
*---------------------------------------------------------------------*
         SPACE 1                                             @T2SE06002
         L     R5,CE1CR5           GET SOAP SAVE BLOCK       @T2SE06002
         USING T2SE_ENV,R5         BASE SOAP ENVELOPE            DL0013
*        LA    R4,12(,R5)          LENGTH OF SOAP ENVELOPE   @T2SE06002
*        A     R4,8(,R5)           POINT PAST SOAP ENVELOPE  @T2SE06002
         LA    R4,T2SE_ENVBEG      LENGTH OF SOAP ENVELOPE       DL0013
         A     R4,T2SE_ENVCCT      POINT PAST SOAP ENVELOPE      DL0013
         DROP  R5                                                DL0013
         USING CI0CO,R5                                          DL0013
         S     R1,AAAPARMBEG       CLACULATE LENGTH          @T2SE06002
         STH   R1,0(,R4)           SAVE LENGTH               @T2SE06002
         BZ    A70A0190            NO LENGTH, BRANCH TO IGNORE  SE06002
A70A0110 DC    0H'0'                                         @T2SE06002
         SPACE 1                                             @T2SE06002
         LA    R4,2(,R4)           INCREMENT PASSED LENGTH   @T2SE06002
         L     R0,AAAPARMBEG       MOVE FROM ADDRESS         @T2SE06002
         LR    R5,R1               COPY LENGTH FOR MVCL      @T2SE06002
         MVCL  R4,R0               MOVE AAA PARMS            @T2SE06002
A70A0190 DC    0H'0'                                         @T2SE06002
         L     R1,AAAPARMEND       RESTORE SOAP POINTER      @T2SE06002
         SPACE 1                                             @T2SE06002
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *@T2SE06002
*        The following code has been commented out.                   *
*        This logic has been moved to A711.                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *@T2SE06002
*---------------------------------------------------------------------*
*        Locate attributes:                                           *
*        City="MNL" Appl="DCS" Host="HV"                              *
*        Those attributes determine the format of the AAA init entry  *
*        which will be issued by A711 for dynamic CRI's               *
*---------------------------------------------------------------------*
*        SPACE 1
******** SCAN THE TAGE FOR THE 3 AAA ATTRIBUTES
*T2SE06002 SPACE 1
*T2SE06002 LR    R1,R0                    RESET TO ORIGINAL PNTR
*T2SE06002 ST    R14,TOTLEN               USE REMAINING LENGTH
*T2SE06002 LR    R4,R14                   RETAIN FOR NEXT ATTRIBUTES
*T2SE06002 SPACE 1
*T2SE06002 LA    R14,AAACITY              AAACITY ATTRIBUTE
*T2SE06002 LA    R6,L'AAACITY             LENGTH OF SEARCH ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 BAS   R5,A70A5000              SEARCH CITY ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 B     A70A4400
*T2SE06002 SPACE 1
*T2SE06002 CLI   0(R1),C'"'               DOUBLE QUOTE FOUND
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 CLI   4(R1),C'"'               ENDING DOUBLE QUOTE
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 ST    R4,TOTLEN                RESTORE LENGTH TO BE SEARCHED
*T2SE06002 L     R4,CE1CR3                T2SE FORMATTED MESSAGE
*T2SE06002 MVC   AP0CIT,1(R1)             COPY CITY CODE
*T2SE06002 L     R4,TOTLEN                SAVE LENGTH
*T2SE06002 SPACE 1
*T2SE06002 A70A4400 DS    0H
*T2SE06002 LR    R1,R0                    RESTORE START POINTER
*T2SE06002 SPACE 1
******** SEARCH HOST CODE NAME ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 LA    R14,AAAHOST              AAAHOST ATTRIBUTE
*T2SE06002 LA    R6,L'AAAHOST             LENGTH OF SEARCH ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 BAS   R5,A70A5000              SEARCH HOST ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 B     A70A4500                 HOST ATTRIBUTE NOT FOUND
*T2SE06002 SPACE 1
*T2SE06002 CLI   0(R1),C'"'               DOUBLE QUOTE FOUND
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 CLI   3(R1),C'"'               ENDING DOUBLE QUOTE
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 ST    R4,TOTLEN                RESTORE LENGTH TO BE SEARCHED
*T2SE06002 L     R4,CE1CR3                T2SE FORMATTED MESSAGE
*T2SE06002 MVC   AP0HOST,1(R1)            COPY HOST NAME
*T2SE06002 L     R4,TOTLEN                SAVE LENGTH
*T2SE06002 SPACE 1
*T2SE06002A70A4500 DS    0H
*T2SE06002 SPACE 1
*T2SE06002 LR    R1,R0                    RESTORE START POINTER
******** SEARCH APPLICATION NAME ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 LA    R14,AAAAPPL              AAAAPPL ATTRIBUTE
*T2SE06002 LA    R6,L'AAAAPPL             LENGTH OF SEARCH ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 BAS   R5,A70A5000              SEARCH APPL ATTRIBUTE
*T2SE06002 SPACE 1
*T2SE06002 B     A70A4600                 APPL ATTRIBUTE NOT FOUND
*T2SE06002 SPACE 1
*T2SE06002 CLI   0(R1),C'"'               DOUBLE QUOTE FOUND
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 CLI   4(R1),C'"'               ENDING DOUBLE QUOTE
*T2SE06002 BNE   A70AERR2                 NO, INVALID SYNTAX
*T2SE06002 SPACE 1
*T2SE06002 L     R4,CE1CR3                T2SE FORMATTED MESSAGE
*T2SE06002 MVC   AP0APPL,1(R1)            COPY APPLICATION NAME
*T2SE06002 SPACE 1
*T2SE06002A70A4600 DS    0H
*T2SE06002 LR    R1,R0                    RESTORE START POINTER
*T2SE06002 MVC   TOTLEN,CPYTOTAL          RESTORE REMAINING SIZE
         B     4(R3)                    OK RETURN
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - locate specified string                         *
*        R1    data pointer                                           *
*        R14   search string pointer                                  *
*        R6    search string length                                   *
*        TOTLEN total remanining message length                       *
*        Return: 0(R5) -> error (string not found)                    *
*                4(R5) -> ok return R0  original text pointer         *
*                                   R2  start of string location      *
*                                   R1  pointer beyond string found   *
*                                   R14 contains field length         *
*---------------------------------------------------------------------*
         SPACE 1
A70A5000 DS    0H
         L     R15,CE1CR7     BASE AP4WK                        @CONSTA
*        DBCS changes per change T2SE05004                      @CONSTA
         NI    IND1+3,255-DBCSSTR  CLEAR DBCS INDICATOR         @CONSTA
*                                                               @CONSTA
*        CALCULATE THE LENGTH OF THE DATA TO BE SCANNED       @T2SETPF
*        RATHER THAN RELY ON TOTLEN. CALLERS OF THIS ROUTINE  @T2SETPF
*        ADJUST R1 TO                                         @T2SETPF
*        POINT AT A DIFFERENT SPOT IN THE MESSAGE AND THUS    @T2SETPF
*        WOULD CAUSE THE TOTAL NUMBER OF BYTES TO BE SCANNED  @T2SETPF
*        TO BE INCORRECT                                      @T2SETPF
         L     R15,CE1CR0               BASE  MSG             @T2SETPF
         LR    R0,R1                    POINTER               @T2SETPF
         SR    R0,R15                   DISPL INTO MESSAGE    @T2SETPF
         XR    R15,R15                                        @T2SETPF
         ICM   R15,B'0011',EBCCC0       TOTAL SIZE            @T2SETPF
         SR    R15,R0                   LENGTH OF DATA TO SCAN@T2SETPF
         LR    R0,R15                   KEEP IN R0            @T2SETPF
         L     R15,CE1CR7               BASE AP4WK            @T2SETPF
         ST    R0,TOTLEN                SET UP REAL SCAN SIZE @T2SETPF
*@T2SETPFL     R15,CE1CR7               BASE AP4WK            @T2SESNCF
         L     R7,TOTLEN
         LR    R0,R1       RETAIN ORIGINAL TEXT POINTER
         BCTR  R6,0        PROPER EXECUTE LENGTH
         SPACE 1
A70A5100 DS    0H
         CLC   0(0,R1),0(R14) MATCH FOUND
         EX    R6,*-6
         BE    A70A5200    YES, RETURN
         SPACE 1
*        DBCS changes per change T2SE05004                      @CONSTA
         CLI   0(R1),#SO   DBCS START?                          @CONSTA
         BNE   A70A5110    NOPE - BRANCH                        @CONSTA
         CLI   1(R1),#SP   IS THIS REALLY DBCS START?           @CONSTA
         BNH   A70A5130    NOPE - BRANCH                        @CONSTA
         OI    IND1+3,DBCSSTR  SET DBCS INDICATOR               @CONSTA
         B     A70A5130    BRANCH TO CONTINUE                   @CONSTA
         SPACE 1                                                @CONSTA
A70A5110 DC    0H'0'                                            @CONSTA
         CLI   1(R1),#SP   IS THIS NON-DBCS CHAR?               @CONSTA
         BNH   A70A5120    YES - RESET DBCS FLG                 @CONSTA
         CLI   0(R1),#SI   DBCS END?                            @CONSTA
         BNE   A70A5130    NOPE - BRANCH TO CONTINUE            @CONSTA
A70A5120 DC    0H'0'                                            @CONSTA
         NI    IND1+3,255-DBCSSTR  CLEAR DBCS INDICATOR         @CONSTA
         SPACE 1                                                @CONSTA
A70A5130 DC    0H'0'                                            @CONSTA
         LA    R1,1(,R1)   INPUT TEXT POINTER
         BCT   R7,A70A5100
         SPACE 1
         NI    IND1+3,255-DBCSSTR  CLEAR DBCS INDICATOR         @CONSTA
         B     0(R5)       ERROR RETURN - R14, POINTS TO REASON
         SPACE 1
A70A5200 DS    0H          MATCH FOUND - UPDATE POINTERS
         TM    IND1+3,DBCSSTR  IS THIS DBCS STRING?             @CONSTA
         BO    A70A5130    YES - IGNORE XML TAG                 @CONSTA
         SPACE 1
         LR    R2,R1       POSITION WHERE STRING WAS FOUND
         LA    R6,1(,R6)   RESTORE AFTER EXECUTE
         AR    R1,R6       BEYOND STRING THAT WAS FOUND
         SR    R1,R0       TOTAL FIELD LENGTH
         L     R7,TOTLEN   RESTORE TOTAL LENGTH
         SR    R7,R1       SUBTRACT CURRENT SCAN
         ST    R7,TOTLEN   NEW REMAINING VALUE
         LR    R14,R1      CURRENT SCAN LENGTH
         AR    R1,R0       RESTORE SCAN POINTER
         B     4(,R5)      OK RETURN
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - set up T2SE input parameters                    *
*        This section sets up the fixed part of the internal script   *
*        call block                                                   *
*---------------------------------------------------------------------*
         SPACE 1
A70A6000 DS    0H
         SR    R1,R1           PREPARE FOR INSERT
         ICM   R1,B'0011',EBCCC0  INPUT BLOCK SIZE
*T2SE04003ETCC D3,SIZE=(R1)    T2SE INPUT MESSAGE BLOCK
         MALOC SIZE=R1         OBTAIN STORAGE                @T2SE04003
         ST    R1,CE1CR3       STORE FOR LATER REFERENCE     @T2SE04003
* SOME INSTALLATIONS LEAVE THE MALOC AREA POLLUTED             T2SESNCF
* FOLLOWING ROUTINE CLEARS THE MALOC AREA                      T2SESNCF
         STM   R2,R3,CE1UR2    SAVE R2 AND R3                  T2SESNCF
         SR    R15,R15                                         T2SESNCF
         ICM   R15,B'0011',EBCCC0  INPUT BLOCK SIZE            T2SESNCF
         LR    R2,R1           COPY FROM                       T2SESNCF
         LR    R14,R2          COPY TO                         T2SESNCF
         SR    R3,R3           INDICATE CLEAR WANTED           T2SESNCF
         MVCL  R14,R2          CLEAR MALOC AREA                T2SESNCF
         LM    R2,R3,CE1UR2    RESTORE R2 AND R3               T2SESNCF
*                                                              T2SESNCF
         L     R1,CE1CR0       BASE ORIGINAL BLOCK (POSSIBLE KOALA)
         L     R15,CE1CR7      WORK AREA BASE
         L     R4,CE1CR3       BASE T2SE MESSAGE BLOCK
         MVC   AP0IDH(AP0CIT-AP0IDH),AP0IDHX COPY ORIGINAL HEADER
         MVC   AP0TYP,MSGTYP   SCRIPT EXECUTE MESSAGE TYPE
         MVC   AP0DES,MAXDEF   DEFAULT MAXIMUM MESSAGE SIZE
         LA    R14,AP0TXT      START OF TEXT
         MVC   AP0TXT(L'EXEC),EXEC COPY EXECUTE COMMAND
         LA    R14,AP0TXT+L'EXEC CURRENT TEXT POINTER
         ST    R14,TEXTADD     AND SAVE
         BR    R3              RETURN
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - Locate parameters in XML/SOAP message           *
*        Set up parameters in T2SE (internal) format                  *
*        and append to T2SE block that was built up so far            *
*        0(3): error return    4(r3): ok return                       *
*                                                                     *
*@T2SER2003 THIS ROUTINE IS MOVED TO A70G                             *
*---------------------------------------------------------------------*
         SPACE 1
*@T2SER2003 A70A7000 DS    0H
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - Set up SOAP/XML error message and send response *
*        via A702                                                     *
*        The XML/SOAP indicator in the KCPL is switched off in order  *
*        to let the message pass through A702 without further action  *
*        R7 points to specific error message                          *
*---------------------------------------------------------------------*
         SPACE 1
A70A8000 DS    0H
         CRUSA S0=2                      RELEASE POSSIBLE BLOCK
         SPACE 1
         GETCC D2,L2,FILL=00             OBTAIN CORE FOR ERROR MESSAGE
         L     R2,CE1CR2                 ERROR MESSAGE AMSG
         L     R4,CE1CR2                 ERROR MESSAGE AMSG
         LA    R4,AP0TXT                 TARGET REGISTER
         L     R14,CE1CR0
         SPACE 1
******** COPY T2SE HEADER
         SPACE 1
         MVC   AM0TXT(AP0TXT-AP0TOT),AM0TXT-AM0RID(R14)
         L     R14,CE1CR5                ENVELOPE LOCATION
         L     R1,8(R14)                 LENGTH OF ENVELOPE
         LTR   R1,R1                     ENVELOPE STORED
         BZ    A70A8100                  NO, UNABLE TO RECREATE,
         SPACE 1                         ASSUME ENVELOPE HEADER
******** ENVELOPE WAS STORED - COPY TO MESSAGE
         SPACE 1
         L     R4,CE1CR2                 BASE MESSAGE BLOCK
         LA    R4,AP0TXT                 TARGET REGISTER
         LR    R5,R1                     LENGTH
         LA    R14,12(,R14)              FROM ADDRESS
         LR    R15,R1                    LENGTH
         MVCL  R4,R14                    COPY ENVELOPE
         SPACE 1
A70A8100 DS    0H
         MVC   0(T2SECTLL,R4),T2SECTL    T2SE CONTROL TAG
         LA    R4,T2SECTLL(,R4)         STEP PAST FIXED HEAD
         MVC   0(3,R4),1(R7)             ERROR NUMBER
         MVC   3(FAULTENDL,R4),FAULTEND MORE TAGS
         LA    R4,FAULTENDL+3(,R4)
         SR    R14,R14                   PREPARE FOR INSERT
         IC    R14,0(R7)                 INSERT LENGTH
         SH    R14,=H'5'                 PREPARE FOR EXECUTE
         MVC   0(0,R4),5(R7)             COPY ERROR TEXT
         EX    R14,*-6
         LA    R14,1(,R14)               ADJUST LENGTH
         SPACE 1
         AR    R4,R14                    INCREMENT TEXT POINTER
         MVC   0(SOAPFOOTL,R4),SOAPFOOT  APPEND CLOSING SEQUENCE
         LA    R4,SOAPFOOTL(,R4)         AND INCR PNTR ACCORDINGLY
         LA    R14,AM0TXT                START OF TEXT
         SR    R4,R14                    CURRENT PNTR - START = LENGTH
         AH    R4,=AL2(AM0TXT-AM0LIT)    INCLUDE CONTROL CHARACTERS
         STCM  R4,B'0011',AM0CCT         MESSAGE LENGTH
         SPACE 1
******** CLEAR SOAP INDICATOR IN KCPL-A702 WILL THEN TREAT THE MESSAGE
******** AS A COMMON MESSAGE AND NOT ADD SOAP CONTROLS
         SPACE 1
         L     R4,CE1CR2                 RETURN MESSAGE
         MVC   AP0DES,=C'000000'         CLEAR RESIDUAL COUNT
         LA    R3,EBW000                 KCPL POINTER
         L     R14,KCPLORG               SWAP
         L     R15,KCPLDES ORIGIN
         ST    R14,KCPLDES               AND
         ST    R15,KCPLORG               DESTINATION
         OI    KCPLCTL2,KCPL2RTQ         REPLY TO Q IN KCPL
         OI    KCPLCTL2,KCPL2CID         CORRELATION ID IN KCPL
         NI    KCPLCTL2,255-KCPL2XML     NO NEED TO RETRIEVE FROM FILE
         NI    KCPLCTL1,255-KCPL1DYN     NO CRI YET -> NO RELEASE
         CRUSA S0=3                      RELEASE WORK BLOCK
         SPACE 1
******** CALL MQ SEND PROGRAM
         SPACE 1
         ENTRC A702
         LA    R3,EBW000                 RE-SET KCPL PTR      @T2SESNCF
         SPACE 1
         TM    KCPLCTL0,KCPL0RET         ERROR CONDITION OCCURRED
         BNZ   A70AER1                   YES, ISSUE DUMP
         AIF   (NOT &TALCS).EXIT                               @T2SETPF
         LA    R15,8                     OK, MESSAGE RETURNED
         BACKC ,
         AGO   .BACKC                                          @T2SETPF
.EXIT    ANOP                                                  @T2SETPF
*-TPF SPECIFIC-------------------------------------------------@T2SETPF
*                                                              @T2SETPF
*        DO NOT GO BACK.                                       @T2SETPF
*                                                              @T2SETPF
*--------------------------------------------------------------@T2SETPF
         EXITC                                                 @T2SETPF
.BACKC   ANOP                                                  @T2SETPF
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - Change >> and << signs temporarily in hex value *
*        'fd' and 'fe'.                                               *
*        A script can be part of the XML/SOAP payload (e.g. for an    *
*        execute or an upload.                                        *
*        This routine lets A712 consider the script as one            *
*        parameter. After direct activation, A70E will restore the    *
*        original values, and the script can be uploaded/executd      *
*                                                                     *
*@T2SER2003 THIS ROUTINE IS MOVED TO A70G                             *
*---------------------------------------------------------------------*
         SPACE 1
*T2SER2003 A70AA000 DS    0H
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - check of special system upload                  *
*        When special system upload, program continues in A70S.       *
*---------------------------------------------------------------------*
         SPACE 1
A70AB000 DS    0H
         L     R1,CE1CR0           BASE (START OF) INPUT DATA
         L     R15,CE1CR7          BASE WORK AREA
         SPACE 1
         LA    R14,T2SESCRIPT      BEGINNING OF SCRIPT AREA
         LA    R6,L'T2SESCRIPT     LENGTH OF SEARCH TAG
         SPACE 1
         BAS   R5,A70A5000         LOCATE START OF SCRIPT AREA
         B     A70AERR2            ERROR RETURN
         SPACE 1
         LA    R14,BLANK           FIRST BLANK AFTER SCRIPT NAME
         LA    R6,L'BLANK          LENGTH OF SEARCH STRING
         SPACE 1
         BAS   R5,A70A5000         LOCATE END OF SCRIPT NAME
         B     A70AERR2            ERROR RETURN
         SR    R2,R0               GIVES LENGTH OF SCRIPT NAME
         CH    R2,=Y(L'SYSTEMUP)   SCRIPT NAME SIZE ?
         BNER  R3                  NO - TREAT AS 'NORMAL'
         LR    R1,R0               START OF SCRIPT NAME
         CLC   SYSTEMUP,0(R1)      SCRIPT NAME?
         BNER  R3                  NO - TREAT AS 'NORMAL'
         ENTNC A70S                PROCESS 'SPECIAL' UPLOAD
         EJECT ,
*---------------------------------------------------------------------*
*        Error routines                                               *
*---------------------------------------------------------------------*
         SPACE 1
A70AERR1 DS   0H                         ENVELOPE ERROR
         LA   R7,ERMSG1                  ERROR MESSAGE ONE
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR2 DS   0H                         SCRIPT CONTROL PARMS ERROR
         LA   R7,ERMSG2                  ERROR MESSAGE TWO
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR3 DS   0H                         PARAMETER LIST ERROR
         LA   R7,ERMSG3                  ERROR MESSAGE THREE
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR4 DS   0H                         SCRIPT GROUP OMITTED
         LA   R7,ERMSG4                  ERROR MESSAGE FOUR
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR5 DS   0H                         INCORRECT VERSION NUMBER
         LA   R7,ERMSG5                  ERROR MESSAGE FIVE
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR6 DS   0H                         BRACKET ERROR IN PARM SECTION
         LA R7,ERMSG6                    ERROR MESSAGE SIX
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR7 DS   0H                         BRACKET ERROR IN PARM SECTION
         LA   R7,ERMSG7                  ERROR MESSAGE SEVEN
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR8 DS   0H                         INVALID USERID IN T2SE VAR
         LA   R7,ERMSG8                  ERROR MESSAGE EIGHT
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERR9 DS   0H                         RESOURCE NOT ACTIVE
         LA   R7,ERMSG9                  ERROR MESSAGE NINE
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERRA DS   0H                         RCB RETRIEVAL ERROR
         LA   R7,ERMSGA                  ERROR MESSAGE A
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         SPACE 1
A70AERRB DS   0H                         TOKEN DOES NOT MATCH CURRENT
         LA   R7,ERMSGB                  ERROR MESSAGE B
         B     A70A8000                  BUILD/SEND ERROR  MESSAGE
         EJECT ,
*---------------------------------------------------------------------*
*        Data constants                                               *
*---------------------------------------------------------------------*
         SPACE 1
******** SOAP RELATED CONSTANTS
         SPACE 1
STARTTAG DC    C'<SoapEnv:Envelope' START OF SOAP ENVELOPE
ENDTAG   DC    C'</SoapEnv:Envelope>' END OF SOAP ENVELOPE
CLOSEBRACKET DC C'>'
STARTBRACKET DC C'<'
SYSTEMUP  DC  C'SYSTEMUP'               SPECIAL UPLOAD SCRIPT NAME
SOAPBODY  DC  C'<SoapEnv:Body>'         START OF BODY AREA
T2SESCRIPT DC  C'<T2SEScript:'          START OF SCRIPT AREA
T2SEEND  DC    C'</T2SEScript:'         END OF SCRIPT AREA
T2SEUSERID DC  C'<T2SEuserid>'          USERID
T2SETOKEN  DC  C'<T2SEtoken>'           AND TOKEN
SOAPGRP  DC    C'Group='                GROUP CODE IN SOAP
BLANK    DC    AL1(#SP)
DBQUOTE  DC    C'"'                     QUOTES AROUND ATTRIBUTE
XMLVERSION DC  C'Version='
HEADER   DC    C'<SoapEnv:Header>'      START OF HEADER SECTION
USERID   DC    C'<UserID>'              USER TAG
CLOSETAG DC    C'</'
T2SECTL  DC    C' <T2SEScript:PARSEResponse>',AL1(#CAR)
         DC    C' <T2SEControl-PARSE>',AL1(#CAR),C'<faultcode>'
T2SECTLL EQU   *-T2SECTL
FAULTEND DC    C'</faultcode>',AL1(#CAR),C'<faultstring>'
FAULTENDL EQU  *-FAULTEND
         SPACE 1
SOAPFOOT DC    C' </faultstring>',AL1(#CAR)
         DC    C' </T2SEControl-PARSE>',AL1(#CAR)
         DC    C' </T2SEScript:PARSEResponse>',AL1(#CAR)
         DC    C' </SoapEnv:Body>',AL1(#CAR)
         DC    C' </SoapEnv:Envelope>'
SOAPFOOTL EQU *-SOAPFOOT
         SPACE 1
******** INTERNAL MESSAGE BUILD UP DEFINITIONS
         SPACE
MSGTYP   DC    C'0003'                  SCRIPT EXECUTE
MAXDEF   DC    C'064000'                DEFAULT MAX MESSAGE SIZE
EXEC     DC    C'>>++EXEC++SCRIPTNAME='   FIRST PART OF SCRIPT CALL
GROUP    DC    C'++GROUP='
CLSCMD   DC    C'<<'
OPENCMD  DC    C'>>'
VERSION  DC    C'++VERSION='
T2SEPARM DC    C'>>++I++D'
PARMNUM  DC    F'0'                 PARAMETER NUMBER IN LIST
DOUBLEQ  DC    X'7D7D'
TRAILER1 DC    C'</SoapEnv:Body>'
TRAILER2 DC    C'</SoapEnv:Envelope>'
         SPACE 1
******** AAA INIT ATTRIBUTES
AAACITY  DC    C'City='
AAAAPPL  DC    C'Appl='
AAAHOST  DC    C'Host='
         SPACE 1
TRAILER  EQU   1100                 DISPLACEMENT OF TRAILER
A70ATRT  TRT   0(0,R14),0(R7)      ISSUE TRANSLATE AND TEST
         SPACE 1
******** ERROR TEXT CONSTANTS
         SPACE 1
ERMSG1   DC    AL1(L'ERMSG1T)
ERMSG1T  DC    C'013 Unable to read SOAP envelope'
         SPACE 1
ERMSG2   DC    AL1(L'ERMSG2T)
ERMSG2T  DC    C'014 Invalid script control parameters'
         SPACE 1
ERMSG3   DC    AL1(L'ERMSG3T)
ERMSG3T  DC    C'015 Unresolvable error in parameter list'
         SPACE 1
ERMSG4   DC    AL1(L'ERMSG4T)
ERMSG4T  DC    C'016 Script group omitted'
         SPACE 1
ERMSG5   DC    AL1(L'ERMSG5T)
ERMSG5T  DC    C'017 Version number omitted or invalid'
         SPACE 1
ERMSG6   DC    AL1(L'ERMSG6T)
ERMSG6T  DC    C'018 Unbalanced brackets in parameter section'
         SPACE 1
ERMSG7   DC    AL1(L'ERMSG7T)
ERMSG7T  DC    C'019 ERROR in T2SE user data'
         SPACE 1
ERMSG8   DC    AL1(L'ERMSG8T)
ERMSG8T  DC    C'007 Invalid userd id'
         SPACE 1
ERMSG9   DC    AL1(L'ERMSG9T)
ERMSG9T  DC    C'011 Resource not active'
         SPACE 1
ERMSGA   DC    AL1(L'ERMSGAT)
ERMSGAT  DC    C'019 Internal error - warn system control'
         SPACE 1
ERMSGB   DC    AL1(L'ERMSGBT)
ERMSGBT  DC    C'012 Specified token does not match current token'
         SPACE 1
         EJECT ,
*---------------------------------------------------------------------*
*        More serious errors                                          *
*---------------------------------------------------------------------*
         SPACE 1
A70AER1  DS    0H
         LA    R0,ERTXT1-1
         SERRC E,(APP0E16),MSG=YES  UNABLE TO RETURN CONTROL MESSAGE
         DC    AL1(L'ERTXT1)
ERTXT1   DC    C'T2SE - UNABLE TO RETURN XML/SOAP ERROR MESSAGE'
         EJECT ,
         FINIS ,
         END ,
