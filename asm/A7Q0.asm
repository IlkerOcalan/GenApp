         PRINT NOGEN
         BEGIN NAME=A7Q0,VERSION=&VV,SHR=NONE,XCL=NONE
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
*        Receiver segment for the ALCS MQ bridge user exit DXCUMQB0   *
*        and from TCP/IP server via user exit DXCUTCP2.               *
*                                                                     *
*        This segment is activated whenever a T2SE request queue      *
*        receives a SOAP request.                                     *
*                                                                     *
*        This segment handles messages specifically addressed to      *
*        T2SE (recognized by the SOAP envelope and the reference to   *
*        the T2SE package). All other messages result in the return   *
*        to ALCS indicating that the message is not handled. If       *
*        additional messages must be processed, coding to handle      *
*        these messages must be included and the applicable segment   *
*        must be activated.                                           *
*                                                                     *
*        A successfull handling of the message results in the return  *
*        to ALCS with the indication to ignore the message.           *
*        A7Q0 performs the following functions:                       *
*        - check the incoming message                                 *
*          If the message is XML/SOAP directed to T2SE, it is         *
*          handled here. Else the message is returned to ALCS.        *
*                                                                     *
*        T2SE message handling                                        *
*        - the CRN of the MQ bridge CRI, or the TCP/IP server CRI     *
*          is checked in the T2SE Control Record.                     *
*          If not present, an error is returned.                      *
*                                                                     *
*        - the KCPL is set up and the XML/SOAP parsing segment (A70A) *
*          is activated.                                              *
*                                                                     *
*---------------------------------------------------------------------*
         EJECT ,
***********************************************************************
*  PROGRAM NAME : A7Q0R1                                              *
*    PROGRAMMER : GEOFF LOWRY                                         *
*          DATE : 10JUL08                 CHANGE NUMBER : @T2SE08     *
*                                                                     *
*  PURPOSE : INITIAL BUILD OF A7Q0                                    *
*            STREAMLINE CODE                                          *
*                                                                     *
*                                                                     *
***********************************************************************
*  DL0104 22SEP2014  Enhance T2SE to support messages from TCP/IP     *
*  DL0141 19FEB2015  Enhance T2SE to support JCA msgs from TCP/IP     *
***********************************************************************
         EJECT ,
*---------------------------------------------------------------------*
*        Data macros                                                  *
*---------------------------------------------------------------------*
         SPACE ,
         T2SEEQ                    T2SE EQUATES
         COPY  T2SESET                                         @T2SETPF
         AM0SG REG=R2              AMSG DSECT
         AP0MS REG=R4              AMSG LAYOUT
         KCPL  REG=R15             KCPL DSECT
         CI0CO REG=R5              RCB                           DL0141
         DROP  R5                  USING AS REQUIRED             DL0141
         AP0MS REG=R5,SUFFIX=#     AMSG LAYOUT
         AP0IN REG=R7              T2SE CONTROL RECORD (ROUTER)
         CO0IC REG=R14             COMIC AREA
         CMQA                      MQ EQUATES
         T2SEHDR REG=R0            T2SE Header from TCP/IP       DL0104
         DROP  R0                  Do using as required          DL0104
         RC0PL REG=R1              RCPL DSECT                    DL0104
         DROP  R1                  Do using as required          DL0104
         SPACE 1
*---------------------------------------------------------------------*
*        Start of processing                                          *
*        Level 0 holds the request message (heap storage)             *
*        Level 1 holds the MQ message descritpor or TCP/IP HDR   DL0104
*        Retrieve control record on level A                           *
*        Retrieve input message blocks on level F                     *
*---------------------------------------------------------------------*
         SPACE ,
         CMQMDA DSECT=YES,LIST=YES  MESSAGE DESCRIPTOR
         SPACE 1
         SPACE 1
         RSECT ,                    RESTORE PROGRAM CSECT BASE
         SPACE 1
         USING MQMD,R3              DSECT FOR MESSAGE DESCRIPTOR
         SPACE 1
*                                                                DL0104
* First check for TCP/IP message and strip off the header        DL0104
* and place on D1                                                DL0104
*                                                                DL0104
         #IF (LEVTA,D1,NOTUSED)     IF NO MESSAGE DESCRIPTOR     DL0104
*                                   MUST BE FROM TCP/IP          DL0104
           MVI   EBW103,KCPL3IP     SWITCH FOR TCP/IP INPUT      DL0104
           #IF (LEVTA,D0,INUSE)     IF MSG IN CORE BLOCK         DL0104
             L     R2,CE1CR0        BASE INPUT CORE BLOCK        DL0104
           #ELSE                    IF MSG IN MALOC BLOCK        DL0104
             L     R2,CE1FA0        BASE INPUT MALOC BLOCK       DL0104
             ST    R2,CE1CR0        STORE MALOC ADDRESS          DL0104
           #EIF                                                  DL0104
           ICM   R15,B'1111',AM0CCX GET MESSAGE COUNT            DL0104
           AHI   R15,-(AM0TXT-AM0LIT)  ADJUST FOR CRI STUFF      DL0104
           LA    R2,AM0TXT          START OF MESSAGE             DL0104
           DROP R2                  DROP AM0SG                   DL0104
           USING T2SEHDR,R2                                      DL0104
           #IF (CLC,=C'T2SE',EQ,T2SEHDR_DEST)     EBCDIC T2SE    DL0141
           #ELIF (CLC,=CA'T2SE',EQ,T2SEHDR_DEST)  ASCII T2SE     DL0141
             OI    EBW103,KCPL3AS   SET ASCII                    DL0141
           #ELIF (CLC,=C'JCA ',EQ,T2SEHDR_DEST)   EBCDIC JCA     DL0141
             OI    EBW103,KCPL3JC   SET JCA REQUEST              DL0141
           #ELIF (CLC,=CA'JCA ',EQ,T2SEHDR_DEST)  ASCII JCA      DL0141
             OI    EBW103,KCPL3AS+KCPL3JC  SET ASCII + JCA       DL0104
           #ELIF (CLC,=C'ECHO',EQ,T2SEHDR_DEST),OR,  EBCDIC ECHO DL0141
           #     (CLC,=CA'ECHO',EQ,T2SEHDR_DEST)     ASCII ECHO  DL0141
             OI    EBW103,KCPL3EC   SET ECHO REQUEST             DL0141
             LA    R1,CE1RCPL       BASE RCPL                    DL0141
             USING RC0PL,R1                                      DL0141
             LA    R15,EBW000       KCPL                         DL0141
             XC    EBW000(16),EBW000 CLEAR KCPL AREA             DL0141
             MVC   KCPLDES,RCPLDES  MOVE DEST CRI FROM RCPL      DL0141
             MVC   KCPLORG,RCPLORG  MOVE ORIGIN CRI FROM RCPL    DL0141
             #GOTO ECHO_INPUT                                    DL0141
           #ELSE                    NOT VALID T2SE/JCA - ERROR   DL0141
             MVI   EBER01,X'01'     SET ERROR INDICATOR          DL0104
             XC    EBW000(16),EBW000 CLEAR KCPL AREA             DL0104
             #GOTO GET_CONTROL_RECORD                            DL0141
           #EIF                                                  DL0104
           SR    R00,R00            CLEAR FOR INSERT             DL0104
           ICM   R00,B'1100',T2SEHDR_TOT_SIZE  BIG ENDIAN?       DL0104
           #IF (NZ)                 LITTLE ENDIAN?               DL0104
*          LITTLE ENDIAN HAS INTEGER BYTES IN REVERSE SEQUENCE   DL0104
             ICM   R00,B'0001',0+T2SEHDR_TOT_SIZE MAKE BIG ENDIANDL0104
             ICM   R00,B'0010',1+T2SEHDR_TOT_SIZE MAKE BIG ENDIANDL0104
             ICM   R00,B'0100',2+T2SEHDR_TOT_SIZE MAKE BIG ENDIANDL0104
             ICM   R00,B'1000',3+T2SEHDR_TOT_SIZE MAKE BIG ENDIANDL0104
             OI    EBW103,KCPL3LE  SET LITTLE ENDIAN             DL0104
             ST    R00,T2SEHDR_TOT_SIZE  SAVE AS BIG ENDIAN      DL0104
           #ELSE                                                 DL0104
             L     R00,T2SEHDR_TOT_SIZE  GET TOTAL LENGTH        DL0104
           #EIF                                                  DL0104
           #IF (CR,R00,NE,R15)      IS LENGTH CORRECT?           DL0104
              MVI   EBER01,X'02'    NO - SET LENGTH ERROR        DL0104
              LR    R00,R15         USE AM0CCX SIZE              DL0104
           #EIF                                                  DL0104
           GETCC D1,L0              GET BLOCK FOR HEADER         DL0104
           MVC   0(T2SEHDR_LEN,R14),T2SEHDR  MOVE HEADER TO D1   DL0104
           AHI   R0,-T2SEHDR_LEN    ADJUST FOR HEADER            DL0104
           ST    R0,EBW004          SIZE OF MESSAGE              DL0104
           LR    R3,R0              LENGTH TO MOVE               DL0104
           LR    R4,R2              CALCULATE FROM ADDRESS       DL0104
           DROP  R2                                              DL0141
           USING AM0SG,R2                                        DL0141
           L     R2,CE1CR0          MOVE TO BEGINING OF BLOCK    DL0104
           #IF   (TM,EBW103,KCPL3JC,ON) FOR JCA SETUP IMSG       DL0141
              AHI   R3,AM0NP1-AM0LIT  CALCULATE COUNT FIELD      DL0141
              STH   R3,AM0CCT       STORE COUNT                  DL0141
              LR    R3,R0           REFRESH WITH MOVE COUNT      DL0141
              LA    R2,AM0NP1       CREATE IMSG FOR UII1         DL0141
           #EIF                                                  DL0141
           AHI   R4,T2SEHDR_LEN     MOVE PAST HEADER             DL0104
           LR    R5,R0              LENGTH TO MOVE               DL0104
           MVCL  R2,R4              MOVE TO START OF BLOCK       DL0104
         #ELSE                                                   DL0104
           MVI   EBW103,KCPL3MQ     MQ INPUT SWITCH              DL0104
         #EIF                                                    DL0104
         L     R2,CE1CR0            BASE INPUT MESSAGE
         #IF   (TM,EBW103,KCPL3JC,ON) FOR JCA SETUP IMSG         DL0141
            LA    R2,AM0NP1       CREATE IMSG FOR UII1           DL0141
         #EIF                                                    DL0141
         #IF (TM,EBW103,KCPL3AS,ON) IF INPUT IN ASCII            DL0104
           ASCIC FROM,DATA=(R2),LENGTH=EBW004  TRANSLATE ASCII   DL0104
         #EIF                                                    DL0104
         #GOTO GET_CONTROL_RECORD,IF,(TM,EBW103,KCPL3JC,ON)      DL0141
         LA    R15,1000             CHECK A MAXIMUM OF 1000 BYTES
*        PREVENT SCANNING FOR TAG BEYOND SIZE OF INPUT         @CONST2A
         L     R3,EBW004            SIZE OF MESSAGE            @CONST2A
         AHI   R3,-25               LENGTH OF STRING & SOME      DL0104
*                                   EXTRAS                     @CONST2A
         #IF   R3,NP                                           @CONST2A
           MVI EBER01,X'01'         ERROR                      @CONST2A
           XC  EBW000(16),EBW000    CLEAR KCPL AREA            @CONST2A
           #GOTO GET_CONTROL_RECORD                              DL0141
         #EIF                                                  @CONST2A
         #IF   R15,GT,R3            R15 BEYOND END OF DATA?    @CONST2A
           LR  R15,R3               THEN FIX IT                @CONST2A
         #EIF                                                  @CONST2A
* * * * * * * *
*        SCAN FOR SOAP ENVELOPE START
* * * * * * * *
         #DO TIMES=(R15)
            #EXIF (CLC,=C'<SoapEnv:',EQ,0(R2))  XML/SOAP MESSAGE DL0141
         #OREL
            LA    R02,1(R02)        NEXT CHARACTER
         #ELOP
            MVI   EBER01,X'01'      NOT FOUND - SET ERROR
            XC    EBW000(16),EBW000  CLEAR KCPL AREA
            #GOTO GET_CONTROL_RECORD
         #EDO
         LA    R02,9(R02)           STEP OVER PREVIOUS FIELD
         SH    R15,=H'9'            ADJUST REMAINING COUNT
* * * * * * * *
*        SCAN FOR T2SE SCRIPT
* * * * * * * *
         #DO TIMES=(R15)            SCAN FOR T2SE SCRIPT
         #EXIF (CLC,=C'xmlns:T2SEScript',EQ,0(R2))
         #OREL
            LA    R2,1(R2)          NEXT CHARACTER
         #ELOP
            MVI   EBER01,X'01'      SET ERROR INDICATOR
            XC    EBW000(16),EBW000  CLEAR KCPL AREA
            #GOTO GET_CONTROL_RECORD
         #EDO
         EJECT
***********************************************************************
*        T 2 S E   M E S S A G E                                      *
*                                                                     *
*        REFORMAT TO AP0MS (T2SE) FORMAT                              *
***********************************************************************
         SPACE 1
         L     R2,CE1CR0            BASE INPUT MESSAGE
         L     R3,EBW004            LENGTH OF MESSAGE
         SPACE 1
         AHI   R3,200               SPACE FOR HEADERS            DL0104
         #IF (C,R3,NL,=A(#LXSIZE))  IS INPUT TOO LARGE?
            MVI   EBER01,X'02'                               @T2SE0500X
            XC    EBW000(16),EBW000  CLEAR KCPL AREA         @T2SE0500X
            #GOTO GET_CONTROL_RECORD                             DL0141
         #EIF
         STCM  R3,B'0011',EBCCC2    FILL IN BLOCK COUNT      @T2SE04003
         LR    R14,R3               DON'T TOUCH R3           @T2SE04003
         MALOC SIZE=(R14)           OBTAIN STORAGE           @T2SE04003
         ST    R14,CE1CR2           STORE IN CAW 2           @T2SE04003
*CLEAR                                                       @CONST2A
         LR    R15,R3               SIZE                     @CONST2A
         LR    R2,R14               POINTER TO AREA          @CONST2A
         XR    R3,R3                TELL MVCL TO CLEAR       @CONST2A
         MVCL  R14,R2               CLEAR IT                 @CONST2A
         L     R14,CE1CR2           AND GET POINTER BACK     @CONST2A
*ENDCLEAR                                                    @CONST2A
         L     R4,CE1CR2            BASE NEW INPUT MESSAGE
         L     R2,CE1CR0            BASE OLD INPUT MESSAGE
         MVC   AP0TYP(L'T2SEHEADER),T2SEHEADER  INSERT HEADER
         LA    R6,AP0TXT            START OF APPLICATION TEXT
         L     R03,EBW004           SIZE OF MESSAGE
         LR    R7,R3                LENGTH REGISTER
         MVCL  R6,R2                SHIFT ENTIRE MESSAGE
         LR    R14,R6               GET END OF DATA
         LA    R15,AP0LEN           BEGINNING OF DATA
         SR    R14,R15              CALCULATE LENGTH
         STCM  R14,B'0011',AP0CNT   SET LENGTH
         LR    R14,R6               END OF DATA
         LA    R15,AP0TOT           GET TOTAL FIELD
         SR    R14,R15              CALCULATE TOTAL SIZE
         STCM  R14,B'0011',AP0TOT   SET TOTAL SIZE
         LR    R14,R6               GET END OF DATA
         LA    R15,AP0BID+(AM0LIT-AM0RID)  COUNT FOR AM0SG
         SR    R14,R15              CALCULATE LENGTH
         STCM  R14,B'0011',AP0BID+(AM0CCT-AM0RID)  SET AM0SG LENGTH
         SPACE 1
* * * * * * * *
*        DISCARD ORIGINAL INPUT MESSAGE
* * * * * * * *
         #IF (LEVTA,D0,INUSE)       IF MSG IN CORE BLOCK         DL0104
           RELCC D0                 RELEASE CORE BLOCK           DL0104
         #ELSE                      IF MSG IN MALOC BLOCK        DL0104
           L     R14,CE1CR0                                      DL0104
           FREEC BLOCK=(R14)        FREE STORAGE BLOCK           DL0104
           XC    CE1CR0,CE1CR0      SPOIL ANY ATTEMPTED REFERENCEDL0104
         #EIF                                                    DL0104
         SPACE 1
* * * * * * * *
*        MOVE NEW FORMAT MESSAGE TO D0
* * * * * * * *
         FLIPC D0,D2                NEW T2SE FORMAT MESSAGE TO LEVEL0
         SPACE 1
         XC    EBW000(16),EBW000    CLEAR KCPL AREA
         SPACE 1
* * * * * * * *
 #LOCA  GET_CONTROL_RECORD                                       DL0141
* * * * * * * *
         #PERF R4,RETRIEVE_CONTROL_RECORD
         SPACE 1
* * * * * * * *
*        VALIDATE WHETHER INCOMING RESOURCE DEFINED
* * * * * * * *
         SPACE 1
         COMIC CRI=EBROUT,          POINT TO MQ COMMCC SLOT            -
               DATA=SYS,                                               -
               AREA=(0,ICELN2)
         #GOTO BAD_CRI,IF,(4)       INVALID CRI
         SPACE 1
         #IF  (TM,EBW103,KCPL3IP,ON)
*          For TCP/IP we need the base server comms table        DL0104
           COMIC CRI=ICEORC+1,        POINT TO TCP/IP BASE SERV  DL0104-
               DATA=SYS,                                         DL0104-
               AREA=(0,ICELN2)                                   DL0104
           #GOTO NO_BASE_SERVER,IF,(4)  NO BASE TCPIP SERVER
         #EIF
         SPACE 1                                                 DL0104
         LA    R1,AP0MQM            MAX NUMBER OF SLOTS
         SPACE 1
         #DO TIMES=(R1)             LOOK FOR SLOT
         #EXIF (AP0CRN,EQ,ICECRN)   SLOT FOUND
         #OREL
           LA    R7,AP0MQI(,R7)     INCREMENT POINTER
         #ELOP
*          UNABLE TO FIND MQ LINK - ISSUE ERROR MESSAGE
*
*          PREVENT RUNNING INTO UPDATING A SLOT FOR A LINK WHICH
*          IS NOT REPRESENTED IN THE CONTROL RECORD
           RCUNC DA
           MVI   EBER01,X'01'       INVALID MESSAGE
           #GOTO SEND_ERROR_RESPONSE
           SPACE 1
         #EDO
*
         LA    R15,EBW000           KCPL BASE
         #IF  (TM,EBW103,KCPL3JC,OFF)  NOT FOR JCA               DL0141
           L     R4,CE1CR0            INPUT MESSAGE
           MVC   KCPLORG,AP0MQA       COPY ORIGINATING APPL NAME
           MVC   KCPLDES,=C'T2SE'     DESTINATION APPLICATION
           L     R2,CE1CR0            INPUT MESSAGE
           SR    R15,R15              PREPARE FOR INSERT
           ICM   R15,B'0011',AP0TOT   MESSAGE LENGTH
         #ELSE                                                   DL0141
           DROP  R3                                              DL0141
           USING T2SEHDR,R3                                      DL0141
           DROP  R2                                              DL0141
           USING AM0SG,R2                                        DL0141
           XC    EBW000(16),EBW000    CLEAR KCPL AREA            DL0141
           L     R3,CE1CR1            GET T2SEHDR                DL0141
           L     R2,CE1CR0            GET IMSG                   DL0141
           SR    R0,R0                CLEAR FOR INSERT           DL0141
           ICM   R0,B'0011',AP0CNU    GET MAX INDEX THIS APPL    DL0141
           N     R0,=X'00003FFF'      REMOVE ANY LOG FLAG        DL0141
           SR    R14,R14              CLEAR FOR INSERT           DL0141
           ICM   R14,B'0111',T2SEHDR_CRI  GET MSG CRI INDEX      DL0141
           #IF  (CR,R14,GT,R0)        IF CRI TOO LARGE           DL0141
             MVI   EBER01,X'03'       BAD CRI INDEX              DL0141
           #EIF                                                  DL0141
           ICM   R0,B'0011',AP0CST    GET JCA BASE CRI           DL0141
           AR    R0,R14               ADD BASE CRI               DL0141
           ICM   R0,B'0100',=AL1(T2SECRI)  ADD T2SE CRI          DL0141
           STCM  R0,B'0111',AM0LIT     PUT CRI IN IMSG           DL0141
           STCM  R0,B'0111',EBROUT     PUT CRI IN EBROUT         DL0141
         #EIF                                                    DL0141
         EJECT ,
*---------------------------------------------------------------------*
*        Non- control message found - test T2SE options               *
*---------------------------------------------------------------------*
         SPACE ,
         ICM   R14,15,AP0CIN        COUNT OF INCOMING MESSAGES
         LA    R14,1(R14)           ADD 1
         STCM  R14,15,AP0CIN        STORE NEW COUNT
         SPACE 1
         L     R4,CE1CR0            (CONVERTED) INPUT MESSAGE
         LA    R15,EBW000           ADDRESS KCPL DSECT
         MVC   KCPLDES,=C'T2SE'     DESTINATION APPLICATION
         MVC   KCPLORG,AP0MQA       ORIGINATING APPLICATION NAME
         MVC   KCPLCTL3,EBW103      MOVE IP/MQ/AS/LE FLAGS       DL0104
         OI    KCPLCTL2,KCPL2CID    KCPL FOLLOWED BY 24 BYTES CORR ID
         L     R3,CE1CR1            BASE MESSAGE DESCRIPTOR
         #IF (TM,EBW103,KCPL3IP,ON)  TCP/IP INPUT REQUEST        DL0104
           DROP R3                  DROP MQMD                    DL0104
           USING T2SEHDR,R3                                      DL0104
           LA    R1,CE1RCPL         BASE RCPL                    DL0104
           USING RC0PL,R1                                        DL0104
           MVC   KCPLDES,RCPLDES    MOVE DEST CRI FROM RCPL      DL0104
           MVC   KCPLORG,RCPLORG    MOVE ORIGIN CRI FROM RCPL    DL0104
           MVC   KCPLCID(L'T2SEHDR_CORRELID),T2SEHDR_CORRELID    DL0104
           DROP R3                  DROP T2SEHDR                 DL0104
           DROP R1                  DROP RCOPL                   DL0104
         #ELSE                      ELSE MQ INPUT REQUEST        DL0104
           USING MQMD,R3            DSECT FOR MESSAGE DESCRIPTOR DL0104
           OI    KCPLCTL2,KCPL2RTQ  KCPL FOLLOWED BY REPLY TO Q NAME
           MVC   KCPLCID(L'MQMD_CORRELID),MQMD_CORRELID
           MVC   KCPLRTQ(L'MQMD_REPLYTOQ),MQMD_REPLYTOQ
         #EIF                                                    DL0104
         SPACE 1
******** XML/SOAP REQUEST - ACTIVATE SOAP SUPPORT PACKAGE
         SPACE 1
         FILUC DA                   FILE UPDATED T2SE CONTROL RECORD
         LA    R15,EBW000           ADDRESS KCPL
         #GOTO SEND_ERROR_RESPONSE,IF,EBER01,NONZERO             DL0141
*
         MVI   EBW103,X'00'         CLEAR TEMP TCP/IP SWITCH     DL0104
         #IF  (TM,KCPLCTL3,KCPL3JC,ON)    JCA MESSAGE            DL0141
           COMIC CRI=EBROUT,DATA=SYS,AREA=(0,ICELN2)             DL0141
           #IF (4)                    CRI NOT FOUND              DL0141
              MVI   EBER01,X'03'      BAD CRI INDEX              DL0141
           #EIF                                                  DL0141
           #IF  (TM,ICESAAA,L'ICESAAA,ON)   AAA HOLD ON          DL0141
             MVI    EBER01,X'04'      AAA Hold                   DL0141
           #EIF                                                  DL0141
           #GOTO SEND_ERROR_RESPONSE,IF,EBER01,NONZERO           DL0141
*
           MVI   EBCM01,B'00100110'  RETRIEVE RCB WITH HOLD      DL0141
           ENTRC A7RB                RCB WITH HOLD               DL0141
           LA    R15,EBW000          ADDRESS KCPL                DL0141
           #GOTO ERR6,IF,(TM,EBW040,X'80',ON)  GO DUMP IF ERROR  DL0141
*
           DROP  R5                                              DL0141
           USING CI0CO,R5                                        DL0141
           L     R5,CE1CR3           ADDRESS RCB                 DL0141
           MVC   CI0RTE,KCPL         MOVE KCPL TO RCB            DL0141
           MVC   CI0OPT(L'T2SEHDR_CORRELID),KCPLCID              DL0141
           FILUC D3                  FILE RCB                    DL0141
           DROP  R5                  DROP USING FOR RCB          DL0141
*
           SR    R0,R0               SET FOR UPPER CASE          DL0141
           ENTRC A7UP                set message to UPPER CASE   DL0141
*
           SR    R14,R14             CLEAR LENGTH FOR CREMC      DL0141
           CREMC NAME=UII1,LEVEL=D0  PROCESS JCA INPUT           DL0141
           EXITC                                                 DL0141
*
         #ELSE                        NOT JCA MESSAGE            DL0141
           OI    KCPLCTL1,KCPL1WKU    INDICATE INTERNAL FORMAT
           OI    KCPLCTL1,KCPL1DYN    DYNAMIC CRI REQUIRED
           OI    KCPLCTL2,KCPL2XML    XML/SOAP MESSAGE
           ENTNC A70A                 ACTIVATE XML/SOAP PARSING
         #EIF                                                    DL0141
         SPACE 1
         EJECT ,
*---------------------------------------------------------------------*
*        Error - illegal request                                      *
*        Either one of the following conditions occurred:             *
*        - Incoming MQ Queue is not defined in control table          *
*        - Message is unknown (currently only XML/SOAP for T2SE)      *
*        - Bad CRI index in JCA header                                *
*---------------------------------------------------------------------*
         SPACE ,
 #LOCA SEND_ERROR_RESPONSE                                       DL0141
         #IF (LEVTA,D0,INUSE)       IF MSG IN CORE BLOCK         DL0104
           RELCC D0                 RELEASE CORE BLOCK           DL0104
         #ELSE                      IF MSG IN MALOC BLOCK        DL0104
           L     R14,CE1CR0                                      DL0104
           FREEC BLOCK=(R14)        FREE STORAGE BLOCK           DL0104
           XC    CE1CR0,CE1CR0      SPOIL ANY ATTEMPTED REFERENCEDL0104
         #EIF                                                    DL0104
         LA    R2,#T2SE4K          GET 4K SIZE                   DL0104
         GETCC D0,SIZE=(R2)        GET CORE FOR ERROR RESPONSE   DL0104
         L     R4,CE1CR0           AP0MS DSECT
         L     R2,CE1CR0           AMSG DSECT
         SR    R14,R14             CLEAR FOR IC                  DL0141
         IC    R14,EBER01          GET ERROR NUMBER              DL0141
         #CAST R14,MAX=4                                         DL0141
         #CASE 1                                                 DL0141
           MVC   AM0TXT(AMQ0ILLL),AMQ0ILL ERROR MESSAGE
           LA    R14,AMQ0ILLL        MESSAGE LENGTH
         #CASE 2                                                 DL0141
           MVC   AM0TXT(AMQ02BIGL),AMQ02BIG    ERROR MESSAGE     DL0141
           LA    R14,AMQ02BIGL       MESSAGE LENGTH              DL0141
         #CASE 3                                                 DL0141
           MVC   AM0TXT(AMQ03CRIL),AMQ03CRI  CRI INDEX BAD       DL0141
           LA    R14,AMQ03CRIL       MESSAGE LENGTH              DL0141
         #CASE 4                                                 DL0141
           MVC   AM0TXT(AMQ04AAAL),AMQ04AAA  AAA HOLD            DL0141
           LA    R14,AMQ04AAAL       MESSAGE LENGTH              DL0141
         #CASE ERROR                                             DL0141
           MVC   AM0TXT(AMQ0ILLL),AMQ0ILL INVALID MSG            DL0141
           LA    R14,AMQ0ILLL        MESSAGE LENGTH              DL0141
         #ECAS                                                   DL0141
         AH    R14,=AL2(AM0TXT-AM0LIT) INCLUDE CONTROL CHARACTERS
         STCM  R14,B'0011',AM0CCT  AMSG MESSAGE LENGTH
         EJECT ,
* * * * * * * *
*        ERROR SEND ROUTINE
* * * * * * * *
         MVI   EBER01,X'00'      CLEAR ERRO INDICATOR        @T2SE0500X
         #IF LEVTA,DA,INUSE
           ICM   R14,15,AP0CIN      COUNT OF INCOMING MESSAGES
           LA    R14,1(R14)         ADD 1
           STCM  R14,15,AP0CIN      STORE NEW COUNT
           FILUC DA                 FILE UPDATED T2SE CONTROL RECORD
         #EIF
 #LOCA ECHO_INPUT                                                DL0141
         SPACE 1
         LA    R15,EBW000           KCPL POINTER
         SPACE 1
******** SWAP ROUTING IN ORDER TO RETURN MESSAGE
         SPACE 1
         L     R14,KCPLORG          ORIGINATOR
         L     R0,KCPLDES           DESTINATION
         ST    R14,KCPLDES
         ST    R0,KCPLORG
         MVC   KCPLCTL3,EBW103      MOVE IP/MQ/LE/AS FLAGS       DL0104
*
         #IF (TM,KCPLCTL3,KCPL3EC,ON)  ECHO                      DL0141
            #IF (LEVTA,D0,INUSE)    INPUT IN CORE BLOCK          DL0141
               LH    R14,CE1CC0     GET SIZE FOR MALOC           DL0141
               MALOC SIZE=R14       GET MALOC BLOCK FOR A702     DL0141
               ST    R14,CE1FA0     STORE ADDRESS FOR ROUTC      DL0141
               LR    R02,R14        NEEDED FOR A702              DL0141
               LH    R15,CE1CC0     GET SIZE FOR MOVE            DL0141
               LR    R01,R15        COPY SIZE                    DL0141
               L     R00,CE1CR0     INPUT ECHO MSG               DL0141
               MVCL  R14,R00        MOVE ECHO TO MALOC           DL0141
               RELCC D0             RELEASE CORE BLOCK           DL0141
               ST    R02,CE1CR0     STORE MALOC ADDR FOR A702    DL0141
            #EIF                                                 DL0141
         #ELSE                      NOT ECHO                     DL0141
            OI    KCPLCTL2,KCPL2CID KCPL FOLLOWED BY CORREL ID   DL0104
            SPACE 1
            L     R3,CE1CR1            BASE MESSAGE DESCRIPTOR
            #IF (TM,EBW103,KCPL3IP,ON) IF TCP/IP REQUEST         DL0104
              DROP  R3                 DROP MQMD                 DL0104
              USING T2SEHDR,R3         USING FRO TCP/IP HEADER   DL0104
              MVC   KCPLCID(L'T2SEHDR_CORRELID),T2SEHDR_CORRELID DL0104
              DROP  R3                 DROP USING FOR T2SEHDR    DL0104
            #ELSE                                                DL0104
              USING MQMD,R3            USING FOR MQMD            DL0104
              OI    KCPLCTL2,KCPL2RTQ  KCPL FOLLOWED BY REPLY TO Q NAME
              MVC   KCPLRTQ(L'MQMD_REPLYTOQ),MQMD_REPLYTOQ
              MVC   KCPLCID(L'MQMD_CORRELID),MQMD_CORRELID
            #EIF                                                 DL0104
         #EIF                                                    DL0141
         LA    R3,EBW000            KCPL ADDRESS FOR A702
         FLIPC D0,D2                A702 EXPECTS MSG ON LEVEL D2
         CRUSA S0=1                 RELEASE MESSAGE DESCRIPTOR
         SPACE 1
******** CALL MQ SEND PROGRAM
         SPACE 1
         ENTRC A702
         SPACE 1
         LA    R15,EBW000          RE-BASE KCPL POINTER       @CONST2A
         #GOTO SEND_FAILED,IF,(TM,KCPLCTL0,KCPL0RET,ON)
         EXITC
         EJECT ,
*---------------------------------------------------------------------*
*        Subroutine - retrieve T2SE control record                    *
*---------------------------------------------------------------------*
         #SUBR RETRIEVE_CONTROL_RECORD,R4
           LA    R0,#T2SERTR       RECORD ORDINAL
           LA    R6,#T2SERTF       RECORD TYPE
           LA    R7,CE1FAA         TARGET LEVEL FOR FACE
           SPACE 1
           ENTRC FACE
           SPACE 1
           #GOTO CTL_REC_RETRIEVE_ERROR,IF,(R0,ZERO)  FACE ERROR
           SPACE 1
           MVC   CE1FAA(2),=AL2(TLOGID)  RECORD ID
           #GOTO CTL_REC_RETRIEVE_ERROR,IF,(FIWHC,DA,NOK)  FIND ERROR
           L     R7,CE1CRA         SET BASE CONTROL RECORD
           SPACE 1
         #ESUB
         EJECT ,
*---------------------------------------------------------------------*
*        Error situations and dumps                                   *
*---------------------------------------------------------------------*
         SPACE 1
 #LOCA CTL_REC_RETRIEVE_ERROR
         LA    R0,ERMSG2-1
         ICM   R1,B'0111',=AL3(APP0E08) OPR- NUMBER              DL0141
         #GOTO GODUMP                                            DL0141
 #LOCA BAD_CRI
         LA    R0,ERMSG3-1
         ICM   R1,B'0111',=AL3(APP0E12) OPR- NUMBER              DL0141
         #GOTO GODUMP                                            DL0141
 #LOCA SEND_FAILED
         LA    R0,ERMSG4-1
         ICM   R1,B'0111',=AL3(APP0E16) OPR- NUMBER              DL0141
         #GOTO GODUMP                                            DL0141
 #LOCA NO_BASE_SERVER
         LA    R0,ERMSG5-1
         ICM   R1,B'0111',=AL3(APP0E12) OPR- NUMBER              DL0141
         #GOTO GODUMP                                            DL0141
 #LOCA ERR6                                                      DL0141
         LA    R0,ERMSG6-1                                       DL0141
         ICM   R1,B'0111',=AL3(APP0E12) OPR- NUMBER              DL0141
*        #GOTO GODUMP                                            DL0141
 #LOCA GODUMP                                                    DL0141
         SERRC E,(R1),MSG           DUMP WITH EXIT               DL0141
         EJECT ,
*---------------------------------------------------------------------*
*        Constants and messages                                       *
*---------------------------------------------------------------------*
         SPACE 1
         DC    AL1(L'ERMSG2)
ERMSG2   DC    C'T2SE - UNABLE TO RETRIEVE CTL RECORD'
         SPACE 1
         DC    AL1(L'ERMSG3)
ERMSG3   DC    C'COMIC - UNABLE TO FIND RESOURCE'
         SPACE 1
         DC    AL1(L'ERMSG4)
ERMSG4   DC    C'T2SE - UNABLE TO RETURN CONTROL MESSAGE'
         SPACE 1
         DC    AL1(L'ERMSG5)
ERMSG5   DC    C'COMIC - UNABLE TO FIND BASE TCP/IP SERVER'
         SPACE 1
         DC    AL1(L'ERMSG6)                                     DL0141
ERMSG6   DC    C'A7RB - UNABLE TO FIND RCB'                      DL0141
         SPACE 1
AMQ0ILL  DC    C'<SoapEnv:Body>'                             @T2SE0500X
         DC    X'0D25'                                       @T2SE0500X
         DC    C'ILLEGAL ACCESS REQUEST'
         DC    C'</SoapEnv:Body>'                            @T2SE0500X
         DC    AL1(#EOM)
AMQ0ILLL EQU   *-AMQ0ILL            LENGTH OF MESSAGE        @T2SE0500X
         SPACE 1
AMQ02BIG DC    C'<SoapEnv:Body>'                             @T2SE0500X
         DC    X'0D25'                                       @T2SE0500X
         DC    C'INPUT REJECTED - LENGTH ERROR'                  DL0104
         DC    C'</SoapEnv:Body>'                            @T2SE0500X
         DC    AL1(#EOM)
AMQ02BIGL EQU  *-AMQ02BIG           LENGTH OF MESSAGE        @T2SE0500X
*
AMQ03CRI DC    C'INPUT REJECTED - BAD CRI INDEX'                 DL0141
         DC    AL1(#EOM)                                         DL0141
AMQ03CRIL EQU  *-AMQ03CRI                                        DL0141
*
AMQ04AAA DC    C'INPUT REJECTED - AAA HOLD'                      DL0141
         DC    AL1(#EOM)                                         DL0141
AMQ04AAAL EQU  *-AMQ04AAA                                        DL0141
         SPACE 1
OMID     DC    C'OM'
         DC    X'00'
T2SEHEADER DC  C'0003099000000000000000'
         FINIS ,
         END   ,
