         PRINT NOGEN
         BEGIN NAME=A7TO,VERSION=&VV,SHR=NONE,XCL=NONE
.* Endevor ALCS241 merge - force assembly                        DL0070
*---------------------------------------------------------------------*
*-*-*-*- ALCS SPECIFIC
         SPACE 2
***********************************************************************
*                                                                     *
* Program Title: - convert TOD clock value to local time              *
*                                                                     *
***********************************************************************
*                                                                     *
* PURPOSE      1. This program is activated only by the T2SCO macro   *
*                 in order to convert the tod clock value to          *
*                 different output formats.                           *
*                                                                     *
*              2. The tod clock value is presented as UTC value,      *
*                 therefore the program recalculates the specified    *
*                 time into a local time.                             *
*                                                                     *

***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
* INPUT:                                                              *
* ======                                                              *
*                                                                     *
* R0:      ADDRESS OF SPECIFIED 'AREA=' LOCATION (INPUT)/OUTPUT.      *
*          high order bit specifies if UTC is requested (bit on) or   *
*          local time requested.                                      *
*                                                                     *
* AREA= 1ST 8 BYTES: 0 = CONVERT CURRENT TOD (VIA 'STCK')             *
*                    ELSE = TOD CLOCK VALUE TO BE CONVERTED           *
*                                                                     *
*                                                                     *
* R0 - R6  WORK                                                       *
* R7:      BASE T2SDS DSECT (= OUTPUT AREA)                           *
*                                                                     *
*                                                                     *
* OUTPUT:                                                             *
* =======                                                             *
*                                                                     *
* R0 - R7: SAME AS INPUT                                              *
*                                                                     *
***********************************************************************
         EJECT
*---------------------------------------------------------------------*
*                                                                     *
*        D a t a   M a c r o s   a n d   D S E C T s                  *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
         PUSH  PRINT
         PRINT GEN START
         T2SDS REG=R7              OUTPUT DSECT
         T2SEEQ ,                                               @CONSTA
         COPY  T2SESET                                          @CONSTA
         PRINT GEN END
         POP   PRINT
         EJECT
*---------------------------------------------------------------------*
*                                                                     *
*        M a i n   P r o c e s s i n g                                *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
*@CONSTA STM R1,R7,CE1UF0               SAVE REGS
         STM R1,R7,CE1UR0               SAVE REGS               @CONSTA
         LR R7,R0                       AREA= ADDRESS
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
* Ensure the given parameter is not in the ECB outside the workarea   *
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         N     R0,=X'00FFF000'          round to 4k
         CR    R0,R9                    param within ECB ?
         BNE   A7TONECB                 no ->
         LA    R4,0(,R7)                clear poss top bit
         LA    R3,EBCM03+1              end of work area 1
         CR    R4,R3                    beyond workarea 1 ?
         BL    A7TOK10                  no ->
         LA    R3,EBX000                start of work area 2
         CR    R4,R3                    below workarea 2 ?
         BL    A7TOBAD                  yes -> signal error
         LA    R3,EBXSW7                end of work area 2
         CR    R4,R3                    beyond workarea 2 ?
         BH    A7TOBAD                  yes -> signal error
A7TOK10  LA    R3,EBW000                start of work area 1
         CR    R4,R3                    before workarea 1 ?
         BL    A7TOBAD                  yes -> signal error
         LA    R4,72(,R7)               point to end of area
         LA    R3,EBCM03+1              end of work area 1
         CR    R4,R3                    beyond workarea 1 ?
         BL    A7TONECB                 no ->
         LA    R3,EBX000                start of work area 2
         CR    R4,R3                    below workarea 2 ?
         BL    A7TOBAD                  yes ->
         LA    R3,EBXSW7                end of work area 2
         CR    R4,R3                    beyond workarea 2 ?
         BH    A7TOBAD                  yes ->
A7TONECB EQU   *
         SPACE ,
         LR    R0,R7                    Restore for later UTC check
         LA    R7,0(,R7)                clear possible top bit
         MVC   TODJUD(1),26(R7)         Extended indicator
         #IF   OC,TODCLK,TODCLK,Z       NO INPUT TOD CLOCK TO CONVERT
         STCK  TODCLK                   original UTC
         #EIF
         LM    R2,R3,TODCLK             UTC requested
         SPACE ,
*---------------------------------------------------------------------*
*        Adjust GMT-TOD to Local TOD (SYSPLEX Timer Environment)      *
*---------------------------------------------------------------------*
         SPACE ,
         #IF   LTR,R0,R0,P              LOCAL TIME REQUESTED
         TIMEC DISPLAY,TIMEDIFF,ZONE=(,HOST)  R14->DIFF
         L     R15,0(,R14)              time diff in minutes
         LR    R1,R15
         M     R14,=F'60000000'         make it micro seconds
         SLDL  R14,12                   in TOD format
         SLR   R3,R15                   subtr low order reg
         #IF   12
         BCTR  R2,0                     borrow from high order
         #EIF
               SLR R2,R14               subtr high order reg
         #EIF
               SRDL R2,12               SHIFT OUT MICRO SECONDS
               D R2,=F'8000000'
               LR R5,R2
               SR R4,R4
               D R4,=F'1000000'
               SR R2,R2
               SLDL R2,3                *8
               ALR R3,R5                TOTAL SECONDS
               BC 12,A7TO08             NO CARRY
               LA R2,1(R2)              CARRY ADD 1
A7TO08   EQU   *
               D R2,A7TOSED             R2= SECONDS PAST MIDNIGHT
*                                       R3= DAYS SINCE 1900
               ST R2,TODSEC             SAVE SECONDS THIS DAY
               LA R0,365                DAYS IN 1900
         #IF   CR,R3,L,R0               IT'S NOT A YEAR
               LR R6,R3                 DAYS THIS YEAR
               SR R3,R3                 YEAR = 00
         #ELSE
               SR R3,R0                 MINUS YEAR 1900
               SR R2,R2
               D R2,A7TOLP              R3= LEAP YEARS
*                                       R2= DAYS SINCE LAST LEAP YEAR
               LR R5,R2
               SR R4,R4
               DR R4,R0                 R6= DAYS THIS YEAR
*                                       R7= YEAR SINCE LAST LEAP YEAR
         #IF   CH,R5,EQ,=H'4'           LEAP YEAR 31DEC
               LR R4,R0                 DAYS PER YEAR
               BCTR R5,0                YEAR MINUS 1
         #EIF
               LA R6,1(R4)
               SLL R3,2                 * 4
               AR R3,R5
               LA R3,1(R3)
         #EIF
               LR R4,R3                 SAVE # OF YEARS > 1900
         #DO WHILE=(R4,P,AND,CH,R4,GE,=H'100') YEARS >= 2000?
               SH R4,=H'100'            MINUS 100 YEAR DUE NEED
*                                       ONLY HEX VALUE OF DECIMAL
         #EDO                           DIGITS
               STH R4,TODYEAR           HEX YEAR VALUE
               CVD R3,TODWKA1           YEAR
               SR R4,R4
               LR R14,R4
         #IF   LTR,R5,R3,P              DON'T REDUCE YEAR IF 1900
               BCTR R5,0                YEAR - 1
         #EIF
               LR R15,R5                YEAR - 1
               SRL R15,2                (YEAR - 1)/4
               N R5,A7TON3
               MH R15,=H'5'             * 5
               AR R15,R5
               AR R15,R6                ADD DAYS THIS YEAR
               D R14,A7TOD7             DIVIDE BY 7
               MH R14,=H'9'
               LA R14,A7TOWKD(R14)
               MVC TODWKD,0(R14)        STORE WEEKDAY
               IC R15,TODJUD            EXTENDED INDICATOR
               MVC TODTIME(A7TOFIX),A7TOTIME  MOVE FIXED DATAS
               STC R15,TODJUD           EXTENDED INDICATOR
         #IF   LTR,R3,R3,Z              YEAR 1900
               LA R6,1(,R6)             ADJUST TO START WITH DAY 1
         #EIF
               CVD R6,TODWKA2
               MVC TODJUD+2(2),TODWKA2+6 SAVE JULIAN DAY
               LH R15,TODWKA1+6         GET PACKED YEAR 0YYC
               SRL R15,4                SHIFT OUT PACK ZONE
               STC R15,TODJUD+1         SAVE JULIAN JEAR
               LH R14,TODWKA1+6         PACKED YEAR
               SRDL R14,12              SHIRT OUT YEARS
         #IF   LTR,R14,R14,Z            1900
               LA R14,X'19'             SET TO 'PACKED' 19TH CENTURY
         #ELSE
               LA R14,X'1F'(,R14)       INCR TO PACKED 20NTH CENTURY
         #EIF
               SLDL R14,12              RESTORE YEARS
         #IF   CLI,TODJUD,EQ,C'E'       EXTENDED FORMAT REQUESTED
               ST R14,TODWKA1+4
               OI TODWKA1+7,#BITR       SET PROPER
               OI TODWKA2+7,#BITR         ZONE
               UNPK TODJULC(4),TODWKA1+5(3)  CENTURY/YEAR
               UNPK TODJULY+2(3),TODWKA2+6(2)   DAY
               MVC TODEYAR,TODJULC           CENTURY/YEAR
         #EIF
*---------------------------------------------------------------------*
*        CONVERT JULIAN DATE TO GREGORIAN    (DMKCPI)                 *
*---------------------------------------------------------------------*
         #IF   LTR,R3,R3,NZ,AND,        NOT 1900 AND ...
         #     N,R3,A7TON3,Z            A LEAP YEAR
               LA R4,1
         #EIF
               LA R3,59(R4)
         #IF   CR,R6,H,R3
               AH R6,=H'2'
               SR R6,R4
         #EIF
               AH R6,=H'91'
               LR R15,R6
               SR R14,R14
               M R14,A7TOMP
               D R14,A7TODV
               LR R3,R15
               M R2,A7TODV
               D R2,A7TOMP
               SR R6,R3                 DAY OF THE MONTH
               BCTR R15,0
               BCTR R15,0
               STH R6,TODDAY            DAY OF THE MONTH
               STH R15,TODMTH           MONTH OF THE YEAR
               BCTR R15,0
               MH R15,=H'3'             MONTH ITEM LENGTH
               LA R15,A7TOMON(R15)      POINT TO MONTH
               MVC TODDAT1+2(3),0(R15)  SAVE MONTH
*---------------------------------------------------------------------*
*        CALCULATE TIME OF DAY                                        *
*---------------------------------------------------------------------*
               SR R2,R2
               LR R4,R2
               L R3,TODSEC              SECONDS SINCE LAST MIDNIGHT
               D R2,A7TOMIN             DIVIDE BY 60 = MIN
               LR R6,R2                 REMAINING SECONDS
               SR R2,R2
               D R2,A7TOMIN             DIVIDE BY 60 = HOURS
               CVD R3,TODWKA2
               LH R3,TODWKA2+6          GET PACKED HOURS 0HHC
               SRL R3,4                 SHIFT OUT PACK ZONE
               STC R3,TODTIM1           HOURS
               UNPK TODTIME(2),TODWKA2+6(2) HOURS CHAR
               OI TODTIME+1,X'F0'
               CVD R2,TODWKA2
               LH R2,TODWKA2+6          GET PACKED MIN   0MMC
               SRL R2,4                 SHIFT OUT PACK ZONE
               STC R2,TODTIM1+1         MINUTES
               UNPK TODTIME+3(2),TODWKA2+6(2) MINUTES CHAR
               OI TODTIME+4,X'F0'
               CVD R6,TODWKA2
               LH R6,TODWKA2+6          GET PACKED SEC   0SSC
               SRL R6,4                 SHIFT OUT PACK ZONE
               STC R6,TODTIM1+2         SECONDS
               UNPK TODTIME+6(2),TODWKA2+6(2) SECONDS CHAR
               OI TODTIME+7,X'F0'
               LH R6,TODDAY             DAY OF THE MONTH
               LH R3,TODMTH             MONTH
               CVD R6,TODWKA2
               UNPK TODDATE(2),TODWKA2+6(2)   * DAY
               OI TODDATE+1,X'F0'
               CVD R3,TODWKA2
               UNPK TODDATE+3(2),TODWKA2+6(2) * MONTH
               OI TODDATE+4,X'F0'
               UNPK TODDATE+6(2),TODWKA1+6(2) * YEAR
               OI TODDATE+7,X'F0'
               MVC TODDAT1(2),TODDATE    YEAR
         #IF   CLI,TODJUD,EQ,C'E'        EXTENDED FORMAT REQUESTED
               MVC TODDDMM(2),TODDATE        DAY
               MVC TODDDMM+2(2),TODDATE+3    MONTH
         #EIF
               SPACE 3
               MVI TODJUD,0             CLEAR EXT INDICATION
               LR R0,R7                 FLIP BACK 'AREA=' POINTER
         #LOCA NOCONV
*@CONSTA       LM R1,R7,CE1UF0          RELOAD REGS
               LM R1,R7,CE1UR0          RELOAD REGS             @CONSTA
         BACKC
A7TOBAD  EQU   *
         WHOCC NESTLVL=-1            get calling pgm name
*@CONSTA MVC   CE1URA(4),4(R14)      SAVE CALLERS NAME
         MVC   CE1TRC(4),4(R14)      SAVE CALLERS NAME          @CONSTA
         LR    R0,R7                 restore 'AREA=' pointer
*@CONSTA LM    R1,R3,CE1UF0          RELOAD REGS
         LM    R1,R3,CE1UR0          RELOAD REGS                @CONSTA
*
         SNAPC E,12345,PROG=CE1TRC,MSG=MESS,REGS=YES,ECB=YES
*
         EXITC
MESS     DC    AL1(L'MSGTUF)
MSGTUF   DC    C'INVALID T2SCO MACRO AREA PARAMETER GIVEN'
         EJECT
***********************************************************************
*                                                                     *
*        C O N S T A N T S   A N D   L I T E R A L S                  *
*                                                                     *
***********************************************************************
         SPACE 1
A7TOWKD  DC    CL9'MONDAY'
         DC    CL9'TUESDAY'
         DC    CL9'WEDNESDAY'
         DC    CL9'THURSDAY'
         DC    CL9'FRIDAY'
         DC    CL9'SATURDAY'
         DC    CL9'SUNDAY'
*
A7TOMON  DC    CL3'JAN'
         DC    CL3'FEB'
         DC    CL3'MAR'
         DC    CL3'APR'
         DC    CL3'MAY'
         DC    CL3'JUN'
         DC    CL3'JUL'
         DC    CL3'AUG'
         DC    CL3'SEP'
         DC    CL3'OCT'
         DC    CL3'NOV'
         DC    CL3'DEC'
*
A7TON3   DC    F'3'
A7TOSED  DC    AL4(60*60*24)
A7TOLP   DC    AL4(4*365+1)
A7TOD7   DC    F'7'
A7TODV   DC    F'3055'
A7TOMP   DC    F'100'
A7TOMIN  DC    F'60'
A7TOTIME DC    C'HH.MM.SS'
A7TODAT  DC    C'DD/MM/YY'
A7TOJUL  DC    XL4'00'             00YYDDDF
A7TOTIM  DC    XL4'00'             HHMMSS00
A7TOFIX  EQU   *-A7TOTIME
         SPACE 1
         LTORG
         SPACE 3
         FINIS
         END   ,
