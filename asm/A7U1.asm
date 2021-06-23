         PRINT NOGEN
         BEGIN NAME=A7U1,VERSION=&VV,SHR=NONE,XCL=NONE
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
*---------------------------------------------------------------------*
*        This Product contains "Restricted materials of Datalex       *
*        Netherlands and KLM Royal Dutch Airlines".                   *
*        c COPYRIGHT Datalex/KLM  2003                                *
*        LICENSED MATERIAL   Program property of Datalex/KLM          *
*---------------------------------------------------------------------*
*        Description:                                                 *
*        User exit segment.                                           *
*        Activation: Direct activation from a script                  *
*                                                                     *
*        Following has to be customised based on installation   @CONSTA
*        requirements                                           @CONSTA
*        This segment is adapted to Phillipine Airlines requirements  *
*        It is used to directly activate the SXA package to allow     *
*        the AB entry to iniitialise the appropriate terminal settings*
*---------------------------------------------------------------------*
         SPACE 1
         EJECT ,
         AM0SG REG=R2
         AM0SG REG=R3,SUFFIX=A
         PRINT GEN
         GETCC D2,L0               IMG BLOCK
         L     R2,CE1CR0           LOAD BASE INPUT MESSAGE
         L     R3,CE1CR2
         MVC AM0CCTA+2(3),AM0CCT+2
         SR    R14,R14             PREPARE FOR INSERT
         ICM   R14,B'0011',AM0CCT  MESSAGE SIZE
         SH    R14,=H'2'           REMOVE AMSG CONTROLS
         BCTR  R14,0               PREPARE FOR EXECUTE
EXECUTE  DS    0H
         MVC   AM0TXTA-2(0),AM0TXT  SHIFT COMMAND TO THE LEFT
         EX    R14,EXECUTE
         LA    R14,1(,R14)         RE ADJUST LENGTH
         SPACE 1
         STCM  R14,B'0011',AM0CCTA AND RESTORE NEW LENGTH
         SPACE 1
         FLIPC D2,D0               NEW IMG TO LEVEL 0
         SPACE 1
         RELCC D2
         ENTRC WGR1            RETRIEVE AAA FOR SXA1
         SPACE 1
         ENTDC SXA1                ACTIVATE AB ENTRY PROCESSING
         FINIS ,
         END  ,
