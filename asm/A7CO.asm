           BEGIN NAME=A7CO,VERSION=&VV,AMODE=31,SHR=NONE,XCL=NONE
*          BEGIN NAME=A7CO,VERSION=R1,AMODE=31,SHR=NONE,XCL=NONE
           PRINT GEN
*
**********************COMMENTS************************************
* INITIALISE RCB FOR T2SE (COPY OF AAAA)
*
**********************COMMENTS************************************
*
* REGISTER USAGE :
*     R00 = FACE PARAMETER
*     R03 = RECORD ORDINAL NUMBER
*     R06 = FACE PAREMETER
*     R07 = FACE PAREMETER
**********************DSECTLER*************************************
*
*
******************RECORD'A ULASIP UPDATE ETME**********************
*
           LH       R03,BORD
           LH       R02,EORD
 #DO UNTIL=(R03,GE,R02)
    #IF LEVTA,D4,INUSE
       CRUSA    S0=4                    RELEASE LEVEL D4
    #EIF
       LR       R00,R03                 ORDINAL NUMBER
       SR       R06,R06
       ICM      R06,B'0011',FTYPE       FILE TYPE
       LA       R07,CE1FA4
       ENTRC    FACE
 #EXIF R00,ZERO
       WTOPC TEXT='FACE ERROR',HEADER=NO
 #OREL
       MVC      EBCID4,=X'0000'
 #EXIF FINWC,D4,NOK                     EXIT DO IF FINWC ERROR
       WTOPC TEXT='FIND OR FILE ERROR',HEADER=NO
 #OREL
        L        R04,CE1CR4
        MVC      EBCID4(2),FID          RECORD ID
        LH       R05,CE1CC4             GET LENGTH OF BLOCK
        LA       R06,FID                RECORD ID TO MOVE INTO BLOCK
        LA       R07,2                  JUST RECORD ID - ZERO FILL
        MVCL     R04,R06                INITIALISE RECORD
        FILEC    LEVEL=D4               FILE A RECORD
       #STPR R03,1
 #ELOP
        WTOPC TEXT='DONE!',HEADER=NO
 #EDO
        EXITC
FTYPE      DC    AL2(#RCBRA)           RCB FILE TYPE
FID        DC    C'CO'                 RCB RECORD ID
*BORD       DC    H'0'
*EORD       DC    H'511'
BORD       DC    H'511'
EORD       DC    H'1087'
           LTORG
           FINIS
           END
