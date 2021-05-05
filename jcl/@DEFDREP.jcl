//EYUDREP JOB 241901,'CPSM GENERATE',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=H
//*
//* LICENSED MATERIALS - PROPERTY OF IBM
//*
//* "RESTRICTED MATERIALS OF IBM"
//*
//* CB12
//*
//* (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED
//*
//*  US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,
//*  OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE
//*  CONTRACT WITH IBM CORPORATION
//*
//*----------------------------------------------------------
//* DELETE EXISTING DREP
//*----------------------------------------------------------
//DELWREP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *

   DELETE IBMUSER.IYI0CMAS.EYUDREP
   SET MAXCC=0
/*
//*----------------------------------------------------------
//* DEFINE NEW WUI SERVER REPOSITORY
//*----------------------------------------------------------
//DEFDREP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DEFINE CLUSTER (                                -
                    NAME( IBMUSER.IYI0CMAS.EYUDREP ) -
                    STORCLAS( STANDARD )           -
                    RECORDS( 500 3000  )           -
                    CONTROLINTERVALSIZE( 8192 )    -
                    RECSZ(200,6550)                -
                    KEYS(64,0)                     -
                    SHR(2)                         -
                    SPEED                          -
                    REUSE                          -
                    ERASE                          -
                    INDEXED                        -
                  )
/*
//*-----------------------------------------------------------
//DREPINIT EXEC PGM=EYU9XDUT,
//             COND=(8,LT),
//             PARM=('CMASNAME=IYI0CMAS',
//             'DAYLIGHT=N',
//             'TIMEZONE=B',
//             'SYSID=ICMA',
//             'ZONEOFFSET=0')
//EYUXDPRM  DD *
 WUIAPPLID=IYI0WUI
 WUINAME=IYI0WUI
 WUISYSID=IWUI
/*
//STEPLIB   DD  DISP=SHR,DSN=DFH550.CPSM.SEYUAUTH
//EYUDREP   DD  DISP=SHR,DSN=IBMUSER.IYI0CMAS.EYUDREP
//SYSPRINT  DD  SYSOUT=*
//*
