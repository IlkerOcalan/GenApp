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

   DELETE <USRHLQ>.<CMASAPPL>.EYUDREP
   SET MAXCC=0
/*
//*----------------------------------------------------------
//* DEFINE NEW WUI SERVER REPOSITORY
//*----------------------------------------------------------
//DEFDREP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DEFINE CLUSTER (                                -
                    NAME( <USRHLQ>.<CMASAPPL>.EYUDREP ) -
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
//             PARM=('CMASNAME=<CMASAPPL>',
//             'DAYLIGHT=N',
//             'TIMEZONE=B',
//             'SYSID=<CMASYSID>',
//             'ZONEOFFSET=0')
//EYUXDPRM  DD *
 WUIAPPLID=<WUIAPPL>
 WUINAME=<WUIAPPL>
 WUISYSID=<WUISYSID>
/*
//STEPLIB   DD  DISP=SHR,DSN=<CPSMHLQ>.SEYUAUTH
//EYUDREP   DD  DISP=SHR,DSN=<USRHLQ>.<CMASAPPL>.EYUDREP
//SYSPRINT  DD  SYSOUT=*
//*
