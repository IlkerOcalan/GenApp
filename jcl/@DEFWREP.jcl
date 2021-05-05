//EYUWREP JOB 241901,'CPSM GENERATE',NOTIFY=&SYSUID,
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
//* DELETE EXISTING WUI SERVER REPOSITORY
//*----------------------------------------------------------
//DELWREP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *

   DELETE IBMUSER.IYI0WUI.EYUWREP
   SET MAXCC=0
/*
//*----------------------------------------------------------
//* DEFINE NEW WUI SERVER REPOSITORY
//*----------------------------------------------------------
//DEFWREP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DEFINE CLUSTER (                                -
                    NAME( IBMUSER.IYI0WUI.EYUWREP ) -
                    VOLUMES(SYSDAV)                -
                    STORCLAS( STANDARD )           -
                    RECORDS( 5000 5000 )           -
                    CONTROLINTERVALSIZE( 8192 )    -
                    SPANNED                        -
                    INDEXED                        -
                    SHAREOPTIONS( 2 )              -
                  )                                -
          DATA    (                                -
                    NAME( IBMUSER.IYI0WUI.EYUWREP.DATA ) -
                    KEYS( 20 20 )                  -
                    RECORDSIZE( 8192 32000 )       -
                  )                                -
          INDEX   (                                -
                    NAME( IBMUSER.IYI0WUI.EYUWREP.INDEX ) -
                  )
/*
