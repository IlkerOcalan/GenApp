//CICSSTSQ JOB ,'NAMEDCOUNTER',CLASS=A,MSGCLASS=H
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
//SHARETSQ EXEC PGM=DFHXQMN,
// PARM=('POOLNAME=GENA',
//       'MAXQUEUES=500',
//       'BUFFERS=750')
//STEPLIB  DD   DISP=SHR,DSN=DFH550.CICS.SDFHAUTH
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//
