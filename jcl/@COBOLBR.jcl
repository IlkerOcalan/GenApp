//GENACOMP JOB 241901,'COB GENERATE',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H,
//       REGION=32M
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
//*-------------------------------------------------------------------*
//HBRPROC  PROC
//*-------------------------------------------------------------------*
//*  INVOKE THE COBOL COMPILE                                         *
//*-------------------------------------------------------------------*
//*
//COBL     EXEC PGM=IGYCRCTL,
// PARM='NODYNAM,LIB,RENT,APOST,LIB,CICS(''SP''),SIZE(4000K)'
//STEPLIB  DD DSN=IGY620.SIGYCOMP,DISP=SHR
//         DD DSN=DFH550.CICS.SDFHLOAD,DISP=SHR
//SYSLIB   DD DSN=DFH550.CICS.SDFHCOB,DISP=SHR
//         DD DSN=.SHBRCOBC,DISP=SHR
//         DD DSN=DFH550.CICS.SDFHMAC,DISP=SHR
//         DD DSN=DFH550.CICS.SDFHSAMP,DISP=SHR
//         DD DSN=IBMUSER.GENAPP.MAPCOPY,DISP=SHR
//         DD DSN=IBMUSER.GENAPP.SOURCE,DISP=SHR
//SYSIN    DD DISP=SHR,DSN=IBMUSER.GENAPP.SOURCE(&MEM)
//SYSLIN   DD DSN=&&LOADSET,DISP=(NEW,PASS),UNIT=SYSDA,
//         SPACE=(TRK,(20,10))
//SYSUT1   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT2   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT3   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT4   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT5   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT6   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT7   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSPRINT DD SYSOUT=*
//*-------------------------------------------------------------------*
//*  REBLOCK DFHEILIA, FOR USE BY THE LINKEDIT STEP                   *
//*-------------------------------------------------------------------*
//*
//COPYLINK EXEC PGM=IEBGENER,COND=(7,LT,COBL)
//SYSUT1   DD DISP=SHR,DSN=DFH550.CICS.SDFHCOB(DFHEILIC)
//SYSUT2   DD DISP=(NEW,PASS),DSN=&&COPYLINK,
//            DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB),
//            UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD DUMMY
//SYSIN    DD DUMMY
//*-------------------------------------------------------------------*
//*  INVOKE THE MVS LINKAGE-EDITOR PROGRAM                            *
//*-------------------------------------------------------------------*
//*
//LKED     EXEC PGM=HEWL,COND=(7,LT,COBL),
//  PARM='LIST,XREF,RENT,NAME=&MEM'
//SYSLIB   DD DISP=SHR,DSN=DFH550.CICS.SDFHLOAD
//         DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DISP=SHR,DSN=IBMUSER.GENAPP.LOAD
//HBRLIB   DD DISP=SHR,DSN=.SHBRCICS
//SYSLMOD  DD DISP=SHR,DSN=IBMUSER.GENAPP.LOAD(&MEM)
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,
//            SPACE=(CYL,(1,1))
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DISP=(OLD,DELETE),DSN=&&COPYLINK
//         DD DISP=(OLD,DELETE),DSN=&&LOADSET
//         DD DISP=SHR,DSN=IBMUSER.GENAPP.SOURCE(LINKHBRP)
//         PEND
//*
//LGAPBR01 EXEC HBRPROC,MEM=LGAPBR01
