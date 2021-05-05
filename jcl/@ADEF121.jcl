//GENAVSAM JOB 241901,'VSAM FILES',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
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
//DELETE1   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE IBMUSER.GENAPP.KSDSCUST
 IF MAXCC=8            -
 THEN SET MAXCC=0
/*
//DEFINE1   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DEFINE CLUSTER(NAME(IBMUSER.GENAPP.KSDSCUST)-
          INDEXED -
          VOLUME(USER01) -
          CYL(100 30)-
          SHR(3)-
          LOG(UNDO) -
          FREESPACE(10 10)-
           REUSE) -
        DATA(NAME(IBMUSER.GENAPP.KSDSCUST.DATA)-
          KEYS(10 0)-
          RECORDSIZE(225 225) -
          CISZ(8000)) -
       INDEX(NAME(IBMUSER.GENAPP.KSDSCUST.INDEX))
//*
//DELETE2   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE IBMUSER.GENAPP.KSDSPOLY
 IF MAXCC=8            -
 THEN SET MAXCC=0
/*
//DEFINE2   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DEFINE CLUSTER(NAME(IBMUSER.GENAPP.KSDSPOLY)-
          INDEXED -
          VOLUME(USER01) -
          CYL(100 30)-
          SHR(3)-
          LOG(UNDO) -
          FREESPACE(10 10)-
           REUSE) -
        DATA(NAME(IBMUSER.GENAPP.KSDSPOLY.DATA)-
          KEYS(21 0)-
          RECORDSIZE(64 64) -
          CISZ(8000)) -
       INDEX(NAME(IBMUSER.GENAPP.KSDSPOLY.INDEX))
/*
//*----------------------------------------------------------
//CUSTDATA EXEC PGM=IDCAMS,COND=(7,LT,DEFINE1)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO                     -
    INFILE(DDIN)            -
    OUTFILE(DDOUT)
/*
//DDIN     DD DISP=SHR,DSN=IBMUSER.GENAPP.KSDSCUST.SEQ
//DDOUT    DD DISP=SHR,DSN=IBMUSER.GENAPP.KSDSCUST
//*
//POLYDATA EXEC PGM=IDCAMS,COND=(7,LT,DEFINE2)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO                     -
    INFILE(DDIN)            -
    OUTFILE(DDOUT)
/*
//DDIN     DD DISP=SHR,DSN=IBMUSER.GENAPP.KSDSPOLY.SEQ
//DDOUT    DD DISP=SHR,DSN=IBMUSER.GENAPP.KSDSPOLY
//*