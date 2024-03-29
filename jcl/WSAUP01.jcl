//GENASOAP  JOB  ,S8SMITH,CLASS=A,NOTIFY=&SYSUID,
//         MSGCLASS=A,REGION=900M
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
//********************************************************
//* LANGUAGE STRUCTURE TO WSDL CONVERSION ROUTINE
//* Update POLICY Details
//********************************************************
//JOBPROC JCLLIB ORDER=<CICSHLQ>.SDFHINST
//*
//* Motor Policy
//*
//LS2WS     EXEC DFHLS2WS,
//    JAVADIR='java601_bit64_ga/J6.0.1_64',
//    USSDIR='cics660',
//    PATHPREF='',
//    TMPDIR='/tmp',
//    TMPFILE='LS2WS'
//INPUT.SYSUT1 DD *
 PDSLIB=<USRHLQ>.CB12.SOURCE
 LANG=COBOL
 PGMNAME=LGUPOL01
 REQMEM=SOAIPM1
 RESPMEM=SOAIPM1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGUPOLM1.LOG
 URI=GENAPP/LGUPOLM1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGUPOLM1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGUPOLM1.wsdl
 HTTPPROXY=PROXY.HURSLEY.IBM.COM:80
/*
//*
//* House Policy
//*
//LS2WS     EXEC DFHLS2WS,
//    JAVADIR='java601_bit64_ga/J6.0.1_64',
//    USSDIR='cics660',
//    PATHPREF='',
//    TMPDIR='/tmp',
//    TMPFILE='LS2WS'
//INPUT.SYSUT1 DD *
 PDSLIB=<USRHLQ>.CB12.SOURCE
 LANG=COBOL
 PGMNAME=LGUPOL01
 REQMEM=SOAIPH1
 RESPMEM=SOAIPH1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGUPOLH1.LOG
 URI=GENAPP/LGUPOLH1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGUPOLH1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGUPOLH1.wsdl
 HTTPPROXY=PROXY.HURSLEY.IBM.COM:80
/*
//*
//* Endowment Policy
//*
//LS2WS     EXEC DFHLS2WS,
//    JAVADIR='java601_bit64_ga/J6.0.1_64',
//    USSDIR='cics660',
//    PATHPREF='',
//    TMPDIR='/tmp',
//    TMPFILE='LS2WS'
//INPUT.SYSUT1 DD *
 PDSLIB=<USRHLQ>.CB12.SOURCE
 LANG=COBOL
 PGMNAME=LGUPOL01
 REQMEM=SOAIPE1
 RESPMEM=SOAIPE1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGUPOLE1.LOG
 URI=GENAPP/LGUPOLE1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGUPOLE1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGUPOLE1.wsdl
 HTTPPROXY=PROXY.HURSLEY.IBM.COM:80
/*
//*
//* Commercial Policy
//*
//LS2WS     EXEC DFHLS2WS,
//    JAVADIR='java601_bit64_ga/J6.0.1_64',
//    USSDIR='cics660',
//    PATHPREF='',
//    TMPDIR='/tmp',
//    TMPFILE='LS2WS'
//INPUT.SYSUT1 DD *
 PDSLIB=<USRHLQ>.CB12.SOURCE
 LANG=COBOL
 PGMNAME=LGUPOL01
 REQMEM=SOAIPB1
 RESPMEM=SOAIPB1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGUPOLB1.LOG
 URI=GENAPP/LGUPOLB1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGUPOLB1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGUPOLB1.wsdl
 HTTPPROXY=PROXY.HURSLEY.IBM.COM:80
/*
