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
//* ADD POLICY Details
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
 PGMNAME=LGAPOL01
 REQMEM=SOAIPM1
 RESPMEM=SOAIPM1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGAPOLM1.LOG
 URI=GENAPP/LGAPOLM1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGAPOLM1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGAPOLM1.wsdl
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
 PGMNAME=LGAPOL01
 REQMEM=SOAIPH1
 RESPMEM=SOAIPH1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGAPOLH1.LOG
 URI=GENAPP/LGAPOLH1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGAPOLH1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGAPOLH1.wsdl
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
 PGMNAME=LGAPOL01
 REQMEM=SOAIPE1
 RESPMEM=SOAIPE1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGAPOLE1.LOG
 URI=GENAPP/LGAPOLE1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGAPOLE1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGAPOLE1.wsdl
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
 PGMNAME=LGAPOL01
 REQMEM=SOAIPB1
 RESPMEM=SOAIPB1
 LOGFILE=/u/s8smith/genapp/logs/LS2WS_LGAPOLB1.LOG
 URI=GENAPP/LGAPOLB1
 PGMINT=COMMAREA
 WSBIND=/u/s8smith/genapp/wsdir/LGAPOLB1.wsbind
 WSDL=/u/s8smith/genapp/wsdir/LGAPOLB1.wsdl
 HTTPPROXY=PROXY.HURSLEY.IBM.COM:80
/*