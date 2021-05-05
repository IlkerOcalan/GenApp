//<WUIAPPL> JOB 241901,'CTS4.2',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
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
//GENAPP1  EXEC PGM=DFHSIP,TIME=1440,REGION=0M,MEMLIMIT=4G,
// PARM=('START=INITIAL',SYSIN)
//STEPLIB  DD DISP=SHR,DSN=<CICSHLQ>.SDFHAUTH
//         DD DISP=SHR,DSN=<CPSMHLQ>.SEYUAUTH
//         DD DISP=SHR,DSN=<CEEHLQ>.SCEECICS
//*--------------------------------------------------------
//DFHRPL   DD DISP=SHR,DSN=<CICSHLQ>.SDFHLOAD
//         DD DISP=SHR,DSN=<CPSMHLQ>.SEYULOAD
//         DD DISP=SHR,DSN=<CEEHLQ>.SCEECICS
//         DD DISP=SHR,DSN=<CEEHLQ>.SCEERUN
//         DD DISP=SHR,DSN=<LOADX>
//*------------------------------------------------------------------
//EYUWREP  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.EYUWREP
//EYULOG   DD SYSOUT=*
//EYUWUI   DD *
  TCPIPHTTPHOST(YES)
  TCPIPHOSTNAME(127.0.0.1)
  TCPIPPORT(6345)
  CMCIPORT(6346)
  INACTIVETIMEOUT(3600)
  DEFAULTCONTEXT(GNAPPLEX)
  DEFAULTSCOPE(GNAPPLEX)
  DEFAULTCMASCTXT(<CMASAPPL>)
  AUTOIMPORTDSN(<CPSMHLQ>.SEYUVIEW)
  AUTOIMPORTMEM(EYUEA*)
/*
//EYUPARM  DD *
  CICSPLEX(WUIP<CMASYSID>)
  CMASSYSID(<CMASYSID>)
  MASPLTWAIT(YES)
  MASINITTIME(20)
  MAXHISTRECS(50)
  HISTRECSMSG(10)
  HISTSECS(10)
/*
//*------------------------------------------------------------------
//DFHCXRF  DD SYSOUT=*
//MSGFILE  DD SYSOUT=*
//LOGUSR   DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=136)
//MSGUSR   DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=136)
//PLIMSG   DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=137)
//COUT     DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=137)
//CEEMSG   DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=165)
//CEEOUT   DD SYSOUT=*,DCB=(DSORG=PS,RECFM=V,BLKSIZE=137)
//DFHSNAP  DD SYSOUT=*,OUTLIM=0
//SYSUDUMP DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//PRINTER  DD SYSOUT=*,DCB=BLKSIZE=125
//*-----------------------------------------------------------------
//*        USER SUPPLIED CICS DATASETS
//*-----------------------------------------------------------------
//DFHCSD   DD DISP=SHR,DSN=<CSDNAME>
//DFHDMPA  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHDMPA
//DFHDMPB  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHDMPB
//DFHAUXT  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHAUXT
//DFHBUXT  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHBUXT
//DFHGCD   DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHGCD
//DFHINTRA DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHINTRA
//DFHLCD   DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHLCD
//DFHTEMP  DD DISP=SHR,DSN=<USRHLQ>.<WUIAPPL>.DFHTEMP
//SYSIN    DD *
GRPLIST=(DFHLIST)
APPLID=<WUIAPPL>
SYSIDNT=IWUI
CPSMCONN=WUI
INITPARM=(EYU9VKEC='ENU',EYU9VWAN='ENU1')
TCPIP=YES
PARMERR=IGNORE
GMTEXT='WELCOME TO CICS TS 4.2'
CHKSTSK=CURRENT
CHKSTRM=CURRENT
USSHOME=NONE
SEC=NO
AICONS=YES
PGAIPGM=INACTIVE
EDSALIM=500M,
MXT=600
GMTRAN=CESN,
DFLTUSER=<SQLID>
MN=OFF
MNPER=OFF
MNEXC=OFF
RLS=YES
NCPLDFT=GENA
XCMD=NO
XDCT=NO
XFCT=NO
XJCT=NO
XPCT=NO
XPPT=NO
XPSB=NO
XTST=NO
XRES=NO
XTRAN=NO
STGPROT=YES
TRANISO=YES
CICSSVC=224
SPOOL=YES
STATRCD=ON
STATINT=003000
EDSALIM=900M,
MAXOPENTCBS=400
BMS=FULL
MCT=NO
IRCSTRT=YES
ISC=YES
DB2CONN=NO
