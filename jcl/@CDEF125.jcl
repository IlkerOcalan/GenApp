//GENACSD JOB 241901,'CSD GENERATE',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
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
//*****
//***** CSD definitions for Event processing - Transaction throughput
//*****
//CSDDEFS  EXEC PGM=DFHCSDUP,REGION=1M
//STEPLIB  DD DISP=SHR,DSN=DFH550.CICS.SDFHLOAD
//DFHCSD   DD DSN=DFH550.CICS.DFHCSD,DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(1024,(100,100))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
**********************************************************
*******
Add    Group(GENAEVNT)    List(TORLIST)
*******

******Terminal Owning Region definitions **************
***** Program
Define Program(LGASTAT1)      Group(GENAEVNT)
Define Program(LGWEBST5)      Group(GENAEVNT)

***** Transaction
Define Transaction(LGST)      Group(GENAEVNT)
       Program(LGASTAT1)
Define Transaction(SSST)      Group(GENAEVNT)
       Program(LGWEBST5)

***** Bundle for Business events
Define Bundle(GENAEV01)       Group(GENAEVNT)
       BundleDir(/u/s8smith/genapp/genapp)
