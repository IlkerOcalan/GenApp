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
//***** CSD definitions for WEBSERVICES
//*****
//CSDDEFS  EXEC PGM=DFHCSDUP,REGION=1M
//STEPLIB  DD DISP=SHR,DSN=<CICSHLQ>.SDFHLOAD
//DFHCSD   DD DSN=<CSDNAME>,DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(1024,(100,100))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
**********************************************************
*******
Add    Group(GENAWSRV)    List(AORLIST)
*******

******Application Owning Region definitions **************
***** TCPIPSERVICE
Define TCPIPservice(GENATCP1) Group(GENAWSRV)
       Portnumber(4321)       Transaction(CWXN)
       Protocol(HTTP)         URM(NONE)

***** Pipeline
Define Pipeline(GENAPIP1)     Group(GENAWSRV)
       Configfile(/usr/lpp/cicsts/cics660/samples/pipelines/basicsoap11*
 provider.xml)
       Shelf(/var/cicstsge/)  WSdir(/u/s8smith/genapp/wsdir)
