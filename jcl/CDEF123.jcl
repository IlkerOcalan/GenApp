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
//***** CSD definitions for TOR-AOR-DOR configuration
//*****
//CSDDEFS  EXEC PGM=DFHCSDUP,REGION=1M
//STEPLIB  DD DISP=SHR,DSN=<CICSHLQ>.SDFHLOAD
//DFHCSD   DD DSN=<CSDNAME>,DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(1024,(100,100))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
**********************************************************
Remove Group(GENATORT)    List(TORLIST)
Remove Group(GENATORP)    List(TORLIST)
Remove Group(GENAAORP)    List(AORLIST)
Remove Group(GENADORP)    List(DORLIST)
Remove Group(GENADORD)    List(DORLIST)
Remove Group(GENA)        List(TORLIST)
Remove Group(GENA)        List(AORLIST)
Remove Group(GENA)        List(DORLIST)
Delete Group(GENATORT) All
Delete Group(GENATORP) All
Delete Group(GENAAORP) All
Delete Group(GENADORP) All
Delete Group(GENADORD) All
Delete Group(GENA)     All
*
Add    Group(GENATORT)    List(TORLIST)
Add    Group(GENATORP)    List(TORLIST)
Add    Group(GENAAORP)    List(AORLIST)
Add    Group(GENADORP)    List(DORLIST)
Add    Group(GENADORT)    List(DORLIST)
Add    Group(GENADORD)    List(DORLIST)
Add    Group(GENA)        List(TORLIST)
Add    Group(GENA)        List(AORLIST)
Add    Group(GENA)        List(DORLIST)
*******

******Terminal Owning Region definitions *****************
***** Transactions
Define Transaction(SSC1) Group(GENATORT)
       Program(LGTESTC1) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(SSP1) Group(GENATORT)
       Program(LGTESTP1) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(SSP2) Group(GENATORT)
       Program(LGTESTP2) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(SSP3) Group(GENATORT)
       Program(LGTESTP3) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(SSP4) Group(GENATORT)
       Program(LGTESTP4) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(LGSE) Group(GENATORT)
       Program(LGSETUP)  TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(LGCF) Group(GENATORT)
       Program(LGICVS01) TaskDataLoc(Any)   TaskDataKey(User)
Define Transaction(LGPF) Group(GENATORT)
       Program(LGIPVS01) TaskDataLoc(Any)   TaskDataKey(User)

***** Programs
Define Program(LGICVS01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSCI)
Define Program(LGIPVS01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSPI)
Define Program(LGTESTC1) Group(GENATORP)
       Description(Solution Customer Menu)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGTESTP1) Group(GENATORP)
       Description(Solution Motor Policy Menu)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGTESTP2) Group(GENATORP)
       Description(Solution Life Policy Menu)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGTESTP3) Group(GENATORP)
       Description(Solution House Policy Menu)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGTESTP4) Group(GENATORP)
       Description(Solution Commercial Policy Menu)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(SSMAP)    Group(GENATORP)
       Description(BMS Map)
       Language(Cobol)   DataLocation(Any)   Execkey(User)

Define Program(LGACUS01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGAPOL01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGAPBR01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGDPOL01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGICUS01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGIPOL01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGUCUS01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)
Define Program(LGUPOL01) Group(GENATORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)

Define Program(LGSETUP)  Group(GENATORP)
       Description(Initial setup of TSQueue)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGSTSQ)   Group(GENA)
       Description(Message output to TSQueues)
       Language(Cobol)   DataLocation(Any)   Execkey(User)


******Application Owning Region definitions *****************
***** Programs
Define Program(LGACUS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGAPOL01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGAPBR01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGDPOL01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGICUS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGIPOL01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUCUS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUPOL01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)

Define Program(LGACDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSCA)
Define Program(LGACDB02) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSCA)
Define Program(LGAPDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSPA)
Define Program(LGDPDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSPD)
Define Program(LGICDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSCI)
Define Program(LGIPDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSPI)
Define Program(LGUCDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSCI)
Define Program(LGUPDB01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(DSPI)
Define Program(LGACVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSCA)
Define Program(LGAPVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSPA)
Define Program(LGDPVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSPD)
Define Program(LGICVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSCI)
Define Program(LGIPVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSPI)
Define Program(LGUCVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSC1)
Define Program(LGUPVS01) Group(GENAAORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
       Dynamic(Yes)      TransID(VSP1)

******DATA Owning Region definitions *****************
***** Transactions
Define Transaction(DSCA) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSCI) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSC1) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSPA) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSPD) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSPI) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSPU) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(DSP1) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSCA) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSCI) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSC1) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSPA) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSPD) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSPI) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
Define Transaction(VSP1) Group(GENADORT)
       Program(DFHMIRS)  Profile(DFHCICSA)
***** Programs
Define Program(LGACDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGACDB02) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGAPDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGDPDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGICDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGIPDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUCDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUPDB01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGACVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGAPVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGDPVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGICVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGIPVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUCVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)
Define Program(LGUPVS01) Group(GENADORP)
       Language(Cobol)   DataLocation(Any)   Execkey(User)

***** DB2 Attachment
Define DB2Conn(<DB2SSID>)     Group(GENADORD) DB2ID(<DB2SSID>)
       NONtermrel(No)    ResyncMember(No)
       MSGqueue1(CSMT)   StatsQueue(CSMT)
       TCBlimit(400)     ThreadError(Abend)
       AccountRec(Txid)  AuthType(Userid)
       Drollback(Yes)    ThreadLimit(250)
Define DB2Entry(<DB2SSID>LU1) Group(GENADORD) Transid(DS*)
       Authid(<SQLID>)   AccountRec(Txid)
       Plan(GENAONE)     ThreadLimit(250)
Define DB2Entry(<DB2SSID>LU2) Group(GENADORD) Transid(SS*)
       Authid(<SQLID>)   AccountRec(Txid)
       Plan(GENAONE)     ThreadLimit(250)

***** File Definitions
  Define File(KSDSCUST)    Group(GENA)
         DSname(<USRHLQ>.GENAPP.KSDSCUST)
         Recordsize(82)    Keylength(10)
         Status(Enabled)   Opentime(Firstref)
         Add(Yes)          Browse(Yes)        Read(Yes)
         Delete(No)        Update(Yes)
  Define File(KSDSPOLY)    Group(GENA)
         DSname(<USRHLQ>.GENAPP.KSDSPOLY)
         Recordsize(64)    Keylength(21)
         Status(Enabled)   Opentime(Firstref)
         Add(Yes)          Browse(Yes)        Read(Yes)
         Delete(Yes)       Update(Yes)
*
  Define Enqmodel(GENAMOD) Group(GENA)
         Description(GRS for GENAPP)
         ENQscope(GENA)    ENQname(GENACNTL)
*
* Define TSmodel(GENASTSQ) Group(GENA)
*        Poolname(GENA)
*        Prefix(GENA++++)
