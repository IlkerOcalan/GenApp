//GENASTL  JOB 241901,'ITP TPNS',REGION=7M,NOTIFY=&SYSUID,
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
//STL      PROC
//ITPSTL   EXEC PGM=ITPSTL
//STEPLIB  DD DSN=<WSIMHLQ>.SITPLOAD,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SEQOUT   DD SYSOUT=*
//SYSIN    DD DISP=SHR,DSN=<WSIMWSX>(&MEM)
//SYSLIB   DD DISP=SHR,DSN=<WSIMWSX>
//MSGDD    DD DISP=SHR,DSN=<WSIMMSX>
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(10,10,50))
//         PEND
//#ONCICS  EXEC STL,MEM=#ONCICS
//#SSVARS  EXEC STL,MEM=#SSVARS
//SSC1A1   EXEC STL,MEM=SSC1A1
//SSC1I1   EXEC STL,MEM=SSC1I1
//SSP1A1   EXEC STL,MEM=SSP1A1
//SSP1D1   EXEC STL,MEM=SSP1D1
//SSP1I1   EXEC STL,MEM=SSP1I1
//SSP1U1   EXEC STL,MEM=SSP1U1
//SSP2A1   EXEC STL,MEM=SSP2A1
//SSP2D1   EXEC STL,MEM=SSP2D1
//SSP2I1   EXEC STL,MEM=SSP2I1
//SSP2U1   EXEC STL,MEM=SSP2U1
//SSP3A1   EXEC STL,MEM=SSP3A1
//SSP3D1   EXEC STL,MEM=SSP3D1
//SSP3I1   EXEC STL,MEM=SSP3I1
//SSP3U1   EXEC STL,MEM=SSP3U1
//SSP4A1   EXEC STL,MEM=SSP4A1
//SSP4D1   EXEC STL,MEM=SSP4D1
//SSP4I1   EXEC STL,MEM=SSP4I1
//STOP     EXEC STL,MEM=STOP
//WSC1A1   EXEC STL,MEM=WSC1A1
//WSC1I1   EXEC STL,MEM=WSC1I1
