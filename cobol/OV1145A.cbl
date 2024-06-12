      ***------------------------------------------------------------***
      * Datum               : (vgl. ClearCase-Historie)
      * MusterVersion       : 3.3 vom 26.11.2009
      * Typ                 : Unterprogramm
      * Sprache             : mcob
      *
      * Anwendung/Subsystem : GO_Partner
      * Komponente          : OVMandatLoeschen
      * Name:               : OV1145A
      *
      ***------------------------------------------------------------***
      *
      * Dokumentation:
      * OK_PartnerMandatLoeschen <OV1145A>
      *      Klasse zum Loeschen des Mandats
      *
      ***------------------------------------------------------------***
      *
      *************************************************
      * Methode           : MANDATLOESCHEN
      * mandatLoeschen
      *
      * Uebergabebereiche : SE3989,
      *                   : SO0726E
      * Verarbeitungsmodul: OV1145A
      * *
      * *************************************************
      * Methode           : MANDATREAKTIVIEREN
      * mandatReaktivieren
      *
      * Uebergabebereiche : SE3989,
      *                   : SO0726E
      * Verarbeitungsmodul: OV1145A
      * *
      * *************************************************
      * Methode           : MANDATARCHIVIEREN
      * mandatReaktivieren
      *
      * Uebergabebereiche : SE3989,
      *                   : SO0726E
      * Verarbeitungsmodul: OV1145A
      * *
      * *************************************************

      ***------------------------------------------------------------***

       IDENTIFICATION DIVISION.
       PROGRAM-ID.     OV1145A.
       AUTHOR.         A. Kasten

       DATE-WRITTEN.   28.11.2011
       DATE-COMPILED.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

      ***------------------------------------------------------------***

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      ***------------------------------------------------------------***
      *  Bereich für Programmstart
      ***------------------------------------------------------------***

       01 WS-START.
          05 WS-EYECATCHER.
             10 FILLER                 PIC  X(30)
                                       USAGE IS DISPLAY
                                 VALUE '**********WS-ANFANG*********'.
             10 FILLER                 PIC  X(08)
                                       USAGE IS DISPLAY
                                       VALUE 'OV1145A'.

      ***------------------------------------------------------------***
      *  Copystrecke fuer allgemeinen Konstantenbereich
      ***------------------------------------------------------------***
           COPY SE4397.

      ***------------------------------------------------------------***
      *  Bereich für allgemeine Variable
      ***------------------------------------------------------------***
       01 WS-BEREICH-ALLG.
          05 WS-METHODE                  PIC X(30)
                                         USAGE IS DISPLAY.
          05 WS-DSTRUKTUR                PIC X(8)
                                         USAGE IS DISPLAY.
          05 WS-DVERSNR                  PIC X(2)
                                         USAGE IS DISPLAY.
          05 WS-DSTRUKTUR-MD5            PIC X(32)
                                         USAGE IS DISPLAY.


      ***------------------------------------------------------------***
      *  Copystrecken
      ***------------------------------------------------------------***

      * Technische Copystrecke fuer Aufruf von BOB-Modulen
           COPY SE3989 REPLACING ==:PRF:== BY ==U1==.

      * Copystrecken zur Initialisierung der Ausgabegabebereiche

      * Copystrecken für den Aufruf von BOB-Modulen

      *  Copystrecken fuer das OD1145A
      *  Methoden AKTIVESMANDATLOESCHEN, ARCHIVIERTESMANDATLOES,
      *           MANDATSHISTLOESCHEN
           COPY SO0726E REPLACING ==:PRF:== BY ==EB01==.
      *  Initialisierung
           COPY SO0726E REPLACING ==:PRF:== BY ==EI01==.

      *  Copystrecken fuer das OD1144A
      *  Methode für MANDATINARCHIVSPEICHER
           COPY SO0733E REPLACING ==:PRF:== BY ==EB02==.
      *  Initialisierung
           COPY SO0733E REPLACING ==:PRF:== BY ==EI02==.

      *  Copystrecken fuer das OD1143A
      *  Methode MANDATZUBVIDHOLEN, MANDATARCHZUBVIDHOLEN
           COPY SO0721A REPLACING ==:PRF:== BY ==AB03==.
           COPY SO0721E REPLACING ==:PRF:== BY ==EB03==.
      *  Initialisierung
           COPY SO0721E REPLACING ==:PRF:== BY ==EI03==.

      ***------------------------------------------------------------***
      ***------------------------------------------------------------***BOBENV
      * * LOESCHSTRUKTUREN FUER BOB-ENVIRONMENT                        *BOBENV
      ***------------------------------------------------------------***BOBENV
           COPY SE3989 REPLACING ==:PRF:== BY ==DL11==.                 BOBENV
           COPY SE4055V2 REPLACING ==:PRF:== BY ==DL12==.               BOBENV
           COPY SE3991   REPLACING ==:PRF:== BY ==DL13==.               BOBENV
           COPY SF0001   REPLACING ==:PRF:== BY ==DL01==.               BOBENV
           COPY SF0005V2 REPLACING ==:PRF:== BY ==DL05==                BOBENV
                                   ==:OCC:== BY ==1==.                  BOBENV
      * * BEREICH FUER ENTSPRECHENDEN MACH-BER                          BOBENV
      ***------------------------------------------------------------***BOBENV
      * * UEBERGABE STRUKTUREN FUER BOB-ENVIRONMENT                    *BOBENV
      ***------------------------------------------------------------***BOBENV
      * * STANDARD UEBERGABEBEREICH MODULDATEN (LOG.NAME ETC)           BOBENV
           COPY SE3989 REPLACING ==:PRF:== BY ==SY11==.                 BOBENV
      * * SAVEAREA des rufenden Pgms (wg.MY-INIT=INTERN)                BOBENV
           COPY SE4055V2 REPLACING ==:PRF:== BY ==SY12==.               BOBENV
      * * Standard-Uebergabebereich (rufendes Programm, Typ)            BOBENV
           COPY SE3991 REPLACING ==:PRF:== BY ==SY13==.                 BOBENV
      * * External-Bereich fuer MaCH                                    BOBENV
      * * EX-M-...                                                      BOBENV
           COPY SE3994.                                                 BOBENV
      * * External-Bereich techn. Daten (TS für Sperrkonzept)           BOBENV
      * * EX-T-...                                                      BOBENV
           COPY SE3995.                                                 BOBENV
      * * External-Bereich für Test                                     BOBENV
           COPY SE3996.                                                 BOBENV
           COPY SF0001   REPLACING ==:PRF:== BY ==SY01==.               BOBENV
           COPY SF0005V2 REPLACING ==:PRF:== BY ==SY05==                BOBENV
                                   ==:OCC:== BY ==1==.                  BOBENV
      * * BEREICH FUER ENTSPRECHENDEN MACH-BER                          BOBENV
      ***------------------------------------------------------------***BOBENV
      * * VARIABLENDEFINITIONEN FUER BOB-ENVIRONMENT                   *BOBENV
      ***------------------------------------------------------------***BOBENV
       77  MY-NAME           PIC X(8)                                   BOBENV
                             VALUE 'OV1145A'.                           BOBENV
       77  MY-SYSTEM         PIC X(4)                                   BOBENV
                             VALUE 'BOB'.                               BOBENV
       77  MY-NAME-LOG       PIC X(30)                                  BOBENV
                             VALUE 'OKMandatLoeschen'.                  BOBENV
       77  MY-NAMESPACE      PIC X(80)                                  BOBENV
                             VALUE 'GO_PARTNER'.                        BOBENV
       77  MY-TYP            PIC X(10)                                  BOBENV
                             VALUE 'UPRO'.                              BOBENV
       01  MY-METHOD         PIC X(30)                                  BOBENV
                             VALUE SPACES.                              BOBENV
       01  MY-CALLER         PIC X(8)                                   BOBENV
                             VALUE SPACES.                              BOBENV
       01  PROGRAM-STATE     PIC X(1)                                   BOBENV
                             VALUE LOW-VALUE.                           BOBENV
           88 INITIAL-STATE  VALUE LOW-VALUE.                           BOBENV
           88 LAST-USED-STATE                                           BOBENV
                             VALUE HIGH-VALUE.                          BOBENV
      ***------------------------------------------------------------***BOBENV
      * * ENDE VARIABLEN FUER BOB-ENVIRONMENT                          *BOBENV
      ***------------------------------------------------------------***BOBENV
      ***------------------------------------------------------------***BOBENV
      * * ANFANG DEFINITIONEN FUER MACH-ENVIRONMENT                    *BOBENV
      ***------------------------------------------------------------***BOBENV
      * * Aufruf-Schalter MaCH                                          BOBENV
       01  SF0005-CALL-FLAG  PIC X(1)                                   BOBENV
                             VALUE LOW-VALUE.                           BOBENV
       88  CALL-SF0005       VALUE LOW-VALUE.                           BOBENV
       88  DONT-CALL-SF0005  VALUE HIGH-VALUE.                          BOBENV
      * * ACHTUNG PETER! Diesen Schalter nur einmal generieren          BOBENV
      * * erforderlich bei MSG-FETCH, MSG-DELETE oder MSG-REPL          BOBENV
      ***------------------------------------------------------------***BOBENV
      * * WS-Arbeitsbereich fuer den MaCH                               BOBENV
      ***------------------------------------------------------------***BOBENV
           COPY SE4038   REPLACING ==:PRF:== BY ==WS-MACH==.            BOBENV
      * * ist nur einmal(!) je Modul zu generieren                      BOBENV
      * * und zu Anfang auch zu initialisieren                          BOBENV
      ***------------------------------------------------------------***BOBENV
      * * ENDE DEFINITIONEN FUER MACH-ENVIRONMENT                      *BOBENV
      ***------------------------------------------------------------***BOBENV
      ***------------------------------------------------------------***LOGGING
      * * ANFANG DEFINITIONEN FUER LOGGING                             *LOGGING
      ***------------------------------------------------------------***LOGGING
       01 K-LOG                  PIC X(30)                              LOGGING
                                 VALUE 'LOG'                            LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-CHECK-LOGGING        PIC X(30)                              LOGGING
                                 VALUE 'CHECK-LOGGING'                  LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-RESET-KEY-STATUS     PIC X(30)                              LOGGING
                                 VALUE 'RESET-KEY-STATUS'               LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-INIT-LOGGING-OPTIONS PIC X(30)                              LOGGING
                                 VALUE 'INIT-LOGGING-OPTIONS'           LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-ALL                  PIC X(8)                               LOGGING
                                 VALUE 'ALL'                            LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-TRACE                PIC X(8)                               LOGGING
                                 VALUE 'TRACE'                          LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-INFO                 PIC X(8)                               LOGGING
                                 VALUE 'INFO'                           LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-WARNING              PIC X(8)                               LOGGING
                                 VALUE 'WARNING'                        LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-ERROR                PIC X(8)                               LOGGING
                                 VALUE 'ERROR'                          LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01 K-CRITICAL             PIC X(8)                               LOGGING
                                 VALUE 'CRITICAL'                       LOGGING
                                 USAGE IS DISPLAY.                      LOGGING
       01  LOG-IND        PIC S9(4) USAGE IS BINARY VALUE 0.            LOGGING
       01  LOG-LEVEL.                                                   LOGGING
           05 LOG-LEVEL-X             PIC X(8)                          LOGGING
                                      USAGE IS DISPLAY                  LOGGING
                                      VALUE HIGH-VALUE.                 LOGGING
           05 LOG-LEVEL-NR            PIC X(1)                          LOGGING
                                      USAGE IS DISPLAY                  LOGGING
                                      VALUE HIGH-VALUE.                 LOGGING
              88 LOGGING-INITIAL      VALUE HIGH-VALUE.                 LOGGING
           05 LOG-STATUS REDEFINES LOG-LEVEL-NR PIC 9(1).               LOGGING
              88 LOGGING-ACTIVE       VALUE 0 THRU 5.                   LOGGING
       01  LOG-LEVEL-88  REDEFINES LOG-LEVEL PIC X(9).                  LOGGING
           88 LOG-LEVEL-ALL           VALUE 'ALL     0'.                LOGGING
           88 LOG-LEVEL-TRACE         VALUE 'TRACE   1'.                LOGGING
           88 LOG-LEVEL-INFO          VALUE 'INFO    2'.                LOGGING
           88 LOG-LEVEL-WARNING       VALUE 'WARNING 3'.                LOGGING
           88 LOG-LEVEL-ERROR         VALUE 'ERROR   4'.                LOGGING
           88 LOG-LEVEL-CRITICAL      VALUE 'CRITICAL5'.                LOGGING
           88 LOG-LEVEL-OFF           VALUE 'OFF     6'.                LOGGING
       01  LOG-OPTS-AENDTS            PIC X(26)                         LOGGING
                                      USAGE IS DISPLAY                  LOGGING
                                      VALUE LOW-VALUE.                  LOGGING
           88 LOG-OPTS-INITIAL        VALUE LOW-VALUE.                  LOGGING
       01  WS-LOG-METHODE             PIC X(30)                         LOGGING
                                      USAGE IS DISPLAY                  LOGGING
                                      VALUE SPACES.                     LOGGING
      ***------------------------------------------------------------***LOGGING
      * * UPRO-NAME DEFINIEREN                                         *LOGGING
      ***------------------------------------------------------------***LOGGING
       01  SF0011                     PIC X(8)                          LOGGING
                                      VALUE 'SF0011'.                   LOGGING
      ***------------------------------------------------------------***LOGGING
      * * Übergabebereich für SF0011 (Logging)                         *LOGGING
      ***------------------------------------------------------------***LOGGING
           COPY SF0011V2 REPLACING ==:PRF:== BY ==LG14==.               LOGGING
      * * Löschbereich für SF0011 (Logging)                            *LOGGING
      ***------------------------------------------------------------***LOGGING
           COPY SF0011V2 REPLACING ==:PRF:== BY ==DL14==.               LOGGING
      * * STANDARD UEBERGABEBEREICH MODULDATEN (LOG.NAME ETC)           LOGGING
           COPY SE3989 REPLACING ==:PRF:== BY ==LG11==.                 LOGGING
      * * Standard-Uebergabebereich (rufendes Programm, Typ)            LOGGING
           COPY SE3991 REPLACING ==:PRF:== BY ==LG13==.                 LOGGING
      ***------------------------------------------------------------***LOGGING
      * * ENDE DEFINITIONEN FUER LOGGING                               *LOGGING
      ***------------------------------------------------------------***LOGGING
       77  OD1145A        PIC X(8)  VALUE 'OD1145A'.                    MCALL
       77  OD1143A        PIC X(8)  VALUE 'OD1143A'.                    MCALL
       77  OD1144A        PIC X(8)  VALUE 'OD1144A'.                    MCALL
      ***------------------------------------------------------------***MUPRODEF
      * * UPRO-NAME DEFINIEREN                                         *MUPRODEF
      ***------------------------------------------------------------***MUPRODEF
       01  SF0005  PIC X(10)  VALUE 'SF0005'.                           MUPRODEF
      ***------------------------------------------------------------***MUPRODEF
      * * UPRO-NAME DEFINIEREN                                         *MUPRODEF
      ***------------------------------------------------------------***MUPRODEF
       01  SF0001  PIC X(10)  VALUE 'SF0001'.                           MUPRODEF
       LINKAGE SECTION.

      * Technische Copystrecke mit Aufrufdaten des rufenden Programms
           COPY SE3989 REPLACING ==:PRF:== BY ==L1==.

      * Dummys für die dynamische Adresszuordnung der Uebergabebereiche
       01 LK-TO-EIN                     PIC X(01) USAGE IS DISPLAY.
       01 LK-TO-AUS                     PIC X(01) USAGE IS DISPLAY.
       01 LK-TO-AKZ                     PIC X(01) USAGE IS DISPLAY.

      * Fachliche Copystrecken fuer Eingabe, Ausgabe und
      * Aenderungskennzeichen
           COPY SO0726E REPLACING ==:PRF:== BY ==E01==.


      ***------------------------------------------------------------***
      ***------------------------------------------------------------***MSG-FETC
      * * DEFINITIONEN FUER MACH                                       *MSG-FETC
      ***------------------------------------------------------------***MSG-FETC
           COPY SF0005V2 REPLACING ==:PRF:== BY ==LK05==                MSG-FETC
                                   ==:OCC:== BY ==2000==.               MSG-FETC
      * * BEREICH FUER NACHRICHTEN BEI $FETCH-MSG                       MSG-FETC
      ***------------------------------------------------------------***MSG-FETC
       PROCEDURE DIVISION
                 USING L13989
                   LK-TO-AUS
                       LK-TO-EIN
                       LK-TO-AKZ.

      ***------------------------------------------------------------***
      * Aufbau des BOB-Environment
      ***------------------------------------------------------------***

      *|-< $BOBENV MY-NAME-LOG='OKMandatLoeschen'
      *    ...     MY-NAMESPACE='GO_Partner'
      *    ...     MY-INIT=FOLGEANMELDUNG
      *    ...     MY-TYP='UPRO'
      *    ...     MY-SYSTEM='BOB'
       BOB-SYSTEM.                                                      BOBENV
           SET ALLES-OK TO TRUE                                         BOBENV
           PERFORM AS010-ANMELDUNG-PGM                                  BOBENV
           IF LOG-OPTS-AENDTS < EX-D-LOG-OPTS-AENDTS                    LOGGING
              PERFORM AS020-CHECK-LOGGING                               LOGGING
           END-IF                                                       LOGGING
           IF LOG-LEVEL-NR < 2                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-TRACE TO LG14-LOG-LEVEL                            LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'PGM-TRACE    '                                              LOG
           MY-CALLER                                                    LOG
           ' called '                                                   LOG
           MY-NAME                                                      LOG
           ' method='                                                   LOG
           MY-METHOD                                                    LOG
           END-IF                                                       LOG
           IF ALLES-OK                                                  BOBENV
              PERFORM STEUER                                            BOBENV
           END-IF                                                       BOBENV
           PERFORM ES990-ABMELDUNG                                      BOBENV
           GOBACK.                                                      BOBENV


       STEUER.

      ***------------------------------------------------------------***
      * Programmsteuerung
      ***------------------------------------------------------------***

      *|-< $MPERFORM A0010-VORINITIALISIERUNG
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'A0010-VORINITIALISIERUNG'                                   LOG
           END-IF                                                       LOG
           PERFORM A0010-VORINITIALISIERUNG                             MPERFORM
           IF ALLES-OK
      *|-<    $MPERFORM D0000-HAUPTVERARBEITUNG
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0000-HAUPTVERARBEITUNG'                                 LOG
              END-IF                                                    LOG
              PERFORM D0000-HAUPTVERARBEITUNG                           MPERFORM
           END-IF

           EXIT.


       A0010-VORINITIALISIERUNG.

      ***------------------------------------------------------------***
      * Allgemeine Vorinitialisierung
      ***------------------------------------------------------------***

           INITIALIZE WS-BEREICH-ALLG

      *    Methoden-Initialisierung
           MOVE L1-MODUL-METHODE TO WS-METHODE
           INSPECT WS-METHODE
           CONVERTING 'abcdefghijklmnopqrstuvwxyz'
           TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           MOVE L13989 TO U13989


      ***------------------------------------------------------------***
      *    Methoden prüfen
      ***------------------------------------------------------------***

           EVALUATE WS-METHODE

      *    ---------------------------------------------------------
      *    Methode MANDATLOESCHEN
      *    ---------------------------------------------------------
           WHEN 'MANDATLOESCHEN'

      *       Bereiche zuordnen
              SET ADDRESS OF E01-SO0726E
                 TO ADDRESS OF LK-TO-EIN

      *|-<    $MPERFORM U0001-VERS-PRUEFUNG-SO0726E
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'U0001-VERS-PRUEFUNG-SO0726E'                             LOG
              END-IF                                                    LOG
              PERFORM U0001-VERS-PRUEFUNG-SO0726E                       MPERFORM

      *    ---------------------------------------------------------
      *    Methode MANDATREAKTIVIEREN
      *    ---------------------------------------------------------
           WHEN 'MANDATREAKTIVIEREN'

      *       Bereiche zuordnen
              SET ADDRESS OF E01-SO0726E
                 TO ADDRESS OF LK-TO-EIN

      *|-<    $MPERFORM U0001-VERS-PRUEFUNG-SO0726E
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'U0001-VERS-PRUEFUNG-SO0726E'                             LOG
              END-IF                                                    LOG
              PERFORM U0001-VERS-PRUEFUNG-SO0726E                       MPERFORM

      *    ---------------------------------------------------------
      *    Methode MANDATARCHIVIEREN
      *    ---------------------------------------------------------
           WHEN 'MANDATARCHIVIEREN'

      *       Bereiche zuordnen
              SET ADDRESS OF E01-SO0726E
                 TO ADDRESS OF LK-TO-EIN

      *|-<    $MPERFORM U0001-VERS-PRUEFUNG-SO0726E
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'U0001-VERS-PRUEFUNG-SO0726E'                             LOG
              END-IF                                                    LOG
              PERFORM U0001-VERS-PRUEFUNG-SO0726E                       MPERFORM

      *    ---------------------------------------------------------
      *    Methode UNGUELTIG
      *    ---------------------------------------------------------
           WHEN OTHER
      *|-<    $MPERFORM F6102-FEHLER-METHODE
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'F6102-FEHLER-METHODE'                                    LOG
              END-IF                                                    LOG
              PERFORM F6102-FEHLER-METHODE                              MPERFORM

           END-EVALUATE

           EXIT.


       D0000-HAUPTVERARBEITUNG.

      ***------------------------------------------------------------***
      * In dieser Section ist die Hauptverarbeitung des Unterprogramms
      * der Version 01 durchzuführen.
      ***------------------------------------------------------------***

           EVALUATE WS-METHODE
      *    ---------------------------------------------------------
      *    Methode MANDATLOESCHEN
      *    ---------------------------------------------------------
           WHEN 'MANDATLOESCHEN'
      *|-<    $MPERFORM D0100-MANDATLOESCHEN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0100-MANDATLOESCHEN'                                    LOG
              END-IF                                                    LOG
              PERFORM D0100-MANDATLOESCHEN                              MPERFORM

      *    ---------------------------------------------------------
      *    Methode MANDATREAKTIVIEREN
      *    ---------------------------------------------------------
           WHEN 'MANDATREAKTIVIEREN'
      *|-<    $MPERFORM D0200-MANDATREAKTIVIEREN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0200-MANDATREAKTIVIEREN'                                LOG
              END-IF                                                    LOG
              PERFORM D0200-MANDATREAKTIVIEREN                          MPERFORM

      *    ---------------------------------------------------------
      *    Methode MANDATARCHIVIEREN
      *    ---------------------------------------------------------
           WHEN 'MANDATARCHIVIEREN'
      *|-<    $MPERFORM D0300-MANDATARCHIVIEREN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0300-MANDATARCHIVIEREN'                                 LOG
              END-IF                                                    LOG
              PERFORM D0300-MANDATARCHIVIEREN                           MPERFORM

      *    -------------------------------------------------------------
      *    Methode UNGUELTIG
      *    -------------------------------------------------------------
           WHEN OTHER
      *|-<    $MPERFORM F6102-FEHLER-METHODE
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'F6102-FEHLER-METHODE'                                    LOG
              END-IF                                                    LOG
              PERFORM F6102-FEHLER-METHODE                              MPERFORM

           END-EVALUATE

           EXIT.


       D0100-MANDATLOESCHEN.
      * **--------------------------------------------------------***
      *  Verarbeitung MANDATLOESCHEN
      * **--------------------------------------------------------***

      *|-< $MPERFORM D0110-AKT-MANDAT-LOESCHEN
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'D0110-AKT-MANDAT-LOESCHEN'                                  LOG
           END-IF                                                       LOG
           PERFORM D0110-AKT-MANDAT-LOESCHEN                            MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM D0120-HIST-MANDAT-LOESCHEN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0120-HIST-MANDAT-LOESCHEN'                              LOG
              END-IF                                                    LOG
              PERFORM D0120-HIST-MANDAT-LOESCHEN                        MPERFORM
           END-IF

           IF ALLES-OK
      *|-<    $MPERFORM D0130-ARCH-MANDAT-LOESCHEN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0130-ARCH-MANDAT-LOESCHEN'                              LOG
              END-IF                                                    LOG
              PERFORM D0130-ARCH-MANDAT-LOESCHEN                        MPERFORM
           END-IF

           EXIT.


       D0110-AKT-MANDAT-LOESCHEN.
      * **--------------------------------------------------------***
      *  Aktuelles Mandat loeschen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0100-AKT-MANDAT-LOESCHEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0100-AKT-MANDAT-LOESCHEN-E'                                LOG
           END-IF                                                       LOG
           PERFORM M0100-AKT-MANDAT-LOESCHEN-E                          MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0100-AKT-MANDAT-LOESCHEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0100-AKT-MANDAT-LOESCHEN-V'                             LOG
              END-IF                                                    LOG
              PERFORM M0100-AKT-MANDAT-LOESCHEN-V                       MPERFORM
      *       not found soll kein Fehler sein
      *|-<    $MPERFORM M0100-AKT-MANDAT-LOESCHEN-F2
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0100-AKT-MANDAT-LOESCHEN-F2'                            LOG
              END-IF                                                    LOG
              PERFORM M0100-AKT-MANDAT-LOESCHEN-F2                      MPERFORM
           END-IF

           EXIT.


       D0120-HIST-MANDAT-LOESCHEN.
      * **--------------------------------------------------------***
      *  Mandats-Historie loeschen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0200-HIST-MANDAT-LOESCHEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0200-HIST-MANDAT-LOESCHEN-E'                               LOG
           END-IF                                                       LOG
           PERFORM M0200-HIST-MANDAT-LOESCHEN-E                         MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0200-HIST-MANDAT-LOESCHEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0200-HIST-MANDAT-LOESCHEN-V'                            LOG
              END-IF                                                    LOG
              PERFORM M0200-HIST-MANDAT-LOESCHEN-V                      MPERFORM
      *       not found soll kein Fehler sein
      *|-<    $MPERFORM M0200-HIST-MANDAT-LOESCHEN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0200-HIST-MANDAT-LOESCHEN-F'                            LOG
              END-IF                                                    LOG
              PERFORM M0200-HIST-MANDAT-LOESCHEN-F                      MPERFORM
           END-IF

           EXIT.


       D0130-ARCH-MANDAT-LOESCHEN.
      * **--------------------------------------------------------***
      *  Archiviertes Mandat loeschen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0300-ARCH-MANDAT-LOESCHEN-E'                               LOG
           END-IF                                                       LOG
           PERFORM M0300-ARCH-MANDAT-LOESCHEN-E                         MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0300-ARCH-MANDAT-LOESCHEN-V'                            LOG
              END-IF                                                    LOG
              PERFORM M0300-ARCH-MANDAT-LOESCHEN-V                      MPERFORM
      *       not found soll kein Fehler sein
      *|-<    $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-F2
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0300-ARCH-MANDAT-LOESCHEN-F2'                           LOG
              END-IF                                                    LOG
              PERFORM M0300-ARCH-MANDAT-LOESCHEN-F2                     MPERFORM
           END-IF

           EXIT.


       D0200-MANDATREAKTIVIEREN.
      * **--------------------------------------------------------***
      *  Verarbeitung MANDATREAKTIVIEREN
      * **--------------------------------------------------------***

      *|-< $MPERFORM D0210-ARCH-MANDAT-HOLEN
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'D0210-ARCH-MANDAT-HOLEN'                                    LOG
           END-IF                                                       LOG
           PERFORM D0210-ARCH-MANDAT-HOLEN                              MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM D0220-AKT-MANDAT-SPEICHERN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0220-AKT-MANDAT-SPEICHERN'                              LOG
              END-IF                                                    LOG
              PERFORM D0220-AKT-MANDAT-SPEICHERN                        MPERFORM

              IF ALLES-OK
      *|-<       $MPERFORM D0230-MANDAT-IN-ARCH-LOESCHEN
                 IF LOG-LEVEL-NR < 1                                    LOG
                    MOVE DL140011V2 TO LG140011V2                       LOG
                    MOVE K-ALL TO LG14-LOG-LEVEL                        LOG
                    MOVE K-LOG TO WS-LOG-METHODE                        LOG
                    PERFORM U9900-CALL-SF0011                           LOG
                 DISPLAY '  '                                           LOG
                 'D0230-MANDAT-IN-ARCH-LOESCHEN'                        LOG
                 END-IF                                                 LOG
                 PERFORM D0230-MANDAT-IN-ARCH-LOESCHEN                  MPERFORM
              END-IF
           END-IF

           EXIT.


       D0210-ARCH-MANDAT-HOLEN.
      * **--------------------------------------------------------***
      *  Archiviertes Mandat holen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0600-MANDATARCHZUBVIDHOLEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0600-MANDATARCHZUBVIDHOLEN-E'                              LOG
           END-IF                                                       LOG
           PERFORM M0600-MANDATARCHZUBVIDHOLEN-E                        MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0600-MANDATARCHZUBVIDHOLEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0600-MANDATARCHZUBVIDHOLEN-V'                           LOG
              END-IF                                                    LOG
              PERFORM M0600-MANDATARCHZUBVIDHOLEN-V                     MPERFORM
      *|-<    $MPERFORM M0600-MANDATARCHZUBVIDHOLEN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0600-MANDATARCHZUBVIDHOLEN-F'                           LOG
              END-IF                                                    LOG
              PERFORM M0600-MANDATARCHZUBVIDHOLEN-F                     MPERFORM
           END-IF

           EXIT.


       D0220-AKT-MANDAT-SPEICHERN.
      * **--------------------------------------------------------***
      *  Archiviertes Mandat im aktuellen Bestand speichern
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0700-MANDAT-AKT-SPEICHERN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0700-MANDAT-AKT-SPEICHERN-E'                               LOG
           END-IF                                                       LOG
           PERFORM M0700-MANDAT-AKT-SPEICHERN-E                         MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0700-MANDAT-AKT-SPEICHERN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0700-MANDAT-AKT-SPEICHERN-V'                            LOG
              END-IF                                                    LOG
              PERFORM M0700-MANDAT-AKT-SPEICHERN-V                      MPERFORM
      *|-<    $MPERFORM M0700-MANDAT-AKT-SPEICHERN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0700-MANDAT-AKT-SPEICHERN-F'                            LOG
              END-IF                                                    LOG
              PERFORM M0700-MANDAT-AKT-SPEICHERN-F                      MPERFORM
           END-IF

           EXIT.


       D0230-MANDAT-IN-ARCH-LOESCHEN.
      * **--------------------------------------------------------***
      *  Archiviertes Mandat loeschen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0300-ARCH-MANDAT-LOESCHEN-E'                               LOG
           END-IF                                                       LOG
           PERFORM M0300-ARCH-MANDAT-LOESCHEN-E                         MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0300-ARCH-MANDAT-LOESCHEN-V'                            LOG
              END-IF                                                    LOG
              PERFORM M0300-ARCH-MANDAT-LOESCHEN-V                      MPERFORM
      *|-<    $MPERFORM M0300-ARCH-MANDAT-LOESCHEN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0300-ARCH-MANDAT-LOESCHEN-F'                            LOG
              END-IF                                                    LOG
              PERFORM M0300-ARCH-MANDAT-LOESCHEN-F                      MPERFORM
           END-IF

           EXIT.


       D0300-MANDATARCHIVIEREN.
      * **--------------------------------------------------------***
      *  Verarbeitung MANDATARCHIVIEREN
      * **--------------------------------------------------------***

      *|-< $MPERFORM D0310-AKT-MANDAT-HOLEN
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'D0310-AKT-MANDAT-HOLEN'                                     LOG
           END-IF                                                       LOG
           PERFORM D0310-AKT-MANDAT-HOLEN                               MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM D0320-MANDAT-IN-ARCH-SPEICHERN
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'D0320-MANDAT-IN-ARCH-SPEICHERN'                          LOG
              END-IF                                                    LOG
              PERFORM D0320-MANDAT-IN-ARCH-SPEICHERN                    MPERFORM

              IF ALLES-OK
      *|-<       $MPERFORM D0330-AKT-MANDAT-LOESCHEN
                 IF LOG-LEVEL-NR < 1                                    LOG
                    MOVE DL140011V2 TO LG140011V2                       LOG
                    MOVE K-ALL TO LG14-LOG-LEVEL                        LOG
                    MOVE K-LOG TO WS-LOG-METHODE                        LOG
                    PERFORM U9900-CALL-SF0011                           LOG
                 DISPLAY '  '                                           LOG
                 'D0330-AKT-MANDAT-LOESCHEN'                            LOG
                 END-IF                                                 LOG
                 PERFORM D0330-AKT-MANDAT-LOESCHEN                      MPERFORM
              END-IF
           END-IF

           EXIT.


       D0310-AKT-MANDAT-HOLEN.
      * **--------------------------------------------------------***
      *  Aktuelles Mandat holen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0400-MANDATZUBVIDHOLEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0400-MANDATZUBVIDHOLEN-E'                                  LOG
           END-IF                                                       LOG
           PERFORM M0400-MANDATZUBVIDHOLEN-E                            MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0400-MANDATZUBVIDHOLEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0400-MANDATZUBVIDHOLEN-V'                               LOG
              END-IF                                                    LOG
              PERFORM M0400-MANDATZUBVIDHOLEN-V                         MPERFORM
      *|-<    $MPERFORM M0400-MANDATZUBVIDHOLEN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0400-MANDATZUBVIDHOLEN-F'                               LOG
              END-IF                                                    LOG
              PERFORM M0400-MANDATZUBVIDHOLEN-F                         MPERFORM
           END-IF

           EXIT.


       D0320-MANDAT-IN-ARCH-SPEICHERN.
      * **--------------------------------------------------------***
      *  Aktuelles Mandat im Archiv speichern
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0500-MANDAT-ARCH-SPEICHERN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0500-MANDAT-ARCH-SPEICHERN-E'                              LOG
           END-IF                                                       LOG
           PERFORM M0500-MANDAT-ARCH-SPEICHERN-E                        MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0500-MANDAT-ARCH-SPEICHERN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0500-MANDAT-ARCH-SPEICHERN-V'                           LOG
              END-IF                                                    LOG
              PERFORM M0500-MANDAT-ARCH-SPEICHERN-V                     MPERFORM
      *|-<    $MPERFORM M0500-MANDAT-ARCH-SPEICHERN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0500-MANDAT-ARCH-SPEICHERN-F'                           LOG
              END-IF                                                    LOG
              PERFORM M0500-MANDAT-ARCH-SPEICHERN-F                     MPERFORM
           END-IF

           EXIT.


       D0330-AKT-MANDAT-LOESCHEN.
      * **--------------------------------------------------------***
      *  Aktuelles Mandat loeschen
      * **--------------------------------------------------------***

      *|-< $MPERFORM M0100-AKT-MANDAT-LOESCHEN-E
           IF LOG-LEVEL-NR < 1                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-ALL TO LG14-LOG-LEVEL                              LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'M0100-AKT-MANDAT-LOESCHEN-E'                                LOG
           END-IF                                                       LOG
           PERFORM M0100-AKT-MANDAT-LOESCHEN-E                          MPERFORM

           IF ALLES-OK
      *|-<    $MPERFORM M0100-AKT-MANDAT-LOESCHEN-V
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0100-AKT-MANDAT-LOESCHEN-V'                             LOG
              END-IF                                                    LOG
              PERFORM M0100-AKT-MANDAT-LOESCHEN-V                       MPERFORM
      *|-<    $MPERFORM M0100-AKT-MANDAT-LOESCHEN-F
              IF LOG-LEVEL-NR < 1                                       LOG
                 MOVE DL140011V2 TO LG140011V2                          LOG
                 MOVE K-ALL TO LG14-LOG-LEVEL                           LOG
                 MOVE K-LOG TO WS-LOG-METHODE                           LOG
                 PERFORM U9900-CALL-SF0011                              LOG
              DISPLAY '  '                                              LOG
              'M0100-AKT-MANDAT-LOESCHEN-F'                             LOG
              END-IF                                                    LOG
              PERFORM M0100-AKT-MANDAT-LOESCHEN-F                       MPERFORM
           END-IF

           EXIT.


      ***------------------------------------------------------------***
      *                U N T E R - S E C T I O N S
      * Sections, die von mehreren Stellen aus aufgerufen
      * werden können.
      ***------------------------------------------------------------***


      ***------------------------------------------------------------***
      * Paragraphen sind wie folgt gegliedert :
      * M0100-Methode   (Steuerung Methoden-Aufruf)
      * M0100-Methode-E (Eingabe)
      * M0100-Methode-V (Verarbeitung)
      * M0100-Methode-F (Fehler)
      * M0100-Methode-A (Ausgabe)
      ***------------------------------------------------------------***

       M0100-AKT-MANDAT-LOESCHEN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI01-SO0726E           TO EB01-SO0726E

           MOVE E01-BANKVERBINDUNGID   TO EB01-BANKVERBINDUNGID

           EXIT.


       M0100-AKT-MANDAT-LOESCHEN-V.
      ***------------------------------------------------------------***
      * Aktives Mandat loeschen
      ***------------------------------------------------------------***

      *|-< $MCALL OD1145A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='AKTIVESMANDATLOESCHEN'
      *    ...    U13989
      *    ...    OMITTED
      *    ...    EB01-SO0726E
      *    ...    OMITTED
      *    /* * $MCALL OD1145A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='AKTIVESMANDATLOESCHEN'                         BSHMDOKU
           MOVE 'AKTIVESMANDATLOESCHEN' TO U1-MODUL-METHODE             MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1145A USING U13989                                    MCALL
                              OMITTED                                   MCALL
                              EB01-SO0726E                              MCALL
                              OMITTED                                   MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0100-AKT-MANDAT-LOESCHEN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


       M0100-AKT-MANDAT-LOESCHEN-F2.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
      *|-<    $MSG-FETCH
      *       ... FUNCTION=2
      *       ... SYNCPOINT=LAST-CALL
      *       ... TEXT-ERMITTELN=N
      *    /* * $MSG-FETCH                                              BSHMDOKU
              MOVE DL05-UEBERGABE         TO SY05-UEBERGABE             MSG-FETC
              MOVE DL113989               TO SY113989                   MSG-FETC
              MOVE DL133991               TO SY133991                   MSG-FETC
              MOVE 'FetchMsg_All'          TO SY11-MODUL-METHODE        MSG-FETC
              SET SY05-K1-LAST-CALL TO TRUE                             MSG-FETC
              SET SY05-KLARTEXT-NICHT-ERMITTELN TO TRUE                 MSG-FETC
              MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER       MSG-FETC
              IF LAST-CALL-MSG-NONE                                     MSG-FETC
                  MOVE ZERO TO SY05-MSG-CTR-FETCHED                     MSG-FETC
              ELSE                                                      MSG-FETC
                  PERFORM FS101-MACH-AUFRUF-FETCH                       MSG-FETC
              END-IF                                                    MSG-FETC

              IF SY05-MSG-CTR-FETCHED = 1 AND
                 LK05-MSG-NUM-X4(1) = '9008'
      *          -------------------------------------------------------
      *          Fehler 'keine weitere Daten gefunden' loeschen
      *          -------------------------------------------------------
      *|-<       $MSG-DELETE
      *          ... FUNCTION=3
      *          ... SYNCPOINT=LAST-CALL
      *          ... MSG-NO-OLD='9008'
      *          ... MSG-SYSTEM-OLD='BOB'
      *    /* * $MSG-DELETE                                             BSHMDOKU
                 MOVE DL05-UEBERGABE         TO SY05-UEBERGABE          MSG-DELE
                 MOVE DL113989               TO SY113989                MSG-DELE
                 MOVE DL133991               TO SY133991                MSG-DELE
                 MOVE 'DelMsg_SyncMsgID'     TO SY11-MODUL-METHODE      MSG-DELE
                 SET SY05-K1-LAST-CALL      TO TRUE                     MSG-DELE
                 MOVE '9008'            TO SY05-K1-MSG-NUM-X4-ALT       MSG-DELE
                 MOVE 'BOB'        TO SY05-K1-MSG-SYSTEM-ALT            MSG-DELE
                 MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER    MSG-DELE
                 IF LAST-CALL-MSG-NONE                                  MSG-DELE
                     MOVE ZERO TO SY05-MSG-CTR-DELETED                  MSG-DELE
                 ELSE                                                   MSG-DELE
                     PERFORM FS100-MACH-AUFRUF                          MSG-DELE
                 END-IF                                                 MSG-DELE
              ELSE
                 SET NOT-ALLES-OK TO TRUE
              END-IF
           END-IF

           EXIT.


       M0200-HIST-MANDAT-LOESCHEN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI01-SO0726E           TO EB01-SO0726E

           MOVE E01-BANKVERBINDUNGID   TO EB01-BANKVERBINDUNGID

           EXIT.


       M0200-HIST-MANDAT-LOESCHEN-V.
      ***------------------------------------------------------------***
      * Historische Mandate loeschen
      ***------------------------------------------------------------***

      *|-< $MCALL OD1145A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='MANDATSHISTLOESCHEN'
      *    ...    U13989
      *    ...    OMITTED
      *    ...    EB01-SO0726E
      *    ...    OMITTED
      *    /* * $MCALL OD1145A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='MANDATSHISTLOESCHEN'                           BSHMDOKU
           MOVE 'MANDATSHISTLOESCHEN' TO U1-MODUL-METHODE               MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1145A USING U13989                                    MCALL
                              OMITTED                                   MCALL
                              EB01-SO0726E                              MCALL
                              OMITTED                                   MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0200-HIST-MANDAT-LOESCHEN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
      *|-<    $MSG-FETCH
      *       ... FUNCTION=2
      *       ... SYNCPOINT=LAST-CALL
      *       ... TEXT-ERMITTELN=N
      *    /* * $MSG-FETCH                                              BSHMDOKU
              MOVE DL05-UEBERGABE         TO SY05-UEBERGABE             MSG-FETC
              MOVE DL113989               TO SY113989                   MSG-FETC
              MOVE DL133991               TO SY133991                   MSG-FETC
              MOVE 'FetchMsg_All'          TO SY11-MODUL-METHODE        MSG-FETC
              SET SY05-K1-LAST-CALL TO TRUE                             MSG-FETC
              SET SY05-KLARTEXT-NICHT-ERMITTELN TO TRUE                 MSG-FETC
              MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER       MSG-FETC
              IF LAST-CALL-MSG-NONE                                     MSG-FETC
                  MOVE ZERO TO SY05-MSG-CTR-FETCHED                     MSG-FETC
              ELSE                                                      MSG-FETC
                  PERFORM FS101-MACH-AUFRUF-FETCH                       MSG-FETC
              END-IF                                                    MSG-FETC

              IF SY05-MSG-CTR-FETCHED = 1 AND
                 LK05-MSG-NUM-X4(1) = '9008'
      *          -------------------------------------------------------
      *          Fehler 'keine weitere Daten gefunden' loeschen
      *          -------------------------------------------------------
      *|-<       $MSG-DELETE
      *          ... FUNCTION=3
      *          ... SYNCPOINT=LAST-CALL
      *          ... MSG-NO-OLD='9008'
      *          ... MSG-SYSTEM-OLD='BOB'
      *    /* * $MSG-DELETE                                             BSHMDOKU
                 MOVE DL05-UEBERGABE         TO SY05-UEBERGABE          MSG-DELE
                 MOVE DL113989               TO SY113989                MSG-DELE
                 MOVE DL133991               TO SY133991                MSG-DELE
                 MOVE 'DelMsg_SyncMsgID'     TO SY11-MODUL-METHODE      MSG-DELE
                 SET SY05-K1-LAST-CALL      TO TRUE                     MSG-DELE
                 MOVE '9008'            TO SY05-K1-MSG-NUM-X4-ALT       MSG-DELE
                 MOVE 'BOB'        TO SY05-K1-MSG-SYSTEM-ALT            MSG-DELE
                 MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER    MSG-DELE
                 IF LAST-CALL-MSG-NONE                                  MSG-DELE
                     MOVE ZERO TO SY05-MSG-CTR-DELETED                  MSG-DELE
                 ELSE                                                   MSG-DELE
                     PERFORM FS100-MACH-AUFRUF                          MSG-DELE
                 END-IF                                                 MSG-DELE
              ELSE
                 SET NOT-ALLES-OK TO TRUE
              END-IF
           END-IF

           EXIT.


       M0300-ARCH-MANDAT-LOESCHEN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI01-SO0726E           TO EB01-SO0726E

           MOVE E01-BANKVERBINDUNGID   TO EB01-BANKVERBINDUNGID

           EXIT.


       M0300-ARCH-MANDAT-LOESCHEN-V.
      ***------------------------------------------------------------***
      * Archiviertes Mandat loeschen
      ***------------------------------------------------------------***

      *|-< $MCALL OD1145A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='ARCHIVIERTESMANDATLOES'
      *    ...    U13989
      *    ...    OMITTED
      *    ...    EB01-SO0726E
      *    ...    OMITTED
      *    /* * $MCALL OD1145A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='ARCHIVIERTESMANDATLOES'                        BSHMDOKU
           MOVE 'ARCHIVIERTESMANDATLOES' TO U1-MODUL-METHODE            MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1145A USING U13989                                    MCALL
                              OMITTED                                   MCALL
                              EB01-SO0726E                              MCALL
                              OMITTED                                   MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0300-ARCH-MANDAT-LOESCHEN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


       M0300-ARCH-MANDAT-LOESCHEN-F2.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
      *|-<    $MSG-FETCH
      *       ... FUNCTION=2
      *       ... SYNCPOINT=LAST-CALL
      *       ... TEXT-ERMITTELN=N
      *    /* * $MSG-FETCH                                              BSHMDOKU
              MOVE DL05-UEBERGABE         TO SY05-UEBERGABE             MSG-FETC
              MOVE DL113989               TO SY113989                   MSG-FETC
              MOVE DL133991               TO SY133991                   MSG-FETC
              MOVE 'FetchMsg_All'          TO SY11-MODUL-METHODE        MSG-FETC
              SET SY05-K1-LAST-CALL TO TRUE                             MSG-FETC
              SET SY05-KLARTEXT-NICHT-ERMITTELN TO TRUE                 MSG-FETC
              MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER       MSG-FETC
              IF LAST-CALL-MSG-NONE                                     MSG-FETC
                  MOVE ZERO TO SY05-MSG-CTR-FETCHED                     MSG-FETC
              ELSE                                                      MSG-FETC
                  PERFORM FS101-MACH-AUFRUF-FETCH                       MSG-FETC
              END-IF                                                    MSG-FETC

              IF SY05-MSG-CTR-FETCHED = 1 AND
                 LK05-MSG-NUM-X4(1) = '9008'
      *          -------------------------------------------------------
      *          Fehler 'keine weitere Daten gefunden' loeschen
      *          -------------------------------------------------------
      *|-<       $MSG-DELETE
      *          ... FUNCTION=3
      *          ... SYNCPOINT=LAST-CALL
      *          ... MSG-NO-OLD='9008'
      *          ... MSG-SYSTEM-OLD='BOB'
      *    /* * $MSG-DELETE                                             BSHMDOKU
                 MOVE DL05-UEBERGABE         TO SY05-UEBERGABE          MSG-DELE
                 MOVE DL113989               TO SY113989                MSG-DELE
                 MOVE DL133991               TO SY133991                MSG-DELE
                 MOVE 'DelMsg_SyncMsgID'     TO SY11-MODUL-METHODE      MSG-DELE
                 SET SY05-K1-LAST-CALL      TO TRUE                     MSG-DELE
                 MOVE '9008'            TO SY05-K1-MSG-NUM-X4-ALT       MSG-DELE
                 MOVE 'BOB'        TO SY05-K1-MSG-SYSTEM-ALT            MSG-DELE
                 MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER    MSG-DELE
                 IF LAST-CALL-MSG-NONE                                  MSG-DELE
                     MOVE ZERO TO SY05-MSG-CTR-DELETED                  MSG-DELE
                 ELSE                                                   MSG-DELE
                     PERFORM FS100-MACH-AUFRUF                          MSG-DELE
                 END-IF                                                 MSG-DELE
              ELSE
                 SET NOT-ALLES-OK TO TRUE
              END-IF
           END-IF

           EXIT.


       M0400-MANDATZUBVIDHOLEN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI03-SO0721E           TO EB03-SO0721E

           MOVE E01-BANKVERBINDUNGID   TO EB03-BANKVERBINDUNGID

           EXIT.


       M0400-MANDATZUBVIDHOLEN-V.
      ***------------------------------------------------------------***
      * Aktives Mandat zu BankverbindungsID holen.
      ***------------------------------------------------------------***

      *|-< $MCALL OD1143A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='MANDATZUBVIDHOLEN'
      *    ...    U13989
      *    ...    AB03-SO0721A
      *    ...    EB03-SO0721E
      *    /* * $MCALL OD1143A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='MANDATZUBVIDHOLEN'                             BSHMDOKU
           MOVE 'MANDATZUBVIDHOLEN' TO U1-MODUL-METHODE                 MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1143A USING U13989                                    MCALL
                              AB03-SO0721A                              MCALL
                              EB03-SO0721E                              MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0400-MANDATZUBVIDHOLEN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


       M0500-MANDAT-ARCH-SPEICHERN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI02-SO0733E             TO EB02-SO0733E

           MOVE AB03-BANKVERBINDUNGID(1) TO EB02-BANKVERBINDUNGID
           MOVE AB03-MANDATREFNR(1)      TO EB02-MANDATREFNR
           MOVE AB03-MANDATSART(1)       TO EB02-MANDATSART
           MOVE AB03-VERWENDUNGSART(1)   TO EB02-VERWENDUNGSART
           MOVE AB03-LETZTVERWEND(1)     TO EB02-VERWENDDAT
           MOVE AB03-VERWENDBARBIS(1)    TO EB02-VERWENDBARBIS
           MOVE AB03-MANDATSSTATUS(1)    TO EB02-MANDATSSTATUS
           MOVE AB03-ABLAGEORT(1)        TO EB02-ABLAGEORT
           MOVE AB03-MANDANT(1)          TO EB02-MANDANT
           MOVE AB03-ANLDAT(1)           TO EB02-ANLDAT
           MOVE AB03-GUELTVONZP(1)       TO EB02-GUELTVONZP
           MOVE AB03-GUELTBISZP(1)       TO EB02-GUELTBISZP
           MOVE AB03-ANLAENDID(1)        TO EB02-ANLAENDID
           MOVE AB03-ANLAENDPG(1)        TO EB02-ANLAENDPG
           MOVE AB03-ANLAENDZP(1)        TO EB02-ANLAENDZP

           EXIT.


       M0500-MANDAT-ARCH-SPEICHERN-V.
      ***------------------------------------------------------------***
      * Mandat in Archiv speichern
      ***------------------------------------------------------------***

      *|-< $MCALL OD1144A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='MANDATINARCHIVSPEICHER'
      *    ...    U13989
      *    ...    OMITTED
      *    ...    EB02-SO0733E
      *    ...    OMITTED
      *    /* * $MCALL OD1144A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='MANDATINARCHIVSPEICHER'                        BSHMDOKU
           MOVE 'MANDATINARCHIVSPEICHER' TO U1-MODUL-METHODE            MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1144A USING U13989                                    MCALL
                              OMITTED                                   MCALL
                              EB02-SO0733E                              MCALL
                              OMITTED                                   MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0500-MANDAT-ARCH-SPEICHERN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


       M0600-MANDATARCHZUBVIDHOLEN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI03-SO0721E           TO EB03-SO0721E

           MOVE E01-BANKVERBINDUNGID   TO EB03-BANKVERBINDUNGID

           EXIT.


       M0600-MANDATARCHZUBVIDHOLEN-V.
      ***------------------------------------------------------------***
      * Aktives Mandat zu BankverbindungsID holen.
      ***------------------------------------------------------------***

      *|-< $MCALL OD1143A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='MANDATARCHZUBVIDHOLEN'
      *    ...    U13989
      *    ...    AB03-SO0721A
      *    ...    EB03-SO0721E
      *    /* * $MCALL OD1143A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='MANDATARCHZUBVIDHOLEN'                         BSHMDOKU
           MOVE 'MANDATARCHZUBVIDHOLEN' TO U1-MODUL-METHODE             MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1143A USING U13989                                    MCALL
                              AB03-SO0721A                              MCALL
                              EB03-SO0721E                              MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0600-MANDATARCHZUBVIDHOLEN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


       M0700-MANDAT-AKT-SPEICHERN-E.
      * **--------------------------------------------------------***
      * Eingabedaten prüfen und zur Weiterverarbeitung übertragen
      * **--------------------------------------------------------***

           MOVE EI02-SO0733E             TO EB02-SO0733E

           MOVE AB03-BANKVERBINDUNGID(1) TO EB02-BANKVERBINDUNGID
           MOVE AB03-MANDATREFNR(1)      TO EB02-MANDATREFNR
           MOVE AB03-MANDATSART(1)       TO EB02-MANDATSART
           MOVE AB03-VERWENDUNGSART(1)   TO EB02-VERWENDUNGSART
           MOVE AB03-LETZTVERWEND(1)     TO EB02-VERWENDDAT
           MOVE AB03-VERWENDBARBIS(1)    TO EB02-VERWENDBARBIS
           MOVE AB03-MANDATSSTATUS(1)    TO EB02-MANDATSSTATUS
           MOVE AB03-ABLAGEORT(1)        TO EB02-ABLAGEORT
           MOVE AB03-MANDANT(1)          TO EB02-MANDANT
           MOVE AB03-ANLDAT(1)           TO EB02-ANLDAT
           MOVE AB03-GUELTVONZP(1)       TO EB02-GUELTVONZP
           MOVE AB03-GUELTBISZP(1)       TO EB02-GUELTBISZP
           MOVE AB03-ANLAENDID(1)        TO EB02-ANLAENDID
           MOVE AB03-ANLAENDPG(1)        TO EB02-ANLAENDPG
           MOVE AB03-ANLAENDZP(1)        TO EB02-ANLAENDZP

           EXIT.


       M0700-MANDAT-AKT-SPEICHERN-V.
      ***------------------------------------------------------------***
      * Mandat in Archiv speichern
      ***------------------------------------------------------------***

      *|-< $MCALL OD1144A
      *    ...    ENTITAET='ODMandat'
      *    ...    METHODE='AKTIVESMANDATSPEICHERN'
      *    ...    U13989
      *    ...    OMITTED
      *    ...    EB02-SO0733E
      *    ...    OMITTED
      *    /* * $MCALL OD1144A                                          BSHMDOKU
      *    /* * ENTITAET='ODMandat'                                     BSHMDOKU
      *    /* * METHODE='AKTIVESMANDATSPEICHERN'                        BSHMDOKU
           MOVE 'AKTIVESMANDATSPEICHERN' TO U1-MODUL-METHODE            MCALL
           MOVE 'ODMandat' TO U1-ZIEL-ENTITAET                          MCALL
           CALL OD1144A USING U13989                                    MCALL
                              OMITTED                                   MCALL
                              EB02-SO0733E                              MCALL
                              OMITTED                                   MCALL
           END-CALL                                                     MCALL

           EXIT.


       M0700-MANDAT-AKT-SPEICHERN-F.
      ***------------------------------------------------------------***
      * Fehlerverarbeitung
      ***------------------------------------------------------------***

           IF LAST-CALL-NOT-OK
              SET NOT-ALLES-OK TO TRUE
           END-IF

           EXIT.


      ***------------------------------------------------------------***
      *                U N T E R - S E C T I O N S
      * Sections, die von mehreren Stellen aus aufgerufen
      * werden koennen.
      * Sections sind wie folgt gegliedert :
      * U0001 - U8999 = interne Verarbeitung
      * U9000 - U9999 = reserviert für technische Verarbeitung
      ***------------------------------------------------------------***

       U0001-VERS-PRUEFUNG-SO0726E.
      * ***--------------------------------------------------------***
      * * Pruefung Datenstrukturname, Versionsnummer und MD5-Hash
      * ***--------------------------------------------------------***

           IF NOT E01-DSTRUKTUR-OK
              OR NOT E01-DVERSNR-OK
              OR NOT E01-DSTRUKTUR-MD5-OK
                 MOVE E01-DSTRUKTUR TO WS-DSTRUKTUR
                 MOVE E01-DVERSNR TO WS-DVERSNR
                 MOVE E01-DSTRUKTUR-MD5 TO WS-DSTRUKTUR-MD5
      *|-<       $MPERFORM F6103-FEHLER-DSTRUKTUR
                 IF LOG-LEVEL-NR < 1                                    LOG
                    MOVE DL140011V2 TO LG140011V2                       LOG
                    MOVE K-ALL TO LG14-LOG-LEVEL                        LOG
                    MOVE K-LOG TO WS-LOG-METHODE                        LOG
                    PERFORM U9900-CALL-SF0011                           LOG
                 DISPLAY '  '                                           LOG
                 'F6103-FEHLER-DSTRUKTUR'                               LOG
                 END-IF                                                 LOG
                 PERFORM F6103-FEHLER-DSTRUKTUR                         MPERFORM
           END-IF

           EXIT.


      ***------------------------------------------------------------***
      *                F E H L E R - S E C T I O N S
      *
      * Sections zur Fehlerverarbeitung.
      ***------------------------------------------------------------***


       F6102-FEHLER-METHODE.
      ***------------------------------------------------------------***
      * Fehler: Falsche Methode
      ***------------------------------------------------------------***

      *|-< $MSG-ADD MSG-TYPE=VALID
      *    ...      MSG-SEVERITY-NEW=CRITICAL
      *    ...      MSG-NO-NEW='6102'
      *    ...      MSG-METHOD-LONG=L1-MODUL-METHODE
      *    ...      MSG-FIELD='L1-MODUL-METHODE'
      *    ...      MSG-TYPE=VALID
      *    ...      MSG-VARIABLE1=L1-MODUL-METHODE
      *    /* * $MSG-ADD                                                BSHMDOKU
           MOVE DL05-UEBERGABE         TO SY05-UEBERGABE                MSG-ADD
           MOVE DL113989               TO SY113989                      MSG-ADD
           MOVE DL133991               TO SY133991                      MSG-ADD
           MOVE 'AddMsg'               TO SY11-MODUL-METHODE            MSG-ADD
           SET SY05-K1-SEVERITY-CRITICAL TO TRUE                        MSG-ADD
           MOVE '6102'            TO SY05-K1-MSG-NUM-X4-NEU             MSG-ADD
           MOVE MY-NAME                TO SY05-K1-MSG-MODUL-NEU         MSG-ADD
           MOVE MY-SYSTEM              TO SY05-K1-MSG-SYSTEM-NEU        MSG-ADD
           MOVE MY-NAME-LOG            TO SY05-K1-MSG-MODUL-LOG         MSG-ADD
           MOVE L1-MODUL-METHODE       TO SY05-K1-MSG-METHODE           MSG-ADD
           MOVE 'L1-MODUL-METHODE'             TO SY05-K1-MSG-FEHLERFELDMSG-ADD
           SET SY05-K1-MSG-TYPE-VALID TO TRUE                           MSG-ADD
           MOVE L1-MODUL-METHODE TO SY05-K1-MSG-VARIABLE(1)             MSG-ADD
           SET SY05-K1-MSG-ECR-NULL  TO TRUE                            MSG-ADD
           MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER          MSG-ADD
           IF LOG-LEVEL-NR < 6                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-CRITICAL TO LG14-LOG-LEVEL                         LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'Fehler / Hinweis-Nr. '                                      LOG
           '6102'                                                       LOG
           ' mit Severity='                                             LOG
           'CRITICAL im MaCH eingestellt.'                              LOG
           DISPLAY '  '                                                 LOG
           'Bitte ggf. BOBERROR-File beachten!'                         LOG
           END-IF                                                       LOG
           PERFORM FS100-MACH-AUFRUF                                    MSG-ADD

           SET NOT-ALLES-OK TO TRUE

           EXIT.


       F6103-FEHLER-DSTRUKTUR.
      ***------------------------------------------------------------***
      * Fehler: Falsches Transferobjekt
      ***------------------------------------------------------------***

      *|-< $MSG-ADD
      *    ... MSG-SEVERITY-NEW=CRITICAL
      *    ... MSG-NO-NEW=6103
      *    ... MSG-METHOD-LONG=L1-MODUL-METHODE
      *    ... MSG-FIELD=WS-DSTRUKTUR
      *    ... MSG-TYPE=VALID
      *    ... MSG-VARIABLE1=WS-DSTRUKTUR
      *    ... MSG-VARIABLE2=WS-DVERSNR
      *    ... MSG-VARIABLE3=WS-DSTRUKTUR-MD5
      *    /* * $MSG-ADD                                                BSHMDOKU
           MOVE DL05-UEBERGABE         TO SY05-UEBERGABE                MSG-ADD
           MOVE DL113989               TO SY113989                      MSG-ADD
           MOVE DL133991               TO SY133991                      MSG-ADD
           MOVE 'AddMsg'               TO SY11-MODUL-METHODE            MSG-ADD
           SET SY05-K1-SEVERITY-CRITICAL TO TRUE                        MSG-ADD
           MOVE 6103            TO SY05-K1-MSG-NUM-X4-NEU               MSG-ADD
           MOVE MY-NAME                TO SY05-K1-MSG-MODUL-NEU         MSG-ADD
           MOVE MY-SYSTEM              TO SY05-K1-MSG-SYSTEM-NEU        MSG-ADD
           MOVE MY-NAME-LOG            TO SY05-K1-MSG-MODUL-LOG         MSG-ADD
           MOVE L1-MODUL-METHODE       TO SY05-K1-MSG-METHODE           MSG-ADD
           MOVE WS-DSTRUKTUR             TO SY05-K1-MSG-FEHLERFELD      MSG-ADD
           SET SY05-K1-MSG-TYPE-VALID TO TRUE                           MSG-ADD
           MOVE WS-DSTRUKTUR TO SY05-K1-MSG-VARIABLE(1)                 MSG-ADD
           MOVE WS-DVERSNR TO SY05-K1-MSG-VARIABLE(2)                   MSG-ADD
           MOVE WS-DSTRUKTUR-MD5 TO SY05-K1-MSG-VARIABLE(3)             MSG-ADD
           SET SY05-K1-MSG-ECR-NULL  TO TRUE                            MSG-ADD
           MOVE MY-NAME                TO SY05-K1-AUFTRAGGEBER          MSG-ADD
           IF LOG-LEVEL-NR < 6                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-CRITICAL TO LG14-LOG-LEVEL                         LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'Fehler / Hinweis-Nr. '                                      LOG
           6103                                                         LOG
           ' mit Severity='                                             LOG
           'CRITICAL im MaCH eingestellt.'                              LOG
           DISPLAY '  '                                                 LOG
           'Bitte ggf. BOBERROR-File beachten!'                         LOG
           END-IF                                                       LOG
           PERFORM FS100-MACH-AUFRUF                                    MSG-ADD

           SET NOT-ALLES-OK       TO TRUE

           EXIT.
      ***------------------------------------------------------------***MACHAEPI
      * * PUZI CODE EXTENTION AREA MACH   Attention                    *MACHAEPI
      * * AUFRUF DES BOB-MACH                                          *MACHAEPI
      ***------------------------------------------------------------***MACHAEPI
       FS100-MACH-AUFRUF.                                               MACHAEPI
           MOVE MY-NAME     TO SY13-AUFRUFPGM                           MACHAEPI
           MOVE MY-NAME-LOG TO SY13-AUFRUFPGM-LOG                       MACHAEPI
      ***------------------------------------------------------------***MUPRO
      * UNTERPROGRAMM SF0005 AUFRUFEN                                  *MUPRO
      ***------------------------------------------------------------***MUPRO
           CALL SF0005 USING SY113989                                   MUPRO
                   SY124055                                             MUPRO
                   SY133991                                             MUPRO
                   SY050005V2                                           MUPRO
           END-CALL                                                     MUPRO
           IF SF-INTERN-NOT-OK                                          MACHAEPI
              SET NOT-ALLES-OK TO TRUE                                  MACHAEPI
           END-IF                                                       MACHAEPI
           INITIALIZE WS-MACH-DATEN                                     MACHAEPI
           EXIT.                                                        MACHAEPI
      ***------------------------------------------------------------***MACHAEPI
      * * MACH LESEZUGRIFF                                             *MACHAEPI
      ***------------------------------------------------------------***MACHAEPI
       FS101-MACH-AUFRUF-FETCH.                                         MACHAEPI
            MOVE MY-NAME     TO SY13-AUFRUFPGM                          MACHAEPI
            MOVE MY-NAME-LOG TO SY13-AUFRUFPGM-LOG                      MACHAEPI
      ***------------------------------------------------------------***MUPRO
      * UNTERPROGRAMM SF0005 AUFRUFEN                                  *MUPRO
      ***------------------------------------------------------------***MUPRO
           CALL SF0005 USING SY113989                                   MUPRO
                   SY124055                                             MUPRO
                   SY133991                                             MUPRO
                   SY050005V2                                           MUPRO
           END-CALL                                                     MUPRO
           SET ADDRESS OF LK050005V2 TO SY05-MSG-AREA-PTR               MACHAEPI
           IF SF-INTERN-NOT-OK                                          MACHAEPI
              SET NOT-ALLES-OK TO TRUE                                  MACHAEPI
           END-IF                                                       MACHAEPI
           INITIALIZE WS-MACH-DATEN                                     MACHAEPI
           EXIT.                                                        MACHAEPI
      ***------------------------------------------------------------***BOBEPI
      * * PUZI CODE EXTENTION AREA    Attention                        *BOBEPI
      * * ANMELDUNG AM BOB-ENVIRONMENT                                 *BOBEPI
      ***------------------------------------------------------------***BOBEPI
       AS010-ANMELDUNG-PGM.                                             BOBEPI
           MOVE EX-M-CURRENT-PGM   TO MY-CALLER                         BOBEPI
           MOVE L1-MODUL-METHODE   TO MY-METHOD                         BOBEPI
           MOVE LENGTH OF SY010001 TO SY01-LGF                          BOBEPI
           MOVE LENGTH OF DL010001 TO DL01-LGF                          BOBEPI
           MOVE DL124055 TO SY124055                                    BOBEPI
           MOVE DL113989 TO SY113989                                    BOBEPI
           MOVE DL133991 TO SY133991                                    BOBEPI
           MOVE DL010001 TO SY010001                                    BOBEPI
           MOVE 'FOLGEANMELDUNG'  TO SY11-MODUL-METHODE                 BOBEPI
           MOVE MY-TYP      TO SY13-AUFRUFPGM-TYP                       BOBEPI
           MOVE MY-NAME     TO SY13-AUFRUFPGM                           BOBEPI
           MOVE MY-NAME-LOG TO SY13-AUFRUFPGM-LOG                       BOBEPI
           MOVE MY-SYSTEM   TO SY01-SYSTEM                              BOBEPI
      ***------------------------------------------------------------***MUPRO
      * UNTERPROGRAMM SF0001 AUFRUFEN                                  *MUPRO
      ***------------------------------------------------------------***MUPRO
           CALL SF0001 USING SY113989                                   MUPRO
                   SY124055                                             MUPRO
                   SY133991                                             MUPRO
                   SY010001                                             MUPRO
           END-CALL                                                     MUPRO
           IF SF-INTERN-NOT-OK                                          BOBEPI
              SET NOT-ALLES-OK TO TRUE                                  BOBEPI
           END-IF                                                       BOBEPI
           EXIT.                                                        BOBEPI
      ***------------------------------------------------------------***BOBEPI
      * * ABMELDUNG VOM BOB-ENVIRONMENT                                *BOBEPI
      ***------------------------------------------------------------***BOBEPI
       ES990-ABMELDUNG.                                                 BOBEPI
           MOVE DL113989    TO SY113989                                 BOBEPI
           MOVE DL133991    TO SY133991                                 BOBEPI
           MOVE DL010001    TO SY010001                                 BOBEPI
           MOVE MY-NAME     TO SY13-AUFRUFPGM                           BOBEPI
           MOVE MY-NAME-LOG TO SY13-AUFRUFPGM-LOG                       BOBEPI
           MOVE 'ABMELDUNG' TO SY11-MODUL-METHODE                       BOBEPI
      ***------------------------------------------------------------***MUPRO
      * UNTERPROGRAMM SF0001 AUFRUFEN                                  *MUPRO
      ***------------------------------------------------------------***MUPRO
           CALL SF0001 USING SY113989                                   MUPRO
                   SY124055                                             MUPRO
                   SY133991                                             MUPRO
                   SY010001                                             MUPRO
           END-CALL                                                     MUPRO
           IF SF-INTERN-NOT-OK                                          BOBEPI
              SET NOT-ALLES-OK TO TRUE                                  BOBEPI
           END-IF                                                       BOBEPI
           IF INITIAL-STATE                                             BOBEPI
              SET LAST-USED-STATE TO TRUE                               BOBEPI
           END-IF                                                       BOBEPI
           IF LOG-LEVEL-NR < 2                                          LOG
              MOVE DL140011V2 TO LG140011V2                             LOG
              MOVE K-TRACE TO LG14-LOG-LEVEL                            LOG
              MOVE K-LOG TO WS-LOG-METHODE                              LOG
              PERFORM U9900-CALL-SF0011                                 LOG
           DISPLAY '  '                                                 LOG
           'PGM-TRACE    '                                              LOG
           ' end of program '                                           LOG
           MY-NAME                                                      LOG
           ' method='                                                   LOG
           MY-METHOD                                                    LOG
           END-IF                                                       LOG
           EXIT.                                                        BOBEPI
      ***------------------------------------------------------------***CALL-SF0
      * * Aufruf SF0011                                                *CALL-SF0
      ***------------------------------------------------------------***CALL-SF0
       U9900-CALL-SF0011.                                               CALL-SF0
           MOVE DL113989 TO LG113989                                    CALL-SF0
           MOVE DL133991 TO LG133991                                    CALL-SF0
           MOVE WS-LOG-METHODE       TO LG11-MODUL-METHODE              CALL-SF0
           MOVE MY-NAME                 TO LG13-AUFRUFPGM               CALL-SF0
           MOVE MY-NAME-LOG     TO LG13-AUFRUFPGM-LOG                   CALL-SF0
           MOVE MY-TYP          TO LG13-AUFRUFPGM-TYP                   CALL-SF0
           MOVE MY-NAMESPACE         TO LG14-LOG-NAMESPACE              CALL-SF0
           CALL SF0011 USING LG113989                                   CALL-SF0
                                         OMITTED                        CALL-SF0
                             LG133991                                   CALL-SF0
                             LG140011V2                                 CALL-SF0
           EXIT.                                                        CALL-SF0
      ***------------------------------------------------------------***CHECK-LO
      * * Prüfung, ob Logging für dieses Programm                      *CHECK-LO
      * * aktiv ist                                                    *CHECK-LO
      ***------------------------------------------------------------***CHECK-LO
       AS020-CHECK-LOGGING.                                             CHECK-LO
           SET LOG-LEVEL-OFF TO TRUE                                    CHECK-LO
           IF EX-D-LOGGING-ACTIVE                                       CHECK-LO
              MOVE DL140011V2 TO LG140011V2                             CHECK-LO
              MOVE K-CHECK-LOGGING TO WS-LOG-METHODE                    CHECK-LO
              PERFORM U9900-CALL-SF0011                                 CHECK-LO
              MOVE LG14-LOG-LEVEL    TO LOG-LEVEL-X                     CHECK-LO
              MOVE LG14-LOG-LEVEL-NR TO LOG-LEVEL-NR                    CHECK-LO
           END-IF                                                       CHECK-LO
           MOVE EX-D-LOG-OPTS-AENDTS TO LOG-OPTS-AENDTS                 CHECK-LO
           EXIT.                                                        CHECK-LO
