      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM07REPORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INPUT-FILE
           ASSIGN TO "TRNSISAM.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT SORT-FILE
           ASSIGN TO "SORT-WORK".

           SELECT MASTER-FILE
           ASSIGN "VSAM.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS MR-ID
           FILE STATUS IS MASTER-SWITCH.

           SELECT OUTPUT-FILE
           ASSIGN TO "P7REPORT.TXT".
      *     ASSIGN TO DISPLAY.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IR-RECORD                   PIC X(80).

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-NUM                  PIC X(3).
           05                          PIC X(77).

       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05  MR-ID                   PIC X(5).
           05                          PIC X(75).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

       01  INPUT-RECORD.
           05  IR-CUS-NAME             PIC X(25).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  IR-QTY-PCH              PIC 999.
           05  FILLER                  PIC X VALUE SPACES.
           05  IR-PRO-NUM              PIC 999.


      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-CUS-NAME             PIC X(25).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  DL-QTY-PCH              PIC 999.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  DL-PRO-NUM              PIC 999.

       01  DR-RECORD                   VALUE SPACES.
           05  DR-REC                  PIC X(63).
           05  FILLER                  PIC X(5).
           05  DR-MESSAGE              PIC X(10).

      *HEADING AREAS...
       01  PC-PAGE-CONTROLS.
           05  PC-PAGE-NUM             PIC S99  VALUE 0.
           05  PC-LINES-USED           PIC S99  VALUE 0.
           05  PC-LINES-PER-PAGE       PIC S99  VALUE 27.

       01  HL-HEADING-LINE.
           05  HL-DATE.
               10 HL-MO                PIC XX/.
               10 HL-DAY               PIC XX/.
               10 HL-YY                PIC XX.
           05  FILLER                  PIC X(3).
           05  HL-HEAD-TITLE           PIC X(20).
           05  FILLER                  PIC X(5).
           05                          PIC X(4) VALUE "PAGE".
           05  HL-PAGE-NUM             PIC Z9.

       01  SWITCHES.
           05  EOD                     PIC XXX.
           05  IR-SWITCH               PIC XXX VALUE "NO".
           05  MASTER-SWITCH           PIC XX.
           05  OR-SWITCH               PIC XXX VALUE "NO".

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 030-OPEN
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
           PERFORM 200-PROCESSING
           PERFORM 700-TOTALS
           PERFORM 020-CLOSE.

       030-OPEN.
           OPEN INPUT      INPUT-FILE
                OUTPUT     OUTPUT-FILE.

       020-CLOSE.
           CLOSE     INPUT-FILE
                     OUTPUT-FILE
           STOP RUN.

       100-INITIALIZE.
           MOVE "NO"   TO EOD.
           MOVE SPACES TO PRINT-RECORD
           ACCEPT  WS-DATE FROM DATE
           MOVE    WS-MO TO HL-MO
           MOVE    WS-DAY TO HL-DAY
           MOVE    WS-YY TO HL-YY

           .

       200-PROCESSING.
           PERFORM UNTIL EOD = "YES"
               READ INPUT-FILE INTO INPUT-RECORD
                 AT  END
                   MOVE "YES" TO EOD
                 NOT AT END
                   PERFORM 210-PROCESSING
               END-READ
           END-PERFORM.


       210-PROCESSING.
      *>      IF MASTER-SWITCH = "00"
      *>          PERFORM 210-MATCH
      *>      ELSE
      *>          PERFORM 230-NOT-ON-MASTER
      *>      END-IF.

           MOVE IR-CUS-NAME TO DL-CUS-NAME
           MOVE IR-QTY-PCH TO DL-QTY-PCH
           MOVE IR-PRO-NUM TO DL-PRO-NUM


           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.


       400-INPUT.


       410-OUTPUT.

       700-TOTALS.

       850-HEADINGS.
           MOVE "TRANSACTION REPORT" TO HL-HEAD-TITLE
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD
           ADD 5 TO PC-LINES-USED.
