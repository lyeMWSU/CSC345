      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT      INPUT-FILE
           ASSIGN TO   "TABLE1.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT      OUTPUT-FILE
      *     ASSIGN TO   DISPLAY.
           ASSIGN TO   "P3.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IR-RECORD                   PIC X(80).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(80).
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05  EOD                     PIC XXX.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

       01  SS-SUBSCRIPS.
           05  SS-SUB                  PIC S99.

       01  INPUT-RECORD.
           05  IR-STORE                PIC S99.
               88  VALID-STORE         VALUE 1 THRU 10.
           05  IR-DAY                  PIC S9.
               88  VALID-DAY           VALUE 1 THRU 7.
           05  IR-SALES                PIC S9(3)V99.

       01 DL-DETAIL-LINE.
           05                          PIC().
           05  DL-STORE-NUM            PIC().
           05                          PIC().
           05  DL-SALES-AREA OCCURS 7 TIMES.
               10 DL-SALES             PIC ZZ,ZZZ.99-.
               10 FILLER               PIC XXX.
           05 DL-TOT-STORE             PIC $$$$,$$Z.99.

       01 TL-TOTAL-LINE.
           05                          PIC().
           05  TL-SALES-AREA OCCURS 7 TIMES.
               10 TL-SALES             PIC $Z,ZZZ,ZZZ.99.
               10 FILLER               PIC XX.
           05 TL-TOT-STORE             PIC$$,$$$,$$Z.99.

       01  ST-STORE-TABLE VALUE ZERO.
           05  ST-STORES   OCCURS 10 TIMES.
               10 ST-SALES OCCURS 7 TIMES
                                       PIC X.



      *HEADINGS AREA...
       01  PC-PAGE-CONTROLS.
           05  PC-PAGE-NUM             PIC S99  VALUE 0.
           05  PC-LINES-USED           PIC S99  VALUE 0.
           05  PC-LINES-PER-PAGE       PIC S99  VALUE 27.


       01  HL-HEADING-LINE.
           05                          PIC X(20) VALUE
                                       "TEN TERRIFIC EXCUSES".
           05  FILLER                  PIC X(3).
           05  HL-DATE.
               10 HL-MO                PIC XX/.
               10 HL-DAY               PIC XX/.
               10 HL-YY                PIC XX.
           05  FILLER                  PIC X(18).
           05                          PIC X(4) VALUE "PAGE".
           05  FILLER                  PIC X.
           05  HL-PAGE-NUM             PIC 99.

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 030-OPEN
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
           PERFORM 200-PROCESSING
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
           MOVE    WS-YY TO HL-YY.



       200-PROCESSING.
           PERFORM UNTIL EOD = "YES"
               READ INPUT-FILE INTO INPUT-RECORD
                 AT  END
                   MOVE "YES" TO EOD
      *             PERFORM 700-TOTALS
                 NOT AT END
                   PERFORM 210-PROCESSING
               END-READ
           END-PERFORM.

      *>      PERFORM 300-REPORTS
      *>          VARYING SS-SUB
      *>          FROM 1 BY 1
      *>          UNTIL SS-SUB > 10.


       210-PROCESSING.
           IF VALID-STORE AND VALID-DAY
               ADD IR-SALES TO ST-SALES(IR-STORE, IR-DAY)
           ELSE
               DISPLAY "INVALID".


       850-HEADINGS.
           ADD  1  TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE
                   AFTER  PAGE
      *     WRITE PRINT-RECORD FROM HEADING-TWO
      *             AFTER ADVANCING 3 LINES
      *     WRITE PRINT-RECORD FROM HEADING-THREE
      *             AFTER ADVANCING 1 LINE
           MOVE  SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER 1
           ADD 5 TO PC-LINES-USED.
