      ******************************************************************
      * Author:
      * Date:
      * Purpose:
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
           ASSIGN TO   DISPLAY.
      *     ASSIGN TO   "P3.TXT".
       DATA DIVISION.
       FILE SECTION. FD  INPUT-FILE.
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


       01  INPUT-RECORD.
           05  IR-JOB-NUM              PIC S99.
               88  IR-VALID-JOB        VALUE 1 THRU 11.
           05  IR-EMP-NUM              PIC 9.
               88  IR-VALID-EMP        VALUE 1 THRU 4.
           05  IR-NUM-COMPLETED        PIC S9(4).

       01  DL-DETAIL-LINE.
           05  DL-JOB-NUM              PIC S99.
               88  IR-VALID-JOB        VALUE 1 THRU 11.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  DL-EMP-NUM              PIC 9.
               88  IR-VALID-EMP        VALUE 1 THRU 4.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  DL-NUM-COMPLETED        PIC S9(4).


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
           MOVE IR-JOB-NUM TO DL-JOB-NUM
           MOVE IR-EMP-NUM TO DL-EMP-NUM
           MOVE IR-NUM-COMPLETED TO DL-NUM-COMPLETED

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.




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
