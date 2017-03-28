      ******************************************************************
      * Author: LISA YE
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

       01  INPUT-RECORD.
           05  IR-JOB-NUM              PIC S99.
               88  IR-VALID-JOB        VALUE 1 THRU 11.
           05  IR-EMP-NUM              PIC S9.
               88  IR-VALID-EMP        VALUE 1 THRU 4.
           05  IR-NUM-COMPLETED        PIC S9(4).
           05  IR-NUM-COMPLETED-X      REDEFINES IR-NUM-COMPLETED
                                       PIC X(4).

       01  SS-SUBSCRIPTS.
           05  SS-SUB                  PIC S99.
           05  SS-SUB-JOB              PIC S99.
           05  SS-SUB-EMP              PIC S9.
           05  SS-REC-NUM              PIC S99 VALUE ZEROS.

       01  TC-TOTAL-COUNTS.
           05  TC-ROW-TOTAL            PIC S9(6).
           05  TC-GRAND-TOTAL          PIC S9(8).

       01  JN-JOB-NAME.
           05                          PIC X(16) VALUE "PRESIDENT".
           05                          PIC X(16) VALUE "SECRETARY".
           05                          PIC X(16) VALUE "VICE-PRESIDENT".
           05                          PIC X(16) VALUE "MAID".
           05                          PIC X(16) VALUE "TECH SUPPORT".
           05                          PIC X(16) VALUE "ENGINEER".
           05                          PIC X(16) VALUE "ATTORNEY".
           05                          PIC X(16) VALUE "REPRESENTATIVE".
           05                          PIC X(16) VALUE "DIRECTOR".
           05                          PIC X(16) VALUE "JANITOR".
           05                          PIC X(16) VALUE "SECURITY".

       01  JT-JOB-TABLE REDEFINES JN-JOB-NAME.
           05  JT-JOB OCCURS 11 TIMES  PIC X(16).

      *TABLES...
       01  ST-SUMMARY-TABLE VALUE ZEROS.
           05  ST-JOB-NUM OCCURS 11 TIMES.
               10  ST-EMP-NUM OCCURS 4 TIMES.
                   15 ST-NUM-COMPLETE  PIC 9(6).

       01  ST-SUMMARY-TOTAL.
           05  ST-SUMMARY-TOT          PIC S9(6)V99.

       01  TT-TOTALS-TABLE.
           05  TT-COL-TOT OCCURS 4 TIMES
                                       PIC 9(6).


      *OUTPUT LINE...
       01  DL-DETAIL-LINE.
           05  DL-NUM                  PIC 99.
           05  FILLER                  PIC X(5).
           05  DL-JOB-NAME             PIC X(16).
           05  DL-NUM-COMPLETE OCCURS 4 TIMES
                                       PIC ZZZ,ZZZBB-.
           05  FILLER                  PIC XXX.
           05  DL-ROW-TOTAL            PIC ZZZ,ZZZBB-.

       01  ER-ERROR-REPORT.
           05  ER-STAR                 PIC X.
           05  ER-NUM                  PIC 99-.
           05  FILLER                  PIC X(4).
           05  ER-CONTENTS             PIC X(16).
           05  FILLER                  PIC XXXX.
           05  ER-50                   PIC Z,ZZZ.

       01  TL-TOTALS-LINE.
           05  TL-NUM-COMPLETE OCCURS 4 TIMES
                                       PIC ZZZ,ZZZBB-.

       01  CT-COLUMN-TOTAL.
           05  FILLER                  PIC X(8).
           05  CT-DISPLAY              PIC X(6) VALUE "TOTALS".
           05  FILLER                  PIC X(9).
           05  CT-TOTALS OCCURS 4 TIMES
                                       PIC ZZZ,ZZZBB-.
           05  FILLER                  PIC XXX.
           05  CT-GRAND-TOTALS         PIC ZZZ,ZZZBB-.

       01  SL-STAR-LINE.
           05  FILLER                  PIC X(8).
           05  SL-JOB-NUMBER           PIC XX.
           05  SL-EMPLOYEE-NUMBER      PIC X.
           05  SL-NUMBER-COMPLETED     PIC XXXX.


      *HEADINGS AREA...
       01  PC-PAGE-CONTROLS.
           05  PC-PAGE-NUM             PIC S99  VALUE 0.
           05  PC-LINES-USED           PIC S99  VALUE 0.
           05  PC-LINES-PER-PAGE       PIC S99  VALUE 27.


       01  HL-HEADING-LINE.
           05  HL-DATE.
               10 HL-MO                PIC XX/.
               10 HL-DAY               PIC XX/.
               10 HL-YY                PIC XX.
           05  FILLER                  PIC X(5).
           05                          PIC X(7) VALUE "REPORTS".
           05  FILLER                  PIC X(5).
           05                          PIC X(4) VALUE "PAGE".
           05  FILLER                  PIC X.
           05  HL-PAGE-NUM             PIC Z9.

       01  HEADING-TWO.
           05                          PIC X(3) VALUE "NO.".
           05  FILLER                  PIC X(4) VALUE SPACES.
           05                          PIC X(9) VALUE "JOB NAMES".
           05  FILLER                  PIC X(13) VALUE SPACES.
           05                          PIC X(1) VALUE "1".
           05  FILLER                  PIC X(9) VALUE SPACES.
           05                          PIC X(1) VALUE "2".
           05  FILLER                  PIC X(9) VALUE SPACES.
           05                          PIC X(1) VALUE "3".
           05  FILLER                  PIC X(9) VALUE SPACES.
           05                          PIC X(1) VALUE "4".
           05  FILLER                  PIC X(9) VALUE SPACES.
           05                          PIC X(6) VALUE "TOTALS".

       01  HEADING-THREE.
           05                          PIC X(3) VALUE ALL "_".
           05                          PIC X(4) VALUE SPACES.
           05                          PIC X(9) VALUE ALL "_".
           05                          PIC X(12) VALUE SPACES.
           05                          PIC X(3) VALUE ALL "_".
           05  FILLER                  PIC X(7) VALUE SPACES.
           05                          PIC X(3) VALUE ALL "_".
           05  FILLER                  PIC X(7) VALUE SPACES.
           05                          PIC X(3) VALUE ALL "_".
           05  FILLER                  PIC X(7) VALUE SPACES.
           05                          PIC X(3) VALUE ALL "_".
           05  FILLER                  PIC X(7) VALUE SPACES.
           05                          PIC X(8) VALUE ALL "_".

       01  HEADING-FOUR.
           05  FILLER                  PIC X.
           05                          PIC X(3) VALUE "NO.".
           05  FILLER                  PIC X(4) VALUE SPACES.
           05                          PIC X(10) VALUE "CONTENTS".
           05  FILLER                  PIC X(10) VALUE SPACES.
           05                          PIC X(5) VALUE "ABOVE".

       01  HEADING-FIVE.
           05                          PIC X.
           05                          PIC X(3) VALUE ALL "_".
           05                          PIC X(4).
           05                          PIC X(16) VALUE ALL "_".
           05                          PIC X(4).
           05                          PIC X(5) VALUE ALL "_".

       01  HEADING-TITLE.
           05                          PIC X(14) VALUE "SUMMARY REPORT".

       01  HEADING-TITLE-TWO.
           05                          PIC X(20) VALUE
                                       "EXCEPTION REPORT".


       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 030-OPEN
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
           PERFORM 240-EXCEPTION-HEADINGS
           PERFORM 200-PROCESSING
           PERFORM 320-SUMMARY-HEADINGS
           PERFORM 300-UNLOAD
               VARYING SS-SUB-JOB
               FROM 1 BY 1
               UNTIL SS-SUB-JOB > 11
           PERFORM 710-COL-TOTALS
               VARYING SS-SUB
               FROM 1 BY 1
               UNTIL SS-SUB > 4
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

           INITIALIZE TC-TOTAL-COUNTS
           INITIALIZE SS-SUBSCRIPTS.


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
           ADD 1 TO SS-REC-NUM

           IF IR-VALID-JOB AND IR-JOB-NUM NUMERIC
               AND IR-VALID-EMP AND IR-EMP-NUM NUMERIC
               AND IR-NUM-COMPLETED > 0
               AND IR-NUM-COMPLETED NUMERIC
                   PERFORM 230-VALID
           ELSE
               PERFORM 220-EXCEPTION
           END-IF.


       220-EXCEPTION.
           INITIALIZE SL-STAR-LINE
           MOVE SPACES TO ER-STAR
           MOVE ZEROS TO ER-50
           MOVE SS-REC-NUM TO ER-NUM
           MOVE IR-RECORD TO ER-CONTENTS

           IF IR-VALID-JOB AND IR-JOB-NUM NUMERIC
               MOVE SPACES TO SL-JOB-NUMBER
           ELSE
               MOVE ALL "*" TO SL-JOB-NUMBER
           END-IF
           IF IR-VALID-EMP AND IR-EMP-NUM NUMERIC
               MOVE SPACES TO SL-EMPLOYEE-NUMBER
           ELSE
               MOVE ALL "*" TO SL-EMPLOYEE-NUMBER
           END-IF

           IF IR-NUM-COMPLETED NUMERIC AND IR-NUM-COMPLETED > 0
               MOVE SPACES TO SL-NUMBER-COMPLETED
               IF IR-NUM-COMPLETED > 50
                   MOVE ALL "*" TO ER-STAR
                   MOVE IR-NUM-COMPLETED TO ER-50
               END-IF
           END-IF.

           WRITE PRINT-RECORD FROM ER-ERROR-REPORT AFTER 2
           WRITE PRINT-RECORD FROM SL-STAR-LINE AFTER 1.

       230-VALID.
           IF IR-JOB-NUM IS NUMERIC
               AND IR-EMP-NUM IS NUMERIC
                   ADD IR-NUM-COMPLETED TO
                   ST-NUM-COMPLETE(IR-JOB-NUM, IR-EMP-NUM)
           END-IF.

       240-EXCEPTION-HEADINGS.
           WRITE PRINT-RECORD FROM HEADING-TITLE-TWO AFTER 1
           WRITE PRINT-RECORD FROM HEADING-FOUR AFTER 3.
           WRITE PRINT-RECORD FROM HEADING-FIVE AFTER 1.

       300-UNLOAD.
           PERFORM 310-INNER-LOOP
               VARYING SS-SUB-EMP
               FROM 1 BY 1
               UNTIL SS-SUB-EMP > 4.

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.

       310-INNER-LOOP.
           MOVE SS-SUB-JOB TO DL-NUM.
           MOVE JT-JOB(SS-SUB-JOB) TO DL-JOB-NAME.
           MOVE ST-NUM-COMPLETE(SS-SUB-JOB, SS-SUB-EMP) TO
               DL-NUM-COMPLETE(SS-SUB-EMP)
           ADD ST-NUM-COMPLETE(SS-SUB-JOB, SS-SUB-EMP) TO
               TC-ROW-TOTAL.
           MOVE TC-ROW-TOTAL TO DL-ROW-TOTAL
           ADD ST-NUM-COMPLETE(SS-SUB-JOB, SS-SUB-EMP) TO
               TC-GRAND-TOTAL.

       320-SUMMARY-HEADINGS.
           WRITE PRINT-RECORD FROM HEADING-TITLE AFTER 5
           WRITE PRINT-RECORD FROM HEADING-TWO
                   AFTER ADVANCING 3 LINES
           WRITE PRINT-RECORD FROM HEADING-THREE
                   AFTER ADVANCING 1 LINE.

       700-TOTALS.
           MOVE TC-GRAND-TOTAL TO CT-GRAND-TOTALS
           WRITE PRINT-RECORD FROM CT-COLUMN-TOTAL AFTER 1
           ADD 1 TO PC-LINES-USED
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD FROM TL-TOTALS-LINE AFTER 2.


       710-COL-TOTALS.
           PERFORM VARYING SS-SUB-JOB
               FROM 1 BY 1
               UNTIL SS-SUB-JOB > 11
                   ADD ST-NUM-COMPLETE(SS-SUB-JOB,SS-SUB) TO
                   TT-COL-TOT(SS-SUB)
                   MOVE TT-COL-TOT(SS-SUB) TO CT-TOTALS(SS-SUB)
           END-PERFORM.

       850-HEADINGS.
           ADD  1  TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE
                   AFTER  PAGE
           MOVE  SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER 1
           ADD 5 TO PC-LINES-USED.
