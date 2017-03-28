      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT      INPUT-FILE
           ASSIGN TO   "JOBIND.txt"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT      OUTPUT-FILE
      *     ASSIGN TO   DISPLAY.
           ASSIGN TO   "P4.TXT".
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
           05  IR-JOB                  PIC XX.
           05  IR-EMP                  PIC X.
           05  IR-NUM-COMPLETED        PIC S9(4).
           05  FILLER                  PIC X(7).
           05  IR-REC-NUM              PIC 99.

       01  TT-TABLE.
           05  PIC X(38)               VALUE
           "ALALABAMA     A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "BLBLACK LAGOONA00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "DNDRICKBURY STA00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "CACALIFORNIA  A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "FLFLORIDA     A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "HLHIGHLAND    A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "JUJUJULAND    A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "KCKUKU LAND   A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "LALALA LAND   A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "MIMIAMI       A00000D00000M00000T00000".
           05  PIC X(38)               VALUE
           "NYNEW YORK    A00000D00000M00000T00000".

       01  TT-TABLE-X REDEFINES TT-TABLE.
           05 TT-ITEM OCCURS 11 TIMES INDEXED BY ITEM-NDX.
               10  TT-JOB                  PIC XX.
               10  TT-LOC                  PIC X(12).
               10  TT-NUM-COMPLETED OCCURS 4 TIMES INDEXED COM-NDX.
                   15  TT-EMP-TYPE         PIC X.
                   15  TT-NUM-COM          PIC 99999.

       01  TT-TOTAL-TABLE.
           05  TT-COL-TOTALS OCCURS 4  PIC 9(6).

       01  ST-SUMMARY-TABLE VALUE ZEROS.
           05  ST-JOB-NUM OCCURS 11 TIMES.
               10  ST-EMP-NUM OCCURS 4 TIMES.
                   15 ST-NUM-COMPLETE  PIC 9(6).

       01  ST-SUMMARY-TOTAL.
           05  ST-SUMMARY-TOT          PIC S9(6)V99.

       01  TC-TOTAL-COUNTS.
           05  TC-ROW-TOTAL            PIC S9(6).
           05  TC-GRAND-TOTAL          PIC S9(8).

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-JOB                  PIC XX.
           05  FILLER                  PIC X(5).
           05  DL-LOC                  PIC X(12).
           05  FILLER                  PIC X(5).
           05  DL-NUM-COMPLETED OCCURS 4
                                       PIC ZZZ,ZZZBB-.
           05  FILLER                  PIC X(5).
           05  DL-ROW-TOTAL            PIC ZZZ,ZZZBB-.

       01  ER-EXCEPTION-REPORT.
           05  ER-STAR                 PIC X.
           05  ER-NUM                  PIC 99.
           05  FILLER                  PIC X(5).
           05  ER-CONTENTS             PIC X(16).
           05  FILLER                  PIC X(5).
           05  ER-50                   PIC 999.

       01  CT-COLUMN-TOTAL.
           05  FILLER                  PIC X(17).
           05                          PIC X(6) VALUE "TOTALS".
           05  CT-TOTALS OCCURS 4      PIC -ZZZ,ZZ9BB.
           05  FILLER                  PIC X(3).
           05  CT-GRAND-TOTALS         PIC ZZ,ZZZ,ZZ9-.

       01  SL-STAR-LINE.
           05                          PIC X.
           05  SL-REC-NUM              PIC XX.
           05  FILLER                  PIC X(5).
           05  SL-INPUT-REC.
               10  SL-JOB              PIC XX.
               10  SL-EMPLOYEE-TITLE   PIC X.
               10  SL-NUMBER-COMPLETED PIC XXXX.
           05  FILLER                  PIC X(14).
           05  SL-50                   PIC XXX.

       01  TL-TOTALS-LINE.
           05  TL-NUM-COMPLETE OCCURS 4 TIMES
                                       PIC ZZZ,ZZZBB-.

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
           PERFORM 250-EXCEPTION-HEADINGS
           PERFORM 200-PROCESSING
           PERFORM 320-SUMMARY-HEADINGS
           PERFORM 300-UNLOAD
               VARYING ITEM-NDX
               FROM 1 BY 1
               UNTIL ITEM-NDX > 11
           PERFORM 710-COL-TOTALS
               VARYING COM-NDX
               FROM 1 BY 1
               UNTIL COM-NDX > 4
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
           INITIALIZE SL-STAR-LINE
           MOVE SPACES TO ER-STAR
           MOVE ZEROS TO ER-50.


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

           IF IR-NUM-COMPLETED > 0 AND IR-NUM-COMPLETED NUMERIC
               IF IR-NUM-COMPLETED > 50
                   MOVE IR-REC-NUM TO ER-NUM
                   MOVE IR-RECORD TO ER-CONTENTS
                   MOVE IR-NUM-COMPLETED TO ER-50
                   MOVE ALL "*" TO SL-50
               END-IF
               PERFORM 240-TABLE
           ELSE
               MOVE ALL "*" TO SL-NUMBER-COMPLETED
               MOVE IR-REC-NUM TO ER-NUM
               MOVE IR-RECORD TO ER-CONTENTS

               WRITE PRINT-RECORD FROM ER-EXCEPTION-REPORT AFTER 2
               WRITE PRINT-RECORD FROM SL-STAR-LINE AFTER 1
           END-IF
      *TOTALS
           IF PC-LINES-USED > PC-LINES-PER-PAGE
               MOVE ZEROS TO PC-LINES-USED
           END-IF.

       220-EXCEPTION.

           MOVE IR-RECORD TO ER-CONTENTS

           IF IR-JOB ALPHABETIC
               MOVE SPACES TO SL-JOB
           ELSE
               MOVE ALL "*" TO SL-JOB
           END-IF
           IF IR-EMP ALPHABETIC
               MOVE SPACES TO SL-EMPLOYEE-TITLE
           ELSE
               MOVE ALL "*" TO SL-EMPLOYEE-TITLE
           END-IF

           IF IR-NUM-COMPLETED NUMERIC AND IR-NUM-COMPLETED > 0
               MOVE SPACES TO SL-NUMBER-COMPLETED
               IF IR-NUM-COMPLETED > 50
                   MOVE ALL "*" TO ER-STAR
                   MOVE IR-NUM-COMPLETED TO ER-50
               END-IF
           END-IF.


           WRITE PRINT-RECORD FROM ER-EXCEPTION-REPORT AFTER 2
           WRITE PRINT-RECORD FROM SL-STAR-LINE AFTER 1.

       230-VALID.
           PERFORM 240-TABLE.

       240-TABLE.
           SET ITEM-NDX TO 1
           SEARCH TT-ITEM
               AT END
                   MOVE ALL "*" TO SL-JOB
               WHEN IR-JOB = TT-JOB (ITEM-NDX)
                   SET COM-NDX TO 1
                   SEARCH TT-NUM-COMPLETED
                       AT END
                           MOVE ALL "*" TO SL-EMPLOYEE-TITLE
                       WHEN IR-EMP =
                               TT-EMP-TYPE (ITEM-NDX, COM-NDX)
                           ADD IR-NUM-COMPLETED TO
                               TT-NUM-COM (ITEM-NDX, COM-NDX)
                   END-SEARCH
           END-SEARCH.

       250-EXCEPTION-HEADINGS.
           WRITE PRINT-RECORD FROM HEADING-TITLE-TWO AFTER 1
           WRITE PRINT-RECORD FROM HEADING-FOUR AFTER 3.
           WRITE PRINT-RECORD FROM HEADING-FIVE AFTER 1.


       300-UNLOAD.
           INITIALIZE TC-ROW-TOTAL
           MOVE TT-JOB (ITEM-NDX) TO DL-JOB
           MOVE TT-LOC (ITEM-NDX) TO DL-LOC
           PERFORM 310-INNER-LOOP
               VARYING COM-NDX
               FROM 1 BY 1
               UNTIL COM-NDX > 4

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 2.

       310-INNER-LOOP.
           MOVE TT-NUM-COM (ITEM-NDX, COM-NDX) TO
               DL-NUM-COMPLETED (COM-NDX)
           ADD TT-NUM-COM (ITEM-NDX, COM-NDX) TO TC-ROW-TOTAL
           MOVE TC-ROW-TOTAL TO DL-ROW-TOTAL
           ADD TT-NUM-COM (ITEM-NDX, COM-NDX) TO TC-GRAND-TOTAL.


       320-SUMMARY-HEADINGS.
           WRITE PRINT-RECORD FROM HEADING-TITLE AFTER 5
           WRITE PRINT-RECORD FROM HEADING-TWO
                   AFTER ADVANCING 3 LINES
           WRITE PRINT-RECORD FROM HEADING-THREE
                   AFTER ADVANCING 1 LINE.

       700-TOTALS.
           MOVE TC-GRAND-TOTAL TO CT-GRAND-TOTALS
           WRITE PRINT-RECORD FROM CT-COLUMN-TOTAL AFTER 2
           ADD 1 TO PC-LINES-USED
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD FROM TL-TOTALS-LINE AFTER 2.


       710-COL-TOTALS.
           PERFORM VARYING ITEM-NDX FROM 1 BY 1 UNTIL ITEM-NDX > 11
               ADD TT-NUM-COM (ITEM-NDX, COM-NDX) TO
                   TT-COL-TOTALS (COM-NDX)
               MOVE TT-COL-TOTALS (COM-NDX) TO CT-TOTALS (COM-NDX)
           END-PERFORM.

       850-HEADINGS.
           ADD  1  TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE
                   AFTER  PAGE
           MOVE  SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER 1
           ADD 5 TO PC-LINES-USED.
