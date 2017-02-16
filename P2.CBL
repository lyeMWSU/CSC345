      ******************************************************************
      * Author: LISA YE
      * Date:
      * Purpose:
      ******************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID. PRG02.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
               ASSIGN "TABLE1.TXT"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT PRINT-FILE
               ASSIGN TO "DATA.TXT".

       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE.
       01  INPUT-REC                   PIC X(80).

       FD  PRINT-FILE.
       01  PRINT-REC                   PIC X(80).

       WORKING-STORAGE SECTION.

       01  SWITCHES.
           05  END-OF-DATA             PIC XXX.

       01  PC-PAGE-CONTROLS.
           05  PC-PAGE-NUM             PIC S99 VALUE 00.
           05  PC-LINES-USED           PIC S99 VALUE 00.
           05  PC-LINES-PER-PAGE       PIC S99 VALUE 27.

       01  WS-DATE.
           05  WS-YY                   PIC 99.
           05  WS-MM                   PIC 99.
           05  WS-DD                   PIC 99.

       01  SS-SUBSCRIPS.
           05  SUB                     PIC S99.

       01  TC-TOTAL-CALCS.
           05  TC-RECORD-NUM           PIC 99.
           05  TC-TOTALS        PIC 9999.
           05  TL-HIGHEST        PIC 99.

       01  ET-EXCUSES.
           05                          PIC X(15) VALUE "COMPUTER CRASH".
           05                          PIC X(15) VALUE "DRAKE SAID NO".
           05                          PIC X(15) VALUE "I DIED".
           05                          PIC X(15) VALUE "GOD SAID STOP".
           05                          PIC X(15) VALUE "CAT PEED ON IT".
           05                          PIC X(15) VALUE "NOYNAERT CRIES".
           05                          PIC X(15) VALUE "TRUMP".
           05                          PIC X(15) VALUE "WATCHING MOVIE".
           05                          PIC X(15) VALUE "I AM SAD".
           05                          PIC X(15) VALUE "RUSSIANS".

       01  ET-EXCUSE-TABLE REDEFINES ET-EXCUSES.
           05  ET-EXCUSE OCCURS 10     PIC X(15).

       01  IR-INPUT-REC.
           05  IR-EXCUSE-NUM           PIC 99.
               88  IR-VALID            VALUE 1 THRU 10.
           05                          PIC XXX.
           05  IR-NUM-COUNT            PIC S99.
           05                          PIC X(9).

       01  SR-STAR-REC.
           05  SR-EXCUSE-NUMBER        PIC XX.
           05  SR-NUM-USING            PIC XX.

       01  SR-SUMMARY-REPORT.
           05  SR-TABLE OCCURS 10      PIC 9(4) VALUE 0.

       01  HL-HEADING-LINE.
           05                          PIC X(20).
           05  HL-DATE.
               10  HL-MM               PIC Z9/.
               10  HL-DD               PIC 99/.
               10  HL-YY               PIC 99.
           05                          PIC X(26).
           05  HL-PAGE                 PIC Z9.
           05                          PIC X(20).

       01  DL-DETAIL-LINE.
           05                          PIC X(30).
           05  DL-RECORD-NUM           PIC Z9.
           05                          PIC XX.
           05  DL-INPUT-REC            PIC X(16).
           05                          PIC X(30).

       01  DL-DETAIL-LINE-X.
           05                          PIC X(27).
           05  DL-RECORD-NUM-X         PIC Z9.
           05                          PIC XX.
           05  DL-EXCUSE-NAME-X        PIC X(15).
           05                          PIC XX.
           05  DLT-NUM-USING           PIC ZZZ9-.
           05                          PIC X(27).

       01  CH-COLUMN-HEADING.
           05                          PIC X(30).
           05  CH-RECORD-NUM           PIC X(3) VALUE "RC#".
           05                          PIC X(6).
           05  CH-IMAGE                PIC X(5) VALUE "IMAGE".
           05                          PIC X(36).

       01  HL-HEADING-LINE-X.
           05                          PIC X(30).
           05  TH-TEN-TERRIFIC-EXCUSES PIC X(20)
                                       VALUE "TEN TERRIFIC EXCUSES".

       01  SL-STAR-LINE.
           05                          PIC X(34).
           05  SL-EXCUSE-NUMBER        PIC XX.
           05                          PIC XXX.
           05  SL-NUM-USING            PIC XX.
           05                          PIC X(9).

       01  TL-TOTAL-LINE.
           05                          PIC X(21).
           05  TL-TOTAL-N              PIC X(14) VALUE "TOTAL:".
           05  TL-TOTAL                PIC ZZZ9.
           05                          PIC X(4).
           05  TL-HIGHEST-TOTAL-N      PIC X(14) VALUE "HIGHEST:".
           05  TL-HIGHEST-TOTAL        PIC Z9.

       PROCEDURE DIVISION.

       000-MAINLINE.
      *INITIALIZATION SECTION
           OPEN INPUT INPUT-FILE
               OUTPUT PRINT-FILE
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
      *PROCESSING SECTION
           PERFORM UNTIL END-OF-DATA = "YES"
               READ INPUT-FILE INTO IR-INPUT-REC
                   AT END
                       MOVE "YES" TO END-OF-DATA
                   NOT AT END
                       PERFORM 200-PROCESSING
               END-READ
           END-PERFORM
           PERFORM 800-SUMMARY-HEADING
           PERFORM 300-SUMMARY
               VARYING SUB
               FROM 1 BY 1
               UNTIL SUB > 10
           PERFORM 700-TOTALS
      *TERMINATION SECTION
           CLOSE INPUT-FILE PRINT-FILE
           STOP RUN.
      *

       100-INITIALIZE.
           MOVE "NO " TO END-OF-DATA
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MM TO HL-MM
           MOVE WS-DD TO HL-DD
           MOVE WS-YY TO HL-YY
      *INITIALIZE
           INITIALIZE TC-TOTAL-CALCS
           MOVE SPACES TO PRINT-REC.

       200-PROCESSING.
           INSPECT IR-NUM-COUNT CONVERTING SPACES TO ZEROS
           INITIALIZE DL-DETAIL-LINE
           ADD 1 TO TC-RECORD-NUM

           IF IR-VALID AND IR-EXCUSE-NUM NUMERIC
                   AND IR-NUM-COUNT NUMERIC
               PERFORM 220-VALID
           ELSE
               PERFORM 210-INVALID
           END-IF
      *TOTALS
           IF PC-LINES-USED > PC-LINES-PER-PAGE
               MOVE ZEROS TO PC-LINES-USED
               PERFORM 850-HEADINGS
           END-IF.

       210-INVALID.
           MOVE TC-RECORD-NUM TO DL-RECORD-NUM
           MOVE IR-INPUT-REC TO DL-INPUT-REC
      *STAR LINE
           IF NOT IR-VALID OR IR-EXCUSE-NUM NOT NUMERIC
               MOVE ALL "*" TO SR-EXCUSE-NUMBER
           ELSE
               MOVE SPACES TO SR-EXCUSE-NUMBER
           END-IF
           IF IR-NUM-COUNT NOT NUMERIC
               MOVE ALL "*" TO SR-NUM-USING
           ELSE
               MOVE SPACES TO SR-NUM-USING
           END-IF
      *WRITE PRINT REC
           MOVE SR-EXCUSE-NUMBER TO SL-EXCUSE-NUMBER
           MOVE SR-NUM-USING TO SL-NUM-USING
           WRITE PRINT-REC FROM DL-DETAIL-LINE
               AFTER 2
           WRITE PRINT-REC FROM SL-STAR-LINE
               AFTER 1
           ADD 3 TO PC-LINES-USED.

       220-VALID.
           ADD IR-NUM-COUNT TO SR-TABLE(IR-EXCUSE-NUM)
           ADD IR-NUM-COUNT TO TC-TOTALS.

       300-SUMMARY.
           MOVE SUB TO DL-RECORD-NUM-X
           MOVE ET-EXCUSE(SUB) TO DL-EXCUSE-NAME-X
           MOVE SR-TABLE(SUB) TO DLT-NUM-USING
           WRITE PRINT-REC FROM DL-DETAIL-LINE-X
               AFTER 1
           ADD 1 TO PC-LINES-USED

           IF SR-TABLE(SUB) > TL-HIGHEST
               MOVE SR-TABLE(SUB) TO TL-HIGHEST
           END-IF.

       700-TOTALS.
           MOVE TC-TOTALS TO TL-TOTAL
           MOVE TL-HIGHEST TO TL-HIGHEST-TOTAL
           ADD 1 TO PC-LINES-USED
           MOVE SPACES TO PRINT-REC
           WRITE PRINT-REC FROM TL-TOTAL-LINE AFTER 2.

       800-SUMMARY-HEADING.
           PERFORM 850-HEADINGS
           WRITE PRINT-REC FROM HL-HEADING-LINE-X AFTER 1
           MOVE SPACES TO PRINT-REC
           ADD 1 TO PC-LINES-USED.

       850-HEADINGS.
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE
           WRITE PRINT-REC FROM HL-HEADING-LINE AFTER PAGE
           WRITE PRINT-REC FROM CH-COLUMN-HEADING AFTER 2
           MOVE SPACES TO PRINT-REC
           WRITE PRINT-REC AFTER 1
           ADD 5 TO PC-LINES-USED.
