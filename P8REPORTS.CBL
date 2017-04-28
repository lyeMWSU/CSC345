      ******************************************************************
      * Author:    LISA YE
      * Date:
      * Purpose:    PROGRAM 08 REPORTS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM08.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TRANSACTION-FILE
           ASSIGN TO "ADV9TRN2.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT MASTER-FILE
           ASSIGN TO "P8VSAM.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS MR-ID
           FILE STATUS IS MASTER-SWITCH.

           SELECT OUTPUT-FILE
           ASSIGN TO "P8REPORTS.TXT".
      *     ASSIGN TO DISPLAY.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TF-ID                        PIC X(80).

       FD  MASTER-FILE.
       01  MASTER-REC.
           05  MR-ID                   PIC X(3).
           05                          PIC X(75).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

      *INPUT-RECORD...
       01  TRANSACTION-RECORD.
           05  TR-SAL-NUM              PIC XXX.
           05  FILLER                  PIC XX.
           05  TR-NAME                 PIC X(20).
           05  TR-COM-PER              PIC XX.
           05  TR-TOT-SAL              PIC XXXXX.
           05  TR-PRO-EOD              PIC XXXXXX.
           05  FILLER                  PIC X(5).
           05  TR-CODES                PIC XX.
           05  FILLER                  PIC X(10).
           05  TR-TYP-A                PIC X.

       01  MASTER-RECORD.
           05  MR-SAL-NUM              PIC XXX.
           05  MR-NAME                 PIC X(20).
           05  MR-COM-PER              PIC V99.
           05  MR-TOT-SAL              PIC XXXXX.
           05  MR-PRO-EOD              PIC XXXXXX.
           05  MR-MON-COM              PIC XXXXX.
           05  MR-YTD-COM              PIC XXXXX.
           05  MR-LAS-ACT              PIC XXXXXX.
           05  MR-TYP-A                PIC X.

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-SAL-NUM              PIC XXX.
           05  FILLER                  PIC X(2).
           05  DL-NAME                 PIC X(20).
           05  DL-COM-PER              PIC XX.
           05  DL-TOT-SAL              PIC X(5).
           05  DL-PRO-EOD              PIC XXXXXX.
           05  FILLER                  PIC X(3).
           05  DL-DATE.
               10 DL-MO                PIC XX/.
               10 DL-DAY               PIC XX/.
               10 DL-YY                PIC XX.
           05  FILLER                  PIC X(2).
           05  DL-CODES                PIC XX.
           05  FILLER                  PIC X(2).
           05  DL-TYP-A                PIC X.
           05  FILLER                  PIC X(2).
           05  DL-MESSAGE              PIC X(22).


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
           05  INPUT-SWITCH            PIC XXX VALUE "NO".
           05  MASTER-SWITCH           PIC XX.
           05  OUTPUT-SWITCH           PIC XXX VALUE "NO".

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 020-OPEN
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
           PERFORM 800-READ-TRANS
               UNTIL EOD = "YES"
           PERFORM 030-CLOSE.

       020-OPEN.

           OPEN INPUT      TRANSACTION-FILE
                OUTPUT     OUTPUT-FILE
                I-O        MASTER-FILE.

       030-CLOSE.
           CLOSE     TRANSACTION-FILE
                     OUTPUT-FILE
                     MASTER-FILE
           STOP RUN.

       100-INITIALIZE.
           MOVE "NO " TO EOD
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO DL-MO
           MOVE WS-DAY TO DL-DAY
           MOVE WS-YY TO DL-YY

           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD.

       200-PROCESSING.
      *     INITIALIZE DL-DETAIL-LINE
           MOVE TR-SAL-NUM TO MR-ID
           READ MASTER-FILE INTO MASTER-RECORD

           PERFORM 210-PROCESSING.

       210-PROCESSING.
           MOVE TR-SAL-NUM TO DL-SAL-NUM
           MOVE TR-NAME TO DL-NAME
           MOVE TR-COM-PER TO DL-COM-PER
           MOVE TR-TOT-SAL TO DL-TOT-SAL
           MOVE TR-PRO-EOD TO DL-PRO-EOD
           MOVE TR-CODES TO DL-CODES
           MOVE TR-TYP-A TO DL-TYP-A

           PERFORM 220-PROCESSING.

       220-PROCESSING.
           INITIALIZE DL-MESSAGE

           MOVE TR-TYP-A TO DL-TYP-A

           IF TR-TYP-A = "A" OR "D" OR "C"
               IF TR-TYP-A = "A"
                   IF MR-SAL-NUM = TR-SAL-NUM
                       MOVE "DUPLICATE ADD" TO DL-MESSAGE
                   ELSE
                       MOVE "ADD" TO DL-MESSAGE
                   END-IF
               END-IF

               IF TR-TYP-A = "D"
                   IF MR-NAME = TR-NAME
                       MOVE "DELETE" TO DL-MESSAGE
                   ELSE
                       MOVE "NO MASTER TO DELETE" TO DL-MESSAGE
                   END-IF
               END-IF

               IF TR-TYP-A = "C"
                   IF TR-NAME = SPACES
                       MOVE "NO MASTER TO CHANGE" TO DL-MESSAGE
                   ELSE
                       MOVE "CHANGE" TO DL-MESSAGE
                   END-IF

                   IF TR-TOT-SAL = SPACES
                       MOVE "NO MASTER TO CHANGE" TO DL-MESSAGE
                   ELSE
                       MOVE "CHANGE" TO DL-MESSAGE
                   END-IF

                   IF TR-COM-PER = SPACES
                       MOVE "NO MASTER TO CHANGE" TO DL-MESSAGE
                   ELSE
                       MOVE "CHANGE" TO DL-MESSAGE
                   END-IF

               END-IF
           ELSE
               MOVE "INVALID TRANS.CODE" TO DL-MESSAGE
           END-IF

           REWRITE MASTER-REC FROM MASTER-RECORD
               INVALID KEY
                   MOVE "REWRITE UNSUCCESSFUL" TO DL-MESSAGE
           END-REWRITE.

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.
       800-READ-TRANS.
           MOVE "NO" TO EOD
           READ TRANSACTION-FILE INTO TRANSACTION-RECORD
               AT END
                   MOVE "YES" TO EOD
               NOT AT END
                   PERFORM 200-PROCESSING
           END-READ.

       850-HEADINGS.
           MOVE "TRANSACTION REPORT" TO HL-HEAD-TITLE
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
      *     WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
      *     WRITE PRINT-RECORD FROM HEADING-TWO AFTER 1
      *     WRITE PRINT-RECORD FROM HEADING-THREE AFTER 1
      *     WRITE PRINT-RECORD FROM HEADING-FOUR AFTER 1
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD
           ADD 5 TO PC-LINES-USED.
