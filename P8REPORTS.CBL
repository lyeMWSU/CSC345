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
       01  TF-ID                        PIC X(56).

       FD  MASTER-FILE.
       01  MASTER-REC.
           05  MR-ID                   PIC 9(3).
           05                          PIC X(49).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(80).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

      *INPUT-RECORD...
       01  TRANSACTION-RECORD.
           05  TR-SAL-NUM              PIC 999.
           05  FILLER                  PIC XX.
           05  TR-NAME                 PIC X(20).
           05  TR-COM-PER              PIC V99.
           05  TR-TOT-SAL              PIC 9(5).
           05  TR-PRO-EOD              PIC X(6).
           05  FILLER                  PIC X(17).
           05  TR-TYP-A                PIC X.

       01  MASTER-RECORD.
           05  MR-SAL-NUM              PIC 999.
           05  MR-NAME                 PIC X(20).
           05  MR-COM-PER              PIC V99.
           05  MR-TOT-SAL              PIC 9(5).
           05  MR-PRO-EOD              PIC X(6).
           05  MR-MON-COM              PIC 9(5).
           05  MR-YTD-COM              PIC 9(5).
           05  MR-LAS-ACT              PIC XXXXXX.

       01  DC-DETAIL-CALCS.
           05 DC-MON-COM               PIC 9(5).

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-REC-OUT              PIC X(56).
           05  FILLER                  PIC X(5).
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
           PERFORM 800-READ-TRANS
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
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD.

       200-PROCESSING.
           MOVE TRANSACTION-RECORD TO DL-REC-OUT

           PERFORM 210-PROCESSING.

       210-PROCESSING.
           INITIALIZE DL-MESSAGE

           IF TR-TYP-A = "A" OR "D" OR "C"
           MOVE TR-SAL-NUM TO MR-ID
           READ MASTER-FILE INTO MASTER-RECORD
               IF TR-TYP-A = "A"
                   IF MR-SAL-NUM = TR-SAL-NUM
                       MOVE "DUPLICATE ADD" TO DL-MESSAGE
                   ELSE
                       MOVE "ADD" TO DL-MESSAGE
                       PERFORM 230-ADD
                   END-IF

               END-IF

               IF TR-TYP-A = "D"
                   IF TR-NAME(1:6) = MR-NAME(1:6)
                       MOVE "DELETE" TO DL-MESSAGE
                       PERFORM 250-DELETE
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
                   PERFORM 240-CHANGE
               END-IF
           ELSE
               MOVE "INVALID TRANS. CODE" TO DL-MESSAGE
           END-IF

           PERFORM 220-PROCESSING.

       220-PROCESSING.
           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.
           WRITE MASTER-REC FROM MASTER-RECORD AFTER 1.
           REWRITE MASTER-REC FROM MASTER-RECORD.

       230-ADD.
           MOVE TR-SAL-NUM TO MR-SAL-NUM
           MOVE TR-NAME TO MR-NAME
           MOVE TR-COM-PER TO MR-COM-PER
           MOVE TR-TOT-SAL TO MR-TOT-SAL
           MOVE ZEROS TO MR-MON-COM
           MOVE WS-DATE TO MR-LAS-ACT.


       240-CHANGE.
           MOVE ZEROS TO TR-COM-PER
           MOVE TR-COM-PER TO MR-COM-PER
           IF TR-TOT-SAL = SPACES
               CONTINUE
           ELSE
               IF TR-PRO-EOD < MR-PRO-EOD
                   IF MR-COM-PER = SPACES
                       COMPUTE DC-MON-COM = TR-TOT-SAL * TR-COM-PER
                   ELSE
                       COMPUTE DC-MON-COM = TR-TOT-SAL * MR-COM-PER
                   END-IF
                   ADD DC-MON-COM TO MR-MON-COM
                   ADD DC-MON-COM TO MR-YTD-COM
               ELSE
                   CONTINUE
               END-IF
               ADD TR-TOT-SAL TO MR-TOT-SAL
           END-IF.


       250-DELETE.
           DELETE MASTER-FILE.

       800-READ-TRANS.
           MOVE "NO" TO EOD
           PERFORM UNTIL EOD = "YES"
             READ TRANSACTION-FILE INTO TRANSACTION-RECORD
                 AT END
                     MOVE "YES" TO EOD
                 NOT AT END
                     PERFORM 200-PROCESSING
             END-READ
           END-PERFORM.
