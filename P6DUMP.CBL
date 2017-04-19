      ******************************************************************
      * Author:    LISA YE
      * Date:
      * Purpose:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG6P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO "MSTR.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PRINT-REC.

           SELECT OUTPUT-FILE
               ASSIGN TO DISPLAY.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC               PIC X(80).

       FD  OUTPUT-FILE.
       01  PRINT-REC               PIC X(80).

       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05 EOD                  PIC XXX.

       01  INPUT-RECORD.
           05 RECORD-IN            PIC X(80).

       01  DL-DETAIL-LINE.
           05 RECORD-OUT           PIC X(80).

       PROCEDURE DIVISION.

       100-MAIN-MODULE.
           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE

           MOVE "NO" TO EOD

           PERFORM UNTIL EOD = "YES"
           READ INPUT-FILE INTO INPUT-RECORD
               AT END
                   MOVE "YES" TO EOD
               NOT AT END
                   PERFORM 200-PROCESSING
           END-READ
           END-PERFORM

           CLOSE INPUT-FILE
                 OUTPUT-FILE
           STOP RUN.

       200-PROCESSING.
           MOVE INPUT-REC TO DL-DETAIL-LINE

           WRITE PRINT-REC FROM DL-DETAIL-LINE.
