*
      * Author: Elayne Colvin
      * Description: DUMP
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-IN
               ASSIGN TO "MSTR.TXT"
               ORGANIZATION IS INDEXED
               RECORD KEY IS PRINT-REC
               ACCESS MODE IS SEQUENTIAL.

           SELECT DATA-OUT
               ASSIGN TO "PROG6DUMP.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  DATA-IN.
       01  INPUT-REC                   PIC X(80).

       FD  DATA-OUT.
       01  PRINT-REC                   PIC X(80).

       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05  END-OF-FILE             PIC XXX.

       01  BUFFER.
           05  RECORD-IN               PIC X(80).

       01  DL-DETAIL-LINE.
           05  RECORD-OUT              PIC X(80).

       PROCEDURE DIVISION.

       000-MAINLINE.
           OPEN INPUT DATA-IN
               OUTPUT DATA-OUT

           MOVE "NO" TO END-OF-FILE

           PERFORM UNTIL END-OF-FILE = "YES"
           READ DATA-IN INTO BUFFER
               AT END
                   MOVE "YES" TO END-OF-FILE
               NOT AT END
                   PERFORM 200-PROCESSING
           END-READ
           END-PERFORM

           CLOSE DATA-IN
               DATA-OUT

           STOP RUN.

       200-PROCESSING.
           MOVE INPUT-REC TO DL-DETAIL-LINE
           WRITE PRINT-REC FROM DL-DETAIL-LINE.
