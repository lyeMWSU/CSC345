      ******************************************************************
      * Author:    LISA YE
      * Date:
      * Purpose:   PROGRAM 08 DUMP
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG7P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO "P8VSAM.TXT"
               ORGANIZATION IS INDEXED
               RECORD KEY IS MR-KEY
               ACCESS MODE IS SEQUENTIAL.

           SELECT OUTPUT-FILE
      *         ASSIGN TO DISPLAY
               ASSIGN TO "P8DUMP.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC.
           05 MR-KEY               PIC 9(3).
           05                      PIC X(49).

       FD  OUTPUT-FILE.
       01  PRINT-REC               PIC X(51).

       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05 EOD                  PIC XXX.

       PROCEDURE DIVISION.

       000-MAIN-MODULE.
           OPEN INPUT INPUT-FILE
               OUTPUT OUTPUT-FILE

           MOVE "NO" TO EOD

           PERFORM UNTIL EOD = "YES"
               READ INPUT-FILE
                   AT END
                       MOVE "YES" TO EOD
                   NOT AT END
                       WRITE PRINT-REC FROM INPUT-REC AFTER 1
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
                 OUTPUT-FILE

           STOP RUN.
