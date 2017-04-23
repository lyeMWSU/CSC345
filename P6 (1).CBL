      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM06.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO "ADV6MSTR.TXT"
           ORGANIZATION LINE SEQUENTIAL.

      *>      SELECT SORT-FILE
      *>      ASSIGN TO "SORT-WORK".


           SELECT OUTPUT-FILE
           ASSIGN TO "P6.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IR-RECORD                   PIC X(80).

      *>  SD  SORT-FILE.
      *>  01  SORT-RECORD.
      *>      05  SR-NUM                  PIC X(3).
      *>      05                          PIC X(35).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(80).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

       01  INPUT-RECORD.
           05  IR-PRO-NUM              PIC 999.
           05  IR-PRO-DES              PIC X(27).
           05  FILLER                  PIC X(10).
           05  IR-PRICE                PIC S999V99.
           05  FILLER                  PIC X(4).
           05  IR-MIN-ORD              PIC 999.
           05  IR-QTY-DIS              PIC 999.
           05  FILLER                  PIC X(5).
           05  IR-DIS-PER              PIC V999.


      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-PRO-NUM              PIC ZZ9.
           05  FILLER                  PIC X(3).
           05  DL-PRO-DES              PIC X(26).
           05  FILLER                  PIC X(3).
           05  DL-PRICE                PIC ZZ9.99.
           05  FILLER                  PIC X(3).
           05  DL-MIN-ORD              PIC ZZ9.
           05  FILLER                  PIC X(3).
           05  DL-QTY-DIS              PIC ZZ9.
           05  FILLER                  PIC X(3).
           05  DL-DIS-PER              PIC .999.


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
           05  HL-PAGE-NUM             PIC Z9.

       01  SWITCHES.
           05  EOD                         PIC XXX.
       PROCEDURE DIVISION.
       000-MAINLINE.
      *INITIALIZATION SECTION
           OPEN INPUT INPUT-FILE
               OUTPUT OUTPUT-FILE
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
      *PROCESSING SECTION
           PERFORM UNTIL EOD = "YES"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "YES" TO EOD
                   NOT AT END
                       PERFORM 200-PROCESSING
               END-READ
           END-PERFORM

      *TERMINATION SECTION
           CLOSE INPUT-FILE
                 OUTPUT-FILE
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
           MOVE IR-PRO-NUM TO DL-PRO-NUM
           MOVE IR-PRO-DES TO DL-PRO-DES
           MOVE IR-PRICE TO DL-PRICE
           MOVE IR-MIN-ORD TO DL-MIN-ORD
           MOVE IR-QTY-DIS TO DL-QTY-DIS
           MOVE IR-DIS-PER TO DL-DIS-PER

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.

       850-HEADINGS.
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER 1
           ADD 5 TO PC-LINES-USED.
