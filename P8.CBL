      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM08.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INPUT-FILE
           ASSIGN TO "ADV9BLD2.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT SORT-FILE
           ASSIGN TO "SORT-WORK".

           SELECT SORTED-FILE
           ASSIGN "P8VSAM.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS SR-ISAM-NUM
           FILE STATUS IS SORT-SWITCH.

           SELECT OUTPUT-FILE
           ASSIGN TO "P8.TXT".
      *     ASSIGN TO DISPLAY.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IR-RECORD                   PIC X(80).

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-NUM                  PIC X(3).
           05                          PIC X(77).

       FD  SORTED-FILE.
       01  SR-SORTED-RECORD.
           05  SR-ISAM-NUM             PIC XXX.
           05                          PIC X(77).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(80).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

       01  INPUT-RECORD.
           05  IR-SAL-NUM              PIC XXX.
           05  FILLER                  PIC XX.
           05  IR-NAME                 PIC X(20).
           05  IR-COM-PER              PIC V99.
           05  IR-TOT-SAL              PIC XXXXX.
           05  IR-PRO-EOD              PIC XXXXXX.
           05  IR-MON-COM              PIC XXXXX.
           05  IR-YTD-COM              PIC XXXXX.
           05  IR-LAS-ACT              PIC XXXXXX.
           05  FILLER                  PIC XX.
           05  IR-TYP-A                PIC X.

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-SAL-NUM              PIC XXX.
           05  DL-NAME                 PIC X(20).
           05  DL-COM-PER              PIC .99.
           05  DL-TOT-SAL              PIC XXXXX.
           05  DL-PRO-EOD              PIC XXXXXX.
           05  DL-MON-COM              PIC XXXXX.
           05  DL-YTD-COM              PIC XXXXX.
           05  DL-LAS-ACT              PIC XXXXXX.
           05  DL-TYP-A                PIC X.

       01  DR-RECORD                   VALUE SPACES.
           05  DR-REC                  PIC X(63).
           05  FILLER                  PIC X(5).
           05  DR-MESSAGE              PIC X(10).

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
           05  HL-HEAD-TITLE           PIC X(17).
           05  FILLER                  PIC X(5).
           05                          PIC X(4) VALUE "PAGE".
           05  HL-PAGE-NUM             PIC Z9.

       01  SWITCHES.
           05  IR-SWITCH               PIC XXX VALUE "NO".
           05  SORT-SWITCH             PIC XX.
           05  OR-SWITCH               PIC XXX VALUE "NO".

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 100-INITIALIZE
           SORT SORT-FILE
               ASCENDING KEY SR-NUM
               INPUT PROCEDURE IS 400-INPUT
               OUTPUT PROCEDURE IS 410-OUTPUT
           STOP RUN.

       100-INITIALIZE.
           MOVE "NO " TO IR-SWITCH
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD.


       200-PROCESSING.
           MOVE IR-SAL-NUM TO DL-SAL-NUM
           MOVE IR-NAME TO DL-NAME
           MOVE IR-COM-PER TO DL-COM-PER
           MOVE IR-TOT-SAL TO DL-TOT-SAL
           MOVE IR-PRO-EOD TO DL-PRO-EOD
           MOVE IR-MON-COM TO DL-MON-COM
           MOVE IR-YTD-COM TO DL-YTD-COM
           MOVE IR-LAS-ACT TO DL-LAS-ACT
           MOVE SPACES TO DL-TYP-A


           MOVE INPUT-RECORD TO DR-REC
           WRITE PRINT-RECORD FROM DR-RECORD AFTER 2

           MOVE DL-DETAIL-LINE TO SORT-RECORD
           RELEASE SORT-RECORD.

       210-PROCESSING.
           WRITE SR-SORTED-RECORD
           MOVE SORT-RECORD TO DR-REC

           IF SORT-SWITCH = "00"
               MOVE "WRITTEN" TO DR-MESSAGE
           ELSE
               IF SORT-SWITCH = "22"
                   MOVE "DUPLICATE" TO DR-MESSAGE
               ELSE
                   DISPLAY SORT-SWITCH
               END-IF
           END-IF

           WRITE PRINT-RECORD FROM DR-RECORD AFTER 2

           RETURN SORT-FILE INTO SR-SORTED-RECORD
               AT END
                   MOVE "YES" TO OR-SWITCH.

       400-INPUT.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE
           MOVE "INPUT PROCEDURE" TO HL-HEAD-TITLE
           PERFORM 850-HEADINGS

           PERFORM UNTIL IR-SWITCH = "YES"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "YES" TO IR-SWITCH
                   NOT AT END
                       PERFORM 200-PROCESSING
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE.

       410-OUTPUT.
           OPEN OUTPUT SORTED-FILE
           MOVE "OUTPUT PROCEDURE" TO HL-HEAD-TITLE
           PERFORM 850-HEADINGS
           MOVE "NO" TO OR-SWITCH
           RETURN SORT-FILE INTO SR-SORTED-RECORD
               AT END
                   MOVE "YES" TO OR-SWITCH
           END-RETURN

           PERFORM 210-PROCESSING
               UNTIL OR-SWITCH = "YES"
           CLOSE SORTED-FILE
                 OUTPUT-FILE.

       850-HEADINGS.
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD
           ADD 5 TO PC-LINES-USED.
