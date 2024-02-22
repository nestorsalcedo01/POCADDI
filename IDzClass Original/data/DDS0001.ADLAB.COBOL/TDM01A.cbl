       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDM01A.
      ******************************************************
      *  PROGRAM NAME: TDM01A                              *
      *                                                    *
      *  PROGRAMMER: TIMOTHY DAVID MAGEE                   *
      *              LEXINGTON, KENTUCKY                   *
      *                                                    *
      *  DATE: 02/28/2005                                  *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL WITH A MUILTIPLE *
      *       COMPILE OS/VS COBOL TEST CASE                *
      *                                                    *
      *  FUNCTION OF THIS COMPILE UNIT:                    *
      *                                                    *
      *       THIS IS THE MAIN DRIVER, TDM01A, THAT WILL   *
      *       CALL TDM01B ALONG WITH A NUMBER OF VALUE     *
      *       DATA MODIFICATIONS.  THIS COMPILE UNIT WILL  *
      *       ALSO PERFORM FILE I/O.  THE FILE OUTPUT      *
      *       ASSOCIATED WITH THE DD REPORT WILL BE        *
      *       WRITTEN TO WITH A FORMATED OUTPUT REPORT.    *
      ******************************************************

       ENVIRONMENT DIVISION.

      ******************************************************
      * FILE DEFINTIONS: DD=REPORT, WRITE BUFFER=OUT-BUF   *
      ******************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE
           ASSIGN TO UT-3330-S-REPORT.

       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS OUT-BUF.
       01  OUT-BUF            PIC X(80).

      ******************************************************
      * PROGRAM WORKING STORGAGE                           *
      ******************************************************
       WORKING-STORAGE SECTION.
       01  DTPARM1      PIC 99 VALUE 5.
       01  DTPARM2      PIC 99 VALUE 2.
       01  TDM01B       PIC X(6) VALUE 'TDM01B'.
       01  P1PARM1      PIC 99 VALUE 0.

       01  DTSTRUCT.
         05 PD-TOOL     PIC X(2).
         05 TEAM-LEAD   PIC X(9).
         05 LOC-ID.
           10 STATE     PIC X(2).
           10 CITY      PIC X(3).

      ******************************************************
      * REPORT OUTPUT FORMAT BUFFERS                       *
      ******************************************************
       01  LINE-ONE.
         05 FILL1       PIC X(28) VALUE SPACES.
         05 DATE-STR    PIC X(15) VALUE 'TODAY"S DATE = '.
         05 TODAY-DATE  PIC X(8)  VALUE SPACES.
         05 FILL2       PIC X(29) VALUE SPACES.
       01  LINE-TWO.
         05 FILL1       PIC X(14) VALUE SPACES.
         05 LEAD-STR    PIC X(20) VALUE 'TEAM LEAD FOR IBM PD'.
         05 LEAD-STR    PIC X(22) VALUE ' TOOL DEBUG TOOL IS = '.
         05 LEAD-NAME   PIC X(9).
         05 FILL2       PIC X(15) VALUE SPACES.
       01  LINE-THREE.
         05 FILL1       PIC X(23) VALUE SPACES.
         05 LOC-STR1    PIC X(15) VALUE 'TEAM LEAD WORK '.
         05 LOC-STR2    PIC X(14) VALUE 'LOCATION IS = '.
         05 LEAD-TOWN   PIC X(3)  VALUE SPACES.
         05 PRT-COMMA   PIC X(1)  VALUE ','.
         05 LEAD-STATE  PIC X(2)  VALUE SPACES.
         05 FILL2       PIC X(22) VALUE SPACES.
       01  LINE-BLANK   PIC X(80) VALUE SPACES.

      ******************************************************
      * MAIN LINE PROGRAM                                  *
      ******************************************************
       PROCEDURE DIVISION.

       PROG.
      ******************************************************
      * OPEN OUTPUT FILE                                   *
      ******************************************************
           OPEN OUTPUT PRINT-FILE

      ******************************************************
      * GET THE SYSTEM DATE                                *
      ******************************************************
           MOVE CURRENT-DATE TO TODAY-DATE

      ******************************************************
      * SET THE VARIABLE VALUES BY GROUP NAME              *
      ******************************************************
           MOVE 'DTTIM MAGEEKYLEX' TO DTSTRUCT

      ******************************************************
      * SET THE VARIABLE VALUES BY ELEMENT NAMES           *
      ******************************************************
           MOVE 'GAATL' TO LOC-ID
           MOVE 'DT' TO PD-TOOL
           MOVE 'TIM MAGEE' TO TEAM-LEAD

      ******************************************************
      * SET THE VARIABLE VALUES BY LOWER ELEMENTS NAMES    *
      ******************************************************
           MOVE 'KY' TO STATE
           MOVE 'LEX' TO CITY

      ******************************************************
      * SET THE REPORT VARS                                *
      ******************************************************
           MOVE TEAM-LEAD TO LEAD-NAME
           MOVE STATE TO LEAD-STATE
           MOVE CITY TO LEAD-TOWN

      ******************************************************
      * GENERATE AND OUTPUT REPORT AND CLOSE THE FILE      *
      ******************************************************
           WRITE OUT-BUF FROM LINE-ONE
           WRITE OUT-BUF FROM LINE-BLANK
           WRITE OUT-BUF FROM LINE-TWO
           WRITE OUT-BUF FROM LINE-BLANK
           WRITE OUT-BUF FROM LINE-THREE
           WRITE OUT-BUF FROM LINE-BLANK
           CLOSE PRINT-FILE

      ******************************************************
      * DISPLAY THE PARM VALUES VIA THE OS/VS EXIBIT VERB  *
      ******************************************************
           EXHIBIT 'TEAM LEAD FOR DEBUG TOOL = ' TEAM-LEAD
           .

      ******************************************************
      * PROGA: INTERNAL COBOL CALLED PROCEEDURE            *
      ******************************************************
       PROGA.
           PERFORM  LOOP1 UNTIL DTPARM1 = 0

           IF DTPARM2 = 0   THEN
               PERFORM PROCA.


           PERFORM LOOP2 UNTIL DTPARM2 = 0
           .
           STOP RUN
           .

       PROCA.
           MOVE 10 TO P1PARM1
           .
       LOOP1.
             IF DTPARM1 > 0 THEN
               SUBTRACT 1 FROM DTPARM1
               CALL 'TDM01B'
             OTHERWISE
               MOVE 0 TO DTPARM1.
           .
       LOOP2.
             IF DTPARM2 > 0 THEN
               SUBTRACT 1 FROM DTPARM2
             OTHERWISE
               MOVE 0 TO DTPARM2
           .

       THE-END.
             STOP RUN.