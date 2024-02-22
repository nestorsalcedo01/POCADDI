       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDM01D.
      ******************************************************
      *  PROGRAM NAME: TDM01D                              *
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
      *       THIS IS A SUB CU, IT WILL NOT BE CALLED BY   *
      *       TDM01C UNLESS THE PROGRAM LOGIC IS CHANGED   *
      *       BY DEBUG TOOL.                               *
      ******************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

      ******************************************************
      * WORKING STORAGE                                    *
      ******************************************************
       WORKING-STORAGE SECTION.
       01  DTPARM1      PIC 99 VALUE 5.
       01  DTPARM2      PIC 99 VALUE 2.
       01  P1PARM1      PIC 99 VALUE 0.
       01  CALL-COUNT   PIC 99 VALUE 99.

       PROCEDURE DIVISION.

      ******************************************************
      * PROGRAM MAIN LINE                                  *
      ******************************************************
       PROGD.
           PERFORM LOOP1 UNTIL DTPARM1 > 0

           IF DTPARM2 = 0  THEN
              PERFORM PROCD
           OTHERWISE
              SUBTRACT 1 FROM CALL-COUNT.

           GOBACK
           .

      ******************************************************
      * PROCD: INTERNAL CALLED PROCEEDURE                  *
      ******************************************************
       PROCD.
           MOVE 10 TO P1PARM1
           .

       LOOP1.
           SUBTRACT 1 FROM DTPARM1
           .

       THE-END.
             STOP RUN.