       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDM01C.
      ******************************************************
      *  PROGRAM NAME: TDM01C                              *
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
      *       THIS IS A SUB CU CALLED BY TDM01B. IT WILL   *
      *       CALL TDM01D ALONG WITH A NUMBER OF VALUE     *
      *       DATA MODIFICATIONS.                          *
      ******************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

      ******************************************************
      * WORKING STORAGE                                    *
      ******************************************************
       WORKING-STORAGE SECTION.
       01  DTPARM1      PIC 99 VALUE 5.
       01  DTPARM2      PIC 99 VALUE 2.
       01  TDM01D       PIC X(6) VALUE 'TDM01D'.
       01  P1PARM1      PIC 99 VALUE 0.
       01  CALL-COUNT   PIC 99 VALUE 99.

       PROCEDURE DIVISION.

      ******************************************************
      * MAIN LINE PROGRAM                                  *
      ******************************************************
       PROGC.
           PERFORM LOOP1 UNTIL DTPARM1 = 0

           IF DTPARM2 = 0   THEN
              PERFORM PROCC
           OTHERWISE
              SUBTRACT 1 FROM CALL-COUNT.


           PERFORM LOOP2  UNTIL DTPARM2 = 0
           GOBACK
           .

      ******************************************************
      * INTERNAL PROCEEDURE                                *
      ******************************************************
       PROCC.
           MOVE 10 TO P1PARM1
           IF DTPARM2  = 99  THEN
      * NEVER TRUE, TDM01D NEVER CALLED
              CALL 'TDM01D'.



       LOOP1.
           IF DTPARM1 > 0 THEN
              SUBTRACT 1 FROM DTPARM1.
      ******************************************************
      * THIS IS NEVER TRUE AND TDM01D IS NEVER CALLED      *
      ******************************************************
           IF DTPARM1 = 99  THEN
              CALL 'TDM01D'.


       LOOP2.
           IF DTPARM2 > 0 THEN
              SUBTRACT 1 FROM DTPARM2.

       THE-END.
             STOP RUN.