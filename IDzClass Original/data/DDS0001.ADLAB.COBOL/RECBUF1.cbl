      ******************************************************************
      *  INPUT RECORD BUFFER                                           *
      ******************************************************************
       01  WS-INPUT-RECORD-1.
           05  WS-REC-TYPE-1           PIC X(2).
           05  WS-ACCT-REPRESENTIVE1   PIC X(20).
           05  WS-KEY1                 PIC X(12).
           05  WS-KEY-R1 REDEFINES WS-KEY1.
               10  WS-KEY-CASE1        PIC XX.
               10  WS-KEY1-FA          PIC X(1).
               10  WS-KEY1-FM          PIC X(1).
               10  WS-KEY1-DT          PIC X(1).
               10  WS-KEY1-AM          PIC X(1).
               10  WS-KEY1-WS          PIC X(1).
               10  WS-KEY1-MU          PIC X(1).
               10  WS-KEY-NUMBER1      COMP-1.
           05  WS-KEY-START1           PIC X(2).
           05  WS-VSAM-KEY1            PIC X(4).
           05  WS-KEY-END1             PIC X(2).
           05  RECORD-DATE1.
               10  RECORD-YEAR1        PIC 9(2).
               10  RECORD-MONTH1       PIC 9(2).
               10  RECORD-DAY1         PIC 9(2).
           05  WS-COMPANY-NAME1        PIC X(32).
