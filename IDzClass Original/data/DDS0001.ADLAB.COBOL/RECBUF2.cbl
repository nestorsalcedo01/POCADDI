      ******************************************************************
      *  INPUT RECORD BUFFER                                           *
      ******************************************************************
       01  WS-INPUT-RECORD-2.
           05  WS-REC-TYPE-2           PIC X(2).
           05  WS-ACCT-REPRESENTIVE2   PIC X(20).
           05  WS-KEY2                 PIC X(12).
           05  WS-KEY-R1 REDEFINES WS-KEY2.
               10  WS-KEY-CASE2        PIC XX.
               10  WS-KEY2-FA          PIC X(1).
               10  WS-KEY2-FM          PIC X(1).
               10  WS-KEY2-DT          PIC X(1).
               10  WS-KEY2-AM          PIC X(1).
               10  WS-KEY2-WS          PIC X(1).
               10  WS-KEY2-MU          PIC X(1).
               10  WS-KEY-NUMBER2      COMP-1.
           05  WS-KEY-START2           PIC X(2).
           05  WS-VSAM-KEY2            PIC X(4).
           05  WS-KEY-END2             PIC X(2).
           05  RECORD-DATE2.
               10  RECORD-YEAR2        PIC 9(2).
               10  RECORD-MONTH2       PIC 9(2).
               10  RECORD-DAY2         PIC 9(2).
           05  WS-COMPANY-NAME2        PIC X(32).
