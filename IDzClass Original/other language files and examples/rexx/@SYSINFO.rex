/*  REXX  */
 SAY "OPERATING SYSTEM = " MVSVAR(SYSOPSYS)
 SAY "MVS VERSION      = " MVSVAR(SYSMVS)
 SAY "MVS SYSTEM ID    = " MVSVAR(SYSNAME)
 SAY "MVS SMF ID       = " MVSVAR(SYSSMFID)
 SAY "USER ID          = " SYSVAR("SYSUID")
 SAY "VTAM TERMID      = " SYSVAR("SYSTERMID")
 SAY DATE(USA)  "("DATE(J)")"  TIME()