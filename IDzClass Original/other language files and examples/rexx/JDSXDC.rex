/* simple example code to handle parameters to a REXX program         */
/* in a manner equivalent to the handling of parameters for a         */
/* REXX routine                                                       */
/*                                                                    */
  say "inside JDSXDC.rexx "
                    /* get the call type of this routine              */
                    /* DO NOT turn NOVALUE on at this time!           */
  parse source . callType .
  if callType <> "COMMAND" then
    signal Main     /* called as function or procedure                */
  else
  do
                    /* called from the command line                   */
    args = ""
    if pos( ",", arg(1)) = 0 then
    do
                    /* no comma found in the parameters -- use blanks */
                    /* as argument delimiter                          */

                    /* split argument in multiple arguments using     */
                    /* blanks as argument separator                   */
      do i = 1 to words( arg(1) )
        args = args "'" || word( arg(1),i ) || "'"
        args = args ","
      end /* do i = 1 to words( arg(1) ) */
    end /* if pos( ... */
    else
    do
                    /* at least one comma found in the parameters --  */
                    /* assume commas are used to separate the         */
                    /* parameters                                     */

                    /* split argument in multiple arguments using     */
                    /* commas as argument separator                   */
      argT = strip( arg(1) )
      do while argT <> ""
        parse var argT argC "," argT
        argC = strip( argC )
        args = args || "'" || argC || "'"
        args = args ","
      end /* while argT <> "" */
    end /* else */

    drop argT argC
    interpret "call Main " || args

    if symbol( "RESULT" ) = "VAR" then
    do
      say "return code " || result
                    /* return the return code to the caller           */
      return result
    end /* if symbol( "RESULT" ) = "VAR" then */
    else
      return
  end /* else */

/* main function entry point                                          */
/* Note: Do not use PROCEDURE for this routine                        */

Main:
  say "Main called with " || arg() || " arguments:"
  do i = 1 to arg()
    say "  Argument " || i || " is <" || arg(i) || ">"
  end  /* do i = 1 to arg() */
  rc=isfcalls('ON')
say 'set owner to ' arg(4)
isfowner = arg(4)
Address SDSF "ISFEXEC ST"
say 'about to loop through ' || JNAME.0 || ' jobs'
do ix=1 to JOBID.0
  isfprtdsname = "'USER93.JOBS.OUTPUT'"
  isfprtdisp = 'OLD'
  if JOBID.ix = arg(1) then
    do
      say 'matching job found' JOBID.ix
      /* Issue the ? action character for the job    */
      /* identified by the token variable.  Note     */
      /* the token must be enclosed in single quotes */
      /* Use the prefix option to ensure unique      */
      /* variables are created, beginning with JDS_  */
      Address  SDSF "ISFACT ST TOKEN('"TOKEN.ix"') PARM(NP ?)",
"("prefix JDS_
     /* Get fixed field name from first word */
     /* of isfcols2 special variable          */
     fixedField = word(isfcols2,1)
     Say "Number of rows returned:" isfrows2
       /* Process all rows */
      do jx=1 to isfrows2   /* loop for all rows returned */
        Say "Now processing DD Name :" value(fixedField"."jx)
        if JDS_DDNAME.jx = arg(7) then
        do
          say 'matching DD Name found' JDS_DDNAME.jx
          isfprtmember = JDS_DDNAME.jx
          Address SDSF "ISFACT ST TOKEN('"JDS_TOKEN.jx"') PARM(NP XDC)"
        end
      end
    end
end
   /* Gather diagnostics for problem determination */
   If rc>0 Then
   Do
      Say 'RC='rc isfmsg
      Do jx=1 to isfmsg2.0
         Say isfmsg2.jx
      End
      Do kx=1 to isfulog.0
         Say isfulog.kx
      End
   End
rc=isfcalls(OFF)
return rc