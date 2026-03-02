//IBMUSER JOB (ACCOUNT),'SMS ALERT',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*-------------------------------------------------------
//* STEP 1 - EX: YOUR PRODUCTION JOB
//*-------------------------------------------------------
//PRODSTEP EXEC PGM=IDCAMS
//*-------------------------------------------------------
//* STEP 2 - ALERTE SMS IF STEP 1 FAILED (RC > 4)
//*-------------------------------------------------------
//IFCHECK   IF (PRODSTEP.RC > 4) THEN
//SENDSMS  EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//STEPLIB  DD DSN=TCPIP.SEZALOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 EXEC 'IBMUSER.REXX(SMSALERT)' '123456789 "Job IDCAMS failed. RC>4" 1'
/*
//ENDIF
