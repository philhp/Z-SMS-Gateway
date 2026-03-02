//IBMUSER JOB (ACCOUNT),'SMS ALERT',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*-------------------------------------------------------
//* STEP 1: PRODUCTION JOB
//*-------------------------------------------------------
//SENDSMS  EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//STEPLIB  DD DSN=TCPIP.SEZALOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 EXEC 'IBMUSER.REXX(SMSALERT)' '61412345678 "JobFailed" 1'
/*
