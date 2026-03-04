       IDENTIFICATION DIVISION.
       PROGRAM-ID. HISTORY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * zSMSGateway - GET /history handler
      *---------------------------------------------------------------*


      * SMS Provider Tracking Status Codes Mapping
      * 20000 Message delivered
      * 20001 Absent subscriber
      * 20002 Barred
      * 20003 Carried subscriber
      * 20004 Message Deferred
      * 20005 Message Deleted
      * 20006 Message Detached
      * 20007 Message Duplicated
      * 20008 Message Expired
      * 20009 Failure
      * 20010 Message forwarded
      * 20011 Illegal 
      * 20012 Invalid field
      * 20013 Invalid message format
      * 20014 Mobile application error
      * 20015 No response
      * 20016 Operation not supported
      * 20017 Not provided
      * 20018 Protocol error
      * 20019 Purged
      * 20020 Message read
      * 20021 Message rejected
      * 20022 Message retrieved
      * 20023 Server message error
      * 20024 System error
      * 20025 Message undeliverable
      * 20026 Unkno wn subscriber
      * 20027 Unregistered
      * 20028 Unrecognised
      * 20029 Incomplete 
      * 20030 Receipt timeout
      * 20031  Not Applicable
      * 21029 Congestion
      * 21030 In process
      * 21031 Intermediate
      * 21032 Network error
      * 21033 Rejected transient
      * 21034 Temporary not available
      * 21035 Transient: Absent subscriber                                 


       01  WS-CHARS.
           05  WS-NL        PIC X  VALUE X'25'.  *> EBCDIC Line Feed
           05  WS-CR        PIC X  VALUE X'0D'.  *> EBCDIC Carriage Ret
           05  WS-OPEN-BRACKET  PIC X VALUE X'BA'. *> [
           05  WS-CLOSE-BRACKET PIC X VALUE X'BB'. *> [

       01  WS-STATUSTXT PIC X(32) VALUE SPACES.


       01  WS-JSON-RESPONSE     PIC X(5000) VALUE SPACES.
       01  WS-JSON-LEN          PIC 9(04)   BINARY.
       01  WS-PTR               PIC S9(9)   BINARY VALUE 1.

       01 WS-USERID-NAME        PIC X(10)  VALUE 'UserID'.
       01 WS-USERID-NAME-LEN    PIC S9(8)  BINARY VALUE 6.         
       01 WS-USERID-LEN         PIC S9(8)  BINARY VALUE 10.
       01 WS-USERID-VAL         PIC X(10)  VALUE SPACES.

       01 WS-MSGID-NAME        PIC X(5)   VALUE 'MsgId'.
       01 WS-MSGID-NAME-LEN    PIC S9(8)  BINARY VALUE 5.       
       01 WS-MSGID-LEN         PIC S9(8)  BINARY VALUE 10.
       01 WS-MSGID-VAL         PIC X(10)  VALUE SPACES.

       01 WS-TRACKING-NAME     PIC X(14)  VALUE 'TrackingStatus'.
       01 WS-TRACKING-NAME-LEN PIC S9(8)  BINARY VALUE 14.         
       01 WS-TRACKING-LEN      PIC S9(8)  BINARY VALUE 10.
       01 WS-TRACKING-VAL      PIC X(10)  VALUE SPACES.

       01 WS-TYPE-NAME         PIC X(4)   VALUE 'Type'.
       01 WS-TYPE-NAME-LEN     PIC S9(8)  BINARY VALUE 4.         
       01 WS-TYPE-LEN          PIC S9(8)  BINARY VALUE 10.
       01 WS-TYPE-VAL          PIC X(10)  VALUE SPACES.

       01  WS-RESP              PIC S9(8) BINARY.
       01  WS-RESP-DISP         PIC --------9. 

       01  WS-RESP-MSGID        PIC S9(8) BINARY.
       01  WS-RESP-TRACKING     PIC S9(8) BINARY.
       01  WS-RESP-USERID       PIC S9(8) BINARY.
       01  WS-RESP-TYPE         PIC S9(8) BINARY.

       01  WS-NBSMS-DIS         PIC ZZZ9.

       01  WS-I                 PIC 9(4) BINARY VALUE 0.


       01 DB2-VARS.
          05 D-USER-ID      PIC S9(9) BINARY VALUE 0.
          05 D-MSG-ID       PIC S9(18) BINARY.
          05 D-PHONE        PIC X(20).
          05 D-NB-SMS       PIC S9(9) BINARY.
          05 D-TYPE-MO      PIC X(2)  VALUE 'MO'. 
          05 D-TYPE-MT      PIC X(2)  VALUE 'MT'.
          05 D-CREATED-AT   PIC X(26).
          05 D-TRACKING-ST  PIC X(5).
          *> STRUCTURE POUR LE VARCHAR(1000)
          05 D-MSG-TEXT.
             49 D-MSG-TEXT-LEN  PIC S9(4) BINARY.
             49 D-MSG-TEXT-DATA PIC X(1000).


      * FOR SQL
       01 WS-DATA  PIC X(20) VALUE SPACES.
      * SQLCA NEEDED FOR BINDIND  
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.


           *>Tracking management : Update History
           EXEC CICS WEB READ FORMFIELD(WS-MSGID-NAME)
               NAMELENGTH(WS-MSGID-NAME-LEN)
               VALUE(WS-MSGID-VAL)
               VALUELENGTH(WS-MSGID-LEN)
               RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-MSGID

           EXEC CICS WEB READ FORMFIELD(WS-TRACKING-NAME)
                NAMELENGTH(WS-TRACKING-NAME-LEN)
                VALUE(WS-TRACKING-VAL)
                VALUELENGTH(WS-TRACKING-LEN)
                RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-TRACKING

           EXEC CICS WEB READ FORMFIELD(WS-USERID-NAME)
               NAMELENGTH(WS-USERID-NAME-LEN)
               VALUE(WS-USERID-VAL)
               VALUELENGTH(WS-USERID-LEN)
               RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-USERID

           EXEC CICS WEB READ FORMFIELD(WS-TYPE-NAME)
               NAMELENGTH(WS-TYPE-NAME-LEN)
               VALUE(WS-TYPE-VAL)
               VALUELENGTH(WS-TYPE-LEN)
               RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-TYPE 

            IF WS-RESP-MSGID = DFHRESP(NORMAL)
              DISPLAY 'MSGID : ' WS-MSGID-VAL(1:WS-MSGID-LEN)
            END-IF 
            IF WS-RESP-TRACKING = DFHRESP(NORMAL)
              DISPLAY 'TRACKING : ' WS-TRACKING-VAL(1:WS-TRACKING-LEN)
            END-IF 
            IF WS-RESP-USERID = DFHRESP(NORMAL)
              DISPLAY 'USERID : ' WS-USERID-VAL(1:WS-USERID-LEN)
            END-IF 

           IF WS-RESP-MSGID = DFHRESP(NORMAL) AND 
              WS-RESP-TRACKING = DFHRESP(NORMAL)

              DISPLAY 'MSGID AND TRACKING FOUND '
           
              *> Verify that they are numeric
              IF WS-MSGID-VAL(1:WS-MSGID-LEN) IS NUMERIC AND 
                 WS-TRACKING-VAL(1:WS-TRACKING-LEN) IS NUMERIC

                    DISPLAY 'UPDATING MSGID: ' 
                       WS-MSGID-VAL(1:WS-MSGID-LEN)
                    DISPLAY 'TRACKING STATUS: ' 
                       WS-TRACKING-VAL(1:WS-TRACKING-LEN)


                    EXEC SQL
                        UPDATE ZSMS_MESSAGES
                        SET TRACKING_ST = :WS-TRACKING-VAL
                        WHERE MSG_ID = :WS-MSGID-VAL
                    END-EXEC

                    IF SQLCODE = 0
                        DISPLAY 'Tracking update successful'
                    ELSE
                        DISPLAY 'SQL Error: ' SQLCODE
                    END-IF

                    MOVE 0 TO WS-RESP
                    PERFORM SEND-JSON-ERROR

                    EXEC CICS RETURN END-EXEC  
              END-IF
           END-IF






           IF WS-RESP-USERID NOT = DFHRESP(NORMAL)
               MOVE WS-RESP TO WS-RESP-DISP
               DISPLAY 'ERROR : USERID NOT FOUND '

               PERFORM SEND-JSON-ERROR

               EXEC CICS RETURN END-EXEC       
           END-IF.

           DISPLAY 'WS-RES-USERID:' WS-USERID-VAL(1:WS-USERID-LEN)
           COMPUTE D-USER-ID = FUNCTION NUMVAL(WS-USERID-VAL)

      * Show SMS history for UserID with Type = MO (Mobile Originated)
           *> Type if Type=MO Request
           IF WS-RESP-TYPE = DFHRESP(NORMAL)
              IF WS-TYPE-VAL(1:WS-TYPE-LEN) = 'MO'

              DISPLAY 'TYPE : MO '

              EXEC SQL
                    DECLARE C2 CURSOR FOR
                    SELECT MSG_ID,   
                           USER_ID, 
                           PHONE_NUM, 
                           MSG_TEXT, 
                           NB_SMS, 
                           CREATED_AT,
                           TRACKING_ST
                    FROM ZSMS_MESSAGES 
                    WHERE USER_ID = :D-USER-ID AND TYPE = :D-TYPE-MO
                    ORDER BY CREATED_AT DESC
              END-EXEC
      * -- 1. Opening the cursor
              EXEC SQL OPEN C2 END-EXEC

              MOVE 1 TO WS-PTR
              *>Add caracter : [ + SPACE
              STRING 
                 WS-OPEN-BRACKET DELIMITED BY SIZE
                 ' '             DELIMITED BY SIZE
              INTO WS-JSON-RESPONSE 
              WITH POINTER WS-PTR

      * -- 2. Loop
              PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH C2 
                   INTO :D-MSG-ID,
                        :D-PHONE,
                        :D-MSG-TEXT,
                        :D-CREATED-AT
               END-EXEC

              IF SQLCODE = 0

                  MOVE D-NB-SMS TO WS-NBSMS-DIS


               STRING  
                  '{ '                DELIMITED BY SIZE
                  '"Phone": '         DELIMITED BY SIZE
                  D-PHONE             DELIMITED BY SPACE
                  ','                 DELIMITED BY SIZE
                  
                  '"Content": "'      DELIMITED BY SIZE
                  D-MSG-TEXT-DATA(1:D-MSG-TEXT-LEN) DELIMITED BY SIZE
                  '",'                DELIMITED BY SIZE
                                                               
                  '"Created": "'      DELIMITED BY SIZE
                  D-CREATED-AT        DELIMITED BY SIZE
                  '",'                DELIMITED BY SIZE
                                        
                  '},'                DELIMITED BY SIZE

                  INTO WS-JSON-RESPONSE
                  WITH POINTER WS-PTR
               END-STRING

              END-IF
              END-PERFORM

              *>remove last comma caracter  ","
              SUBTRACT 1 FROM WS-PTR
              *>Add final caracter : ]
              STRING 
                 ' '               DELIMITED BY SIZE
                 WS-CLOSE-BRACKET  DELIMITED BY SIZE
              INTO WS-JSON-RESPONSE 
              WITH POINTER WS-PTR


      * -- 3. Closing Cursor
              EXEC SQL CLOSE C2 END-EXEC

              PERFORM SEND-JSON-RESPONSE

              EXEC CICS RETURN END-EXEC   
              END-IF  
           END-IF

      * Show SMS history for UserID with Type = MT (Mobile Terminated)

           EXEC SQL
                 DECLARE C1 CURSOR FOR
                 SELECT MSG_ID,   
                        USER_ID, 
                        PHONE_NUM, 
                        MSG_TEXT, 
                        NB_SMS, 
                        CREATED_AT,
                        TRACKING_ST
                 FROM ZSMS_MESSAGES 
                 WHERE USER_ID = :D-USER-ID AND TYPE = :D-TYPE-MT
                 ORDER BY CREATED_AT DESC
           END-EXEC.

      * -- 1. Opening the cursor
           EXEC SQL OPEN C1 END-EXEC.

           MOVE 1 TO WS-PTR
           *>Add caracter : [ + SPACE
           STRING 
                 WS-OPEN-BRACKET DELIMITED BY SIZE
                 ' '             DELIMITED BY SIZE
           INTO WS-JSON-RESPONSE 
           WITH POINTER WS-PTR

      * -- 2. Loop
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH C1 
                   INTO :D-MSG-ID,
                        :D-USER-ID,
                        :D-PHONE,
                        :D-MSG-TEXT,
                        :D-NB-SMS,
                        :D-CREATED-AT,
                        :D-TRACKING-ST
               END-EXEC

              IF SQLCODE = 0

                  MOVE D-NB-SMS TO WS-NBSMS-DIS


               STRING  
                  '{ '                DELIMITED BY SIZE
                  '"Phone": '         DELIMITED BY SIZE
                  D-PHONE             DELIMITED BY SPACE
                  ','                 DELIMITED BY SIZE
                  
                  '"Content": "'      DELIMITED BY SIZE
                  D-MSG-TEXT-DATA(1:D-MSG-TEXT-LEN) DELIMITED BY SIZE
                  '",'                DELIMITED BY SIZE
                                   
                  '"NbSMS": '         DELIMITED BY SIZE
                  WS-NBSMS-DIS        DELIMITED BY SIZE
                  ','                 DELIMITED BY SIZE
                                    
                  '"Created": "'      DELIMITED BY SIZE
                  D-CREATED-AT        DELIMITED BY SIZE
                  '",'                DELIMITED BY SIZE
                  
                  '"Tracking": "'     DELIMITED BY SIZE
                  D-TRACKING-ST       DELIMITED BY SIZE
                  '"'                 DELIMITED BY SIZE
                                   
                  '},'                DELIMITED BY SIZE

                  INTO WS-JSON-RESPONSE
                  WITH POINTER WS-PTR
               END-STRING

               

              END-IF

           END-PERFORM.

           *>remove last comma caracter  ","
           SUBTRACT 1 FROM WS-PTR.
           *>Add final caracter : ]
           STRING 
                 ' '               DELIMITED BY SIZE
                 WS-CLOSE-BRACKET  DELIMITED BY SIZE
           INTO WS-JSON-RESPONSE 
           WITH POINTER WS-PTR.


      * -- 3. Closing Cursor
           EXEC SQL CLOSE C1 END-EXEC.

           PERFORM SEND-JSON-RESPONSE

           
           EXEC CICS RETURN END-EXEC.

          
      *==========================================================*
      * SEND-JSON-ERROR
      * ROUTINE TO FORMAT AND SEND JSON ERROR TO THE WEB CLIENT  *
      * INPUT  : WS-RESP (PIC S9(8) COMP)
      *==========================================================*
       SEND-JSON-ERROR.

               MOVE WS-RESP TO WS-RESP-DISP
      * 2. Finding First position of no empty caracters
                 PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-RESP-DISP(WS-I:1) NOT = SPACE OR WS-I > 9
                 END-PERFORM

               INITIALIZE WS-JSON-RESPONSE
               STRING '{"Status": '    DELIMITED BY SIZE
                  WS-RESP-DISP(WS-I:)      DELIMITED BY SPACE
                  '}'                      DELIMITED BY SIZE
                  INTO WS-JSON-RESPONSE
               END-STRING

                MOVE 'OK' TO WS-STATUSTXT
                EXEC CICS WEB SEND
                    FROM(WS-JSON-RESPONSE)
                    MEDIATYPE('application/json')
                    CHARACTERSET('ISO-8859-1')
                    STATUSCODE(200)
                    STATUSTEXT(WS-STATUSTXT)
                END-EXEC

           EXIT.

      *==========================================================*
      * SEND-JSON-ERROR
      * ROUTINE TO FORMAT AND SEND JSON ERROR TO THE WEB CLIENT  *
      * INPUT  : D-MSG-ID.
      *          D-PHONE
      *          D-MSG-TEXT-DATA(1:D-MSG-TEXT-LEN)
      *          D-NB-SMS
      *          D-CREATED-AT
      *==========================================================*
       SEND-JSON-RESPONSE.


      *         DISPLAY WS-JSON-RESPONSE(1:WS-PTR)

                MOVE 'OK' TO WS-STATUSTXT
                EXEC CICS WEB SEND
                    FROM(WS-JSON-RESPONSE)
      *              LENGTH(WS-PTR)
                    MEDIATYPE('application/json')
                    CHARACTERSET('ISO-8859-1')
                    STATUSCODE(200)
                    STATUSTEXT(WS-STATUSTXT)
                END-EXEC

           EXIT.


           