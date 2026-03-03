       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALANCE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * zSMSGateway - GET /balance handler
      *---------------------------------------------------------------*

       01  WS-CHARS.
           05  WS-NL        PIC X  VALUE X'25'.  *> EBCDIC Line Feed
           05  WS-CR        PIC X  VALUE X'0D'.  *> EBCDIC Carriage Ret
           05  WS-OPEN-BRACKET  PIC X VALUE X'BA'. *> [
           05  WS-CLOSE-BRACKET PIC X VALUE X'BB'. *> [

       01 WS-STATUSTXT PIC X(32) VALUE SPACES.

       01  WS-JSON-RESPONSE     PIC X(1000) VALUE SPACES.
       01  WS-JSON-LEN          PIC 9(04) BINARY.
       01  WS-PTR               PIC S9(9) BINARY VALUE 1.

       01 WS-USERID-NAME        PIC X(10)  VALUE 'UserID'.
       01 WS-USERID-NAME-LEN    PIC S9(8)  BINARY VALUE 6.       
       01 WS-USERID-LEN         PIC S9(8)  BINARY VALUE 10.
       01 WS-USERID-VAL         PIC X(10)  VALUE SPACES.

       01 WS-CREDIT-NAME        PIC X(6)   VALUE 'Credit'.
       01 WS-CREDIT-NAME-LEN    PIC S9(8)  BINARY VALUE 6.       
       01 WS-CREDIT-LEN         PIC S9(8)  BINARY VALUE 10.
       01 WS-CREDIT-VAL         PIC X(10)  VALUE SPACES.

       01  WS-RESP              PIC S9(8) BINARY.
       01  WS-RESP-DISP         PIC 9(6). 

       01  WS-RESP-USERID       PIC S9(8) BINARY.
       01  WS-RESP-CREDIT       PIC S9(8) BINARY.

       01 DB2-VARS.
          05 D-USERID-VAL       PIC S9(9) BINARY VALUE 0.
          05 D-CREDIT-VAL       PIC S9(9) BINARY VALUE 0.

       01  WS-NBSMS-DIS         PIC 9999.

       01  WS-AMOUNT-DISP       PIC ZZZZ9.

       01  WS-I                 PIC 9(4) BINARY VALUE 0.

       01 DB2-VARS.
          05 D-USER-ID          PIC S9(9) BINARY VALUE 0.
          05 D-CREDIT-AMOUNT    PIC S9(9) BINARY.

       01  WS-TEMP-EMAIL PIC X(10).


      * FOR SQL
       01 WS-DATA  PIC X(20) VALUE SPACES.
      * SQLCA NEEDED FOR BINDIND  
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.

           EXEC CICS WEB READ FORMFIELD(WS-USERID-NAME)
               NAMELENGTH(WS-USERID-NAME-LEN)
               VALUE(WS-USERID-VAL)
               VALUELENGTH(WS-USERID-LEN)
               RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-USERID

           EXEC CICS WEB READ FORMFIELD(WS-CREDIT-NAME)
               NAMELENGTH(WS-CREDIT-NAME-LEN)
               VALUE(WS-CREDIT-VAL)
               VALUELENGTH(WS-CREDIT-LEN)
               RESP(WS-RESP)
           END-EXEC.
           MOVE WS-RESP TO WS-RESP-CREDIT 

            IF WS-RESP-USERID = DFHRESP(NORMAL)
              DISPLAY 'USERID : ' WS-USERID-VAL(1:WS-USERID-LEN)
            END-IF 
            IF WS-RESP-CREDIT = DFHRESP(NORMAL)
              DISPLAY 'CREDIT : ' WS-CREDIT-VAL(1:WS-CREDIT-LEN)
            END-IF 

           IF WS-RESP-USERID = DFHRESP(NORMAL) AND 
              WS-RESP-CREDIT = DFHRESP(NORMAL)
              DISPLAY 'USERID AND CREDIT FOUND '

              *> Verify that they are numeric
              IF WS-USERID-VAL(1:WS-USERID-LEN) IS NUMERIC AND 
                 WS-CREDIT-VAL(1:WS-CREDIT-LEN) IS NUMERIC

                    DISPLAY 'UPDATING USERID: ' 
                       WS-USERID-VAL(1:WS-USERID-LEN)
                    DISPLAY 'WITH CREDIT: ' 
                       WS-CREDIT-VAL(1:WS-CREDIT-LEN)

                    EXEC SQL
                     UPDATE ZSMS_USERS
                     SET CREDIT_AMOUNT = CREDIT_AMOUNT + :WS-CREDIT-VAL
                     WHERE USER_ID = :WS-USERID-VAL
                    END-EXEC

                    IF SQLCODE = 100
                       *> USERID dont exist : create it
                       MOVE 'UNKNOW' TO WS-TEMP-EMAIL
                  EXEC SQL
                INSERT INTO ZSMS_USERS 
                (USER_ID, EMAIL, CREDIT_AMOUNT)
                VALUES (:WS-USERID-VAL, :WS-TEMP-EMAIL, :WS-CREDIT-VAL)
                  END-EXEC

                    IF SQLCODE = 0
                        DISPLAY 'User created'
                    ELSE
                       DISPLAY 'SQLCODE: ' SQLCODE
                    END-IF

                    ELSE

                    IF SQLCODE = 0
                        DISPLAY 'Credit update successful'
                    ELSE
                        DISPLAY 'SQL Error: ' SQLCODE
                    END-IF

                    END-IF

                    MOVE 0 TO WS-RESP
                    PERFORM SEND-JSON-ERROR

                    EXEC CICS RETURN END-EXEC  

              END-IF   

           END-IF   

           IF WS-RESP-USERID NOT = DFHRESP(NORMAL)
               MOVE WS-RESP TO WS-RESP-DISP
               DISPLAY 'ERROR : CICS FORMFIELD : ' WS-RESP-DISP

               PERFORM SEND-JSON-ERROR

               EXEC CICS RETURN END-EXEC       
           END-IF.

           DISPLAY 'BALANCE FOR USERID:' WS-USERID-VAL(1:WS-USERID-LEN)



           COMPUTE D-USER-ID = FUNCTION NUMVAL(WS-USERID-VAL)
           
           EXEC SQL
                 DECLARE C1 CURSOR FOR
                 SELECT    
                        CREDIT_AMOUNT
                 FROM ZSMS_USERS 
                 WHERE USER_ID = :D-USER-ID
           END-EXEC.

      * -- 1. Opening Cursor
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
                   INTO :D-CREDIT-AMOUNT
               END-EXEC

              IF SQLCODE = 0

                MOVE D-CREDIT-AMOUNT TO WS-AMOUNT-DISP
                DISPLAY 'BALANCE: ' D-CREDIT-AMOUNT

               STRING  WS-NL          DELIMITED BY SIZE
                  '{ '                DELIMITED BY SIZE
                  '"Balance": '       DELIMITED BY SIZE
                  WS-AMOUNT-DISP      DELIMITED BY SIZE        
                  WS-NL               DELIMITED BY SIZE                  
                  '},'                 DELIMITED BY SIZE

                  INTO WS-JSON-RESPONSE
                  WITH POINTER WS-PTR
               END-STRING


              END-IF

           END-PERFORM.

           *>remove last comma caracter  ","
           SUBTRACT 1 FROM WS-PTR.
           *>Add final caracter : ]
           STRING 
                 WS-NL DELIMITED BY SIZE
                 WS-CLOSE-BRACKET DELIMITED BY SIZE
           INTO WS-JSON-RESPONSE 
           WITH POINTER WS-PTR.

           PERFORM SEND-JSON-RESPONSE


      * -- 3. Closing cursor
           EXEC SQL CLOSE C1 END-EXEC.

           *> Send JSON Message depending on Status



           
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


      *        DISPLAY WS-JSON-RESPONSE(1:WS-PTR)

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


           