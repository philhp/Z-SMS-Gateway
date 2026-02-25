       IDENTIFICATION DIVISION.
       PROGRAM-ID. HISTORY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * zSMSGateway - POST /messages handler
      *---------------------------------------------------------------*

       01  WS-CHARS.
           05  WS-NL        PIC X  VALUE X'25'.  *> EBCDIC Line Feed
           05  WS-CR        PIC X  VALUE X'0D'.  *> EBCDIC Carriage Ret
           05  WS-OPEN-BRACKET  PIC X VALUE X'BA'. *> [
           05  WS-CLOSE-BRACKET PIC X VALUE X'BB'. *> [

       01  WS-STATUSTXT PIC X(32) VALUE SPACES.


       01  WS-JSON-RESPONSE      PIC X(5000) VALUE SPACES.
       01  WS-JSON-LEN           PIC 9(04) BINARY.
       01  WS-PTR               PIC S9(9) BINARY VALUE 1.

       01 WS-USERID-NAME         PIC X(10)  VALUE 'UserID'.
       01 WS-USERID-LEN          PIC S9(8)  COMP VALUE 6.
       01 WS-USERID-VAL         PIC X(10)  VALUE SPACES.
       01 WS-VAL-LEN            PIC S9(8)  COMP.

       01  WS-RESP               PIC S9(8) COMP.
       01  WS-RESP-DISP          PIC --------9. 

       01  WS-NBSMS-DIS          PIC ZZZ9.

       01  WS-I                 PIC 9(4) BINARY VALUE 0.


       01 DB2-VARS.
          05 D-USER-ID      PIC S9(9) BINARY VALUE 0.
          05 D-MSG-ID       PIC S9(18) BINARY.
          05 D-PHONE        PIC X(20).
          05 D-NB-SMS       PIC S9(9) BINARY.
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

           EXEC CICS WEB READ FORMFIELD(WS-USERID-NAME)
               NAMELENGTH(WS-USERID-LEN)
               VALUE(WS-USERID-VAL)
               VALUELENGTH(WS-USERID-LEN)
               RESP(WS-RESP)
           END-EXEC.


           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE WS-RESP TO WS-RESP-DISP
               DISPLAY 'ERROR : CICS FORMFIELD : ' WS-RESP-DISP

               PERFORM SEND-JSON-ERROR

               EXEC CICS RETURN END-EXEC       
           END-IF.

           DISPLAY 'WS-RES-USERID:' WS-USERID-VAL(1:WS-USERID-LEN)



           COMPUTE D-USER-ID = FUNCTION NUMVAL(WS-USERID-VAL)


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
                 WHERE USER_ID = :D-USER-ID
                 ORDER BY CREATED_AT DESC
           END-EXEC.

      * -- 1. OUVERTURE DU CURSEUR
           EXEC SQL OPEN C1 END-EXEC.

           MOVE 1 TO WS-PTR

           *>Add caracter : [ + SPACE
           STRING 
                 WS-OPEN-BRACKET DELIMITED BY SIZE
                 ' '             DELIMITED BY SIZE
           INTO WS-JSON-RESPONSE 
           WITH POINTER WS-PTR

      * -- 2. BOUCLE DE LECTURE
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


      * -- 3. FERMETURE
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


           