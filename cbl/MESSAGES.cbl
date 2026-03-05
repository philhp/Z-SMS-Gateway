       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESSAGES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * zSMSGateway - POST /messages handler
      *---------------------------------------------------------------*

       01  WS-CHARS.
           05  WS-NL        PIC X  VALUE X'25'.  *> EBCDIC Line Feed
           05  WS-CR        PIC X  VALUE X'0D'.  *> EBCDIC Carriage Return

       01 WS-STATUSTXT PIC X(32) VALUE SPACES.

       01  WS-JSON-RESPONSE      PIC X(80) VALUE SPACES.
       01  WS-JSON-LEN           PIC 9(04) BINARY.
       01  WS-RESP-VAL           PIC 9(08).

       
       01  WS-BODY-BRUT          PIC X(10000).
       01  WS-BODY-LEN           PIC S9(8) BINARY VALUE 10000.
       01  WS-BODY-LEN-RECV      PIC S9(8) BINARY.
       01  WS-RESP               PIC S9(8) BINARY.
       01  WS-RESP-DISP          PIC --------9. 

       01  WS-FIELDS.
           05  WS-FIELD1           PIC X(1000).     
           05  WS-FIELD2           PIC X(1000).    
           05  WS-LEN-FIELD1       PIC 9(03) VALUE ZERO.
           05  WS-LEN-FIELD2       PIC 9(03) VALUE ZERO.

       01  WS-SMS-PATH          PIC X(250) VALUE SPACES.
       01  WS-SMS-PATH-LEN      PIC S9(8) BINARY.
       01  WS-POINTEUR          PIC S9(8) BINARY VALUE 1.
       01  WS-I                 PIC 9(4) BINARY VALUE 0.

       01 WS-SMS-ZONE.
           05 WS-SMS-TEL      PIC X(20) VALUE SPACES.
           05 WS-SMS-TXT      PIC X(1000) VALUE SPACES.
           05 WS-OUT-MSGID    PIC X(10) VALUE SPACES.
           05 WS-OUT-NBSMS    PIC X(10) VALUE SPACES.
           05 WS-SMS-RET      PIC S9(8) BINARY.

       01  WS-BULK-WORK.
           05  WS-PTR-DA           PIC 9(05) VALUE 1.
           05  WS-CURRENT-TEL      PIC X(20) VALUE SPACES.
           05  WS-DA-DELIM         PIC X(02).

       01 DB2-VARS.
          05 D-USER-ID       PIC S9(9) BINARY VALUE 1.
          05 D-MSG-ID        PIC S9(18) BINARY.
          05 D-PHONE         PIC X(20).
          05 D-NB-SMS        PIC S9(9) BINARY.
          05 D-STATUS        PIC S9(9) BINARY.
          05 D-CREDIT-AMOUNT PIC S9(9) BINARY. 
          05 D-TYPE          PIC X(2)  VALUE 'MT'. 
             
          *> STRUCTURE POUR LE VARCHAR(1000)
          05 D-MSG-TEXT.
             49 D-MSG-TEXT-LEN  PIC S9(4) BINARY.
             49 D-MSG-TEXT-DATA PIC X(1000).

       01  WS-DEBUG-HEXA.
           05  WS-HEX-CHARS        PIC X(16) VALUE '0123456789ABCDEF'.
           05  WS-DISPLAY-HEX      PIC X(100).
           05  WS-PTR              PIC 9(03).
           05  WS-OFF              PIC 9(03).
           05  WS-VAL              PIC 9(03).
           05  WS-CHAR-WORK.
               10 WS-CHAR-INT      PIC 9(04) BINARY.

       01  WS-PARSING-WORK.
           05 WS-PTR-PARSE         PIC 9(05) VALUE 1.
           05 WS-CURRENT-KV        PIC X(500) VALUE SPACES.
           05 WS-KV-LEN            PIC 9(05) VALUE 0.
           05 WS-KEY               PIC X(20) VALUE SPACES.
           05 WS-VALUE             PIC X(480) VALUE SPACES.
       01  WS-PARSING-RESULTS.
           05 WS-RES-DA            PIC X(10000) VALUE SPACES.
           05 WS-RES-DA-LEN        PIC 9(05) VALUE 0. 
           05 WS-RES-CONTENT       PIC X(500) VALUE SPACES.
           05 WS-RES-CONTENT-LEN   PIC 9(05) VALUE 0.
           05 WS-RES-USERID        PIC X(10) VALUE SPACES.
           05 WS-RES-USERID-LEN    PIC 9(05) VALUE 0.
           05 WS-RES-TYPE          PIC X(10) VALUE SPACES.
           05 WS-RES-TYPE-LEN      PIC 9(05) VALUE 0.           

      * ASCII Table (ISO_8859-1)   
       01  WS-CONV-DATA.
           05  WS-FROM-GRP.
      * -- Lettres A-Z (26 bytes)
               10  FILLER PIC X(26) VALUE 
               X'4142434445464748494A4B4C4D4E4F505152535455565758595A'.
      * -- Lettres a-z (26 bytes)
               10  FILLER PIC X(26) VALUE 
               X'6162636465666768696A6B6C6D6E6F707172737475767778797A'.
      * -- Chiffres 0-9 (10 bytes)
               10  FILLER PIC X(10) VALUE X'30313233343536373839'.
      * -- Symboles % . , ; : ! ? / ( ) @ # = + - Espace (17 octets)
               10  FILLER PIC X(17) VALUE 
               X'26252E2C3B3A213F2F282940233D2B2D20'. 
           *> 26+26+10+17 = 85          
           05  WS-ASCII REDEFINES WS-FROM-GRP PIC X(85).

      * EBCDIC Table (IBM01147)
           05  WS-TO-GRP.
      * -- EBCDIC A-Z (26 bytes)
               10  FILLER PIC X(26) VALUE 
               X'C1C2C3C4C5C6C7C8C9D1D2D3D4D5D6D7D8D9E2E3E4E5E6E7E8E9'.
      * -- EBCDIC a-z (26 bytes)
               10  FILLER PIC X(26) VALUE 
               X'818283848586878889919293949596979899A2A3A4A5A6A7A8A9'.
      * -- EBCDIC 0-9 (10 bytes)
               10  FILLER PIC X(10) VALUE X'F0F1F2F3F4F5F6F7F8F9'.
      * -- EBCDIC Symboles (17 bytes)
               10  FILLER PIC X(17) VALUE 
               X'506C4B6B5E7A5A6F614D5D7C7B7E4E6040'.
           *> 26+26+10+17 = 85
           05  WS-EBCDIC   REDEFINES WS-TO-GRP   PIC X(85). 





      * FOR SQL
       01 WS-DATA  PIC X(20) VALUE SPACES.
      * SQLCA NEEDED FOR BINDIND  
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.

           EXEC CICS WEB RECEIVE
               INTO(WS-BODY-BRUT)
               MAXLENGTH(WS-BODY-LEN)
               LENGTH(WS-BODY-LEN-RECV) 
               NOTRUNCATE
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE WS-RESP TO WS-RESP-DISP
               DISPLAY 'ERROR : CICS WEB RECEIVE: ' WS-RESP-DISP

               PERFORM SEND-JSON-ERROR

               EXEC CICS RETURN END-EXEC       
           END-IF.

           PERFORM 8000-DUMP-HEXA.

           *> Global conversion (ASCII -> EBCDIC)
           INSPECT WS-BODY-BRUT CONVERTING WS-ASCII TO WS-EBCDIC.

  

           *> Add delimiter '&' end of string
           ADD 1 TO WS-BODY-LEN-RECV
           MOVE '&' TO WS-BODY-BRUT(WS-BODY-LEN-RECV:1)

           DISPLAY 'BODY:' WS-BODY-BRUT(1:WS-BODY-LEN-RECV)

      * -- 1. Replace '+' to SPACES
      *     INSPECT WS-BODY-BRUT(1:WS-BODY-LEN-RECV)
      *         REPLACING ALL '+' BY ' '.

           PERFORM UNTIL WS-PTR-PARSE >= WS-BODY-LEN-RECV

                 INITIALIZE WS-CURRENT-KV WS-KEY WS-VALUE
      * -- A. Block extraction (ex: "DA=6")
                 UNSTRING WS-BODY-BRUT
                         DELIMITED BY '&'
                         INTO  WS-CURRENT-KV COUNT IN WS-KV-LEN
                         WITH POINTER WS-PTR-PARSE
                 END-UNSTRING
      
      * -- B. Try to find : Field / Value
                 IF WS-KV-LEN > 0
                          UNSTRING WS-CURRENT-KV(1:WS-KV-LEN) 
                              DELIMITED BY '='
                              INTO WS-KEY
                                   WS-VALUE
                          END-UNSTRING
      
                 END-IF
      
                 EVALUATE TRUE
                    WHEN WS-KEY(1:2) = 'DA'
                           MOVE WS-VALUE TO WS-RES-DA
                           *> Payload calcul 
                           COMPUTE WS-RES-DA-LEN = WS-KV-LEN - 3
                    WHEN WS-KEY(1:7) = 'Content'
                           MOVE WS-VALUE TO WS-RES-CONTENT
                           *> Payload calcul 
                           COMPUTE WS-RES-CONTENT-LEN = WS-KV-LEN - 8
                    WHEN WS-KEY(1:6) = 'UserID'
                           MOVE WS-VALUE TO WS-RES-USERID
                           *> Payload calcul 
                           COMPUTE WS-RES-USERID-LEN = WS-KV-LEN - 7
                    WHEN WS-KEY(1:4) = 'Type'
                           MOVE WS-VALUE TO WS-RES-TYPE
                           *> Payload calcul 
                           COMPUTE WS-RES-TYPE-LEN = WS-KV-LEN - 5                    
                 END-EVALUATE

           END-PERFORM.

           DISPLAY 'RES-DA:' WS-RES-DA(1:WS-RES-DA-LEN)
           DISPLAY 'RES-CNT:' WS-RES-CONTENT(1:WS-RES-CONTENT-LEN)
           DISPLAY 'RES-USERID:' WS-RES-USERID(1:WS-RES-USERID-LEN)
      * Check that Content AND UserID AND DA are present
           IF WS-RES-DA-LEN = 0 OR 
              WS-RES-CONTENT-LEN = 0 OR 
              WS-RES-USERID-LEN = 0

               DISPLAY "MANDATORY FIELD IS MISSING"
               MOVE 16 TO WS-RESP 
               PERFORM SEND-JSON-ERROR
               EXEC CICS RETURN END-EXEC          
           END-IF.


           DISPLAY 'WS-RES-DA-LEN: ' WS-RES-DA-LEN
           DISPLAY 'WS-RES-CONTENT-LEN: ' WS-RES-CONTENT-LEN
           DISPLAY 'WS-RES-USERID-LEN: ' WS-RES-USERID-LEN

      *SMS-MO Management (MO : Mobile Originated)
           IF WS-RES-TYPE(1:WS-RES-TYPE-LEN) = 'MO'

               DISPLAY 'Inbound SMS-MO reception'

               MOVE 1 TO WS-OUT-NBSMS
               MOVE 0 TO WS-SMS-RET
               MOVE WS-RES-DA(1:WS-RES-DA-LEN) TO WS-SMS-TEL
               MOVE WS-RES-CONTENT(1:WS-RES-CONTENT-LEN) TO WS-SMS-TXT
               MOVE 0 TO WS-OUT-MSGID
               MOVE 'MO' TO D-TYPE
               PERFORM SAVE-SMS-HISTORY

               MOVE 0 TO WS-RESP 
               PERFORM SEND-JSON-ERROR
               EXEC CICS RETURN END-EXEC  

           END-IF

      *Bulk messaging : 
      *SMS-MT Multi-recipient Sending (MT : Mobile Terminated)
           MOVE 1 TO WS-PTR-DA.

           PERFORM UNTIL WS-PTR-DA > WS-RES-DA-LEN
                INITIALIZE WS-CURRENT-TEL
                *> Split on %0D%0A 
                UNSTRING WS-RES-DA(1:WS-RES-DA-LEN)
                   DELIMITED BY '%0D%0A'
                   INTO WS-CURRENT-TEL
                   WITH POINTER WS-PTR-DA
                END-UNSTRING

                IF WS-CURRENT-TEL NOT = SPACES

                    PERFORM CHECK-USER-CREDIT

                    DISPLAY 'SENDING TO: ' WS-CURRENT-TEL

                    *> Send the SMS
                    MOVE   WS-RES-CONTENT TO WS-SMS-TXT
                    MOVE   WS-CURRENT-TEL TO WS-SMS-TEL
                    CALL 'SENDSMS' USING DFHEIBLK 
                                       DFHCOMMAREA 
                                       WS-SMS-ZONE

                    *> Log sent SMS data to DB2
                    PERFORM SAVE-SMS-HISTORY

                END-IF




           END-PERFORM.
      *Check if User have enought credit





           *> Send JSON Message depending on Status
           PERFORM SEND-JSON-RESPONSE.

           MOVE WS-SMS-RET TO WS-RESP
           MOVE WS-RESP TO WS-RESP-DISP
           DISPLAY 'RESP : ' WS-RESP-DISP
           DISPLAY 'MSGID : ' WS-OUT-MSGID
           DISPLAY 'NBSMS : ' WS-OUT-NBSMS


           EXEC CICS RETURN END-EXEC.

      *==========================================================*
      * 8000-DUMP-HEXA                                           *
      * PURPOSE: CONVERTS RAW BYTES TO HUMAN-READABLE HEX STRING *
      * INPUT  : WS-BODY-BRUT (FIRST 30 BYTES)                   *
      * OUTPUT : WS-DISPLAY-HEX (FORMATTED HEX STRING)           *
      *==========================================================*
       8000-DUMP-HEXA.
      * -- On affiche les 30 premiers octets pour comprendre
           MOVE SPACES TO WS-DISPLAY-HEX
           MOVE 1 TO WS-OFF
           PERFORM VARYING WS-PTR FROM 1 BY 1 UNTIL WS-PTR > 30
               *> Extraire l'octet et le convertir en valeur numerique
               MOVE 0 TO WS-CHAR-INT
               MOVE WS-BODY-BRUT(WS-PTR:1) TO WS-CHAR-WORK(2:1)
               
               *> Calculer les deux positions dans la table HEX-CHARS
               *> On divise par 16 pour le premier digit, modulo 16
               COMPUTE WS-VAL = WS-CHAR-INT / 16 + 1
               MOVE WS-HEX-CHARS(WS-VAL:1) TO WS-DISPLAY-HEX(WS-OFF:1)
               
             COMPUTE WS-VAL = FUNCTION MOD(WS-CHAR-INT, 16) + 1
             MOVE WS-HEX-CHARS(WS-VAL:1) TO WS-DISPLAY-HEX(WS-OFF + 1:1)
               
               MOVE ' ' TO WS-DISPLAY-HEX(WS-OFF + 2:1)
               ADD 3 TO WS-OFF
           END-PERFORM

           DISPLAY 'HEXA : ' WS-DISPLAY-HEX
           EXIT.


      *==========================================================*
      * CHECK-USER-CREDIT
      * Checks if the user has enough credit to send SMS
      * INPUT :  WS-RES-USERID
      *          WS-RES-USERID-LEN
      *==========================================================*
       CHECK-USER-CREDIT.

           *> Convertion UserID string to numeric
           COMPUTE D-USER-ID = 
               FUNCTION NUMVAL-C(WS-RES-USERID(1:WS-RES-USERID-LEN))
           
           *> SQL Query
           EXEC SQL
               SELECT CREDIT_AMOUNT
               INTO  :D-CREDIT-AMOUNT
               FROM   ZSMS_USERS                
               WHERE  USER_ID = :D-USER-ID
           END-EXEC

           *> Check SQL result
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-RESP-DISP
               DISPLAY 'SQLCODE: ' WS-RESP-DISP
               DISPLAY "SQL ERROR on ZSMS_USERS: " D-USER-ID
               MOVE 16 TO WS-RESP 
               PERFORM SEND-JSON-ERROR
               EXEC CICS RETURN END-EXEC
           END-IF

           SUBTRACT 1 FROM D-CREDIT-AMOUNT.

           *> Check if credit becomes negative
           IF D-CREDIT-AMOUNT < 0
               DISPLAY 'INSUFFICIENT CREDIT FOR: ' D-USER-ID
               MOVE 20 TO WS-RESP
               PERFORM SEND-JSON-ERROR
               EXEC CICS RETURN END-EXEC
           END-IF

           *> If OK, update the credit in the table
           EXEC SQL
               UPDATE ZSMS_USERS
               SET CREDIT_AMOUNT = :D-CREDIT-AMOUNT
               WHERE USER_ID = :D-USER-ID
           END-EXEC

           *> Check if the SQL update was successful

           IF SQLCODE NOT = 0                
               DISPLAY "ERROR: CREDIT UPDATE FAILED FOR : " D-USER-ID                
               MOVE 21 TO WS-RESP                
               PERFORM SEND-JSON-ERROR                
               EXEC CICS RETURN END-EXEC                
           END-IF

           EXIT.


      *==========================================================*
      * SAVE-SMS-HISTORY
      * LOGS SENT SMS DATA, INCLUDING STATUS AND MESSAGES ID TO DB2
      * INPUT  :  WS-SMS-TEL
      *           WS-SMS-TXT (eq WS-RES-CONTENT)
      *           WS-RES-CONTENT-LEN
      *           WS-SMS-RET
      *           WS-OUT-NBSMS
      *           WS-OUT-MSGID
      *           D-TYPE
      *==========================================================*

       SAVE-SMS-HISTORY.

                 *> Preparation data to DB2 access
                 MOVE WS-SMS-TEL TO D-PHONE
                 MOVE WS-SMS-RET TO D-STATUS        
      
                 IF WS-SMS-RET = 0 
                    *> IF SMS IS SENDED
                    COMPUTE D-NB-SMS = FUNCTION NUMVAL(WS-OUT-NBSMS)
                    COMPUTE D-MSG-ID = FUNCTION NUMVAL(WS-OUT-MSGID)
                 ELSE
                    MOVE 0 TO D-NB-SMS
                    MOVE 0 TO D-MSG-ID
                 END-IF
      
                 *> Content VARCHAR management
                 MOVE WS-SMS-TXT TO D-MSG-TEXT-DATA
                 MOVE WS-RES-CONTENT-LEN TO D-MSG-TEXT-LEN
      
                 EXEC SQL
                   INSERT INTO ZSMS_MESSAGES (
                      MSG_ID,
                      USER_ID,
                      PHONE_NUM,
                      TYPE,
                      MSG_TEXT,
                      NB_SMS,
                      STATUS
                   ) VALUES (
                      :D-MSG-ID,
                      :D-USER-ID,
                      :D-PHONE,
                      :D-TYPE,
                      :D-MSG-TEXT,
                      :D-NB-SMS,
                      :D-STATUS
                   )
                 END-EXEC
      
                 MOVE SQLCODE TO WS-RESP-DISP
                 DISPLAY 'SQLCODE: ' WS-RESP-DISP

           EXIT.


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
      * INPUT  : WS-OUT-MSGID    PIC X(10).
      *          WS-OUT-NBSMS    PIC X(5).
      *          WS-SMS-RET      PIC S9(8) BINARY. 
      *==========================================================*
       SEND-JSON-RESPONSE.

               MOVE WS-SMS-RET TO WS-RESP-DISP
      * 2. Finding First position of no empty caracters
                 PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-RESP-DISP(WS-I:1) NOT = SPACE OR WS-I > 9
                 END-PERFORM

               INITIALIZE WS-JSON-RESPONSE
               STRING '{ "Status": '    DELIMITED BY SIZE
                  WS-RESP-DISP(WS-I:)      DELIMITED BY SPACE
                  ','                      DELIMITED BY SIZE
                  WS-NL                    DELIMITED BY SIZE
                  '"MsgId": '              DELIMITED BY SIZE
                  WS-OUT-MSGID             DELIMITED BY SPACE
                  ','                      DELIMITED BY SIZE
                  WS-NL                    DELIMITED BY SIZE
                  '"NbSMS": '              DELIMITED BY SIZE
                  WS-OUT-NBSMS             DELIMITED BY SPACE
                  WS-NL                    DELIMITED BY SIZE
                  '}'                      DELIMITED BY SIZE
                  INTO WS-JSON-RESPONSE
               END-STRING

               DISPLAY WS-JSON-RESPONSE

                MOVE 'OK' TO WS-STATUSTXT
                EXEC CICS WEB SEND
                    FROM(WS-JSON-RESPONSE)
                    MEDIATYPE('application/json')
                    CHARACTERSET('ISO-8859-1')
                    STATUSCODE(200)
                    STATUSTEXT(WS-STATUSTXT)
                END-EXEC

           EXIT.


           