       IDENTIFICATION DIVISION.
       PROGRAM-ID. SENDSMS.

           COPY ZSMSPWD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables internes au sous-programme ici
       01  WS-HOST               PIC X(15) VALUE '10.20.0.138'.
       01  WS-PORT               PIC S9(8) BINARY VALUE 50408.

       01  WS-HTTP-STATUS        PIC S9(8) BINARY.
       01  WS-SESSTOKEN          PIC X(16).

       01  WS-RESPONSE-BUFFER    PIC X(2000).
       01  WS-RESP-LEN           PIC S9(8) BINARY VALUE 2000.
       01  WS-STATUSTEXT         PIC X(64).

       01  WS-SMS-PATH          PIC X(1000) VALUE SPACES.
       01  WS-SMS-PATH-LEN      PIC S9(8) BINARY.
       01  WS-POINTEUR          PIC S9(8) BINARY VALUE 1.      

       01 WS-CICS-ERRORS.
           05 WS-RESPONSE-CODE  PIC S9(8) BINARY.
           05 WS-RESP2-CODE     PIC S9(8) BINARY.
           05 WS-DISP-CODE      PIC ZZZ9.

       01 WS-EXTRACTION-TOOLS.
           05 WS-START-POS         PIC S9(8) BINARY.
           05 WS-EXTRACT-BUFFER    PIC X(100).
           *> Variables cibles finales
           05 WS-OUT-STATUS        PIC X(10).

       LINKAGE SECTION.

      * C'est ici que tu définis ce que le programme principal envoie
       01 LS-SMS-PARAM.
          05 LS-MOB-PHONE       PIC X(20).
          05 LS-CONTENT         PIC X(1000).
          05 LS-OUT-MSGID       PIC X(10).
          05 LS-OUT-NBSMS       PIC X(10).
          05 LS-RETOUR-CODE     PIC S9(8) BINARY.
             88 LS-SMS-OK       VALUE 00.
             88 LS-SMS-KO       VALUE 99.

       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LS-SMS-PARAM.

           DISPLAY 'START SENDSMS...'.

      * 1. Open connexion du SMS Provider
           EXEC CICS WEB OPEN
               HOST(WS-HOST)
               PORTNUMBER(WS-PORT)
               HTTP
               SESSTOKEN(WS-SESSTOKEN)
               RESP(WS-RESPONSE-CODE)
               RESP2(WS-RESP2-CODE)
           END-EXEC.
           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
              *> 1. Logger l'erreur pour l'exploitation (dans CEEMSG)
              DISPLAY 'ERROR CONNEXION - RESP: ' WS-RESPONSE-CODE
              DISPLAY 'RESP2: ' WS-RESP2-CODE
              MOVE -90 TO LS-RETOUR-CODE
                GOBACK
           END-IF.
      * 2. Sending GET request

           DISPLAY 'CONNECTION SUCCESSFUL '.

           MOVE SPACES TO WS-SMS-PATH.
           MOVE 1      TO WS-POINTEUR.
           STRING '/dispatcher/httpconnectserver/' DELIMITED BY SIZE
                  'FdC_MOB'              DELIMITED BY SIZE
                  '?UserName='           DELIMITED BY SIZE
                  :USERNAME:             DELIMITED BY SIZE
                  '&Password='           DELIMITED BY SIZE
                  :PASSWORD:             DELIMITED BY SIZE
                  '&DA='                 DELIMITED BY SIZE
                  LS-MOB-PHONE           DELIMITED BY SPACE
                  '&SOA='                DELIMITED BY SIZE
                  :SOA:                  DELIMITED BY SIZE
                  '&MsgSubType=0'        DELIMITED BY SIZE
                  '&SenderAppId='        DELIMITED BY SIZE
                  :SENDERAPPID:          DELIMITED BY SIZE
                  '&Content='            DELIMITED BY SIZE
                  LS-CONTENT             DELIMITED BY SPACE
                  '&Flags=3'             DELIMITED BY SIZE
                  INTO WS-SMS-PATH
                  WITH POINTER WS-POINTEUR
           END-STRING

           COMPUTE WS-SMS-PATH-LEN = WS-POINTEUR - 1.

           DISPLAY 'WS-SMS-PATH: ' WS-SMS-PATH.

           EXEC CICS WEB CONVERSE
               SESSTOKEN(WS-SESSTOKEN)
               PATH(WS-SMS-PATH)
               PATHLENGTH(WS-SMS-PATH-LEN)        
               METHOD(DFHVALUE(GET))
               CHARACTERSET('iso-8859-1')
               INTO(WS-RESPONSE-BUFFER)
               MAXLENGTH(2000)
               TOLENGTH(WS-RESP-LEN)
               STATUSCODE(WS-HTTP-STATUS) 
               STATUSTEXT(WS-STATUSTEXT)
               RESP(WS-RESPONSE-CODE)
           END-EXEC.
           MOVE WS-RESPONSE-CODE TO WS-DISP-CODE
           DISPLAY 'RESPONSE CODE: ' WS-DISP-CODE.

           DISPLAY '--- SERVER RESPONSE ---'.
           *> On affiche uniquement la partie utile du buffer
           DISPLAY WS-RESPONSE-BUFFER(1:WS-RESP-LEN).
           DISPLAY 'RECEIVED LENGTH: ' WS-RESP-LEN.

           EXEC CICS WEB CLOSE
               SESSTOKEN(WS-SESSTOKEN)
           END-EXEC.


      *--- STATUS EXTRACTION 
           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
              MOVE -91 TO LS-RETOUR-CODE
              GOBACK
           END-IF.

      *--- EXTRACTION DU STATUS        
           MOVE 0 TO WS-START-POS
           INSPECT WS-RESPONSE-BUFFER(1:WS-RESP-LEN) 
               TALLYING WS-START-POS 
               FOR CHARACTERS BEFORE INITIAL 'Status='
           *> INSPECT TALLYING a compté le Nb carac AVANT Status=
           IF WS-START-POS < WS-RESP-LEN
               ADD 8 TO WS-START-POS
               UNSTRING WS-RESPONSE-BUFFER(WS-START-POS:WS-RESP-LEN)
                   DELIMITED BY '<'
                   INTO WS-OUT-STATUS
               DISPLAY 'EXTRACTED STATUS: ' WS-OUT-STATUS
           ELSE
               DISPLAY 'STATUS NOT FOUND IN HTML !'
               MOVE -92 TO LS-RETOUR-CODE
               GOBACK
           END-IF.

      *> Conversion de la chaine "-4" en nombre -4
               COMPUTE LS-RETOUR-CODE = FUNCTION NUMVAL(WS-OUT-STATUS)

      *--- 3. TEST LS-RETOUR-CODE (NUMERIQUE VALUE)
           IF LS-RETOUR-CODE < 0
               DISPLAY 'PROVIDER SMS ERROR : ' WS-OUT-STATUS
               *> USING NEW PROVIDER ERROR CODE
               GOBACK
           END-IF.

      *--- 4. EXTRACTION DU MSGID
           MOVE 0 TO WS-START-POS
           INSPECT WS-RESPONSE-BUFFER(1:WS-RESP-LEN) 
               TALLYING WS-START-POS 
               FOR CHARACTERS BEFORE INITIAL 'MsgId='
           
           IF WS-START-POS < WS-RESP-LEN
               ADD 7 TO WS-START-POS *> "MsgId=" fait 6 car. + 1
               UNSTRING WS-RESPONSE-BUFFER(WS-START-POS:WS-RESP-LEN)
                   DELIMITED BY '<'
                   INTO LS-OUT-MSGID
               DISPLAY 'EXTRACTED MSGID: ' LS-OUT-MSGID
           ELSE
               DISPLAY 'MSGID NOT FOUND IN HTML !'
           END-IF.

      *--- 5. EXTRACTION DU NBSMS
           MOVE 0 TO WS-START-POS
           INSPECT WS-RESPONSE-BUFFER(1:WS-RESP-LEN) 
               TALLYING WS-START-POS 
               FOR CHARACTERS BEFORE INITIAL 'NbSMS='
           
           IF WS-START-POS < WS-RESP-LEN
               ADD 7 TO WS-START-POS *> "NbSMS=" fait 6 car. + 1
               UNSTRING WS-RESPONSE-BUFFER(WS-START-POS:WS-RESP-LEN)
                   DELIMITED BY '<'
                   INTO LS-OUT-NBSMS
               DISPLAY 'EXTRACTED NBSMS : ' LS-OUT-NBSMS
           ELSE
               DISPLAY 'NBSMS NOT FOUND IN HTML !'
           END-IF.
           
           *> LS-RETOUR-CODE ALREADY CONTENT O USING PROVIDER SMS

           GOBACK.