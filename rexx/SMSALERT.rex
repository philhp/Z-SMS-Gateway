/* REXX - Send URL Encoded Alert to zSMSGateway via Native Sockets */
parse arg da content userid
if da = "" | content = "" | userid = "" then do
  say "Usage: EXEC 'IBMUSER.REXX(SMSALERT)' 'DA CONTENT USERID'"
  exit 8
end

/* ---- Init Socket API ---- */
CALL EZASOKET 'INITAPI'

/* ---- Create socket ---- */
CALL EZASOKET 'SOCKET','AF_INET','SOCK_STREAM',0
sock = RETVAL

if sock < 0 then do
   say 'Socket creation failed'
   exit 12
end

/* ---- Build sockaddr ---- */
name = 'AF_INET 9081 127.0.0.1'

/* ---- Connect ---- */
CALL EZASOKET 'CONNECT',sock,name

if RETVAL <> 0 then do
   say 'Connect failed RC=' RETVAL
   exit 12
end

/* Construct URL Encoded Payload */
payload = 'DA='da'&Content='content'&UserID='userid

/* Construct complete HTTP request */
crlf = '0D0A'x

header = ,
'POST /messages HTTP/1.1'||crlf||,
'Host: localhost'||crlf||,
'Content-Type: application/x-www-form-urlencoded'||crlf||,
'Content-Length: 'length(payload)||crlf||crlf


request = header||payload

/* ---- Send ---- */
CALL EZASOKET 'SEND',sock,request,length(request),0

/* ---- Close ---- */
CALL EZASOKET 'CLOSE',sock
CALL EZASOKET 'TERMAPI'

say "SMS Alert sent"

exit 0
