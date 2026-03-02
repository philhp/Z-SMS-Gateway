```text
      ____  __  __ ____   ____       _                           
 ____/ ___||  \/  / ___| / ___| __ _| |_ _____      ____ _ _   _ 
|_  /\___ \| |\/| \___ \| |  _ / _` | __/ _ \ \ /\ / / _` | | | |
 / /  ___) | |  | |___) | |_| | (_| | ||  __/\ V  V / (_| | |_| |
/___||____/|_|  |_|____/ \____|\__,_|\__\___| \_/\_/ \__,_|\__, |
                                                           |___/
```

## Z-SMS-Gateway is a REST API developed in COBOL for CICS TS. It bridges the gap between modern SMS platforms and the legendary transactional power of z/OS.

The project demonstrates that "Big Iron" systems can seamlessly integrate into contemporary microservices architectures by exposing secure, fast, and reliable JSON endpoints.

using international E164 Numbering plan.

Example
For Australia : 61 and 9 digits – without the + : 61412345678


## Tech Stack

- OS: z/OS 

- Monitor: CICS Transaction Server

- Database: IBM DB2 for z/OS

- Codepage mapping : IBM037 IBM01147 ISO_8859-1:1987 

- Language: Enterprise COBOL

- Protocol: HTTP/HTTPS + JSON (Custom Native Parsing Routine)


## Key Features

Native REST Integration: Leverages CICS Web Support to expose services directly without heavy middleware or Java.

Custom JSON Engine: A specialized COBOL routine for parsing and generating JSON payloads, optimized for EBCDIC/ASCII translation without relying on modern system overhead (No Java, No z/OS Connect).

DB2 Persistence: Uses DB2 to manage SMS delivery, logs, and credit management.


## Project Architecture

CICS Web Layer: Handles incoming HTTP requests and extracts the raw payload.

JSON Parser (Homegrown): Includes a native Codepage Translation for ASCII / EBCDIC mapping, ensuring data integrity across distributed environments.

Business Logic: Validates inputs, checks customer balance, and prepares DB2 calls.

Data Layer: Uses DB2 for persistent storage with full SQL integrity.


## Repository Structure

**/cbl/** : Source code for the REST Driver and Business Logic.

**/cics/** : Resource definitions (DFHCSDUP commands for Transactions, Programs, URIMAPs).

**/jcl/** :
BUILZ : Complete compilation and Link-edit JCL.
CSDINST : CICS Resource definition (CSD) installation.
DDLINT : DB2 Database and Table initialization.

## Security & Provider Credentials
To keep the source code provider-agnostic and secure, API credentials are never hardcoded. They are injected during the compilation phase using a COBOL Copybook and the `REPLACE` directive.

**Setup Instructions:**
1. Create a member `ZSMSPWD` in your PDS.
2. Update JCL BUILDZ SYSLIB at COMPSMS step to include your PDS
3. Define your SMS Provider credentials as follows (starting in **Column 12**):

```cobol
           REPLACE ==:USERNAME:== BY =='YOUR_PROVIDER_USER'==
                   ==:PASSWORD:== BY =='YOUR_PROVIDER_KEY'==
                   ==:SOA:== BY =='1299'==
                   ==:SENDERAPPID:== BY =='93'==.
```
SOA (Originating Address) :  is privided by your SMS provider
SENDERAPPID (Sender Application ID) : is privided by your SMS provider

## Quick Start

**Initialize DB2:** Run `jcl/DDLINST` to create the database environment.

**Setup CICS:** Run `jcl/CSDINST` to define the programs, transactions, and 3 URIMAPs (/messages, /history, /balance).

**Compile:** Run `jcl/BUILDZ` to compile and BIND the 3 programs.

**Refresh CICS:** Perform a `CEMT SET PROG(*) NEWCOPY` in CICS to load the new modules.


## Motivation
"Combining the legendary reliability of the Mainframe with the flexibility of JSON to meet modern web requirements."


## JCL Alerting Mechanism

This JCL demonstrates how to automate SMS alerts based on job outcomes.

**Step 1** (PRODSTEP) simulates a production workload.

**Step 2** (IFCHECK) uses JCL conditional logic to monitor the previous step. If PRODSTEP fails with a Return Code greater than 4, it triggers the SENDSMS step.

SENDSMS executes the SMSALERT REXX script to notify administrators.

```jcl
//IBMUSER JOB (ACCOUNT),'SMS ALERT',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*-------------------------------------------------------
//* STEP 1: PRODUCTION JOB (Exemple)
//*-------------------------------------------------------
//PRODSTEP  EXEC PGM=IDCAMS
//*
//*-------------------------------------------------------
//* STEP 2: ALERTE SMS IF STEP 1 FAILED (RC > 4)
//*-------------------------------------------------------
//IFCHECK   IF (PRODSTEP.RC > 4) THEN
//SENDSMS   EXEC PGM=IKJEFT01
//STEPLIB   DD DSN=TCPIP.SEZALOAD,DISP=SHR
//SYSTSPRT  DD SYSOUT=*
//SYSTSIN   DD *
 EXEC 'IBMUSER.REXX(SMSALERT)' '61412345678 "Job IDCAMS failed. RC>4" 1'
/*
//ENDIF
```

## 🔌 API Reference

### 1. Send SMS
**Endpoint:** `/messages`  
**Method:** `POST`
**Parameters:**
* `DA`: Destination Address (E.164 format).
* `Content`: The text message to be sent.
* `UserID`: integer value , User Identifiant (see ZSMS_USERS Table) .

**Response (Success - HTTP 200):**
```json
{ 
  "Status": 0,
  "MsgId": 2103861,
  "NbSMS": 1
}
```

### 2. SMS History 
**Endpoint:** `/history`  
**Method:** `GET`
**Parameters:**
* `UserID`: The unique identifier of the sender. (must be present on ZSMS_USERS table)

**Response (Success - HTTP 200):**
```json
[ { "Phone": 61412345678,"Content": "Test+SMS+from+Z%2FOS","NbSMS":    1,"Created": "2026-02-25-12.01.50.562953","Tracking": "0    "},
  { "Phone": 412345678,"Content": "Test+with+error","NbSMS": 0,"Created": "2026-02-25-10.47.32.308020","Tracking": "0    "} 
]
```

### 3. Check balance 
**Endpoint:** `/balance`  
**Method:** `GET`
**Parameters:**
* `UserID`: The unique identifier of the sender. (must be present on ZSMS_USERS table)

**Response (Success - HTTP 200):**
```json
[ 
{ "Balance":   500 }
]  
```

## Roadmap / TODO List
- [X] **Credit Deduction**: Automatically decrement user balance upon successful SMS delivery.
- [X] **Bulk Messaging**: Implement support for multi-recipient SMS sending in a single API call.
- [X] **Enhanced Tracking**: Integrate real-time delivery status (DLR) tracking within the /history endpoint.
- [X] **Batch Integration**: Develop a JCL utility (IKJEFT01/BPXBATCH) to trigger SMS alerts based on Job Step Return Codes (RC).
- [ ] **Balance Top-up**: Extend the /balance endpoint to allow manual or automated credit additions
- [ ] **Contact Management**: Build a dedicated module for managing customer address books.
- [ ] **Inbound SMS**: Develop a routine to capture and display incoming replies from customers (Two-way SMS).
- [ ] **Authentication**: Implement API Key validation for enhanced security.
- [ ] **Web Dashboard**: Create a simple web interface to visualize SMS history.

