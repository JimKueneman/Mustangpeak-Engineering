unit lcc.types;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils;

const
  LCC_BYTE_COUNT = 1024;       // This is longest data structure defined in Lcc
  MAX_EVENT_LEN  = 8;
  MAX_NODEID_LEN = 6;
  MAX_DATAGRAM_LENGTH = 72;
  MAX_CONFIG_MEM_READWRITE_SIZE = 64;

  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

type
  TLccByteArray = array[0..LCC_BYTE_COUNT-1] of Byte;

  TDatagramArray = array[0..MAX_DATAGRAM_LENGTH-1] of Byte;
  PDatagramArray = ^TDatagramArray;

  TLccConfigDataType = (cdt_String, cdt_Int, cdt_EventID, cdt_Bit);

  // Nodes
  TNodeID = array[0..1] of DWORD;
  PNodeID = ^TNodeID;

  // Events
  TEventID = array[0..MAX_EVENT_LEN-1] of Byte;
  PEventID = ^TEventID;
  TEventState = (evs_Unknown, evs_Valid, evs_InValid);

  TDynamicByteArray = array of Byte;

  // SNIP
  TSimpleNodeInfoPacked = array of Byte;

  // Traction
  TFunctionStatesArray = array[0..28] of Word;

  TNodeIDRec = record
    ID: TNodeID;
    Alias: Word;
  end;

const
  // MTI
  // Raw MTI
  MTI_ADDRESSED_MASK                 = $0008;
  MTI_SIMPLE_PROTOCOL_MASK           = $0010;
  MTI_EVENT_PRESENT_MASK             = $0002;

  MTI_INITIALIZATION_COMPLETE        = $0100;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $0488;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $0490;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $0170;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $0068;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $00A8;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_PROTOCOL_SUPPORT_INQUIRY       = $0828;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $0668;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CONSUMER_IDENTIFY              = $08F4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $04A4;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $04C7;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $04C4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $04C5;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $04C6;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $0914;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $0524;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $0547;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $0544;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $0545;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $0546;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $0968;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $0970;                                //
  MTI_EVENT_LEARN                    = $0594;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $05B4;                                // Databytes = EventID  (Infamouse PCER)

  MTI_SIMPLE_NODE_INFO_REQUEST       = $0DE8;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $0A08;                                // Databytes = Destination Alias, ACDI Data

  MTI_SIMPLE_TRAIN_INFO_REQUEST      = $0DA8;                                // Databytes = Destination Alias
  MTI_SIMPLE_TRAIN_INFO_REPLY        = $09C8;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_PROTOCOL              = $05EA;                                // Databyte = depends
  MTI_TRACTION_REPLY                 = $05E8;                                // Databyte = depends

  MTI_REMOTE_BUTTON_REQUEST          = $0948;
  MTI_REMOTE_BUTTON_REPLY            = $0949;

  MTI_STREAM_INIT_REQUEST            = $0CC8;
  MTI_STREAM_INIT_REPLY              = $0868;
  MTI_STREAM_SEND                    = $1F88;
  MTI_STREAM_PROCEED                 = $0888;
  MTI_STREAM_COMPLETE                = $08A8;

  MTI_DATAGRAM                       = $1C48;
  MTI_DATAGRAM_OK_REPLY              = $0A28;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $0A48;                                // Databytes = Destination Alias, Error Code

  DATAGRAM_OK_ACK_REPLY_PENDING      = $80;

  STREAM_REPLY_CONTENT_TYPE          = $01;                // LSB = 1 = first 6 bytes in data are UID of data type in stream
  STREAM_REPLY_UNEXPECTED_ERROR      = $02;                // Bit 2 = 1 the Stream was Rejected, Out of order, or other "should not happen" error
  STREAM_REPLY_PERMANENT_ERROR       = $40;                // Bit 6 = 1 = if STREAM_REPLY_ACCEPT = 1 then this is the error type where 1 = permanent
  STREAM_REPLY_ACCEPT                = $80;                // MSB = 1 = Accept

  STREAM_REPLY_ERROR_LOGGED          = $01;                // Error was logged
  STREAM_REPLY_INVALID_REQUEST       = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_SOURCE_NOT_PERMITTED  = $40;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_STREAM_NOT_ACCEPTED   = $80;                // if Error is permanent then these are the possible reasons


const
  TCP_FLAG_LCC_MESSAGE  = $8000;        //  Link Message = 0
  TCP_FLAG_CHAINING     = $4000;
  // $2000, $1000 reserved
  TCP_FLAG_MULTI_PART   = $0C00;        // $0000 = Single part, $0020 = First part, $0030 = center part, $0040 = last part
  // Rest are reserved

//  MAX_LCC_TCP_MESSAGE_DATA = 253;
  TCP_MESSAGE_PREAMBLE_MAX = 14;      // 2 - MTI, 6 - Source ID, 6 - Dest ID
  TCP_MESSAGE_PREAMBLE_MIN = 8;       // 2 - MTI, 6 - Source ID
  TCP_HEADER_ONLY_MAX = 17;
//  MAX_HEADER_CONTRIBUTION_TO_SIZE_FIELD_LEN = 12;

//  MAX_LCC_TCP_FRAME_LEN = MAX_LCC_TCP_MESSAGE_DATA + MAX_LCC_TCP_MESSAGE_PREAMBLE;    // Max frame sizse for a TCP message with Header and all included
//  MAX_TCP_MESSAGE_ONLY_LEN = MAX_LCC_TCP_FRAME_LEN - MAX_HEADER_ONLY_LEN;               // Max frame size for just the Lcc Message itself


const
  NULL_NODE_ID: TNodeID = (0, 0);
  NULL_NODE_ID_REC: TNodeIDRec = (ID:(0, 0); Alias: 0);

const
  NULL_EVENT_ID : TEventID = (0, 0, 0, 0, 0, 0, 0, 0);
  EVENT_EMERGENCY_STOP       : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $FF);
  EVENT_NEW_LOG_ENTRY        : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $F8);
  EVENT_IDENT_BUTTON_PRESSED : TEventID = ($01, $00, $00, $00, $00, $00, $FE, $00);
  EVENT_DUPLICATE_ID_DETECTED: TEventID = ($01, $10, $00, $00, $00, $00, $02, $01);
  EVENT_IS_TRAIN             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $03);
  EVENT_IS_PROXY             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $04);
  EVENT_DELIVERS_CLOCK       : TEventID = ($01, $01, $00, $00, $00, $00, $05, $01);

// Traction Protocol
const
  TRACTION_FLAGS_ALIAS_INCLUDED =  $01;

  TRACTION_SPEED_DIR                  = $00;
  TRACTION_FUNCTION                   = $01;
  TRACTION_E_STOP                     = $02;

  TRACTION_QUERY_SPEED                = $10;
  TRACTION_QUERY_FUNCTION             = $11;

  TRACTION_CONTROLLER_CONFIG          = $20;
  TRACTION_CONTROLLER_CONFIG_ASSIGN   = $01;
  TRACTION_CONTROLLER_CONFIG_RELEASE  = $02;
  TRACTION_CONTROLLER_CONFIG_QUERY    = $03;
  TRACTION_CONTROLLER_CONFIG_NOTIFY   = $04;
  TRACTION_CONTROLLER_CONFIG_REPLY_OK = $00;
  TRACTION_CONTROLLER_CONFIG_REPLY_FAIL = $FF;

  TRACTION_CONSIST                    = $30;
  TRACTION_CONSIST_ATTACH             = $01;
  TRACTION_CONSIST_DETACH             = $02;
  TRACTION_CONSIST_QUERY              = $03;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed

// Datagram
const
  REJECTED                        = $0000;
  REJECTED_PERMANENT_ERROR        = $1000;
  REJECTED_INFORMATION_LOGGED     = $1010;
  REJECTED_SOURCE_NOT_PERMITTED   = $1020;
  REJECTED_DATAGRAMS_NOT_ACCEPTED = $1040;
  REJECTED_BUFFER_FULL            = $2000;
  REJECTED_OUT_OF_ORDER           = $6000;
  REJECTED_NO_RESEND_MASK         = $1000;
  REJECTED_RESEND_MASK            = $2000;
  REJECTED_TRANSPORT_ERROR_MASK   = $4000;

  DATAGRAM_PROTOCOL_LOGREQUEST             = $01;
  DATAGRAM_PROTOCOL_LOGREPLY               = $02;
  DATAGRAM_PROTOCOL_CONFIGURATION          = $20;
  DATAGRAM_PROTOCOL_REMOTEBUTTON           = $21;
  DATAGRAM_PROTOCOL_DISPLAY                = $28;
  DATAGRAM_PROTOCOL_TRAINCONTROL           = $30;
  DATAGRAM_PROTOCOL_TWOBYTE                = $E0;
  DATAGRAM_PROTOCOL_SIXBYTE                = $F0;

// Memory Configuration Protocol
const
  MCP_WRITE                           = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_STREAM                    = $20;
  MCP_READ                            = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_READ_STREAM                     = $60;
  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask
  MCP_READ_REPLY                      = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_WRITE_REPLY                     = $10;
  MCP_READ_STREAM_REPLY               = $70;

  MCP_READ_OK                         = $50;
  MCP_READ_ERROR                      = $58;
  MCP_WRITE_OK                        = $10;
  MCP_WRITE_ERROR                     = $18;

  MCP_CDI                             = $03;                                    // Address space = CDI ($FF) access Mask
  MCP_ALL                             = $02;                                    // Address space = All ($FE) access Mask
  MCP_CONFIGURATION                   = $01;                                    // Address space = Basic Configuration ($FD) access Mask
  MCP_NONE                            = $00;                                    // Use the optional {Space} byte in the datagram to defin the address space

  MCP_OP_GET_CONFIG                  = $80;                                     // MemoryConfigurationProtocol Operation - Get Configuration
  MCP_OP_GET_CONFIG_REPLY            = $82;                                     // MemoryConfigurationProtocol Operation - Get Configuration Reply
  MCP_OP_GET_ADD_SPACE_INFO          = $84;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info
  MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY     = $87;
  MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY = $86;                            // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8E;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_FREEZE                      = $A0;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MCP_OP_INDICATE                    = $A4;                                     // MemoryConfigurationProtocol Operation - Indicate
  MCP_OP_RESETS                      = $A8;                                     // MemoryConfigurationProtocol Operation - Resets

  // Memory Space Identifiers
  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_FDI                            = $FA;                                     // MemorySpaceIdentifier - Access the Traction Functions definable Info
  MSI_FUNCTION_CONFIG                = $F9;                                     // MemorySpaceIdentifier = Access the Traction Function State Information

// Protocol Identifier Protocol
const
  PIP_PIP                            = $800000000000;
  PIP_DATAGRAM                       = $400000000000;
  PIP_STREAM                         = $200000000000;
  PIP_MEMORY_CONFIG                  = $100000000000;
  PIP_RESERVATION                    = $080000000000;
  PIP_EVENT_EXCHANGE                 = $040000000000;
  PIP_IDENTIFCIATION                 = $020000000000;
  PIP_TEACH_LEARN                    = $010000000000;
  PIP_REMOTE_BUTTON                  = $008000000000;
  PIP_ABBREVIATED_CDI                = $004000000000;
  PIP_DISPLAY                        = $002000000000;
  PIP_SIMPLE_NODE_INFO               = $001000000000;
  PIP_CDI                            = $000800000000;
  PIP_TRACTION                       = $000400000000;
  PIP_FDI                            = $000200000000;
  PIP_SIMPLE_TRAIN_NODE_INFO         = $000080000000;
  PIP_FUNCTION_CONFIGURATION         = $000040000000;

  PIP_UNASSIGNED                     = $0000FFFFFFF0;
  PIP_RESERVED                       = $00000000000F;

  STR_PIP_PIP                        = 'Protocol Identification Protocol';
  STR_PIP_DATAGRAM                   = 'Datagram Protocol';
  STR_PIP_STREAM                     = 'Stream Protocol';
  STR_PIP_MEMORY_CONFIG              = 'Memory Configuration Protocol';
  STR_PIP_RESERVATION                = 'Reservation Protocol';
  STR_PIP_EVENT_EXCHANGE             = 'Event Exchange Protocol';
  STR_PIP_IDENTIFCIATION             = 'Identification Protocol';
  STR_PIP_TEACH_LEARN                = 'Teach/Learn Protocol';
  STR_PIP_REMOTE_BUTTON              = 'Remote Button Protocol';
  STR_PIP_ABBREVIATED_CDI            = 'Abbreviated CDI Protocol';
  STR_PIP_DISPLAY                    = 'Display Protocol';
  STR_PIP_SIMPLE_NODE_ID             = 'Simple Node ID (SNII/SNIP) Protocol';
  STR_PIP_CDI                        = 'Configuration Description Information (CDI) Protocol';
  STR_PIP_TRACTION                   = 'Traction Protocol';
  STR_PIP_FDI                        = 'Function Description Information (FDI) Protocol';
  STR_PIP_TRACTION_PROTOCOL          = 'Traction Proxy Protocol';

// ACDI
ACDI_MFG_SIZE_VERSION                    = 1;
  ACDI_MFG_SIZE_MANUFACTURER               = 41;
  ACDI_MFG_SIZE_MODEL                      = 41;
  ACDI_MFG_SIZE_HARDWARE_VERSION           = 21;
  ACDI_MFG_SIZE_SOFTWARE_VERSION           = 21;
  ACDI_MFG_SIZE                            = ACDI_MFG_SIZE_VERSION + ACDI_MFG_SIZE_MANUFACTURER + ACDI_MFG_SIZE_MODEL + ACDI_MFG_SIZE_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_SIZE_VERSION                   = 1;
  ACDI_USER_SIZE_NAME                      = 63;
  ACDI_USER_SIZE_DESCRIPTION               = 64;
  ACDI_USER_SIZE                           = ACDI_USER_SIZE_VERSION + ACDI_USER_SIZE_NAME + ACDI_USER_SIZE_DESCRIPTION;

  ACDI_MFG_OFFSET_VERSION                   = 0;
  ACDI_MFG_OFFSET_MANUFACTURER              = ACDI_MFG_SIZE_VERSION;
  ACDI_MFG_OFFSET_MODEL                     = ACDI_MFG_OFFSET_MANUFACTURER + ACDI_MFG_SIZE_MANUFACTURER;
  ACDI_MFG_OFFSET_HARDWARE_VERSION          = ACDI_MFG_OFFSET_MODEL +  ACDI_MFG_SIZE_MODEL;
  ACDI_MFG_OFFSET_SOFTWARE_VERSION          = ACDI_MFG_OFFSET_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_OFFSET_VERSION                  = 0;
  ACDI_USER_OFFSET_NAME                     =  ACDI_USER_SIZE_VERSION;
  ACDI_USER_OFFSET_DESCRIPTION              =  ACDI_USER_OFFSET_NAME + ACDI_USER_SIZE_NAME;

  MCO_WRITE_UNDER_MASK               = $8000;                                   // MemoryConfigurationOptions - Write under mask supported
  MCO_UNALIGNED_READS                = $4000;                                   // MemoryConfigurationOptions - Unaligned memory Reads supported
  MCO_UNALIGNED_WRITES               = $2000;                                   // MemoryConfigurationOptions - Unaligned memory Writes supported
  MCO_ACDI_MFG_READS                 = $0800;                                   // MemoryConfigurationOptions - Address Space 0xFC supported (ACDI Manufacturer Area) for reads
  MCO_ACDI_USER_READS                = $0400;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for reads
  MCO_ACDI_USER_WRITES               = $0200;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for writes
  MCO_RESERVED                       = $1FFF;

  MCWL_ONE_BYTE                      = $80;                                     // MemoryConfigurationWriteLength - 1 Byte Write Supported
  MCWL_TWO_BYTE                      = $40;                                     // MemoryConfigurationWriteLength - 2 Byte Write Supported
  MCWL_FOUR_BYTE                     = $20;                                     // MemoryConfigurationWriteLength - 4 Byte Write Supported
  MCWL_64_BYTE                       = $10;                                     // MemoryConfigurationWriteLength - 64 Byte (exactly) Write Supported
  MCWL_ARBITRARY_BYTE                = $02;                                     // MemoryConfigurationWriteLength - Any Number of Byte Write Supported
  MCWL_STREAM_WRITE_SUPPORTED        = $01;                                     // MemoryConfigurationWriteLength - Stream Write Supported
  MCWL_RESERVED                      = $0C;

var
  Max_Allowed_Datagrams: Integer;
  AllocatedDatagrams: Integer;
  AllocatedMessages: Integer;


implementation

initialization
  AllocatedDatagrams := 0;
  AllocatedDatagrams := 0;
  Max_Allowed_Datagrams := 4096;   // crazy large by default

end.

