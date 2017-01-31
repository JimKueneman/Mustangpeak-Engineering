unit lcc.types.can;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MTI_CAN_ADDRESS_PRESENT                = $00008000;                                // Address in the CAN Data present if set

  MTI_CAN_CAN                            = $00000000;                                // Frame Type CAN Control Message
  MTI_CAN_CID0                           = $07000000;                                // First 12 Bits of 48 bit Node ID
  MTI_CAN_CID1                           = $06000000;                                // 2rd 12 Bits of 48 bit Node ID
  MTI_CAN_CID2                           = $05000000;                                // 3nd 12 Bits of 48 bit Node ID
  MTI_CAN_CID3                           = $04000000;                                // Last 12 Bits of 48 bit Node ID
  MTI_CAN_CID4                           = $03000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID5                           = $02000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID6                           = $01000000;                                // non-OpenLCB Protocol

  MTI_CAN_RID                            = $00700000;                                // Reserve ID
  MTI_CAN_AMD                            = $00701000;                                // Alias Map Definition
  MTI_CAN_AME                            = $00702000;                                // Alias Mapping Enquiry
  MTI_CAN_AMR                            = $00703000;                                // Alias Map Reset Frame

  MTI_CAN_MASK                              = $0FFFF000;
  MTI_CAN_FRAME_TYPE_MASK                   = $0F000000;
  MTI_CAN_FRAME_TYPE_GENERAL                = $09000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY    = $0A000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START   = $0B000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME         = $0C000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END     = $0D000000;

  MTI_CAN_ADDRESSED_MASK                 = $00008000;
  MTI_CAN_SIMPLE_PROTOCOL_MASK           = $00010000;
  MTI_CAN_EVENT_PRESENT_MASK             = $00002000;

  MTI_CAN_INITIALIZATION_COMPLETE        = $09100000;                                // Databytes = Full Node ID
  MTI_CAN_VERIFY_NODE_ID_NUMBER_DEST     = $09488000;                                // Databytes = Destination Alias
  MTI_CAN_VERIFY_NODE_ID_NUMBER          = $09490000;                                //
  MTI_CAN_VERIFIED_NODE_ID_NUMBER        = $09170000;                                // {Optional Full Node ID}
  MTI_CAN_OPTIONAL_INTERACTION_REJECTED  = $09068000;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_CAN_TERMINATE_DUE_TO_ERROR         = $090A8000;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_CAN_PROTOCOL_SUPPORT_INQUIRY       = $09828000;                                // Databytes = Destination Alias
  MTI_CAN_PROTOCOL_SUPPORT_REPLY         = $09668000;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CAN_CONSUMER_IDENTIFY              = $098F4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFY_RANGE        = $094A4000;                                // Databytes = EventID with Mask
  MTI_CAN_CONSUMER_IDENTIFIED_UNKNOWN    = $094C7000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_SET        = $094C4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_CLEAR      = $094C5000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_RESERVED   = $094C6000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENDIFY              = $09914000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFY_RANGE        = $09524000;                                // Databytes = EventID with Mask
  MTI_CAN_PRODUCER_IDENTIFIED_UNKNOWN    = $09547000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_SET        = $09544000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_CLEAR      = $09545000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_RESERVED   = $09546000;                                // Databytes = EventID
  MTI_CAN_EVENTS_IDENTIFY_DEST           = $09968000;                                // Databytes = Destination Alias
  MTI_CAN_EVENTS_IDENTIFY                = $09970000;                                //
  MTI_CAN_EVENT_LEARN                    = $09594000;                                // Databytes = EventID
  MTI_CAN_PC_EVENT_REPORT                = $095B4000;                                // Databytes = EventID  (Infamouse PCER)

  MTI_CAN_SIMPLE_NODE_INFO_REQUEST       = $09DE8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_NODE_INFO_REPLY         = $09A08000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_SIMPLE_TRAIN_INFO_REQUEST      = $09DA8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_TRAIN_INFO_REPLY        = $099C8000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_TRACTION_PROTOCOL              = $095EA000;                                // Databyte = depends
  MTI_CAN_TRACTION_PROXY_PROTOCOL        = $091EA000;
  MTI_CAN_TRACTION_REPLY                 = $095E8000;                                // Databyte = depends
  MTI_CAN_TRACTION_PROXY_REPLY           = $091E8000;

  MTI_CAN_STREAM_INIT_REQUEST            = $09CC8000;
  MTI_CAN_STREAM_INIT_REPLY              = $09868000;
  MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND     = $0F000000;
  MTI_CAN_STREAM_PROCEED                 = $09888000;
  MTI_CAN_STREAM_COMPLETE                = $098A8000;

  MTI_CAN_DATAGRAM_OK_REPLY              = $09A28000;                                // Databytes = Destination Alias
  MTI_CAN_DATAGRAM_REJECTED_REPLY        = $09A48000;                                // Databytes = Destination Alias, Error Code

implementation

end.

