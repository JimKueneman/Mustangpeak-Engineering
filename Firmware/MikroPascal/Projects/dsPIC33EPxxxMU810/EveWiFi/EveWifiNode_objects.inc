
const Screen1_Labels:     array[005] of TLabelPtr;      code; far; external;
const Screen1_EveButtons: array[002] of TEveButtonPtr;  code; far; external;
const Screen1_EveTexts:   array[001] of TEveTextPtr;    code; far; external;

var   Screen1: TScreen; external;


var   LabelIP_Caption: string[32]; external;
var   LabelIP:         TLabel;     external;

var   LabelMAC_Caption: string[32]; external;
var   LabelMAC:         TLabel;     external;

var   LabelMask_Caption: string[32]; external;
var   LabelMask:         TLabel;     external;

var   LabelGateway_Caption: string[32]; external;
var   LabelGateway:         TLabel;     external;

var   LabelStatus_Caption: string[32]; external;
var   LabelStatus:         TLabel;     external;

var   EveButtonDisconnect_Caption: string[10]; external;
var   EveButtonDisconnect_OnClick: TEvent;     external;
var   EveButtonDisconnect:         TEveButton; external;

var   EveButtonConnect_Caption: string[7];  external;
var   EveButtonConnect_OnClick: TEvent;     external;
var   EveButtonConnect:         TEveButton; external;

var   EveTextStatusBar_Caption: string[254]; external;
var   EveTextStatusBar:         TEveText;    external;
const Screen1_Labels:     array[005] of TLabelPtr;      code; far; external;
const Screen1_EveButtons: array[002] of TEveButtonPtr;  code; far; external;
const Screen1_EveTexts:   array[001] of TEveTextPtr;    code; far; external;


var CurrentScreen: TScreenPtr; external;

// Table of object draw handlers
// Use object type constants to access coresponding object draw handler
const DrawHandlerTable: array[44] of TPointer; external;
// ~Table of draw handler pointers
