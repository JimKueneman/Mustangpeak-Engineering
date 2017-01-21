unit hidapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unixtype;

const
{$IFDEF DARWIN}
  HidLib = 'hid.o';
{$ELSE}
  {$IFDEF UNIX}
  HidLib = 'hid.so';
  {$ELSE}
  HidLib = 'hid.dll';
  {$ENDIF}
{$ENDIF}

type
  Thid_device = DWord;
  Phid_device = ^Thid_device;

  Phid_device_info = ^Thid_device_info;
  Thid_device_info = record
    path                : ^char;
    vendor_id           : word;
    product_id          : word;
    serial_number       : ^wchar_t;
    release_number      : word;
    manufacturer_string : ^wchar_t;
    product_string      : ^wchar_t;
    usage_page          : word;
    usage               : word;
    interface_number    : integer;
    Next                : Phid_device_info;
  end;

  function hid_int(): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_exit(): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_enumerate(vendor_id: word; product_id: word): Phid_device_info; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  procedure hid_free_enumeration(devs: Phid_device_info); {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_open(vendor_id: word; product_id: word; serial_number: Pwchar_t): Phid_device; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_open_path(path: Pchar): Phid_device; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_write(hid_device: Phid_device; data: Pchar; length: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_read_timeout(hid_device: Phid_device; data: Pchar; length: size_t; milliseconds: integer): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_read(hid_device: Phid_device; data: Pchar; length: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_set_nonblocking(hid_device: Phid_device; nonblock: integer): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_send_feature_report(hid_device: Phid_device; data: Pchar; length: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_get_feature_report(hid_device: Phid_device; data: Pchar; length: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_close(hid_device: Phid_device): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_get_manufacturer_string(hid_device: Phid_device; _string: Pwchar_t; maxlen: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_get_product_string(hid_device: Phid_device; _string: Pwchar_t; maxlen: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_get_serial_number_string(hid_device: Phid_device; _string: Pwchar_t; maxlen: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_get_indexed_string(hid_device: Phid_device; string_index: integer; _string: Pwchar_t; maxlen: size_t): integer; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;
  function hid_error(): Pwchar_t; {$IFDEF WIN32}cdecl; {$ENDIF} external HidLib;

implementation

end.

