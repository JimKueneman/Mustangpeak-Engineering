unit file_utilities;

// Version 0.1
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimkueneman@yahoo.com> and Vcc
//

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF DARWIN}
  CFBase, CFBundle, CFURL, CFString,
  {$ENDIF}
  Classes, SysUtils, Forms;

const
  PATH_OSX_RESOURCES = 'Contents/Resources/';
  PATH_OSX_EXECUTABLE = 'Contents/MacOS/';
  PATH_UNIX_APPLICATION = '/usr/share/';    // Typical place to store the application foldler
  PATH_UNIX_SETTINGS = '/home/{user}/.config/{executable_name}';  // GetAppConfigDir  does this for us but this is what it returns
  PATH_LINUX_DEV = '/dev/';
  PATH_OSX_DEV = 'dev/';

function GetApplicationPath: string;    // Returns the path to the executible (except for Linix were it returns the root folder of the Application folder) ending in the path delimiter
function GetSettingsPath: string;

implementation

function GetSettingsPath: string;
begin
  // Under OSX we get the path of the executable
  {$IFDEF DARWIN}
  Result := GetApplicationPath + PATH_OSX_RESOURCES;
  {$ENDIF}
    // Under Windows we get the path of the executable
  {$IFDEF Windows}
  Result := GetApplicationPath;
  {$ENDIF}
  {$IFDEF Linux}
  Result := GetAppConfigDir(False);
  {$ENDIF}
end;


function GetApplicationPath: string;
{$IFDEF DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  // Under OSX we get the path of the executable
  {$IFDEF DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  Result := pathStr + '/';
//  Result := ExtractFilePath(Result);
  {$ENDIF}
    // Under Windows we get the path of the executable
  {$IFDEF Windows}
  Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}
  {$IFDEF Linux}
  Result := PATH_UNIX_APPLICATION;    // Linux is typically hardcoded to a path
  {$ENDIF}
end;

end.
