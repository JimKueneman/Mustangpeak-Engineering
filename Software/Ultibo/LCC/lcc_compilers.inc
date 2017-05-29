
{$IFDEF FPC}
  {$IFDEF WINDOWS}   // Lazarus
    {$DEFINE LCC_WINDOWS}
    {$IFDEF WIN64}        // TODO NEED TO GET THE RIGHT VALUES FOR FPC AND DELPHI HERE
      {$DEFINE LCC_WINOWS64}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS} // Delphi
    {$DEFINE LCC_WINDOWS}
    {$IFDEF WIN64}        // TODO NEED TO GET THE RIGHT VALUES FOR FPC AND DELPHI HERE
      {$DEFINE LCC_WIN64}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF Darwin}   // Lazarus
    {$DEFINE LCC_OSX}
  {$ENDIF}
  {$IFDEF MACOS32} // Delphi
    {$DEFINE LCC_OSX}
  {$ENDIF}
  {$IFDEF MACOS} // Delphi
    {$DEFINE LCC_OSX}
  {$ENDIF}
{$ELSE}
  {$IFDEF DCC}
    {$IFDEF IOS}  // Delphi
      {$DEFINE LCC_MOBILE}
    {$ENDIF}
    {$IFDEF ANDROID}
      {$DEFINE LCC_MOBILE}
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      {$DEFINE LCC_WINDOWS}
    {$ENDIF}
  {$ELSE}
    {$IFDEF ULTIBO}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
