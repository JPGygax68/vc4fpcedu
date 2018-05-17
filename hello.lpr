program Hello;

{$mode objfpc}

uses 
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  SysUtils, DispmanX, VC4;

type
  
  DISPMANX_STATE_T = record
    screen_width, screen_height: UInt32;
    dispman_display: DISPMANX_DISPLAY_HANDLE_T;
  end;

procedure InitDispmanX(var state: DISPMANX_STATE_T);
var
  dispman_element: DISPMANX_ELEMENT_HANDLE_T;
  dispman_update: DISPMANX_UPDATE_HANDLE_T;
  success: Int32;
  src_rect, dst_rect: VC_RECT_T;
begin

  Write('Initializing BCM host...');
  BCMHostInit;
  WriteLn('done.');

  Write('Obtaining LCD display size...');
  success := BCMHostGraphicsGetDisplaySize(0 {LCD}, state.screen_width, state.screen_height);
  if success < 0 then raise Exception.Create('Failed to obtain LCD display size');
  WriteLn(state.screen_width, 'x', state.screen_height);

  dst_rect.x := 0;
  dst_rect.y := 0;
  dst_rect.width  := state.screen_width;
  dst_rect.height := state.screen_height;

  src_rect.x := 0;
  src_rect.y := 0;
  src_rect.width  := state.screen_width  shl 16;
  src_rect.height := state.screen_height shl 16;

  Write('Adding Dispman element...');
  dispman_update := vc_dispmanx_update_start(0);
  if dispman_update = 0 then raise Exception.Create('Failed to obtain update handle');
  dispman_element := vc_dispmanx_element_add(dispman_update, state.dispman_display, 
    0 {layer}, @src_rect, 0 {source}, @dst_rect, DISPMANX_PROTECTION_NONE, 
    nil {alpha}, nil {clamp}, 0 {transform});
  if dispman_element = 0 then raise Exception.Create('Failed to add Dispman element');
  WriteLn('done.');

end;

var
  dispmanx_state: DISPMANX_STATE_T;

begin
  try
    
    WriteLn('Initializing DispmanX');
    InitDispmanX(dispmanx_state);

    if vc_dispmanx_display_close(dispmanx_state.dispman_display) <> 0 then
      raise Exception.Create('Error closing the DispmanX display');

  except on E: Exception do 
    WriteLn(StdErr, 'Error: ', E.Message);
  end;
end.
