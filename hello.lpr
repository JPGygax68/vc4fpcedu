program Hello;

{$mode objfpc}

uses 
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  SysUtils, DispmanX, EGL, VC4;

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

type

  EGL_STATE_T = record
    screen_width, screen_height: UInt32;
    display: EGLDisplay;
    surface: EGLSurface;
    context: EGLContext;
    config : EGLConfig;
  end;

procedure InitEGL(var state: EGL_STATE_T);
const
  config_attribs: array[0..10] of EGLint = (
    EGL_RED_SIZE, 8,
    EGL_GREEN_SIZE, 8,
    EGL_BLUE_SIZE, 8,
    EGL_ALPHA_SIZE, 8,
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_NONE
  );
  context_attribs: array[0..2] of EGLint = (
    EGL_CONTEXT_CLIENT_VERSION, 2,
    EGL_NONE
  );
var
  Res: EGLboolean;
  num_configs: EGLint;
  configs: array of EGLConfig;
  //i: Integer;
begin
  WriteLn('Obtaining EGL default display.');
  state.display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if state.display = nil then raise Exception.Create('Failed to obtain EGL default display');

  WriteLn('Initializing EGL display connection');
  Res := eglInitialize(state.display, nil, nil);
  if Res = EGL_FALSE then raise Exception.Create('Failed to initialize the EGL display');

  {$ifdef NOT_DEFINED}
  Write('Obtaining EGL display configs...');
  eglGetConfigs(state.display, nil, 0, @num_configs);
  // TODO: abort if num_configs = 0
  SetLength(configs, num_configs);
  eglGetConfigs(state.display, @configs, num_configs, @num_configs);
  WriteLn('obtained ', num_configs, ' configurations.');
  {$endif}

  Write('Choosing a display config...');
  Res := eglChooseConfig(state.display, @config_attribs, @state.config, 1, @num_configs);
  if Res <> EGL_TRUE then raise Exception.Create('Failed to choose a display configuration');
  WriteLn('done.');

  {$ifdef NOT_DEFINED}
  WriteLn('Creating EGL context');
  state.context := eglCreateContext(state.display, state.config, EGL_NO_CONTEXT, @context_attribs);
  if state.context = nil then raise Exception.Create('Failed to create EGL context');
  {$endif}

  Write('Binding the OpenGL ES API...');
  Res := eglBindAPI(EGL_OPENGL_ES_API);
  if Res <> EGL_TRUE then raise Exception.Create('Failed to bind OpenGL ES API');
  WriteLn('done.');
end;

var
  dispmanx_state: DISPMANX_STATE_T;
  egl_state: EGL_STATE_T;

begin
  try
    
    WriteLn('Initializing DispmanX');
    InitDispmanX(dispmanx_state);

    WriteLn('Initializing EGL');
    InitEGL(egl_state);



    if vc_dispmanx_display_close(dispmanx_state.dispman_display) <> 0 then
      raise Exception.Create('Error closing the DispmanX display');

  except on E: Exception do 
    WriteLn(StdErr, 'Error: ', E.Message);
  end;
end.
