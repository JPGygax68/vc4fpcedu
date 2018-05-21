program Hello;

{$mode objfpc}

uses 
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  SysUtils, DispmanX, EGL, VC4;

type
  
  {$ifdef OLD_CODE}
  DISPMANX_STATE_T = record
    screen_width, screen_height: UInt32;
    dispman_display: DISPMANX_DISPLAY_HANDLE_T;    
  end;
  {$endif}

  EGL_STATE_T = record
    display: EGLDisplay;
    surface: EGLSurface;
    context: EGLContext;
    config : EGLConfig;
    native_window: EGL_DISPMANX_WINDOW_T;
    dispman_display: DISPMANX_DISPLAY_HANDLE_T;
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
  //configs: array of EGLConfig;
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

  Write('Binding the OpenGL ES API...');
  Res := eglBindAPI(EGL_OPENGL_ES_API);
  if Res <> EGL_TRUE then raise Exception.Create('Failed to bind OpenGL ES API');
  WriteLn('done.');

  Write('Creating EGL context...');
  state.context := eglCreateContext(state.display, state.config, EGL_NO_CONTEXT, @context_attribs);
  if state.context = nil then raise Exception.Create('Failed to create EGL context');
  WriteLn('done.');
end;

procedure InitDispmanX(var state: EGL_STATE_T);
var
  screen_width, screen_height: UInt32;
  dispman_element: DISPMANX_ELEMENT_HANDLE_T;
  dispman_update: DISPMANX_UPDATE_HANDLE_T;
  success: Int32;
  src_rect, dst_rect: VC_RECT_T;
begin

  Write('Initializing BCM host...');
  BCMHostInit;
  WriteLn('done.');

  Write('Obtaining LCD display size...');
  success := BCMHostGraphicsGetDisplaySize(0 {LCD}, screen_width, screen_height);
  if success < 0 then raise Exception.Create('Failed to obtain LCD display size');
  WriteLn(screen_width, 'x', screen_height);

  Write('Opening Dispman display...');
  state.dispman_display := vc_dispmanx_display_open(0 {LCD});
  if state.dispman_display = 0 then raise Exception.Create('Failed to open dispman display');
  WriteLn('done.');

  Write('Creating Dispman window from display...');
  dst_rect.x := 0;
  dst_rect.y := 0;
  dst_rect.width  := screen_width;
  dst_rect.height := screen_height;
  src_rect.x := 0;
  src_rect.y := 0;
  src_rect.width  := screen_width  shl 16;
  src_rect.height := screen_height shl 16;
  dispman_update := vc_dispmanx_update_start(0);
  if dispman_update = 0 then raise Exception.Create('Failed to obtain update handle');
  dispman_element := vc_dispmanx_element_add(dispman_update, state.dispman_display, 
    0 {layer}, @src_rect, 0 {source}, @dst_rect, DISPMANX_PROTECTION_NONE, 
    nil {alpha}, nil {clamp}, 0 {transform});
  if dispman_element = 0 then raise Exception.Create('Failed to add Dispman element');
  state.native_window.element := dispman_element;
  state.native_window.width  := screen_width;
  state.native_window.height := screen_height;
  WriteLn('done.');
end;

procedure EGLFromDispmanX(var state: EGL_STATE_T);
var
  Res: EGLboolean;
begin

  Write('Creating a Surface for the native Window...');
  state.surface := eglCreateWindowSurface(state.display, state.config, @state.native_window, nil);
  if state.surface = EGL_NO_SURFACE then raise Exception.Create('Failed to create Window Surface');
  WriteLn('done.');

  Write('Connecting the rendering context to the surface...');
  Res := eglMakeCurrent(state.display, state.surface, state.surface, state.context);
  if Res = EGL_FALSE then raise Exception.Create('Failed to connect the rendering context to the Surface');
  WriteLn('done.');
end;

procedure Cleanup(var state: EGL_STATE_T);
var
  Res: EGLboolean;
begin
    if state.surface <> EGL_NO_SURFACE then
    begin
      Res := eglDestroySurface(state.display, state.surface);
      if Res <> EGL_TRUE then WriteLn(StdErr, 'Failed to destroy EGL surface');
      state.surface := EGL_NO_SURFACE;
    end;
    if state.context <> EGL_NO_CONTEXT then
    begin
      Res := eglDestroyContext(state.display, state.context);
      if Res <> EGL_TRUE then WriteLn(StdErr, 'Failed to destroy EGL rendering context');
      state.context := EGL_NO_CONTEXT;
    end;
    if state.display <> EGL_NO_DISPLAY then
    begin
      Res := eglTerminate(state.display);
      if Res <> EGL_TRUE then WriteLn(StdErr, 'Failed to terminate the EGL display');
      state.display := EGL_NO_DISPLAY;
    end;
    if eglReleaseThread <> EGL_TRUE then WriteLn(StdErr, 'Error releasing EGL thread resources');
    if state.dispman_display <> 0 then
    begin
      if vc_dispmanx_display_close(state.dispman_display) <> 0 then
        WriteLn(StdErr, 'Error closing the DispmanX display');
    end;
end;

var
  egl_state: EGL_STATE_T;

begin
  try
    
    WriteLn;
    WriteLn('Initializing DispmanX');
    InitDispmanX(egl_state);

    WriteLn;
    WriteLn('Initializing EGL');
    InitEGL(egl_state);

    WriteLn;
    WriteLn('Connecting EGL to DispmanX');
    EGLFromDispmanX(egl_state);


    Sleep(1000);

    WriteLn;
    WriteLn('Cleanup');
    Cleanup(egl_state);

  except on E: Exception do 
    WriteLn(StdErr, 'Error: ', E.Message);
  end;
end.
