with Sf.Graphics;
with Sf.System;
with Sf.System.Time;
with Sf.Graphics.RenderStates;
--  with Sf.Graphics.Font;
--  with Sf.Graphics.Text;

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package Quiza.Screen is

   --
   --  Key code / mouse button constants
   --
   type Keycode_Type is
     (Key_A, Key_B, Key_C, Key_D, Key_E, Key_F, Key_G, Key_H, Key_I, Key_J,
      Key_K, Key_L, Key_M, Key_N, Key_O, Key_P, Key_Q, Key_R, Key_S, Key_T,
      Key_U, Key_V, Key_W, Key_X, Key_Y, Key_Z, Key_Num_0, Key_Num_1,
      Key_Num_2, Key_Num_3, Key_Num_4, Key_Num_5, Key_Num_6, Key_Num_7,
      Key_Num_8, Key_Num_9, Key_Escape, Key_L_Control, Key_L_Shift, Key_L_Alt,
      Key_L_System, Key_R_Control, Key_R_Shift, Key_R_Alt, Key_R_System,
      Key_Menu, Key_L_Bracket, Key_R_Bracket, Key_Semicolon, Key_Comma,
      Key_Period, Key_Quote, Key_Slash, Key_Backslash, Key_Tilde, Key_Equal,
      Key_Hyphen, Key_Space, Key_Enter, Key_Back, Key_Tab, Key_Page_Up,
      Key_Page_Down, Key_End, Key_Home, Key_Insert, Key_Delete, Key_Add,
      Key_Subtract, Key_Multiply, Key_Divide, Key_Left, Key_Right, Key_Up,
      Key_Down, Key_Numpad_0, Key_Numpad_1, Key_Numpad_2, Key_Numpad_3,
      Key_Numpad_4, Key_Numpad_5, Key_Numpad_6, Key_Numpad_7, Key_Numpad_8,
      Key_Numpad_9, Key_F1, Key_F2, Key_F3, Key_F4, Key_F5, Key_F6, Key_F7,
      Key_F8, Key_F9, Key_F10, Key_F11, Key_F12, Key_F13, Key_F14, Key_F15,
      Key_Pause, Key_Count, Key_Any, Mouse_Button_Left, Mouse_Button_Middle,
      Mouse_Button_Right);

   --
   --  Screen
   --
   type Screen_Type is tagged private;
   function Make_Screen
     (Width, Height : Positive; Full_Screen : Boolean) return Screen_Type;
   function Make_Screen return Screen_Type;
   function Get_Width (W : Screen_Type) return Positive;
   function Get_Height (W : Screen_Type) return Positive;
   function Is_Open (W : Screen_Type) return Boolean;

   --
   --  Drawing
   --
   procedure Draw_Line
     (W : Screen_Type; X0, Y0, X1, Y1 : Float; R, G, B, A : Integer);
   procedure Draw_Rectangle
     (W : Screen_Type; X, Y, Width, Height : Float; R, G, B, A : Integer);
   procedure Draw_Rectangle
     (W : Screen_Type; X, Y, Width, Height : Float;
      R, G, B, A, Line_R, Line_G, Line_B, Line_A : Integer);
   procedure Draw_Circle
     (W : Screen_Type; X, Y, Radius : Float; R, G, B, A : Integer);
   procedure Draw_Circle
     (W : Screen_Type; X, Y, Radius : Float;
      R, G, B, A, Line_R, Line_G, Line_B, Line_A : Integer);
   procedure Clear (W : Screen_Type);
   procedure Begin_Draw (W : Screen_Type);
   procedure End_Draw (W : in out Screen_Type);

   --
   --  Immediate (eventless) input processing
   --
   function Is_Mouse_Inside (W : Screen_Type) return Boolean;
   function Is_Key_Or_Button_Pressed
     (W : Screen_Type; K : Keycode_Type) return Boolean;
   function Mouse_X (W : Screen_Type) return Integer;
   function Mouse_Y (W : Screen_Type) return Integer;

   --
   --  Frame drawing time
   --
   function Get_Draw_Time (W : Screen_Type) return Float;

   --
   --  Transformations
   --
   procedure Unset_Draw_Scale (W : in out Screen_Type);
   procedure Unset_Draw_Rotation (W : in out Screen_Type);
   procedure Unset_Draw_Translation (W : in out Screen_Type);
   procedure Set_Draw_Scale (W : in out Screen_Type; SX, SY, CX, CY : Float);
   procedure Set_Draw_Rotation (W : in out Screen_Type; Angle, CX, CY : Float);
   procedure Set_Draw_Translation (W : in out Screen_Type; DX, DY : Float);
   function Get_Draw_Scale_SX (W : Screen_Type) return Float;
   function Get_Draw_Scale_SY (W : Screen_Type) return Float;
   function Get_Draw_Scale_CX (W : Screen_Type) return Float;
   function Get_Draw_Scale_CY (W : Screen_Type) return Float;
   function Get_Draw_Rotation_Angle (W : Screen_Type) return Float;
   function Get_Draw_Rotation_CX (W : Screen_Type) return Float;
   function Get_Draw_Rotation_CY (W : Screen_Type) return Float;
   function Get_Draw_Translation_DX (W : Screen_Type) return Float;
   function Get_Draw_Translation_DY (W : Screen_Type) return Float;

   --
   --  Fonts, text drawing
   --
   procedure Load_Fonts_Directory (W : in out Screen_Type; Dir : String);
   procedure Draw_Text
     (W : Screen_Type; X, Y : Float; Str : String; R, G, B, A : Integer;
      Font_Name : String; Size : Positive := 12);

   --
   --  Textures, sprite drawing
   --
   procedure Load_Images_Directory (W : in out Screen_Type; Dir : String);
   procedure Draw_Image
     (W          : Screen_Type; X, Y : Float; Img_Name : String;
      R, G, B, A : Integer := 255);
   procedure Draw_Resized_Image
     (W          : Screen_Type; X, Y, Width, Height : Float; Img_Name : String;
      R, G, B, A : Integer := 255);
   procedure Draw_Sub_Image
     (W : Screen_Type; X, Y : Float; Img_Name : String;
      Rect_X, Rect_Y, Rect_W, Rect_H : Float; R, G, B, A : Integer := 255);

private

   use type Sf.Graphics.SfFont_Ptr;
   package String_Font_Maps is new Ada.Containers.Hashed_Maps
     (Ada.Strings.Unbounded.Unbounded_String, Sf.Graphics.SfFont_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   use type Sf.Graphics.SfTexture_Ptr;
   package String_Texture_Maps is new Ada.Containers.Hashed_Maps
     (Ada.Strings.Unbounded.Unbounded_String, Sf.Graphics.SfTexture_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Screen_Type is tagged record
      --
      --  SFML objects for reuse
      --
      Impl      : Sf.Graphics.SfRenderWindow_Ptr;
      Rectangle : Sf.Graphics.SfRectangleShape_Ptr;
      Circle    : Sf.Graphics.SfCircleShape_Ptr;
      Line      : Sf.Graphics.SfVertexArray_Ptr;
      Clock     : Sf.System.SfClock_Ptr;
      Time      : Sf.System.Time.SfTime;
      States    : Sf.Graphics.RenderStates.SfRenderStates_Ptr;
      Text      : Sf.Graphics.SfText_Ptr;
      Fonts     : String_Font_Maps.Map;
      Sprite    : Sf.Graphics.SfSprite_Ptr;
      Textures  : String_Texture_Maps.Map;

      --
      --  Transformation construction data
      --
      Scale_SX, Scale_SY, Scale_CX, Scale_CY   : Float;
      Rotation_Angle, Rotation_CX, Rotation_CY : Float;
      Translation_DX, Translation_DY           : Float;
   end record;

end Quiza.Screen;
