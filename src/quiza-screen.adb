pragma Ada_2012;

with Sf;                         use Sf;
with Sf.Graphics.BlendMode;      use Sf.Graphics.BlendMode;
with Sf.System;                  use Sf.System;
with Sf.System.Clock;            use Sf.System.Clock;
with Sf.System.Time;             use Sf.System.Time;
with Sf.Window;                  use Sf.Window;
with Sf.Window.VideoMode;        use Sf.Window.VideoMode;
with Sf.Window.Keyboard;         use Sf.Window.Keyboard;
with Sf.Window.Mouse;            use Sf.Window.Mouse;
with Sf.Graphics;                use Sf.Graphics;
with Sf.Graphics.RenderWindow;   use Sf.Graphics.RenderWindow;
with Sf.Graphics.RectangleShape; use Sf.Graphics.RectangleShape;
with Sf.Graphics.CircleShape;    use Sf.Graphics.CircleShape;
with Sf.Graphics.Color;          use Sf.Graphics.Color;
with Sf.Graphics.Vertex;         use Sf.Graphics.Vertex;
with Sf.Graphics.VertexArray;    use Sf.Graphics.VertexArray;
with Sf.Graphics.PrimitiveType;  use Sf.Graphics.PrimitiveType;
with Sf.System.Vector2;          use Sf.System.Vector2;
with Sf.Graphics.Transform;      use Sf.Graphics.Transform;
with Sf.Graphics.RenderStates;   use Sf.Graphics.RenderStates;
with Sf.Graphics.Text;           use Sf.Graphics.Text;
with Sf.Graphics.Font;           use Sf.Graphics.Font;
with Sf.Graphics.Texture;        use Sf.Graphics.Texture;
with Sf.Graphics.Sprite;         use Sf.Graphics.Sprite;
with Sf.Graphics.Rect;           use Sf.Graphics.Rect;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;
with Ada.Text_IO;           use Ada.Text_IO;

package body Quiza.Screen is

   --  package Caches is
   --  end Caches;

   --  package body Caches is
   --  begin
   --     Clock := create;
   --     Rectangle := create;
   --     setOutlineThickness (Rectangle, 1.0);
   --     Circle := create;
   --     setOutlineThickness (Circle, 1.0);
   --     Line := create;
   --     resize (Line, 2);
   --     setPrimitiveType (Line, sfLines);
   --  end Caches;

   ---------------------------------------
   -- Recompute_Render_States_Transform --
   ---------------------------------------

   procedure Recompute_Render_States_Transform (W : Screen_Type) is
   begin
      W.States.Transform := Identity;
      Translate
        (W.States.Transform'Access, W.Translation_DX, W.Translation_DY);
      RotateWithCenter
        (W.States.Transform'Access, W.Rotation_Angle, W.Rotation_CX,
         W.Rotation_CY);
      ScaleWithCenter
        (W.States.Transform'Access, W.Scale_SX, W.Scale_SY, W.Scale_CX,
         W.Scale_CY);
   end Recompute_Render_States_Transform;

   -----------------
   -- Make_Screen --
   -----------------

   function Make_Screen
     (Width, Height : Positive; Full_Screen : Boolean) return Screen_Type
   is
      W : Screen_Type;
   begin
      W.Impl :=
        Create
          (VideoMode.SfVideoMode'(SfUint32 (Width), SfUint32 (Height), 32),
           "Quiza");

      W.Scale_SX       := 1.0;
      W.Scale_SY       := 1.0;
      W.Scale_CX       := 1.0;
      W.Scale_CY       := 1.0;
      W.Rotation_Angle := 0.0;
      W.Rotation_CX    := 0.0;
      W.Rotation_CY    := 0.0;
      W.Translation_DX := 0.0;
      W.Translation_DY := 0.0;

      W.Clock            := Create;
      W.States           := new SfRenderStates;
      W.States.BlendMode := SfBlendAlpha;
      W.States.Transform := Identity;
      W.States.Texture   := null;
      W.States.Shader    := null;
      W.Text             := Create;
      W.Sprite           := Create;

      W.Recompute_Render_States_Transform;

      W.Rectangle := Create;
      SetOutlineThickness (W.Rectangle, 1.0);

      W.Circle := Create;
      SetOutlineThickness (W.Circle, 1.0);

      W.Line := Create;
      Resize (W.Line, 2);
      SetPrimitiveType (W.Line, SfLines);

      return W;
   end Make_Screen;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (W : Screen_Type) return Positive is
   begin
      return Positive (GetSize (W.Impl).X);
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (W : Screen_Type) return Positive is
   begin
      return Positive (GetSize (W.Impl).Y);
   end Get_Height;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (W : Screen_Type) return Boolean is
   begin
      return Boolean (IsOpen (W.Impl));
   end Is_Open;

   -----------
   -- Clear --
   -----------

   procedure Clear (W : Screen_Type) is
   begin
      Clear (W.Impl);
   end Clear;

   ----------------
   -- Begin_Draw --
   ----------------

   procedure Begin_Draw (W : Screen_Type) is
      T : SfTime;
      pragma Unreferenced (T);
   begin
      T := Restart (W.Clock);
   end Begin_Draw;

   --------------
   -- End_Draw --
   --------------

   procedure End_Draw (W : in out Screen_Type) is
   begin
      Display (W.Impl);
      W.Time := GetElapsedTime (W.Clock);
   end End_Draw;

   ---------------------
   -- Is_Mouse_Inside --
   ---------------------

   function Is_Mouse_Inside (W : Screen_Type) return Boolean is
   begin
      return
        W.Mouse_X >= 0 and then W.Mouse_X < W.Get_Width and then W.Mouse_Y >= 0
        and then W.Mouse_Y < W.Get_Height;
   end Is_Mouse_Inside;

   ------------------------------
   -- Is_Key_Or_Button_Pressed --
   ------------------------------

   function Is_Key_Or_Button_Pressed
     (W : Screen_Type; K : Keycode_Type) return Boolean
   is
      -------------------
      -- To_Sf_Keycode --
      -------------------

      function To_Sf_Keycode (K : Keycode_Type) return SfKeyCode is
         C : SfKeyCode := SfKeyUnknown;
      begin
         case K is
            when Key_A =>
               C := SfKeyA;
            when Key_B =>
               C := SfKeyB;
            when Key_C =>
               C := SfKeyC;
            when Key_D =>
               C := SfKeyD;
            when Key_E =>
               C := SfKeyE;
            when Key_F =>
               C := SfKeyF;
            when Key_G =>
               C := SfKeyG;
            when Key_H =>
               C := SfKeyH;
            when Key_I =>
               C := SfKeyI;
            when Key_J =>
               C := SfKeyJ;
            when Key_K =>
               C := SfKeyK;
            when Key_L =>
               C := SfKeyL;
            when Key_M =>
               C := SfKeyM;
            when Key_N =>
               C := SfKeyN;
            when Key_O =>
               C := SfKeyO;
            when Key_P =>
               C := SfKeyP;
            when Key_Q =>
               C := SfKeyQ;
            when Key_R =>
               C := SfKeyR;
            when Key_S =>
               C := SfKeyS;
            when Key_T =>
               C := SfKeyT;
            when Key_U =>
               C := SfKeyU;
            when Key_V =>
               C := SfKeyV;
            when Key_W =>
               C := SfKeyW;
            when Key_X =>
               C := SfKeyX;
            when Key_Y =>
               C := SfKeyY;
            when Key_Z =>
               C := SfKeyZ;
            when Key_Num_0 =>
               C := SfKeyNum0;
            when Key_Num_1 =>
               C := SfKeyNum1;
            when Key_Num_2 =>
               C := SfKeyNum2;
            when Key_Num_3 =>
               C := SfKeyNum3;
            when Key_Num_4 =>
               C := SfKeyNum4;
            when Key_Num_5 =>
               C := SfKeyNum5;
            when Key_Num_6 =>
               C := SfKeyNum6;
            when Key_Num_7 =>
               C := SfKeyNum7;
            when Key_Num_8 =>
               C := SfKeyNum8;
            when Key_Num_9 =>
               C := SfKeyNum9;
            when Key_Escape =>
               C := SfKeyEscape;
            when Key_L_Control =>
               C := SfKeyLControl;
            when Key_L_Shift =>
               C := SfKeyLShift;
            when Key_L_Alt =>
               C := SfKeyLAlt;
            when Key_L_System =>
               C := SfKeyLSystem;
            when Key_R_Control =>
               C := SfKeyRControl;
            when Key_R_Shift =>
               C := SfKeyRShift;
            when Key_R_Alt =>
               C := SfKeyRAlt;
            when Key_R_System =>
               C := SfKeyRSystem;
            when Key_Menu =>
               C := SfKeyMenu;
            when Key_L_Bracket =>
               C := SfKeyLBracket;
            when Key_R_Bracket =>
               C := SfKeyRBracket;
            when Key_Semicolon =>
               C := SfKeySemicolon;
            when Key_Comma =>
               C := SfKeyComma;
            when Key_Period =>
               C := SfKeyPeriod;
            when Key_Quote =>
               C := SfKeyQuote;
            when Key_Slash =>
               C := SfKeySlash;
            when Key_Backslash =>
               C := SfKeyBackslash;
            when Key_Tilde =>
               C := SfKeyTilde;
            when Key_Equal =>
               C := SfKeyEqual;
            when Key_Hyphen =>
               C := SfKeyHyphen;
            when Key_Space =>
               C := SfKeySpace;
            when Key_Enter =>
               C := SfKeyEnter;
            when Key_Back =>
               C := SfKeyBack;
            when Key_Tab =>
               C := SfKeyTab;
            when Key_Page_Up =>
               C := SfKeyPageUp;
            when Key_Page_Down =>
               C := SfKeyPageDown;
            when Key_End =>
               C := SfKeyEnd;
            when Key_Home =>
               C := SfKeyHome;
            when Key_Insert =>
               C := SfKeyInsert;
            when Key_Delete =>
               C := SfKeyDelete;
            when Key_Add =>
               C := SfKeyAdd;
            when Key_Subtract =>
               C := SfKeySubtract;
            when Key_Multiply =>
               C := SfKeyMultiply;
            when Key_Divide =>
               C := SfKeyDivide;
            when Key_Left =>
               C := SfKeyLeft;
            when Key_Right =>
               C := SfKeyRight;
            when Key_Up =>
               C := SfKeyUp;
            when Key_Down =>
               C := SfKeyDown;
            when Key_Numpad_0 =>
               C := SfKeyNum0;
            when Key_Numpad_1 =>
               C := SfKeyNum1;
            when Key_Numpad_2 =>
               C := SfKeyNum2;
            when Key_Numpad_3 =>
               C := SfKeyNum3;
            when Key_Numpad_4 =>
               C := SfKeyNum4;
            when Key_Numpad_5 =>
               C := SfKeyNum5;
            when Key_Numpad_6 =>
               C := SfKeyNum6;
            when Key_Numpad_7 =>
               C := SfKeyNum7;
            when Key_Numpad_8 =>
               C := SfKeyNum8;
            when Key_Numpad_9 =>
               C := SfKeyNum9;
            when Key_F1 =>
               C := SfKeyF1;
            when Key_F2 =>
               C := SfKeyF2;
            when Key_F3 =>
               C := SfKeyF3;
            when Key_F4 =>
               C := SfKeyF4;
            when Key_F5 =>
               C := SfKeyF5;
            when Key_F6 =>
               C := SfKeyF6;
            when Key_F7 =>
               C := SfKeyF7;
            when Key_F8 =>
               C := SfKeyF8;
            when Key_F9 =>
               C := SfKeyF9;
            when Key_F10 =>
               C := SfKeyF10;
            when Key_F11 =>
               C := SfKeyF11;
            when Key_F12 =>
               C := SfKeyF12;
            when Key_F13 =>
               C := SfKeyF13;
            when Key_F14 =>
               C := SfKeyF14;
            when Key_F15 =>
               C := SfKeyF15;
            when Key_Pause =>
               C := SfKeyPause;
            when Key_Count =>
               C := SfKeyCount;
            when others =>
               return raise Program_Error with "Bad conversion to sfKeyCode";
         end case;
         return C;
      end To_Sf_Keycode;

      -----------------------
      -- To_Sf_Mousebutton --
      -----------------------

      function To_Sf_Mousebutton (K : Keycode_Type) return SfMouseButton is
         B : SfMouseButton;
      begin
         case K is
            when Mouse_Button_Left =>
               B := SfMouseLeft;
            when Mouse_Button_Middle =>
               B := SfMouseMiddle;
            when Mouse_Button_Right =>
               B := SfMouseRight;
            when others =>
               return
               raise Program_Error with "Bad conversion to sfMouseButton";
         end case;
         return B;
      end To_Sf_Mousebutton;

      subtype Keyboard_Key_Type is Keycode_Type range Key_A .. Key_Count;
      --  Strictly a keyboard key
      subtype Mousebutton_Key_Type is
        Keycode_Type range Mouse_Button_Left .. Mouse_Button_Right;
      --  Strictly a mouse button

      Pressed : Boolean := False;
   begin
      if K in Keyboard_Key_Type then
         Pressed := Boolean (IsKeyPressed (To_Sf_Keycode (K)));
      elsif K in Mousebutton_Key_Type then
         Pressed := Boolean (IsButtonPressed (To_Sf_Mousebutton (K)));
      elsif K = Key_Any then
         --  Any key means any keyboard or any mouse button
         for I in Keyboard_Key_Type'Range loop
            if Boolean (IsKeyPressed (To_Sf_Keycode (I))) then
               return True;
            end if;
         end loop;
         for I in Mousebutton_Key_Type loop
            if Boolean (IsButtonPressed (To_Sf_Mousebutton (I))) then
               return True;
            end if;
         end loop;
         return False;
      end if;
      return Pressed;
   end Is_Key_Or_Button_Pressed;

   -------------
   -- Mouse_X --
   -------------

   function Mouse_X (W : Screen_Type) return Integer is
   begin
      return Integer (Sf.Graphics.RenderWindow.Mouse.GetPosition (W.Impl).X);
   end Mouse_X;

   -------------
   -- Mouse_Y --
   -------------

   function Mouse_Y (W : Screen_Type) return Integer is
   begin
      return Integer (Sf.Graphics.RenderWindow.Mouse.GetPosition (W.Impl).Y);
   end Mouse_Y;

   -------------------
   -- Get_Draw_Time --
   -------------------

   function Get_Draw_Time (W : Screen_Type) return Float is
   begin
      return AsSeconds (W.Time);
   end Get_Draw_Time;

   ----------------------
   -- Unset_Draw_Scale --
   ----------------------

   procedure Unset_Draw_Scale (W : in out Screen_Type) is
   begin
      W.Scale_SX := 1.0;
      W.Scale_SY := 1.0;
   end Unset_Draw_Scale;

   -------------------------
   -- Unset_Draw_Rotation --
   -------------------------

   procedure Unset_Draw_Rotation (W : in out Screen_Type) is
   begin
      W.Rotation_Angle := 0.0;
      W.Rotation_CX    := Float (W.Get_Width) / 2.0;
      W.Rotation_CY    := Float (W.Get_Height) / 2.0;
   end Unset_Draw_Rotation;

   ----------------------------
   -- Unset_Draw_Translation --
   ----------------------------

   procedure Unset_Draw_Translation (W : in out Screen_Type) is
   begin
      W.Translation_DX := 0.0;
      W.Translation_DY := 0.0;
      W.Recompute_Render_States_Transform;
   end Unset_Draw_Translation;

   --------------------
   -- Set_Draw_Scale --
   --------------------

   procedure Set_Draw_Scale (W : in out Screen_Type; SX, SY, CX, CY : Float) is
   begin
      W.Scale_SX := SX;
      W.Scale_SY := SY;
      W.Scale_CX := CX;
      W.Scale_CY := CY;
      W.Recompute_Render_States_Transform;
   end Set_Draw_Scale;

   -----------------------
   -- Set_Draw_Rotation --
   -----------------------

   procedure Set_Draw_Rotation (W : in out Screen_Type; Angle, CX, CY : Float)
   is
   begin
      W.Rotation_Angle := Angle;
      W.Rotation_CX    := CX;
      W.Rotation_CY    := CY;
      W.Recompute_Render_States_Transform;
   end Set_Draw_Rotation;

   --------------------------
   -- Set_Draw_Translation --
   --------------------------

   procedure Set_Draw_Translation (W : in out Screen_Type; DX, DY : Float) is
   begin
      W.Translation_DX := DX;
      W.Translation_DY := DY;
      W.Recompute_Render_States_Transform;
   end Set_Draw_Translation;

   -----------------------
   -- Get_Draw_Scale_SX --
   -----------------------

   function Get_Draw_Scale_SX (W : Screen_Type) return Float is
   begin
      return W.Scale_SX;
   end Get_Draw_Scale_SX;

   -----------------------
   -- Get_Draw_Scale_SY --
   -----------------------

   function Get_Draw_Scale_SY (W : Screen_Type) return Float is
   begin
      return W.Scale_SY;
   end Get_Draw_Scale_SY;

   -----------------------
   -- Get_Draw_Scale_CX --
   -----------------------

   function Get_Draw_Scale_CX (W : Screen_Type) return Float is
   begin
      return W.Scale_CX;
   end Get_Draw_Scale_CX;

   -----------------------
   -- Get_Draw_Scale_CY --
   -----------------------

   function Get_Draw_Scale_CY (W : Screen_Type) return Float is
   begin
      return W.Scale_CY;
   end Get_Draw_Scale_CY;

   -----------------------------
   -- Get_Draw_Rotation_Angle --
   -----------------------------

   function Get_Draw_Rotation_Angle (W : Screen_Type) return Float is
   begin
      return W.Rotation_Angle;
   end Get_Draw_Rotation_Angle;

   --------------------------
   -- Get_Draw_Rotation_CX --
   --------------------------

   function Get_Draw_Rotation_CX (W : Screen_Type) return Float is
   begin
      return W.Rotation_CX;
   end Get_Draw_Rotation_CX;

   --------------------------
   -- Get_Draw_Rotation_CY --
   --------------------------

   function Get_Draw_Rotation_CY (W : Screen_Type) return Float is
   begin
      return W.Rotation_CY;
   end Get_Draw_Rotation_CY;

   -----------------------------
   -- Get_Draw_Translation_DX --
   -----------------------------

   function Get_Draw_Translation_DX (W : Screen_Type) return Float is
   begin
      return W.Translation_DX;
   end Get_Draw_Translation_DX;

   -----------------------------
   -- Get_Draw_Translation_DY --
   -----------------------------

   function Get_Draw_Translation_DY (W : Screen_Type) return Float is
   begin
      return W.Translation_DY;
   end Get_Draw_Translation_DY;

   --------------------------
   -- Load_Fonts_Directory --
   --------------------------

   procedure Load_Fonts_Directory (W : in out Screen_Type; Dir : String) is
      Dir_Entry  : Directory_Entry_Type;
      Dir_Search : Search_Type;
      Counter    : Integer := 0;
   begin
      Start_Search (Search => Dir_Search, Directory => Dir, Pattern => "*.*");
      loop
         Get_Next_Entry (Dir_Search, Dir_Entry);
         if Kind (Dir_Entry) = Ordinary_File then
            declare
               F : constant SfFont_Ptr :=
                     CreateFromFile (Full_Name (Dir_Entry));
               Font_Name : constant String := (Simple_Name (Dir_Entry));
            begin
               if F /= null then
                  W.Fonts.Include (To_Unbounded_String (Font_Name), F);
                  Put_Line ("Loaded font '" & Font_Name & "'");
                  Counter := Counter + 1;
               else
                  Put_Line ("Error loading font " & Full_Name (Dir_Entry));
               end if;
            end;
         end if;
         exit when not More_Entries (Dir_Search);
      end loop;

      if Counter = 0 then
         Put_Line ("No loadable fonts were present in " & Dir);
      end if;
      End_Search (Dir_Search);

   end Load_Fonts_Directory;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (W : Screen_Type; X, Y : Float; Str : String; R, G, B, A : Integer;
      Font_Name : String; Size : Positive := 12)
   is
      --  TODO: optimize lookups
   begin
      if not W.Fonts.Contains (To_Unbounded_String (Font_Name)) then
         raise Program_Error with "Requested font is not loaded";
      else
         declare
            F : constant SfFont_Ptr :=
                  W.Fonts.Element (To_Unbounded_String (Font_Name));
         begin
            SetFont (W.Text, F);
            SetString (W.Text, Str);
            SetCharacterSize (W.Text, SfUint32 (Size));
            SetFillColor
              (W.Text,
               SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
            SetPosition (W.Text, SfVector2f'(X, Y));
            DrawText (W.Impl, W.Text, W.States);
            null;
         end;
      end if;

   end Draw_Text;

   ---------------------------
   -- Load_Images_Directory --
   ---------------------------

   procedure Load_Images_Directory (W : in out Screen_Type; Dir : String) is
      Dir_Entry  : Directory_Entry_Type;
      Dir_Search : Search_Type;
      Counter    : Integer := 0;
   begin
      Start_Search (Search => Dir_Search, Directory => Dir, Pattern => "*.*");
      loop
         Get_Next_Entry (Dir_Search, Dir_Entry);
         if Kind (Dir_Entry) = Ordinary_File then
            declare
               I : constant SfTexture_Ptr :=
                     CreateFromFile (Full_Name (Dir_Entry));
               Image_Name : constant String := (Simple_Name (Dir_Entry));
            begin
               if I /= null then
                  W.Textures.Include (To_Unbounded_String (Image_Name), I);
                  Put_Line ("Loaded image '" & Image_Name & "'");
                  Counter := Counter + 1;
               else
                  Put_Line ("Error loading image" & Full_Name (Dir_Entry));
               end if;
            end;
         end if;
         exit when not More_Entries (Dir_Search);
      end loop;

      if Counter = 0 then
         Put_Line ("No loadable images were present in " & Dir);
      end if;
      End_Search (Dir_Search);
   end Load_Images_Directory;

   ----------------
   -- Draw_Image --
   ----------------

   procedure Draw_Image
     (W          : Screen_Type; X, Y : Float; Img_Name : String;
      R, G, B, A : Integer := 255)
   is
   begin
      if not W.Textures.Contains (To_Unbounded_String (Img_Name)) then
         raise Program_Error with "Requested image is not loaded: " & Img_Name;
      else
         declare
            I : constant SfTexture_Ptr :=
                  W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            SetTexture (W.Sprite, I, SfTrue);
            SetColor
              (W.Sprite,
               SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
            SetPosition (W.Sprite, SfVector2f'(X, Y));
            SetScale (W.Sprite, SfVector2f'(1.0, 1.0));
            DrawSprite (W.Impl, W.Sprite, W.States);
            null;
         end;
      end if;
   end Draw_Image;

   ------------------------
   -- Draw_Resized_Image --
   ------------------------

   procedure Draw_Resized_Image
     (W          : Screen_Type; X, Y, Width, Height : Float; Img_Name : String;
      R, G, B, A : Integer := 255)
   is
   begin
      if not W.Textures.Contains (To_Unbounded_String (Img_Name)) then
         raise Program_Error with "Requested image is not loaded: " & Img_Name;
      else
         declare
            I : constant SfTexture_Ptr :=
                  W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            SetTexture (W.Sprite, I, SfTrue);
            SetColor
              (W.Sprite,
               SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
            SetPosition (W.Sprite, SfVector2f'(X, Y));
            declare
               Img_W : constant Float :=
                         Float (Sf.Graphics.Texture.GetSize (I).X);
               Img_H : constant Float :=
                         Float (Sf.Graphics.Texture.GetSize (I).Y);
               SX : constant Float := Width / Img_W;
               SY : constant Float := Height/ Img_H;
            begin
               SetScale (W.Sprite, SfVector2f'(SX, SY));
            end;
            DrawSprite (W.Impl, W.Sprite, W.States);
            null;
         end;
      end if;
   end Draw_Resized_Image;

   --------------------
   -- Draw_Sub_Image --
   --------------------

   procedure Draw_Sub_Image
     (W : Screen_Type; X, Y : Float; Img_Name : String;
      Rect_X, Rect_Y, Rect_W, Rect_H : Float; R, G, B, A : Integer := 255)
   is
   begin
      if not W.Textures.Contains (To_Unbounded_String (Img_Name)) then
         raise Program_Error with "Requested image is not loaded: " & Img_Name;
      else
         declare
            I : constant SfTexture_Ptr :=
                  W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            SetTexture (W.Sprite, I);
            SetTextureRect
              (W.Sprite,
               SfIntRect'
                 (Integer (Rect_X), Integer (Rect_Y), Integer (Rect_W),
                  Integer (Rect_H)));
            SetPosition (W.Sprite, SfVector2f'(X, Y));
            SetColor
              (W.Sprite,
               SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
            DrawSprite (W.Impl, W.Sprite, W.States);
            null;
         end;
      end if;
   end Draw_Sub_Image;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (W : Screen_Type; X0, Y0, X1, Y1 : Float; R, G, B, A : Integer)
   is
      V0 : constant access Sf.Graphics.Vertex.SfVertex :=
             GetVertex (W.Line, 0);
      V1 : constant access Sf.Graphics.Vertex.SfVertex :=
             GetVertex (W.Line, 1);
   begin
      V0.Position.X := X0;
      V0.Position.Y := Y0;
      V1.Position.X := X1;
      V1.Position.Y := Y1;
      V0.Color := SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A));
      V1.Color := SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A));
      DrawVertexArray (W.Impl, W.Line, W.States);
   end Draw_Line;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (W : Screen_Type; X, Y, Width, Height : Float; R, G, B, A : Integer)
   is
   begin
      Draw_Rectangle (W, X, Y, Width, Height, R, G, B, A, R, G, B, A);
   end Draw_Rectangle;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (W : Screen_Type; X, Y, Width, Height : Float;
      R, G, B, A, Line_R, Line_G, Line_B, Line_A : Integer)
   is
   begin
      SetPosition (W.Rectangle, SfVector2f'(X, Y));
      SetSize (W.Rectangle, SfVector2f'(Width, Height));
      SetOutlineColor
        (W.Rectangle,
         SfColor'
           (SfUint8 (Line_R), SfUint8 (Line_G), SfUint8 (Line_B),
            SfUint8 (Line_A)));
      SetFillColor
        (W.Rectangle,
         SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
      DrawRectangleShape (W.Impl, W.Rectangle, W.States);
   end Draw_Rectangle;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (W : Screen_Type; X, Y, Radius : Float; R, G, B, A : Integer)
   is
   begin
      Draw_Circle (W, X, Y, Radius, R, G, B, A, R, G, B, A);
   end Draw_Circle;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (W : Screen_Type; X, Y, Radius : Float;
      R, G, B, A, Line_R, Line_G, Line_B, Line_A : Integer)
   is
   begin
      SetRadius (W.Circle, Radius);
      SetPosition (W.Circle, SfVector2f'(X, Y));
      SetOutlineColor
        (W.Circle,
         SfColor'
           (SfUint8 (Line_R), SfUint8 (Line_G), SfUint8 (Line_B),
            SfUint8 (Line_A)));
      SetFillColor
        (W.Circle,
         SfColor'(SfUint8 (R), SfUint8 (G), SfUint8 (B), SfUint8 (A)));
      DrawCircleShape (W.Impl, W.Circle, W.States);
   end Draw_Circle;

end Quiza.Screen;
