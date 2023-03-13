pragma Ada_2012;

with Sf;                         use Sf;
with Sf.Graphics.BlendMode;      use Sf.Graphics.BlendMode;
with Sf.System;                  use Sf.System;
with Sf.System.Clock;            use Sf.System.Clock;
with Sf.System.Time;             use Sf.System.Time;
with Sf.Window;                  use Sf.Window;
with Sf.Window.Window;           use Sf.Window.Window;
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
with Sf.Graphics.RenderTexture;  use Sf.Graphics.RenderTexture;
with Sf.Graphics.Text;           use Sf.Graphics.Text;
with Sf.Graphics.Font;           use Sf.Graphics.Font;
with Sf.Graphics.Texture;        use Sf.Graphics.Texture;
with Sf.Graphics.Sprite;         use Sf.Graphics.Sprite;
with Sf.Graphics.Rect;           use Sf.Graphics.Rect;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;
with Ada.Text_IO;           use Ada.Text_IO;

package body Quiza.Screen is

   ---------------------------------------
   -- Recompute_Render_States_Transform --
   ---------------------------------------

   procedure Recompute_Render_States_Transform (W : Screen_Type) is
   begin
      W.States.transform := Identity;
      translate
        (W.States.transform'Access, W.Translation_DX, W.Translation_DY);
      rotateWithCenter
        (W.States.transform'Access, W.Rotation_Angle, W.Rotation_CX,
         W.Rotation_CY);
      scaleWithCenter
        (W.States.transform'Access, W.Scale_SX, W.Scale_SY, W.Scale_CX,
         W.Scale_CY);
   end Recompute_Render_States_Transform;

   -----------------
   -- Make_Screen --
   -----------------

   function Make_Screen
     (Width, Height : Positive; Full_Screen : Boolean) return Screen_Type
   is
      W : Screen_Type;
      --  Style : SfWindowStyle := sfDefaultStyle
   begin
      W.Impl :=
        create
          (VideoMode.sfVideoMode'
             (sfUint32 (Width), sfUint32 (Height),
              sfUint32 (Get_Desktop_Bits_Per_Pixel)),
           "Quiza",
           sfDefaultStyle or (if Full_Screen then sfFullscreen else 0));
      W.Offscreen_Mode := False;

      W.Scale_SX       := 1.0;
      W.Scale_SY       := 1.0;
      W.Scale_CX       := 1.0;
      W.Scale_CY       := 1.0;
      W.Rotation_Angle := 0.0;
      W.Rotation_CX    := 0.0;
      W.Rotation_CY    := 0.0;
      W.Translation_DX := 0.0;
      W.Translation_DY := 0.0;

      W.Clock            := create;
      W.States           := new sfRenderStates;
      W.States.blendMode := sfBlendAlpha;
      W.States.transform := Identity;
      W.States.texture   := null;
      W.States.shader    := null;
      W.Text             := create;
      W.Sprite           := create;

      W.Recompute_Render_States_Transform;

      W.Rectangle := create;
      setOutlineThickness (W.Rectangle, 1.0);

      W.Circle := create;
      setOutlineThickness (W.Circle, 1.0);

      W.Line := create;
      resize (W.Line, 2);
      setPrimitiveType (W.Line, sfLines);

      return W;
   end Make_Screen;

   -----------------
   -- Make_Screen --
   -----------------

   function Make_Screen return Screen_Type is
   begin
      return Make_Screen (Get_Desktop_Width, Get_Desktop_Height, True);
   end Make_Screen;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (W : Screen_Type) return Positive is
   begin
      return Positive (getSize (W.Impl).x);
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (W : Screen_Type) return Positive is
   begin
      return Positive (getSize (W.Impl).y);
   end Get_Height;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (W : Screen_Type) return Boolean is
   begin
      return Boolean (isOpen (W.Impl));
   end Is_Open;

   -----------
   -- Clear --
   -----------

   procedure Clear (W : Screen_Type) is
   begin
      if W.Offscreen_Mode then
         clear (W.Offscreen, sfColor'(0, 0, 0, 255));
      else
         clear (W.Impl);
      end if;
   end Clear;

   ----------------
   -- Begin_Draw --
   ----------------

   procedure Begin_Draw (W : in out Screen_Type) is
      T : sfTime;
      pragma Unreferenced (T);
   begin
      W.Offscreen_Mode := False;
      T                := restart (W.Clock);
   end Begin_Draw;

   ------------------------------------
   -- Begin_Draw_To_Offscreen_Buffer --
   ------------------------------------

   procedure Begin_Draw_To_Offscreen_Buffer
     (W : in out Screen_Type; Width, Height : Positive)
   is
      T : sfTime;
      pragma Unreferenced (T);
   begin
      if W.Offscreen /= null then
         raise Program_Error with "Previous offscreen not taken";
         --  Destroy (W.Offscreen);
      end if;

      W.Offscreen :=
        Sf.Graphics.RenderTexture.create
          (sfUint32 (Width), sfUint32 (Height), sfFalse);

      W.Offscreen_Mode := True;
      T                := restart (W.Clock);
   end Begin_Draw_To_Offscreen_Buffer;

   --------------
   -- End_Draw --
   --------------

   procedure End_Draw (W : in out Screen_Type) is
   begin
      if W.Offscreen_Mode then
         display (W.Offscreen);
      else
         display (W.Impl);
      end if;
      W.Time := getElapsedTime (W.Clock);
   end End_Draw;

   -------------------------------
   -- Download_Offscreen_Buffer --
   -------------------------------

   function Download_Offscreen_Buffer (W : in out Screen_Type) return Offscreen_Buffer_Type is
      Offscreen : Offscreen_Buffer_Type := (Tex => W.Offscreen);
   begin
      --  NOTE: caller takes ownership of the render texture
      W.Offscreen := null;
      return Offscreen;
   end Download_Offscreen_Buffer;

   ------------------------------
   -- Destroy_Offscreen_Buffer --
   ------------------------------

   procedure Destroy_Offscreen_Buffer (O : in out Offscreen_Buffer_Type) is
   begin
      if O.Tex = null then
         raise Program_Error with "Double destroy of Offscreen";
      end if;
      destroy (O.Tex);
      O.Tex := null;
   end Destroy_Offscreen_Buffer;

   ---------------------------
   -- Draw_Offscreen_Buffer --
   ---------------------------

   procedure Draw_Offscreen_Buffer (W : Screen_Type; X, Y : Float; O : Offscreen_Buffer_Type)
   is
   begin
      if O.Tex = null then
         raise Program_Error with "Attempt to draw null Offscreen";
      end if;
      setTexture (W.Sprite, getTexture (O.Tex), sfTrue);
      setPosition (W.Sprite, sfVector2f'(X, Y));
      setScale (W.Sprite, sfVector2f'(1.0, 1.0));
      if W.Offscreen_Mode then
         drawSprite (W.Offscreen, W.Sprite, W.States);
      else
         drawSprite (W.Impl, W.Sprite, W.States);
      end if;
   end Draw_Offscreen_Buffer;

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

      function To_Sf_Keycode (K : Keycode_Type) return sfKeyCode is
         C : sfKeyCode := sfKeyUnknown;
      begin
         case K is
            when Key_A =>
               C := sfKeyA;
            when Key_B =>
               C := sfKeyB;
            when Key_C =>
               C := sfKeyC;
            when Key_D =>
               C := sfKeyD;
            when Key_E =>
               C := sfKeyE;
            when Key_F =>
               C := sfKeyF;
            when Key_G =>
               C := sfKeyG;
            when Key_H =>
               C := sfKeyH;
            when Key_I =>
               C := sfKeyI;
            when Key_J =>
               C := sfKeyJ;
            when Key_K =>
               C := sfKeyK;
            when Key_L =>
               C := sfKeyL;
            when Key_M =>
               C := sfKeyM;
            when Key_N =>
               C := sfKeyN;
            when Key_O =>
               C := sfKeyO;
            when Key_P =>
               C := sfKeyP;
            when Key_Q =>
               C := sfKeyQ;
            when Key_R =>
               C := sfKeyR;
            when Key_S =>
               C := sfKeyS;
            when Key_T =>
               C := sfKeyT;
            when Key_U =>
               C := sfKeyU;
            when Key_V =>
               C := sfKeyV;
            when Key_W =>
               C := sfKeyW;
            when Key_X =>
               C := sfKeyX;
            when Key_Y =>
               C := sfKeyY;
            when Key_Z =>
               C := sfKeyZ;
            when Key_Num_0 =>
               C := sfKeyNum0;
            when Key_Num_1 =>
               C := sfKeyNum1;
            when Key_Num_2 =>
               C := sfKeyNum2;
            when Key_Num_3 =>
               C := sfKeyNum3;
            when Key_Num_4 =>
               C := sfKeyNum4;
            when Key_Num_5 =>
               C := sfKeyNum5;
            when Key_Num_6 =>
               C := sfKeyNum6;
            when Key_Num_7 =>
               C := sfKeyNum7;
            when Key_Num_8 =>
               C := sfKeyNum8;
            when Key_Num_9 =>
               C := sfKeyNum9;
            when Key_Escape =>
               C := sfKeyEscape;
            when Key_L_Control =>
               C := sfKeyLControl;
            when Key_L_Shift =>
               C := sfKeyLShift;
            when Key_L_Alt =>
               C := sfKeyLAlt;
            when Key_L_System =>
               C := sfKeyLSystem;
            when Key_R_Control =>
               C := sfKeyRControl;
            when Key_R_Shift =>
               C := sfKeyRShift;
            when Key_R_Alt =>
               C := sfKeyRAlt;
            when Key_R_System =>
               C := sfKeyRSystem;
            when Key_Menu =>
               C := sfKeyMenu;
            when Key_L_Bracket =>
               C := sfKeyLBracket;
            when Key_R_Bracket =>
               C := sfKeyRBracket;
            when Key_Semicolon =>
               C := sfKeySemicolon;
            when Key_Comma =>
               C := sfKeyComma;
            when Key_Period =>
               C := sfKeyPeriod;
            when Key_Quote =>
               C := sfKeyQuote;
            when Key_Slash =>
               C := sfKeySlash;
            when Key_Backslash =>
               C := sfKeyBackslash;
            when Key_Tilde =>
               C := sfKeyTilde;
            when Key_Equal =>
               C := sfKeyEqual;
            when Key_Hyphen =>
               C := sfKeyHyphen;
            when Key_Space =>
               C := sfKeySpace;
            when Key_Enter =>
               C := sfKeyEnter;
            when Key_Back =>
               C := sfKeyBack;
            when Key_Tab =>
               C := sfKeyTab;
            when Key_Page_Up =>
               C := sfKeyPageUp;
            when Key_Page_Down =>
               C := sfKeyPageDown;
            when Key_End =>
               C := sfKeyEnd;
            when Key_Home =>
               C := sfKeyHome;
            when Key_Insert =>
               C := sfKeyInsert;
            when Key_Delete =>
               C := sfKeyDelete;
            when Key_Add =>
               C := sfKeyAdd;
            when Key_Subtract =>
               C := sfKeySubtract;
            when Key_Multiply =>
               C := sfKeyMultiply;
            when Key_Divide =>
               C := sfKeyDivide;
            when Key_Left =>
               C := sfKeyLeft;
            when Key_Right =>
               C := sfKeyRight;
            when Key_Up =>
               C := sfKeyUp;
            when Key_Down =>
               C := sfKeyDown;
            when Key_Numpad_0 =>
               C := sfKeyNum0;
            when Key_Numpad_1 =>
               C := sfKeyNum1;
            when Key_Numpad_2 =>
               C := sfKeyNum2;
            when Key_Numpad_3 =>
               C := sfKeyNum3;
            when Key_Numpad_4 =>
               C := sfKeyNum4;
            when Key_Numpad_5 =>
               C := sfKeyNum5;
            when Key_Numpad_6 =>
               C := sfKeyNum6;
            when Key_Numpad_7 =>
               C := sfKeyNum7;
            when Key_Numpad_8 =>
               C := sfKeyNum8;
            when Key_Numpad_9 =>
               C := sfKeyNum9;
            when Key_F1 =>
               C := sfKeyF1;
            when Key_F2 =>
               C := sfKeyF2;
            when Key_F3 =>
               C := sfKeyF3;
            when Key_F4 =>
               C := sfKeyF4;
            when Key_F5 =>
               C := sfKeyF5;
            when Key_F6 =>
               C := sfKeyF6;
            when Key_F7 =>
               C := sfKeyF7;
            when Key_F8 =>
               C := sfKeyF8;
            when Key_F9 =>
               C := sfKeyF9;
            when Key_F10 =>
               C := sfKeyF10;
            when Key_F11 =>
               C := sfKeyF11;
            when Key_F12 =>
               C := sfKeyF12;
            when Key_F13 =>
               C := sfKeyF13;
            when Key_F14 =>
               C := sfKeyF14;
            when Key_F15 =>
               C := sfKeyF15;
            when Key_Pause =>
               C := sfKeyPause;
            when Key_Count =>
               C := sfKeyCount;
            when others =>
               return raise Program_Error with "Bad conversion to sfKeyCode";
         end case;
         return C;
      end To_Sf_Keycode;

      -----------------------
      -- To_Sf_Mousebutton --
      -----------------------

      function To_Sf_Mousebutton (K : Keycode_Type) return sfMouseButton is
         B : sfMouseButton;
      begin
         case K is
            when Mouse_Button_Left =>
               B := sfMouseLeft;
            when Mouse_Button_Middle =>
               B := sfMouseMiddle;
            when Mouse_Button_Right =>
               B := sfMouseRight;
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
         Pressed := Boolean (isKeyPressed (To_Sf_Keycode (K)));
      elsif K in Mousebutton_Key_Type then
         Pressed := Boolean (isButtonPressed (To_Sf_Mousebutton (K)));
      elsif K = Key_Any then
         --  Any key means any keyboard or any mouse button
         for I in Keyboard_Key_Type'Range loop
            if Boolean (isKeyPressed (To_Sf_Keycode (I))) then
               return True;
            end if;
         end loop;
         for I in Mousebutton_Key_Type loop
            if Boolean (isButtonPressed (To_Sf_Mousebutton (I))) then
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
      return Integer (Sf.Graphics.RenderWindow.Mouse.getPosition (W.Impl).x);
   end Mouse_X;

   -------------
   -- Mouse_Y --
   -------------

   function Mouse_Y (W : Screen_Type) return Integer is
   begin
      return Integer (Sf.Graphics.RenderWindow.Mouse.getPosition (W.Impl).y);
   end Mouse_Y;

   -------------------
   -- Get_Draw_Time --
   -------------------

   function Get_Draw_Time (W : Screen_Type) return Float is
   begin
      return asSeconds (W.Time);
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
               F : constant sfFont_Ptr :=
                 createFromFile (Full_Name (Dir_Entry));
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
            F : constant sfFont_Ptr :=
              W.Fonts.Element (To_Unbounded_String (Font_Name));
         begin
            setFont (W.Text, F);
            setString (W.Text, Str);
            setCharacterSize (W.Text, sfUint32 (Size));
            setFillColor
              (W.Text,
               sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));
            setPosition (W.Text, sfVector2f'(X, Y));
            drawText (W.Impl, W.Text, W.States);
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
               I : constant sfTexture_Ptr :=
                 createFromFile (Full_Name (Dir_Entry));
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
            I : constant sfTexture_Ptr :=
              W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            setTexture (W.Sprite, I, sfTrue);
            setColor
              (W.Sprite,
               sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));
            setPosition (W.Sprite, sfVector2f'(X, Y));
            setScale (W.Sprite, sfVector2f'(1.0, 1.0));
            --  DrawSprite (W.Impl, W.Sprite, W.States);
            if W.Offscreen_Mode then
               drawSprite (W.Offscreen, W.Sprite, W.States);
            else
               drawSprite (W.Impl, W.Sprite, W.States);
            end if;
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
            I : constant sfTexture_Ptr :=
              W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            setTexture (W.Sprite, I, sfTrue);
            setColor
              (W.Sprite,
               sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));
            setPosition (W.Sprite, sfVector2f'(X, Y));
            declare
               Img_W : constant Float :=
                 Float (Sf.Graphics.Texture.getSize (I).x);
               Img_H : constant Float :=
                 Float (Sf.Graphics.Texture.getSize (I).y);
               SX : constant Float := Width / Img_W;
               SY : constant Float := Height / Img_H;
            begin
               setScale (W.Sprite, sfVector2f'(SX, SY));
            end;
            --  DrawSprite (W.Impl, W.Sprite, W.States);
            if W.Offscreen_Mode then
               drawSprite (W.Offscreen, W.Sprite, W.States);
            else
               drawSprite (W.Impl, W.Sprite, W.States);
            end if;
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
            I : constant sfTexture_Ptr :=
              W.Textures.Element (To_Unbounded_String (Img_Name));
         begin
            setTexture (W.Sprite, I);
            setTextureRect
              (W.Sprite,
               sfIntRect'
                 (Integer (Rect_X), Integer (Rect_Y), Integer (Rect_W),
                  Integer (Rect_H)));
            setPosition (W.Sprite, sfVector2f'(X, Y));
            setColor
              (W.Sprite,
               sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));
            --  DrawSprite (W.Impl, W.Sprite, W.States);
            if W.Offscreen_Mode then
               drawSprite (W.Offscreen, W.Sprite, W.States);
            else
               drawSprite (W.Impl, W.Sprite, W.States);
            end if;
         end;
      end if;
   end Draw_Sub_Image;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (W : Screen_Type; X0, Y0, X1, Y1 : Float; R, G, B, A : Integer)
   is
      V0 : constant access Sf.Graphics.Vertex.sfVertex :=
        getVertex (W.Line, 0);
      V1 : constant access Sf.Graphics.Vertex.sfVertex :=
        getVertex (W.Line, 1);
   begin
      V0.position.x := X0;
      V0.position.y := Y0;
      V1.position.x := X1;
      V1.position.y := Y1;
      V0.color := sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A));
      V1.color := sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A));
      --  DrawVertexArray (W.Impl, W.Line, W.States);
      if W.Offscreen_Mode then
         drawVertexArray (W.Offscreen, W.Line, W.States);
      else
         drawVertexArray (W.Impl, W.Line, W.States);
      end if;
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
      setPosition (W.Rectangle, sfVector2f'(X, Y));
      setSize (W.Rectangle, sfVector2f'(Width, Height));
      setOutlineThickness (W.Rectangle, -1.0);
      setOutlineColor
        (W.Rectangle,
         sfColor'
           (sfUint8 (Line_R), sfUint8 (Line_G), sfUint8 (Line_B),
            sfUint8 (Line_A)));
      setFillColor
        (W.Rectangle,
         sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));
      --  DrawRectangleShape (W.Impl, W.Rectangle, W.States);

      if W.Offscreen_Mode then
         drawRectangleShape (W.Offscreen, W.Rectangle, W.States);
      else
         drawRectangleShape (W.Impl, W.Rectangle, W.States);
      end if;
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
      setRadius (W.Circle, Radius);
      setPosition (W.Circle, sfVector2f'(X, Y));
      setOutlineColor
        (W.Circle,
         sfColor'
           (sfUint8 (Line_R), sfUint8 (Line_G), sfUint8 (Line_B),
            sfUint8 (Line_A)));
      setFillColor
        (W.Circle,
         sfColor'(sfUint8 (R), sfUint8 (G), sfUint8 (B), sfUint8 (A)));

      --  DrawCircleShape (W.Impl, W.Circle, W.States);
      if W.Offscreen_Mode then
         drawCircleShape (W.Offscreen, W.Circle, W.States);
      else
         drawCircleShape (W.Impl, W.Circle, W.States);
      end if;

   end Draw_Circle;

end Quiza.Screen;
