--  with Quiza; use Quiza;
--  with Ada.Numerics.Discrete_Random;

with Quiza;
with Quiza.Screen; use Quiza.Screen;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test is

   --  subtype R_Int is Integer range 1 .. 50;

   --  package Int_Rnd is new Ada.Numerics.Discrete_Random (R_Int);

   --  Gen : Int_Rnd.Generator;

   W : Screen_Type := Make_Screen (1_024, 768, False);

begin
   --  Int_Rnd.Reset (Gen);
   --
   --  Init (1_024, 768, False);
   --
   --  for y in 100 .. 768 loop
   --     for x in 1 .. 1_024 loop
   --        Begin_Drawing;
   --        Draw_Rectangle (100, 100, 600, 550, 0, 0, 140);
   --        Draw_Line
   --          (0, 0, x + Int_Rnd.Random (Gen), y + Int_Rnd.Random (Gen), 0, 255,
   --           0);
   --        Draw_Rectangle (x, y, 50, 100, 100, 0, 0);
   --        Draw_Circle (200, 070, 100, 0, 255, 0, 100);
   --        Draw_Circle (Mouse_X - 30, Mouse_Y - 30, 30, 255, 255, 0, 100);
   --        End_Drawing;
   --        delay (1.0 / 60.0);
   --     end loop;
   --  end loop;
   --
   --  Shutdown;
   --  null;
   W.Load_Fonts_Directory ("/home/nbrk/tmp/fonts");
   W.Load_Images_Directory ("/home/nbrk/tmp/images");

   while W.Is_Open loop
      exit when W.Is_Key_Or_Button_Pressed (Key_Escape);

      if W.Is_Key_Or_Button_Pressed (Key_Left) then
         W.Set_Draw_Translation
           (W.Get_Draw_Translation_DX + 10.0, W.Get_Draw_Translation_DY);
      end if;
      if W.Is_Key_Or_Button_Pressed (Key_Right) then
         W.Set_Draw_Translation
           (W.Get_Draw_Translation_DX - 10.0, W.Get_Draw_Translation_DY);
      end if;
      if W.Is_Key_Or_Button_Pressed (Key_Up) then
         W.Set_Draw_Translation
           (W.Get_Draw_Translation_DX, W.Get_Draw_Translation_DY + 10.0);
      end if;
      if W.Is_Key_Or_Button_Pressed (Key_Down) then
         W.Set_Draw_Translation
           (W.Get_Draw_Translation_DX, W.Get_Draw_Translation_DY - 10.0);
      end if;
      if W.Is_Key_Or_Button_Pressed (Key_Z) then
         W.Set_Draw_Scale
           (W.Get_Draw_Scale_SX + 0.1, W.Get_Draw_Scale_SY + 0.1,
            Float (W.Mouse_X), Float (W.Mouse_Y));
      end if;
      if W.Is_Key_Or_Button_Pressed (Key_X) then
         W.Set_Draw_Scale
           (W.Get_Draw_Scale_SX - 0.1, W.Get_Draw_Scale_SY - 0.1,
            Float (W.Mouse_X), Float (W.Mouse_Y));
      end if;

      W.Begin_Draw;
      W.Clear;
      W.Draw_Line (0.0, 0.0, 100.0, 100.0, 0, 255, 0, 255);
      W.Draw_Circle (100.0, 100.0, 60.0, 0, 0, 255, 255);
      W.Draw_Rectangle
        (100.0, 200.0, 200.0, 200.0, 0, 0, 0, 255, 255, 255, 255, 255);
      --  W.Draw_Line
      --    (1_024.0 / 2.0, 768.0 / 2.0, Float (W.Mouse_X), Float (W.Mouse_Y), 255,
      --     255, 0, 255);
      W.Draw_Text
        (200.0, 200.0, "Hello", 0, 255, 255, 255, "DroidSans.ttf", 24);

      if W.Is_Mouse_Inside then
         W.Draw_Text
           (Float (W.Mouse_X) + 10.0, Float (W.Mouse_Y) + 10.0,
            W.Mouse_X'Image & ", " & W.Mouse_Y'Image, 255, 255, 255, 255,
            "Roboto-Bold.ttf", 20);

         W.Draw_Image (300.0, 100.0, "nick.jpg");

      end if;

      W.End_Draw;

      delay (0.016_6) - Duration (W.Get_Draw_Time);
   end loop;

end Test;
