# Quiza
*Quiza* stands for "Quick visualization in Ada". It is a small package on top of `ASFML` (the SFML bindings for Ada)
that greatly abstracts and simplifies some common routines like drawing to screen, checking for inputs, loading images, setting transforms, etc. 
With this package you can get something on the screen in no time. The intended use case is quick and dirty visualizations, with "quick" being
primarily *quick to code in Ada*, but the performance should be acceptable for these small tasks as well.

# Example
``` ada
with Quiza;
with Quiza.Screen; use Quiza.Screen;

procedure Test is
   S : Screen_Type := Make_Screen (1_024, 768, False);
begin
   Quiza.Init;
   S.Load_Fonts_Directory ("/home/nbrk/tmp/fonts");
   S.Load_Images_Directory ("/home/nbrk/tmp/images");

   while S.Is_Open loop
      exit when S.Is_Key_Or_Button_Pressed (Key_Escape);

      if S.Is_Key_Or_Button_Pressed (Key_Left) then
         S.Set_Draw_Translation
           (S.Get_Draw_Translation_DX + 10.0, S.Get_Draw_Translation_DY);
      end if;
      if S.Is_Key_Or_Button_Pressed (Key_Right) then
         S.Set_Draw_Translation
           (S.Get_Draw_Translation_DX - 10.0, S.Get_Draw_Translation_DY);
      end if;
      if S.Is_Key_Or_Button_Pressed (Key_Up) then
         S.Set_Draw_Translation
           (S.Get_Draw_Translation_DX, S.Get_Draw_Translation_DY + 10.0);
      end if;
      if S.Is_Key_Or_Button_Pressed (Key_Down) then
         S.Set_Draw_Translation
           (S.Get_Draw_Translation_DX, S.Get_Draw_Translation_DY - 10.0);
      end if;
      if S.Is_Key_Or_Button_Pressed (Key_Z) then
         S.Set_Draw_Scale
           (S.Get_Draw_Scale_SX + 0.1, S.Get_Draw_Scale_SY + 0.1,
            Float (S.Mouse_X), Float (S.Mouse_Y));
      end if;
      if S.Is_Key_Or_Button_Pressed (Key_X) then
         S.Set_Draw_Scale
           (S.Get_Draw_Scale_SX - 0.1, S.Get_Draw_Scale_SY - 0.1,
            Float (S.Mouse_X), Float (S.Mouse_Y));
      end if;

      S.Begin_Draw;
      S.Clear;
      S.Draw_Line (0.0, 0.0, 100.0, 100.0, 0, 255, 0, 255);
      S.Draw_Circle (100.0, 100.0, 60.0, 0, 0, 255, 255);
      S.Draw_Rectangle
        (100.0, 200.0, 200.0, 200.0, 0, 0, 0, 255, 255, 255, 255, 255);
      S.Draw_Text
        (200.0, 200.0, "Hello", 0, 255, 255, 255, "DroidSans.ttf", 24);

      if S.Is_Mouse_Inside then
         S.Draw_Text
           (Float (S.Mouse_X) + 10.0, Float (S.Mouse_Y) + 10.0,
            S.Mouse_X'Image & ", " & S.Mouse_Y'Image, 255, 255, 255, 255,
            "Roboto-Bold.ttf", 20);

         S.Draw_Image (300.0, 100.0, "nick.jpg");

         if S.Is_Key_Or_Button_Pressed (Mouse_Button_Right) then
            S.Draw_Line
              (1_024.0 / 2.0, 768.0 / 2.0, Float (S.Mouse_X),
               Float (S.Mouse_Y), 255, 255, 0, 255);
         end if;
      end if;

      S.End_Draw;

      delay (0.016_6) - Duration (S.Get_Draw_Time);
   end loop;
```
