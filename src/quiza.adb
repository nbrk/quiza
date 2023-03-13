pragma Ada_2012;

with Sf.Window.VideoMode;

package body Quiza is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      null;
   end Init;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      null;
   end Shutdown;

   -----------------------
   -- Get_Desktop_Width --
   -----------------------

   function Get_Desktop_Width return Positive is
   begin
      return Positive (Sf.Window.VideoMode.getDesktopMode.width);
   end Get_Desktop_Width;

   ------------------------
   -- Get_Desktop_Height --
   ------------------------

   function Get_Desktop_Height return Positive is
   begin
      return Positive (Sf.Window.VideoMode.getDesktopMode.height);
   end Get_Desktop_Height;

   --------------------------------
   -- Get_Desktop_Bits_Per_Pixel --
   --------------------------------

   function Get_Desktop_Bits_Per_Pixel return Positive is
   begin
      return Positive (Sf.Window.VideoMode.getDesktopMode.bitsPerPixel);
   end Get_Desktop_Bits_Per_Pixel;

end Quiza;
