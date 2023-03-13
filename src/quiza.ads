package Quiza is

   procedure Init;
   --  Initialize Quiza

   procedure Shutdown;
   --  Shutdown Quiza

   function Get_Desktop_Width return Positive;
   --  Get current desktop video mode width

   function Get_Desktop_Height return Positive;
   --  Get current desktop video mode height

   function Get_Desktop_Bits_Per_Pixel return Positive;
   --  Get current desktop video mode BPP

end Quiza;
