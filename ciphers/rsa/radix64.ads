--  Copyright (c) 2010, John R. Marino
--  All rights reserved.
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


--  This package encodes binary into radix64 and back again (RFC 1113)
--
--    Value Encoding  Value Encoding  Value Encoding  Value Encoding
--          0 A            17 R            34 i            51 z
--          1 B            18 S            35 j            52 0
--          2 C            19 T            36 k            53 1
--          3 D            20 U            37 l            54 2
--          4 E            21 V            38 m            55 3
--          5 F            22 W            39 n            56 4
--          6 G            23 X            40 o            57 5
--          7 H            24 Y            41 p            58 6
--          8 I            25 Z            42 q            59 7
--          9 J            26 a            43 r            60 8
--         10 K            27 b            44 s            61 9
--         11 L            28 c            45 t            62 +
--         12 M            29 d            46 u            63 /
--         13 N            30 e            47 v
--         14 O            31 f            48 w         (pad) =
--         15 P            32 g            49 x
--         16 Q            33 h            50 y


with Key_4096; use Key_4096;
with RSATypes; use RSATypes;

package Radix64 is

   subtype OctetString   is String (1 .. 2);
   type TAscii is range 0 .. 127;

   Internal_Error_Code : TCryptoError := 0;

   function Encode_to_Radix64 (BinaryString : TBinaryString) return String;
   --  Converts an array of bytes to 64-bit encoded ASCII text
   --  The output string is 33% longer than the input string


   function Decode_Radix64 (Radix64String : String) return TBinaryString;
   --  Converts the 64-bit encoded ASCII text back into an array of bytes.
   --  Error code is stored internally
   --  Encrypted message length is stored internally


private

   PAD : constant MByte := 16#3D#;  --  Character of "="
   BIN2ASC : constant String (1 .. 64) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                          "abcdefghijklmnopqrstuvwxyz" &
                                          "0123456789+/";
   ASC2BIN : constant array (TAscii) of MByte := (
         16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#80#, 16#80#, 16#3E#, 16#80#, 16#80#, 16#80#, 16#3F#,
         16#34#, 16#35#, 16#36#, 16#37#, 16#38#, 16#39#, 16#3A#, 16#3B#,
         16#3C#, 16#3D#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#,
         16#07#, 16#08#, 16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#,
         16#0F#, 16#10#, 16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#,
         16#17#, 16#18#, 16#19#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#,
         16#80#, 16#1A#, 16#1B#, 16#1C#, 16#1D#, 16#1E#, 16#1F#, 16#20#,
         16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#,
         16#29#, 16#2A#, 16#2B#, 16#2C#, 16#2D#, 16#2E#, 16#2F#, 16#30#,
         16#31#, 16#32#, 16#33#, 16#80#, 16#80#, 16#80#, 16#80#, 16#80#
   );

   subtype FourSequence is String (1 .. 4);
   subtype TCount is Natural range 0 .. 2;
   subtype ShiftRange is Natural range 1 .. 7;


   function Octet2MByte (Octet : OctetString) return MByte;
   --  Takes a 2-character hexidecimal string and returns an MByte


   function EncodeByte (c : MByte) return Character;
   --  Takes a byte, and returns the appropriate Radix64 character.


   function Encode_Three_Bytes (BinaryString : TBinaryString;
                                Index        : Natural;
                                Count        : TCount)
   return FourSequence;
   --  Takes three bytes and returns four 7-bit ASCII characters


   function Scroll_Left (original : MByte;
                        bits     : ShiftRange) return MByte;
   --  Recieves a byte, and shifts it left by "bits" bits, but doesn't
   --  wrap them around.  Overflown bits just fall off.


   function Scroll_Right (original : MByte;
                        bits     : ShiftRange) return MByte;
   --  Recieves a byte, and shifts it right by "bits" bits, but doesn't
   --  wrap them around.  Overflown bits just fall off.





end Radix64;