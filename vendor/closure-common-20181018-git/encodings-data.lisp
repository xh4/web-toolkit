(in-package :runes-encoding)

(progn
  (add-name :us-ascii "ANSI_X3.4-1968") 
  (add-name :us-ascii "iso-ir-6") 
  (add-name :us-ascii "ANSI_X3.4-1986") 
  (add-name :us-ascii "ISO_646.irv:1991") 
  (add-name :us-ascii "ASCII") 
  (add-name :us-ascii "ISO646-US") 
  (add-name :us-ascii "US-ASCII") 
  (add-name :us-ascii "us") 
  (add-name :us-ascii "IBM367") 
  (add-name :us-ascii "cp367") 
  (add-name :us-ascii "csASCII") 

  (add-name :iso-8859-1 "ISO_8859-1:1987") 
  (add-name :iso-8859-1 "iso-ir-100") 
  (add-name :iso-8859-1 "ISO_8859-1") 
  (add-name :iso-8859-1 "ISO-8859-1") 
  (add-name :iso-8859-1 "latin1") 
  (add-name :iso-8859-1 "l1") 
  (add-name :iso-8859-1 "IBM819") 
  (add-name :iso-8859-1 "CP819") 
  (add-name :iso-8859-1 "csISOLatin1") 

  (add-name :iso-8859-2 "ISO_8859-2:1987") 
  (add-name :iso-8859-2 "iso-ir-101") 
  (add-name :iso-8859-2 "ISO_8859-2") 
  (add-name :iso-8859-2 "ISO-8859-2") 
  (add-name :iso-8859-2 "latin2") 
  (add-name :iso-8859-2 "l2") 
  (add-name :iso-8859-2 "csISOLatin2") 

  (add-name :iso-8859-3 "ISO_8859-3:1988") 
  (add-name :iso-8859-3 "iso-ir-109") 
  (add-name :iso-8859-3 "ISO_8859-3") 
  (add-name :iso-8859-3 "ISO-8859-3") 
  (add-name :iso-8859-3 "latin3") 
  (add-name :iso-8859-3 "l3") 
  (add-name :iso-8859-3 "csISOLatin3") 

  (add-name :iso-8859-4 "ISO_8859-4:1988") 
  (add-name :iso-8859-4 "iso-ir-110") 
  (add-name :iso-8859-4 "ISO_8859-4") 
  (add-name :iso-8859-4 "ISO-8859-4") 
  (add-name :iso-8859-4 "latin4") 
  (add-name :iso-8859-4 "l4") 
  (add-name :iso-8859-4 "csISOLatin4") 

  (add-name :iso-8859-6 "ISO_8859-6:1987") 
  (add-name :iso-8859-6 "iso-ir-127") 
  (add-name :iso-8859-6 "ISO_8859-6") 
  (add-name :iso-8859-6 "ISO-8859-6") 
  (add-name :iso-8859-6 "ECMA-114") 
  (add-name :iso-8859-6 "ASMO-708") 
  (add-name :iso-8859-6 "arabic") 
  (add-name :iso-8859-6 "csISOLatinArabic") 

  (add-name :iso-8859-7 "ISO_8859-7:1987") 
  (add-name :iso-8859-7 "iso-ir-126") 
  (add-name :iso-8859-7 "ISO_8859-7") 
  (add-name :iso-8859-7 "ISO-8859-7") 
  (add-name :iso-8859-7 "ELOT_928") 
  (add-name :iso-8859-7 "ECMA-118") 
  (add-name :iso-8859-7 "greek") 
  (add-name :iso-8859-7 "greek8") 
  (add-name :iso-8859-7 "csISOLatinGreek") 

  (add-name :iso-8859-8 "ISO_8859-8:1988") 
  (add-name :iso-8859-8 "iso-ir-138") 
  (add-name :iso-8859-8 "ISO_8859-8") 
  (add-name :iso-8859-8 "ISO-8859-8") 
  (add-name :iso-8859-8 "hebrew") 
  (add-name :iso-8859-8 "csISOLatinHebrew") 

  (add-name :iso-8859-5 "ISO_8859-5:1988") 
  (add-name :iso-8859-5 "iso-ir-144") 
  (add-name :iso-8859-5 "ISO_8859-5") 
  (add-name :iso-8859-5 "ISO-8859-5") 
  (add-name :iso-8859-5 "cyrillic") 
  (add-name :iso-8859-5 "csISOLatinCyrillic") 

  (add-name :iso-8859-9 "ISO_8859-9:1989") 
  (add-name :iso-8859-9 "iso-ir-148") 
  (add-name :iso-8859-9 "ISO_8859-9") 
  (add-name :iso-8859-9 "ISO-8859-9") 
  (add-name :iso-8859-9 "latin5") 
  (add-name :iso-8859-9 "l5") 
  (add-name :iso-8859-9 "csISOLatin5") 

  (add-name :iso-8859-13 "ISO-8859-13")

  (add-name :iso-8859-15 "ISO_8859-15") 
  (add-name :iso-8859-15 "ISO-8859-15") 
  (add-name :iso-8859-15 "Latin-9") 

  (add-name :iso-8859-14 "ISO_8859-14") 
  (add-name :iso-8859-14 "ISO-8859-14") 
  (add-name :iso-8859-14 "ISO_8859-14:1998") 
  (add-name :iso-8859-14 "iso-ir-199") 
  (add-name :iso-8859-14 "latin8") 
  (add-name :iso-8859-14 "iso-celtic") 
  (add-name :iso-8859-14 "l8") 

  (add-name :koi8-r "KOI8-R") 
  (add-name :koi8-r "csKOI8R") 

  (add-name :windows-1250 "windows-1250")

  (add-name :windows-1251 "windows-1251")

  (add-name :windows-1252 "windows-1252")

  (add-name :windows-1253 "windows-1253")

  (add-name :windows-1254 "windows-1254")

  (add-name :windows-1255 "windows-1255")

  (add-name :windows-1257 "windows-1257")

  (add-name :utf-8 "UTF-8") 

  (add-name :utf-16 "UTF-16") 

  (add-name :ucs-4 "ISO-10646-UCS-4") 
  (add-name :ucs-4 "UCS-4") 

  (add-name :ucs-2 "ISO-10646-UCS-2") 
  (add-name :ucs-2 "UCS-2") )


(progn
  (define-encoding :iso-8859-1
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-1)))

  (define-encoding :iso-8859-2
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-2)))

  (define-encoding :iso-8859-3
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-3)))

  (define-encoding :iso-8859-4
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-4)))

  (define-encoding :iso-8859-5
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-5)))

  (define-encoding :iso-8859-6
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-6)))

  (define-encoding :iso-8859-7
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-7)))

  (define-encoding :iso-8859-8
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-8)))

  (define-encoding :iso-8859-13
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-13)))

  (define-encoding :iso-8859-14
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-14)))

  (define-encoding :iso-8859-15
      (make-simple-8-bit-encoding 
        :charset (find-charset :iso-8859-15)))

  (define-encoding :koi8-r
      (make-simple-8-bit-encoding 
        :charset (find-charset :koi8-r)))

  (define-encoding :windows-1250
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1250)))

  (define-encoding :windows-1251
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1251)))

  (define-encoding :windows-1252
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1252)))

  (define-encoding :windows-1253
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1253)))

  (define-encoding :windows-1254
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1254)))

  (define-encoding :windows-1255
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1255)))

  (define-encoding :windows-1257
      (make-simple-8-bit-encoding 
        :charset (find-charset :windows-1257)))

  (define-encoding :utf-8 :utf-8)
  )

(progn
  (define-8-bit-charset :iso-8859-1
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00BA #x00BB #x00BC #x00BD #x00BE #x00BF
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x00D0 #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x00DD #x00DE #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x00F0 #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x00FD #x00FE #x00FF)

  (define-8-bit-charset :iso-8859-2
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x0104 #x02D8 #x0141 #x00A4 #x013D #x015A #x00A7
      #| #o25x |#   #x00A8 #x0160 #x015E #x0164 #x0179 #x00AD #x017D #x017B
      #| #o26x |#   #x00B0 #x0105 #x02DB #x0142 #x00B4 #x013E #x015B #x02C7
      #| #o27x |#   #x00B8 #x0161 #x015F #x0165 #x017A #x02DD #x017E #x017C
      #| #o30x |#   #x0154 #x00C1 #x00C2 #x0102 #x00C4 #x0139 #x0106 #x00C7
      #| #o31x |#   #x010C #x00C9 #x0118 #x00CB #x011A #x00CD #x00CE #x010E
      #| #o32x |#   #x0110 #x0143 #x0147 #x00D3 #x00D4 #x0150 #x00D6 #x00D7
      #| #o33x |#   #x0158 #x016E #x00DA #x0170 #x00DC #x00DD #x0162 #x00DF
      #| #o34x |#   #x0155 #x00E1 #x00E2 #x0103 #x00E4 #x013A #x0107 #x00E7
      #| #o35x |#   #x010D #x00E9 #x0119 #x00EB #x011B #x00ED #x00EE #x010F
      #| #o36x |#   #x0111 #x0144 #x0148 #x00F3 #x00F4 #x0151 #x00F6 #x00F7
      #| #o37x |#   #x0159 #x016F #x00FA #x0171 #x00FC #x00FD #x0163 #x02D9)

  (define-8-bit-charset :iso-8859-3
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x0126 #x02D8 #x00A3 #x00A4 #xFFFF #x0124 #x00A7
      #| #o25x |#   #x00A8 #x0130 #x015E #x011E #x0134 #x00AD #xFFFF #x017B
      #| #o26x |#   #x00B0 #x0127 #x00B2 #x00B3 #x00B4 #x00B5 #x0125 #x00B7
      #| #o27x |#   #x00B8 #x0131 #x015F #x011F #x0135 #x00BD #xFFFF #x017C
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #xFFFF #x00C4 #x010A #x0108 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #xFFFF #x00D1 #x00D2 #x00D3 #x00D4 #x0120 #x00D6 #x00D7
      #| #o33x |#   #x011C #x00D9 #x00DA #x00DB #x00DC #x016C #x015C #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #xFFFF #x00E4 #x010B #x0109 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #xFFFF #x00F1 #x00F2 #x00F3 #x00F4 #x0121 #x00F6 #x00F7
      #| #o37x |#   #x011D #x00F9 #x00FA #x00FB #x00FC #x016D #x015D #x02D9)

  (define-8-bit-charset :iso-8859-4
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x0104 #x0138 #x0156 #x00A4 #x0128 #x013B #x00A7
      #| #o25x |#   #x00A8 #x0160 #x0112 #x0122 #x0166 #x00AD #x017D #x00AF
      #| #o26x |#   #x00B0 #x0105 #x02DB #x0157 #x00B4 #x0129 #x013C #x02C7
      #| #o27x |#   #x00B8 #x0161 #x0113 #x0123 #x0167 #x014A #x017E #x014B
      #| #o30x |#   #x0100 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x012E
      #| #o31x |#   #x010C #x00C9 #x0118 #x00CB #x0116 #x00CD #x00CE #x012A
      #| #o32x |#   #x0110 #x0145 #x014C #x0136 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x0172 #x00DA #x00DB #x00DC #x0168 #x016A #x00DF
      #| #o34x |#   #x0101 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x012F
      #| #o35x |#   #x010D #x00E9 #x0119 #x00EB #x0117 #x00ED #x00EE #x012B
      #| #o36x |#   #x0111 #x0146 #x014D #x0137 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x0173 #x00FA #x00FB #x00FC #x0169 #x016B #x02D9)

  (define-8-bit-charset :iso-8859-5
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x0401 #x0402 #x0403 #x0404 #x0405 #x0406 #x0407
      #| #o25x |#   #x0408 #x0409 #x040A #x040B #x040C #x00AD #x040E #x040F
      #| #o26x |#   #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
      #| #o27x |#   #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E #x041F
      #| #o30x |#   #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
      #| #o31x |#   #x0428 #x0429 #x042A #x042B #x042C #x042D #x042E #x042F
      #| #o32x |#   #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
      #| #o33x |#   #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E #x043F
      #| #o34x |#   #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
      #| #o35x |#   #x0448 #x0449 #x044A #x044B #x044C #x044D #x044E #x044F
      #| #o36x |#   #x2116 #x0451 #x0452 #x0453 #x0454 #x0455 #x0456 #x0457
      #| #o37x |#   #x0458 #x0459 #x045A #x045B #x045C #x00A7 #x045E #x045F)

  (define-8-bit-charset :iso-8859-6
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0660 #x0661 #x0662 #x0663 #x0664 #x0665 #x0666 #x0667
      #| #o07x |#   #x0668 #x0669 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #xFFFF #xFFFF #xFFFF #x00A4 #xFFFF #xFFFF #xFFFF
      #| #o25x |#   #xFFFF #xFFFF #xFFFF #xFFFF #x060C #x00AD #xFFFF #xFFFF
      #| #o26x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o27x |#   #xFFFF #xFFFF #xFFFF #x061B #xFFFF #xFFFF #xFFFF #x061F
      #| #o30x |#   #xFFFF #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627
      #| #o31x |#   #x0628 #x0629 #x062A #x062B #x062C #x062D #x062E #x062F
      #| #o32x |#   #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x0637
      #| #o33x |#   #x0638 #x0639 #x063A #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o34x |#   #x0640 #x0641 #x0642 #x0643 #x0644 #x0645 #x0646 #x0647
      #| #o35x |#   #x0648 #x0649 #x064A #x064B #x064C #x064D #x064E #x064F
      #| #o36x |#   #x0650 #x0651 #x0652 #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o37x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF)

  (define-8-bit-charset :iso-8859-7
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x02BD #x02BC #x00A3 #xFFFF #xFFFF #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #xFFFF #x00AB #x00AC #x00AD #xFFFF #x2015
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x0384 #x0385 #x0386 #x00B7
      #| #o27x |#   #x0388 #x0389 #x038A #x00BB #x038C #x00BD #x038E #x038F
      #| #o30x |#   #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
      #| #o31x |#   #x0398 #x0399 #x039A #x039B #x039C #x039D #x039E #x039F
      #| #o32x |#   #x03A0 #x03A1 #xFFFF #x03A3 #x03A4 #x03A5 #x03A6 #x03A7
      #| #o33x |#   #x03A8 #x03A9 #x03AA #x03AB #x03AC #x03AD #x03AE #x03AF
      #| #o34x |#   #x03B0 #x03B1 #x03B2 #x03B3 #x03B4 #x03B5 #x03B6 #x03B7
      #| #o35x |#   #x03B8 #x03B9 #x03BA #x03BB #x03BC #x03BD #x03BE #x03BF
      #| #o36x |#   #x03C0 #x03C1 #x03C2 #x03C3 #x03C4 #x03C5 #x03C6 #x03C7
      #| #o37x |#   #x03C8 #x03C9 #x03CA #x03CB #x03CC #x03CD #x03CE #xFFFF)

  (define-8-bit-charset :iso-8859-8
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #xFFFF #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00D7 #x00AB #x00AC #x00AD #x00AE #x203E
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00F7 #x00BB #x00BC #x00BD #x00BE #xFFFF
      #| #o30x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o31x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o32x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o33x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #x2017
      #| #o34x |#   #x05D0 #x05D1 #x05D2 #x05D3 #x05D4 #x05D5 #x05D6 #x05D7
      #| #o35x |#   #x05D8 #x05D9 #x05DA #x05DB #x05DC #x05DD #x05DE #x05DF
      #| #o36x |#   #x05E0 #x05E1 #x05E2 #x05E3 #x05E4 #x05E5 #x05E6 #x05E7
      #| #o37x |#   #x05E8 #x05E9 #x05EA #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF)

  (define-8-bit-charset :iso-8859-9
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00BA #x00BB #x00BC #x00BD #x00BE #x00BF
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x011E #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x0130 #x015E #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x011F #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x0131 #x015F #x00FF)

  (define-8-bit-charset :iso-8859-13
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x201D #x00A2 #x00A3 #x00A4 #x201E #x00A6 #x00A7
      #| #o25x |#   #x00D8 #x00A9 #x0156 #x00AB #x00AC #x00AD #x00AE #x00C6
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x201C #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00F8 #x00B9 #x0157 #x00BB #x00BC #x00BD #x00BE #x00E6
      #| #o30x |#   #x0104 #x012E #x0100 #x0106 #x00C4 #x00C5 #x0118 #x0112
      #| #o31x |#   #x010C #x00C9 #x0179 #x0116 #x0122 #x0136 #x012A #x013B
      #| #o32x |#   #x0160 #x0143 #x0145 #x00D3 #x014C #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x0172 #x0141 #x015A #x016A #x00DC #x017B #x017D #x00DF
      #| #o34x |#   #x0105 #x012F #x0101 #x0107 #x00E4 #x00E5 #x0119 #x0113
      #| #o35x |#   #x010D #x00E9 #x017A #x0117 #x0123 #x0137 #x012B #x013C
      #| #o36x |#   #x0161 #x0144 #x0146 #x00F3 #x014D #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x0173 #x0142 #x015B #x016B #x00FC #x017C #x017E #x2019)

  (define-8-bit-charset :iso-8859-14
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x1E02 #x1E03 #x00A3 #x010A #x010B #x1E0A #x00A7
      #| #o25x |#   #x1E80 #x00A9 #x1E82 #x1E0B #x1EF2 #x00AD #x00AE #x0178
      #| #o26x |#   #x1E1E #x1E1F #x0120 #x0121 #x1E40 #x1E41 #x00B6 #x1E56
      #| #o27x |#   #x1E81 #x1E57 #x1E83 #x1E60 #x1EF3 #x1E84 #x1E85 #x1E61
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x0174 #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x1E6A
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x00DD #x0176 #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x0175 #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x1E6B
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x00FD #x0177 #x00FF)

  (define-8-bit-charset :iso-8859-15
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o21x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o23x |#   #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x20AC #x00A5 #x0160 #x00A7
      #| #o25x |#   #x0161 #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x017D #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x017E #x00B9 #x00BA #x00BB #x0152 #x0153 #x0178 #x00BF
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x00D0 #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x00DD #x00DE #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x00F0 #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x00FD #x00FE #x00FF)

  (define-8-bit-charset :koi8-r
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x2500 #x2502 #x250C #x2510 #x2514 #x2518 #x251C #x2524
      #| #o21x |#   #x252C #x2534 #x253C #x2580 #x2584 #x2588 #x258C #x2590
      #| #o22x |#   #x2591 #x2592 #x2593 #x2320 #x25A0 #x2219 #x221A #x2248
      #| #o23x |#   #x2264 #x2265 #x00A0 #x2321 #x00B0 #x00B2 #x00B7 #x00F7
      #| #o24x |#   #x2550 #x2551 #x2552 #x0451 #x2553 #x2554 #x2555 #x2556
      #| #o25x |#   #x2557 #x2558 #x2559 #x255A #x255B #x255C #x255D #x255E
      #| #o26x |#   #x255F #x2560 #x2561 #x0401 #x2562 #x2563 #x2564 #x2565
      #| #o27x |#   #x2566 #x2567 #x2568 #x2569 #x256A #x256B #x256C #x00A9
      #| #o30x |#   #x044E #x0430 #x0431 #x0446 #x0434 #x0435 #x0444 #x0433
      #| #o31x |#   #x0445 #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E
      #| #o32x |#   #x043F #x044F #x0440 #x0441 #x0442 #x0443 #x0436 #x0432
      #| #o33x |#   #x044C #x044B #x0437 #x0448 #x044D #x0449 #x0447 #x044A
      #| #o34x |#   #x042E #x0410 #x0411 #x0426 #x0414 #x0415 #x0424 #x0413
      #| #o35x |#   #x0425 #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E
      #| #o36x |#   #x041F #x042F #x0420 #x0421 #x0422 #x0423 #x0416 #x0412
      #| #o37x |#   #x042C #x042B #x0417 #x0428 #x042D #x0429 #x0427 #x042A)

  (define-8-bit-charset :windows-1250
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #xFFFF #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #xFFFF #x2030 #x0160 #x2039 #x015A #x0164 #x017D #x0179
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #xFFFF #x2122 #x0161 #x203A #x015B #x0165 #x017E #x017A
      #| #o24x |#   #x00A0 #x02C7 #x02D8 #x0141 #x00A4 #x0104 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x015E #x00AB #x00AC #x00AD #x00AE #x017B
      #| #o26x |#   #x00B0 #x00B1 #x02DB #x0142 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x0105 #x015F #x00BB #x013D #x02DD #x013E #x017C
      #| #o30x |#   #x0154 #x00C1 #x00C2 #x0102 #x00C4 #x0139 #x0106 #x00C7
      #| #o31x |#   #x010C #x00C9 #x0118 #x00CB #x011A #x00CD #x00CE #x010E
      #| #o32x |#   #x0110 #x0143 #x0147 #x00D3 #x00D4 #x0150 #x00D6 #x00D7
      #| #o33x |#   #x0158 #x016E #x00DA #x0170 #x00DC #x00DD #x0162 #x00DF
      #| #o34x |#   #x0155 #x00E1 #x00E2 #x0103 #x00E4 #x013A #x0107 #x00E7
      #| #o35x |#   #x010D #x00E9 #x0119 #x00EB #x011B #x00ED #x00EE #x010F
      #| #o36x |#   #x0111 #x0144 #x0148 #x00F3 #x00F4 #x0151 #x00F6 #x00F7
      #| #o37x |#   #x0159 #x016F #x00FA #x0171 #x00FC #x00FD #x0163 #x02D9)

  (define-8-bit-charset :windows-1251
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x0402 #x0403 #x201A #x0453 #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #x20AC #x2030 #x0409 #x2039 #x040A #x040C #x040B #x040F
      #| #o22x |#   #x0452 #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #xFFFF #x2122 #x0459 #x203A #x045A #x045C #x045B #x045F
      #| #o24x |#   #x00A0 #x040E #x045E #x0408 #x00A4 #x0490 #x00A6 #x00A7
      #| #o25x |#   #x0401 #x00A9 #x0404 #x00AB #x00AC #x00AD #x00AE #x0407
      #| #o26x |#   #x00B0 #x00B1 #x0406 #x0456 #x0491 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x0451 #x2116 #x0454 #x00BB #x0458 #x0405 #x0455 #x0457
      #| #o30x |#   #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
      #| #o31x |#   #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E #x041F
      #| #o32x |#   #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
      #| #o33x |#   #x0428 #x0429 #x042A #x042B #x042C #x042D #x042E #x042F
      #| #o34x |#   #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
      #| #o35x |#   #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E #x043F
      #| #o36x |#   #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
      #| #o37x |#   #x0448 #x0449 #x044A #x044B #x044C #x044D #x044E #x044F)

  (define-8-bit-charset :windows-1252
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #x0192 #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #x02C6 #x2030 #x0160 #x2039 #x0152 #xFFFF #x017D #xFFFF
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #x02DC #x2122 #x0161 #x203A #x0153 #xFFFF #x017E #x0178
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00BA #x00BB #x00BC #x00BD #x00BE #x00BF
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x00D0 #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x00DD #x00DE #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x00F0 #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x00FD #x00FE #x00FF)

  (define-8-bit-charset :windows-1253
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #x0192 #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #xFFFF #x2030 #xFFFF #x2039 #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #xFFFF #x2122 #xFFFF #x203A #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x0385 #x0386 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #xFFFF #x00AB #x00AC #x00AD #x00AE #x2015
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x0384 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x0388 #x0389 #x038A #x00BB #x038C #x00BD #x038E #x038F
      #| #o30x |#   #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
      #| #o31x |#   #x0398 #x0399 #x039A #x039B #x039C #x039D #x039E #x039F
      #| #o32x |#   #x03A0 #x03A1 #xFFFF #x03A3 #x03A4 #x03A5 #x03A6 #x03A7
      #| #o33x |#   #x03A8 #x03A9 #x03AA #x03AB #x03AC #x03AD #x03AE #x03AF
      #| #o34x |#   #x03B0 #x03B1 #x03B2 #x03B3 #x03B4 #x03B5 #x03B6 #x03B7
      #| #o35x |#   #x03B8 #x03B9 #x03BA #x03BB #x03BC #x03BD #x03BE #x03BF
      #| #o36x |#   #x03C0 #x03C1 #x03C2 #x03C3 #x03C4 #x03C5 #x03C6 #x03C7
      #| #o37x |#   #x03C8 #x03C9 #x03CA #x03CB #x03CC #x03CD #x03CE #xFFFF)

  (define-8-bit-charset :windows-1254
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #x0192 #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #x02C6 #x2030 #x0160 #x2039 #x0152 #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #x02DC #x2122 #x0161 #x203A #x0153 #xFFFF #xFFFF #x0178
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00BA #x00BB #x00BC #x00BD #x00BE #x00BF
      #| #o30x |#   #x00C0 #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7
      #| #o31x |#   #x00C8 #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF
      #| #o32x |#   #x011E #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x00D8 #x00D9 #x00DA #x00DB #x00DC #x0130 #x015E #x00DF
      #| #o34x |#   #x00E0 #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7
      #| #o35x |#   #x00E8 #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF
      #| #o36x |#   #x011F #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x00F8 #x00F9 #x00FA #x00FB #x00FC #x0131 #x015F #x00FF)

  (define-8-bit-charset :windows-1255
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #x0192 #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #x02C6 #x2030 #xFFFF #x2039 #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #x02DC #x2122 #xFFFF #x203A #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o24x |#   #x00A0 #x00A1 #x00A2 #x00A3 #x00AA #x00A5 #x00A6 #x00A7
      #| #o25x |#   #x00A8 #x00A9 #x00D7 #x00AB #x00AC #x00AD #x00AE #x203E
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00B8 #x00B9 #x00F7 #x00BB #x00BC #x00BD #x00BE #x00BF
      #| #o30x |#   #x05B0 #x05B1 #x05B2 #x05B3 #x05B4 #x05B5 #x05B6 #x05B7
      #| #o31x |#   #x05B8 #x05B9 #xFFFF #x05BB #x05BC #x05BD #x05BE #x05BF
      #| #o32x |#   #x05C0 #x05C1 #x05C2 #x05C3 #x05F0 #x05F1 #x05F2 #x05F3
      #| #o33x |#   #x05F4 #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF #xFFFF
      #| #o34x |#   #x05D0 #x05D1 #x05D2 #x05D3 #x05D4 #x05D5 #x05D6 #x05D7
      #| #o35x |#   #x05D8 #x05D9 #x05DA #x05DB #x05DC #x05DD #x05DE #x05DF
      #| #o36x |#   #x05E0 #x05E1 #x05E2 #x05E3 #x05E4 #x05E5 #x05E6 #x05E7
      #| #o37x |#   #x05E8 #x05E9 #x05EA #xFFFF #xFFFF #x200E #x200F #xFFFF)

  (define-8-bit-charset :windows-1257
      #| #o00x |#   #x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #| #o01x |#   #x0008 #x0009 #x000A #x000B #x000C #x000A #x000E #x000F
      #| #o02x |#   #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #| #o03x |#   #x0018 #x0019 #x001A #x001B #x001C #x001D #x001E #x001F
      #| #o04x |#   #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #| #o05x |#   #x0028 #x0029 #x002A #x002B #x002C #x002D #x002E #x002F
      #| #o06x |#   #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #| #o07x |#   #x0038 #x0039 #x003A #x003B #x003C #x003D #x003E #x003F
      #| #o10x |#   #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #| #o11x |#   #x0048 #x0049 #x004A #x004B #x004C #x004D #x004E #x004F
      #| #o12x |#   #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #| #o13x |#   #x0058 #x0059 #x005A #x005B #x005C #x005D #x005E #x005F
      #| #o14x |#   #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #| #o15x |#   #x0068 #x0069 #x006A #x006B #x006C #x006D #x006E #x006F
      #| #o16x |#   #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #| #o17x |#   #x0078 #x0079 #x007A #x007B #x007C #x007D #x007E #x007F
      #| #o20x |#   #x20AC #xFFFF #x201A #xFFFF #x201E #x2026 #x2020 #x2021
      #| #o21x |#   #xFFFF #x2030 #xFFFF #x2039 #xFFFF #x00A8 #x02C7 #x00B8
      #| #o22x |#   #xFFFF #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #| #o23x |#   #xFFFF #x2122 #xFFFF #x203A #xFFFF #x00AF #x02DB #xFFFF
      #| #o24x |#   #x00A0 #xFFFF #x00A2 #x00A3 #x00A4 #xFFFF #x00A6 #x00A7
      #| #o25x |#   #x00D8 #x00A9 #x0156 #x00AB #x00AC #x00AD #x00AE #x00C6
      #| #o26x |#   #x00B0 #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7
      #| #o27x |#   #x00F8 #x00B9 #x0157 #x00BB #x00BC #x00BD #x00BE #x00E6
      #| #o30x |#   #x0104 #x012E #x0100 #x0106 #x00C4 #x00C5 #x0118 #x0112
      #| #o31x |#   #x010C #x00C9 #x0179 #x0116 #x0122 #x0136 #x012A #x013B
      #| #o32x |#   #x0160 #x0143 #x0145 #x00D3 #x014C #x00D5 #x00D6 #x00D7
      #| #o33x |#   #x0172 #x0141 #x015A #x016A #x00DC #x017B #x017D #x00DF
      #| #o34x |#   #x0105 #x012F #x0101 #x0107 #x00E4 #x00E5 #x0119 #x0113
      #| #o35x |#   #x010D #x00E9 #x017A #x0117 #x0123 #x0137 #x012B #x013C
      #| #o36x |#   #x0161 #x0144 #x0146 #x00F3 #x014D #x00F5 #x00F6 #x00F7
      #| #o37x |#   #x0173 #x0142 #x015B #x016B #x00FC #x017C #x017E #x02D9)
  )

