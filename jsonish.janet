(setdyn
  :doc
  ``
  Conversion between JSON and JDN.

  * decode: buffer of JSON content -> JDN
  * encode: JDN -> buffer of JSON content
  ``)

# escaping a unicode code point in a json string takes one of the
# following two forms:
#
#   \uHHHH
#   \uHHHH\uHHHH
#
# the first form (\uHHHH) is for a bmp code point.
# the second form (\uHHHH\uHHHH) is for a non-bmp code point and corresponds
# to a UTF-16 surrogate pair.

# First code point  Last code point  Byte 1    Byte 2    Byte 3    Byte 4
# ----------------  ---------------  ------    ------    ------    ------
# U+0000            U+007F           0xxxxxxx
# U+0080            U+07FF           110xxxxx  10xxxxxx
# U+0800            U+FFFF           1110xxxx  10xxxxxx  10xxxxxx
# U+10000           U+10FFFF         11110xxx  10xxxxxx  10xxxxxx  10xxxxxx
#
# https://en.wikipedia.org/wiki/UTF-8
#
# note that only the first three rows above are for bmp
(defn bmp-to-utf-8
  [bmp-cp]
  (def buf @"")
  (cond
    (<= bmp-cp 0x7f)
    (buffer/push buf bmp-cp)
    #
    (<= bmp-cp 0x7ff)
    (buffer/push buf
                 (bor 2r1100_0000
                      (band 2r1_1111 (brshift bmp-cp 6)))
                 (bor 2r1000_0000
                      (band 2r11_1111 bmp-cp)))
    #
    (<= bmp-cp 0xffff)
    (buffer/push buf
                 (bor 2r1110_0000
                      (band 2r1111 (brshift bmp-cp 12)))
                 (bor 2r1000_0000
                      (band 2r11_1111 (brshift bmp-cp 6)))
                 (bor 2r1000_0000
                      (band 2r11_1111 bmp-cp)))
    #
    (errorf "code point out of range: %n" bmp-cp))
  #
  buf)

# U' = yyyyyyyyyyxxxxxxxxxx  // U - 0x10000
# W1 = 110110yyyyyyyyyy      // 0xD800 + yyyyyyyyyy
# W2 = 110111xxxxxxxxxx      // 0xDC00 + xxxxxxxxxx
#
# (W1 - high surrogate)
# (W2 - low surrogate)
# (U is target code point, U' is offset from U)
#
# ...
#
# Since the ranges for the high surrogates (0xD800–0xDBFF), low
# surrogates (0xDC00–0xDFFF), and valid BMP characters (0x0000–0xD7FF,
# 0xE000–0xFFFF) are disjoint, it is not possible for a surrogate to
# match a BMP character, or for two adjacent code units to look like a
# legal surrogate pair.
#
# https://en.wikipedia.org/wiki/UTF-16
(defn surr-pair-to-utf-8
  [high low]
  (def buf @"")
  (def cp
    (+ 0x10000
       (bor (blshift (band 2r11_1111_1111 high) 10)
            (band 2r11_1111_1111 low))))
  (buffer/push buf
               (bor 2r1111_0000
                    (band 2r111 (brshift cp 18)))
               (bor 2r1000_0000
                    (band 2r11_1111 (brshift cp 12)))
               (bor 2r1000_0000
                    (band 2r11_1111 (brshift cp 6)))
               (bor 2r1000_0000
                    (band 2r11_1111 cp)))
  #
  buf)

(def grammar
  ~@{:main (some :input)
     #
     :input (choice :ws :value)
     # XXX: should multiple lines be "eaten" at once?
     # XXX: should eol be separate?
     :ws (choice (some (set " \f\t\v"))
                 (choice "\r\n" "\r" "\n"))
     #
     :value (choice :null :boolean :string :number :array :object)
     #
     :null (cmt (capture (sequence "null" (not :rest-char)))
                ,(fn [_] :null))
     # using `replace` (not cmt) so false can be returned...
     :boolean (replace (capture (sequence (choice "false" "true")
                                          (not :rest-char)))
                       ,|(if (= $ "false") false true))
     #
     :string
     (cmt (capture (sequence `"` (any :char) `"`))
          # XXX: kind of strange how the arguments work here...
          ,|(if (not (empty? $&))
              (string $0 ;(tuple/slice $& 0 -2))
              # drop surrounding quotes
              (string/slice $0 1 -2)))
     #
     :char
     (choice
       (if (range "\x00\x1f") (error (constant "invalid char")))
       :escape
       # only skip \x22 (double quote) because \ handled in :escape above
       (cmt (capture (range "\x20\x21" "\x23\x7f"))
            ,|(do
                #(printf " char $: %n" $)
                $))
       # non-leading byte value in utf-8
       (if (range "\x80\xbf") (error (constant "unexpected byte")))
       # byte 1: 110xxxxx -> 11000000 - 11010111 -> \xc0 - \xd7
       # byte 2: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       (cmt (capture (sequence (range "\xc0\xd7")
                               (range "\x80\xbf")))
            ,|(do
                #(printf " 2 bytes $: %n" $)
                $))
       (if (range "\xd8\xdb") (error (constant "unexpected utf-16 (high)")))
       (if (range "\xdc\xdf") (error (constant "unexpected uft-16 (low)")))
       # byte 1: 1110xxxx -> 11100000 - 11101111 -> \xe0 - \xef
       # byte 2: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       # byte 3: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       (cmt (capture (sequence (range "\xe0\xef")
                               (range "\x80\xbf")
                               (range "\x80\xbf")))
            ,|(do
                #(printf " 3 bytes $: %n" $)
                $))
       # byte 1: 11110xxx -> 11110000 - 11110111 -> \xf0 - \xf7
       # byte 2: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       # byte 3: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       # byte 4: 10xxxxxx -> 10000000 - 10111111 -> \x80 - \xbf
       (cmt (capture (sequence (range "\xf0\xf7")
                               (range "\x80\xbf")
                               (range "\x80\xbf")
                               (range "\x80\xbf")))
            ,|(do
                #(printf " 4 bytes $: %n" $)
                $))
       (if (range "\xf8\xff") (error (constant "invalid byte"))))
     # XXX: needs work? - https://262.ecma-international.org/5.1/#sec-7.8.4
     :escape
     (sequence
       `\`
       (choice (cmt (capture (set `"\/bfnrt`))
                    ,|(do
                        #(printf " escape $: %n" $)
                        (get {`"` `"` `\` `\` `/` `/`
                              `b` "\x08" `f` "\x0c"
                              `n` "\x0a" `r` "\x0d"
                              `t` "\x09"} $)))
               # non-bmp char done via surrogate pair in json
               (cmt (sequence "u" (number 4 16)
                              `\u` (number 4 16))
                    ,|(do
                        #(printf " non-bmp escape check $: %n" $0 $1)
                        (when (and (<= 0xd800 $0 0xdbff)
                                   (<= 0xdc00 $1 0xdfff))
                          (surr-pair-to-utf-8 $0 $1))))
               # bmp character done via utf-8 in json
               (cmt (sequence "u" (number 4 16))
                    ,|(do
                        #(printf " bmp escape $: %n" $)
                        (bmp-to-utf-8 $0)))
               (error (constant "invalid escape"))))
     #
     :number (cmt (capture (sequence (opt "-")
                                     (choice :float :integer)))
                  ,|(scan-number $))
     #
     :float (sequence (choice (sequence :integer "." :d+)
                              :integer)
                      (opt (sequence (set "eE")
                                     (opt (choice "+" "-"))
                                     :d+)))
     #
     :integer (choice "0"
                      (sequence (range "19") :d*))
     #
     :rest-char (range "$$" "09" "AZ" "__" "az" "\x80\xFF")
     # XXX: no nice error messages...
     :array
     (cmt (capture (sequence
                     "[" (any :ws)
                     (choice "]"
                             (sequence (any (sequence :value (any :ws)
                                                      "," (any :ws)
                                                      (not "]")))
                                       :value (any :ws)
                                       "]"))))
          ,|(array ;(slice $& 0 -2)))
     # XXX: no nice error messages...
     :object
     (cmt (capture (sequence
                     "{" (any :ws)
                     (choice "}"
                             (sequence (any (sequence :member (any :ws)
                                                      "," (any :ws)
                                                      (not "}")))
                                       :member (any :ws)
                                       "}"))))
          ,|(table ;(slice $& 0 -2)))
     #
     :member (sequence :string (any :ws)
                       ":" (any :ws)
                       :value)
     })

(defn decode
  ``
  Convert buffer of JSON to JDN.

  `null` is decoded as the keyword `:null`.
  ``
  [src &opt start]
  (default start 0)
  (first (peg/match grammar src start)))

(comment

  (decode "null")
  # =>
  :null

  (decode "false")
  # =>
  false

  (decode "1")
  # =>
  1

  (decode `"fun string"`)
  # =>
  "fun string"

  # 2-byte character
  (decode `"¢"`)
  # =>
  "\xc2\xa2"

  # 3-byte character
  (decode `"空"`)
  # =>
  "\xe7\xa9\xba"

  # 4-byte character
  (decode `"🦆"`)
  # =>
  "\xF0\x9F\xA6\x86"

  # becomes 1 byte
  (decode `"\u0020"`)
  # =>
  " "

  # becomes 2 bytes
  (decode `"\u00a2"`)
  # =>
  "¢"

  # becomes 3 bytes
  (decode `"\ud55c"`)
  # =>
  "한"

  # surrogate pair
  # utf-16: 0xd834 0xdd1e
  #  utf-8: 0xf0 0x9d 0x84 0x9e
  (decode `"\ud834\udd1e"`)
  # =>
  "\xf0\x9d\x84\x9e"

  (decode `"\ud834\udd1e"`)
  # =>
  "𝄞"

  (decode `[1, 2, 8, 9]`)
  # =>
  @[1 2 8 9]

  (decode `{"alice": "smile", "bob": "breathe"}`)
  # =>
  @{"alice" "smile" "bob" "breathe"}

  )

# XXX: n-bytes can be computed from bytes but this is not done because
#      this function is called repeatedly with the same value for
#      bytes and n-bytes
(defn parse-code-point
  [bytes n-bytes i]
  # https://stackoverflow.com/questions/9356169/utf-8-continuation-bytes
  (defn cont-byte?
    [byte]
    (= 2r1000_0000 (band 2r1100_0000 byte)))
  #
  (def byte-1 (get bytes i))
  (cond
    # 1-byte sequence
    (<= byte-1 2r0111_1111)
    [byte-1 1]
    # leading bytes should not be of the form 10xx_xxxx
    (<= # 2r1000_0000
        byte-1 2r1011_1111)
    (errorf "unexpected leading byte: %n at index: %d" byte-1 i)
    # 2-byte sequence - starts with 110x_xxxx
    (<= # 2r1100_0000
        byte-1 2r1101_1111)
    (do
      (assert (< (+ i 1) n-bytes)
              (string/format "truncated 2-byte utf-8 seq at index: %d" i))
      (def byte-2 (get bytes (+ i 1)))
      (assert (cont-byte? byte-2)
              (string/format "not continuation byte at index: %d" (+ i 1)))
      [(+ (blshift (band 2r01_1111 byte-1) 6)
          (band 2r11_1111 byte-2))
       2])
    #
    # if the surrogate pair ranges [0xd800, 0xdbff] and [0xdc00, 0xdfff]
    # got turned into utf-8 byte sequences (which they shouldn't),
    # they would occupy (see misc.janet):
    #
    #   [0xEDA080, 0xEDAFBF]
    #   [0xEDAFC0, 0xEDBFBF]
    #
    # but there are "legit" things that use a leading byte of 0xED, so
    # would need to examine later bytes to tell if there is an error...
    #
    # 3-byte sequence - starts with 1110_xxxx
    (<= # 2r1110_0000
        byte-1 2r1110_1111)
    (do
      (assert (< (+ i 2) n-bytes)
              (string/format "truncated 3-byte utf-8 seq near index: %d" i))
      (def byte-2 (get bytes (+ i 1)))
      (assert (cont-byte? byte-2)
              (string/format "not continuation byte at index: %d" (+ i 1)))
      (def byte-3 (get bytes (+ i 2)))
      (assert (cont-byte? byte-3)
              (string/format "not continuation byte at index: %d" (+ i 2)))
      [(+ (blshift (band 2r00_1111 byte-1) 12)
          (blshift (band 2r11_1111 byte-2) 6)
          (band 2r11_1111 byte-3))
       3])
    # 4-byte sequence - starts with 1111_0xxx
    (<= # 2r1111_0000
        byte-1 2r1111_0111)
    (do
      (assert (< (+ i 3) n-bytes)
              (string/format "truncated 4-byte utf-8 seq near index: %d" i))
      (def byte-2 (get bytes (+ i 1)))
      (assert (cont-byte? byte-2)
              (string/format "not continuation byte at index: %d" (+ i 1)))
      (def byte-3 (get bytes (+ i 2)))
      (assert (cont-byte? byte-3)
              (string/format "not continuation byte at index: %d" (+ i 2)))
      (def byte-4 (get bytes (+ i 3)))
      (assert (cont-byte? byte-4)
              (string/format "not continuation byte at index: %d" (+ i 3)))
      [(+ (blshift (band 2r00_0111 byte-1) 18)
          (blshift (band 2r11_1111 byte-2) 12)
          (blshift (band 2r11_1111 byte-3) 6)
          (band 2r11_1111 byte-4))
       4])
    #
    (errorf "invalid byte: %n at index: %d" byte-1 i)))

# Usually if `bytes` is a symbol or keyword, it should consist of
# UTF-8 only (because of janet_valid_utf8 in parse.c), but it's
# possible to programmatically construct symbols and keywords that
# have non-UTF-8 sequences in them.
#
# `bytes` can also be a string or buffer so can contain any 8-bit
# value.
#
# Technically, one could handle `bytes` with certain non-UTF-8
# sequences in it, but here, only UTF-8 will be handled, and non-UTF-8
# will be guarded against.
(defn to-json-str
  ``
  Return a buffer suitable for use as a JSON string based on `bytes`.

  `bytes` should be a UTF-8 byte sequence.
  ``
  [bytes]
  (defn hex-strs
    [code-point]
    (def hex-str
      {0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9"
       10 "a" 11 "b" 12 "c" 13 "d" 14 "e" 15 "f"})
    #
    [(get hex-str (band 2r1111 (brshift code-point 12)))
     (get hex-str (band 2r1111 (brshift code-point 8)))
     (get hex-str (band 2r1111 (brshift code-point 4)))
     (get hex-str (band 2r1111 code-point))])
  #
  (def buf @`"`)
  (var i 0)
  (def n-bytes (length bytes))
  (while (< i n-bytes)
    # get code point and next round offset
    (def [cp j] (parse-code-point bytes n-bytes i))
    (+= i j)
    # put appropriate form of code point in buf
    (cond
      # printable ascii - better debuggability
      (<= 0x20 cp 0x7f)
      (do
        (when (or (= (chr `"`) cp)
                  (= (chr `\`) cp))
          (buffer/push-string buf `\`))
        (buffer/push buf cp))
      # non-ascii utf-8 and non-printable ascii (0x00 - 0x1f)
      (<= cp 0xffff)
      (buffer/push-string buf `\u` ;(hex-strs cp))
      # utf-16 surrogate pair
      (do
        (def [high low]
          (let [cp-offset (- cp 0x10000)]
            [(+ 0xd800 (brshift cp-offset 10))
             (+ 0xdc00 (band 2r11_1111_1111 cp-offset))]))
        (buffer/push-string buf
                            `\u` ;(hex-strs high)
                            `\u` ;(hex-strs low)))))
  #
  (buffer/push-string buf `"`))

(comment

  (to-json-str "a")
  # =>
  @`"a"`

  (to-json-str `"`)
  # =>
  @`"\""`

  (to-json-str `\`)
  # =>
  @`"\\"`

  (to-json-str "¢")
  # =>
  @`"\u00a2"`

  (to-json-str "空")
  # =>
  @`"\u7a7a"`

  (to-json-str "🦆")
  # =>
  @`"\ud83e\udd86"`

  (to-json-str "𝄞")
  # =>
  @`"\ud834\udd1e"`

  (to-json-str `a " \ ¢ 空 🦆 𝄞`)
  # =>
  @`"a \" \\ \u00a2 \u7a7a \ud83e\udd86 \ud834\udd1e"`

  )

(defn encode*
  [jdn buf]
  (def the-type (type jdn))
  (cond
    (or (= :nil the-type)
        (= :null jdn))
    (buffer/push-string buf "null")
    #
    (in {:boolean true
         :number true} the-type)
    (buffer/push-string buf (string jdn))
    #
    (in {:symbol true
         :string true
         :buffer true
         :keyword true} the-type)
    (buffer/push-string buf (to-json-str jdn))
    #
    (in {:array true
         :tuple true} the-type)
    (do
      (buffer/push-string buf "[")
      (each elt jdn
        (encode* elt buf)
        (buffer/push-string buf ", "))
      # drop trailing comma + space if added
      (when (string/has-suffix? ", " buf)
        (buffer/popn buf 2))
      (buffer/push-string buf "]"))
    #
    (in {:table true
         :struct true} the-type)
    (do
      (buffer/push-string buf "{")
      (loop [[key value] :in (pairs jdn)]
        (encode* key buf)
        (buffer/push-string buf ": ")
        (encode* value buf)
        (buffer/push-string buf ", "))
      # drop trailing comma + space if added
      (when (string/has-suffix? ", " buf)
        (buffer/popn buf 2))
      (buffer/push-string buf "}"))
    # encoding of other types is not supported
    (errorf "value: %s has unsupported type: %s" jdn the-type)
    ))

(defn encode
  [jdn]
  ``
  Convert JDN to buffer of JSON.

  `:null` is converted to `null`.
  ``
  (def buf @"")
  (encode* jdn buf)
  buf)

(comment

  (encode nil)
  # =>
  @"null"

  (encode :null)
  # =>
  @"null"

  (encode true)
  # =>
  @"true"

  (encode 8)
  # =>
  @"8"

  (encode "a string")
  # =>
  @`"a string"`

  (encode @[2 3 8])
  # =>
  @"[2, 3, 8]"

  (deep= (encode {"Socrates" 1 "Seneca" 8})
         (encode {"Seneca" 8 "Socrates" 1}))
  # =>
  true

  )

