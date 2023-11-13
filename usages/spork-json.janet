(import ../jsonish :prefix "")

(comment

  (decode (encode 1))
  # =>
  1

  (decode (encode 100))
  # =>
  100

  (decode (encode true))
  # =>
  true

  (decode (encode false))
  # =>
  false

  (decode (encode (range 1000)))
  # =>
  (range 1000)

  (decode (encode @{"two" 2 "four" 4 "six" 6}))
  # =>
  @{"two" 2 "four" 4 "six" 6}

  (decode (encode @{"hello" "world"}))
  # =>
  @{"hello" "world"}

  (decode (encode @{"john" 1 "billy" "joe" "a" @[1 2 3 4 -1000]}))
  # =>
  @{"john" 1 "billy" "joe" "a" @[1 2 3 4 -1000]}

  (decode (encode @{"john" 1 "∀abcd" "joe" "a" @[1 2 3 4 -1000]}))
  # =>
  @{"john" 1 "∀abcd" "joe" "a" @[1 2 3 4 -1000]}

  (decode (encode (string "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ\n"
                          "ᛋᚳᛖᚪᛚ᛫ᚦᛖᚪᚻ᛫ᛗᚪᚾᚾᚪ᛫ᚷᛖᚻᚹᛦᛚᚳ᛫ᛗᛁᚳᛚᚢᚾ᛫ᚻᛦᛏ᛫ᛞᚫᛚᚪᚾ\n"
                          "ᚷᛁᚠ᛫ᚻᛖ᛫ᚹᛁᛚᛖ᛫ᚠᚩᚱ᛫ᛞᚱᛁᚻᛏᚾᛖ᛫ᛞᚩᛗᛖᛋ᛫ᚻᛚᛇᛏᚪᚾ᛬")))
  # =>
  (string "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ\n"
          "ᛋᚳᛖᚪᛚ᛫ᚦᛖᚪᚻ᛫ᛗᚪᚾᚾᚪ᛫ᚷᛖᚻᚹᛦᛚᚳ᛫ᛗᛁᚳᛚᚢᚾ᛫ᚻᛦᛏ᛫ᛞᚫᛚᚪᚾ\n"
          "ᚷᛁᚠ᛫ᚻᛖ᛫ᚹᛁᛚᛖ᛫ᚠᚩᚱ᛫ᛞᚱᛁᚻᛏᚾᛖ᛫ᛞᚩᛗᛖᛋ᛫ᚻᛚᛇᛏᚪᚾ᛬")

  (decode (encode @["šč"]))
  # =>
  @["šč"]

  (decode (encode "👎"))
  # =>
  "👎"

  (decode `"šč"`)
  # =>
  "šč"

  (decode (encode @{"result" :null}))
  # =>
  @{"result" :null}

  (decode (encode {"result" :null}))
  # =>
  @{"result" :null}

  (decode (encode @{"result" :null}))
  # =>
  @{"result" :null}

  (decode (encode :null))
  # =>
  :null

  (decode (encode nil))
  # =>
  :null

  )
