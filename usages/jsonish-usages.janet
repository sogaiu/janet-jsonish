(import ../jsonish :prefix "")

(comment

  (peg/match grammar " ")
  # =>
  @[]

  (peg/match grammar "true")
  # =>
  @[true]

  (peg/match grammar "false")
  # =>
  @[false]

  (peg/match grammar "null")
  # =>
  @[:null]

  (peg/match grammar "0")
  # =>
  @[0]

  (peg/match grammar "8.3")
  # =>
  @[8.3]

  (peg/match grammar "-1")
  # =>
  @[-1]

  (peg/match grammar "printf")
  # =>
  nil

  (peg/match grammar `"fun"`)
  # =>
  @["fun"]

  (peg/match grammar "[]")
  # =>
  @[@[]]

  (peg/match grammar "[8]")
  # =>
  @[@[8]]

  (peg/match grammar "[8,3]")
  # =>
  @[@[8 3]]

  (peg/match grammar `{}`)
  # =>
  @[@{}]

  (peg/match grammar `{"a": 1}`)
  # =>
  @[@{"a" 1}]

  (peg/match grammar `{"a": 1, "b": 2}`)
  # =>
  @[@{"a" 1 "b" 2}]

  )

(comment

  (decode "true")
  # =>
  true

  (decode "8.9")
  # =>
  8.9

  (decode `"\b"`)
  # =>
  "\b"

  (decode `"\t\f\bhello"`)
  # =>
  "\t\f\bhello"

  (decode `"hi\t\f\bhello"`)
  # =>
  "hi\t\f\bhello"

  # 2-byte characters
  (decode `"¢£"`)
  # =>
  "\xc2\xa2\xc2\xa3"

  # 3-byte characters
  (= (decode `"空海"`)
     "空海")
  # =>
  true

  (= (decode "\"\xe3\x81\x80\"")
     "\xe3\x81\x80")
  # =>
  true

  # 4-byte characters - sumerian cuneiform + egyptian hieroglyphics
  (decode `"𒀀𓀀"`)
  # =>
  "\xf0\x92\x80\x80\xf0\x93\x80\x80"

  (decode `"\ud834\udd1e"`)
  # =>
  "𝄞"

  # 3-byte characters and unicode escape
  (= (decode `"空\u0020海"`)
     "空 海")
  # =>
  true

  (= (decode "\"\xE7\xA9\xBA\xE6\xB5\xB7\"")
     "空海")
  # =>
  true

  (decode `"¢ 空\ud834\udd1e海 🦆"`)
  # =>
  "¢ 空𝄞海 🦆"

  (decode `"\u00a2 \u7a7a\ud834\udd1e\u6d77 \ud83e\udd86"`)
  # =>
  "¢ 空𝄞海 🦆"

  (decode "[8, 0 ,  2.3]")
  # =>
  @[8 0 2.3]

  (decode `{"result": null}`)
  # =>
  @{"result" :null}

  )

(comment

  (encode :smile)
  # =>
  @"\"smile\""

  (encode 'null)
  # =>
  @`"null"`

  (encode "空海")
  # =>
  @`"\u7a7a\u6d77"`

  (encode @[2 nil 8])
  # =>
  @"[2, null, 8]"

  # to account for possible change in order
  (or (deep= @`{"Socrates": 1, "Seneca": 8}`
             (encode {"Socrates" 1 "Seneca" 8}))
      (deep= @`{"Seneca": 8, "Socrates": 1}`
             (encode {"Socrates" 1 "Seneca" 8})))
  # =>
  true

  (encode @{"result" :null})
  # =>
  @`{"result": null}`

  )

(comment

  (def src
    `{"bart": "person", "brian": [3, 8, 9]}`)

  (decode (encode (decode src)))
  # =>
  @{"bart" "person"
    "brian" @[3 8 9]}

  )

