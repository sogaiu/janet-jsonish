# jsonish

JSON <-> JDN conversion in pure Janet.

# Usage

```janet
(import jsonish :as j)

(j/decode `"\u00a2 \u7a7a\ud834\udd1e\u6d77 \ud83e\udd86"`)
# =>
"¢ 空𝄞海 🦆"

(deep= (j/encode {"Socrates" 1 "Seneca" 8})
       (j/encode {"Seneca" 8 "Socrates" 1}))
# =>
true

(def src
  `{"bart": "person", "brian": [3, 8, 9]}`)

(j/decode (j/encode (j/decode src)))
# =>
@{"bart" "person"
  "brian" @[3 8 9]}

(j/encode {"result" :null})
# =>
@`{"result": null}`
```

# Alternatives

## C

* [spork](https://github.com/janet-lang/spork/)

## Pure Janet

* [janet-json-pure](https://github.com/Techcable/janet-json-pure) -
  if Unicode support is important, might want to look at source closely
* [janet-pure-parsers](https://github.com/rlonstein/janet-pure-parsers) -
  if Unicode support is important, might want to look at source closely
* [jayson](https://github.com/CFiggers/jayson)
* [medea](https://github.com/pyrmont/medea)

# Credits

* bakpakin - `spork/json` from [spork](https://github.com/janet-lang/spork/)
* CFiggers - [jayson](https://github.com/CFiggers/jayson)

