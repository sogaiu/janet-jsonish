(-> ["janet"
     "./juat/janet-usages-as-tests/make-and-run-tests.janet"
     # specify file and/or directory paths relative to project root
     "jsonish.janet" "./usages"
     ]
    (os/execute :p)
    os/exit)

