

sbcl --load scoreboard.asd \
     --eval '(ql:quickload :scoreboard)' \
     --eval '(asdf:make :scoreboard)' \
     --eval '(quit)'

mv src/scoreboard .
