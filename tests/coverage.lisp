(require :sb-cover)
(ql:quickload :prove)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:oos 'asdf:load-op :queue :force t)
(prove:run :queue/tests)
(sb-cover:report "coverage/")
(declaim (optimize (sb-cover:store-coverage-data 0)))
(quit)
