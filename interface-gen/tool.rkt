#lang racket/base

(define iface-rex
    #px"export\\s+interface\\s+\\w+\\s+(extends\\s+\\w+\\s+)?\\{([^\\{\\}\\(]|(\\{[^\\}]*\\}))*\\}") 

(define ifaces-file (open-input-file (build-path (find-system-path 'home-dir)
                                                 "racket-langserver/interface.ts")))

(parameterize ([current-input-port ifaces-file])
  (for ([iface (regexp-match* iface-rex (current-input-port))])
    (displayln iface)))