#lang racket/base

;; Defined by JSON RPC
(define PARSE-ERROR -32700)
(define INVALID-REQUEST -32600)
(define METHOD-NOT-FOUND -32601)
(define INVALID-PARAMS -32602)
(define INTERNAL-ERROR -32603)
(define SERVER-ERROR-START -32099)
(define SERVER-ERROR-END -32000)
(define SERVER-NOT-INITIALIZED -32002)
(define UNKNOWN-ERROR-CODE -32001)

;; Defined by LSP protocol
(define REQUEST-CANCELLED -32800)

(provide (all-defined-out))