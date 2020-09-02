#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Julian Herrera
;; SPDX-License-Identifier: MIT
#!r6rs

(import (chezscheme)(lento)(prefix (mpg123) mpg123:))

(define lib-portaudio (load-shared-object "libportaudio.so"))

(define-syntax define-function
  (syntax-rules ()
    ((_ ret name fpname args)
     (define name
       (foreign-procedure (symbol->string 'fpname) args ret)))))

;; PortAudio typedefs
(define-ftype PaError int)
(define-ftype PaDeviceIndex int)
(define-ftype PaSampleFormat unsigned-long)
(define-ftype PaTime double)
(define-ftype PaHostApiIndex int)
(define-ftype PaStreamFlags unsigned-long)
(define-ftype PPaStream void*)          ; void isn't valid as a chez FFI type,
                                        ; instead store pointer to void stream

(define done-ptr (make-ftype-pointer size_t (foreign-alloc (ftype-sizeof size_t))))

;; Constants
(define pa:int16 #x00000008)
(define pa:clip-off #x00000001)

;; PortAudio structs
(define-ftype PaStreamParameters
  (struct
   [device PaDeviceIndex]
   [channelCount int]
   [sampleFormat PaSampleFormat]
   [suggestedLatency PaTime]
   [hostApiSpecificStreamInfo void*]))

(define-ftype PaDeviceInfo
  (struct
   [structVersion int]
   [name (* char)]
   [hostApi PaHostApiIndex]
   [maxInputChannels int]
   [maxOutputChannels int]
   [defaultLowInputLatency PaTime]
   [defaultLowOutputLatency PaTime]
   [defaultHighInputLatency PaTime]
   [defaultHighOutputLatency PaTime]))

;; PortAudio Functions
(define-function PaError %pa:initialize Pa_Initialize ())
(define-function PaError pa:terminate Pa_Terminate ())
(define-function string pa:get-error-text Pa_GetErrorText (PaError))
(define-function PaDeviceIndex pa:get-default-output-device Pa_GetDefaultOutputDevice ())
(define-function (* PaDeviceInfo) pa:get-device-info Pa_GetDeviceInfo (PaDeviceIndex))
(define-function PaError pa:open-stream Pa_OpenStream
  ((* PPaStream)
   int
   (* PaStreamParameters)
   double
   unsigned-long
   PaStreamFlags
   void*
   void*))
(define-function PaError pa:start-stream Pa_StartStream (void*))
(define-function PaError pa:write-stream Pa_WriteStream (void* void* unsigned-long))
(define-function PaError pa:stop-stream Pa_StopStream ((* PPaStream)))
(define-function PaError pa:close-stream Pa_CloseStream ((* PPaStream)))

(define (pa:initialize)
  (let ([err (%pa:initialize)])
    (if (not (zero? err))
        (pa:handle-error err)
        (begin
          (display "portaudio initialized")
          (newline)))))


(define (pa:handle-error err)
  (let ([error-text (pa:get-error-text err)])
    (display error-text)
    (newline)))

;; Initialize the Application
(pa:initialize)
(mpg123:initialize)

(define rate)
(define channels)
(define encoding)

;; set up a mpg123:handle, pass 0 to use the default decoder.
(define handle (mpg123:new-handle))

;; the filename is the first (and only) item in the list `command-line-arguments`
(define audio-file (car (command-line-arguments)))

;; open the audio file, passing in the handle to which you attach everything.
(mpg123:open-stream handle audio-file)

;; once more, handle goes, as well as ADDRESSES to 
(mpg123:get-format handle (lambda (r c e)
                            (set! rate r)
                            (set! channels c)
                            (set! encoding e)))

;; allocate space for the stream parameter struct that needs initializing
(define output-parameters-ptr
 (make-ftype-pointer PaStreamParameters
                     (foreign-alloc (ftype-sizeof PaStreamParameters))))


(define stream (make-ftype-pointer PPaStream (foreign-alloc (ftype-sizeof PPaStream))))

(define output-device (pa:get-default-output-device))
(define device-info-ptr (pa:get-device-info output-device))

;; set values on our PaStreamParameters pointer: output-parameters-ptr
(ftype-set! PaStreamParameters
            (device)
            output-parameters-ptr output-device)
(ftype-set! PaStreamParameters
           (channelCount)
           output-parameters-ptr
           channels)
(ftype-set! PaStreamParameters
           (sampleFormat)
          output-parameters-ptr pa:int16)
(ftype-set! PaStreamParameters
            (suggestedLatency)
          output-parameters-ptr
          (ftype-ref PaDeviceInfo
                    (defaultHighOutputLatency) device-info-ptr))
(ftype-set! PaStreamParameters
            (hostApiSpecificStreamInfo)
            output-parameters-ptr 0)

(pa:handle-error (pa:open-stream                       ; call to open the stream
                  stream                               ; stream ptr
                  0                                    ; inputStream params ptr, nul(0) byte for output stream
                  output-parameters-ptr                ; outputStream params ptr
                  (* rate 1.0)  ; extract rate value from rate ptr
                  64                                  ;;
                 pa:clip-off 0 0))

(define stream-ptr (foreign-ref 'void* (ftype-pointer-address stream) 0)) ;;whats at the pointer to the pointer
(define buffer-size (mpg123:outblock handle))
(define output-buffer (foreign-alloc buffer-size))

(pa:start-stream stream-ptr)

(let play ([sample 0]
           [done #f])
  (guard (x [(warning? x) (set! done #t)])
    (mpg123:read-stream handle output-buffer buffer-size done-ptr))
  (pa:write-stream stream-ptr output-buffer (/ buffer-size 4))
  (if (not done)
      (play (+ 1 sample) done)))

(pa:stop-stream stream-ptr)
(pa:close-stream stream-ptr)
