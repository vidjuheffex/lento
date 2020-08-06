#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Julian Herrera
;; SPDX-License-Identifier: MIT
#!r6rs

(import (chezscheme)(lento))

(define lib-portaudio (load-shared-object "libportaudio.so"))
(define lib-mpg123 (load-shared-object "libmpg123.so"))

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

;; mpg123 Functions
(define-function int %mpg123:initialize mpg123_init ())
(define-function string mpg123:get-error-text mpg123_plain_strerror (int))
(define-function void* %mpg123:decoders mpg123_decoders ())
(define-function string mpg123:get-current-decoder mpg123_current_decoder (void*))
(define-function void* mpg123:new mpg123_new (void*))
(define-function int mpg123:open mpg123_open (void* string))
(define-function int mpg123:get-format mpg123_getformat (void* void* void* void*))
(define-function int mpg123:read mpg123_read (void* void* size_t (* size_t)))
(define-function size_t mpg123:outblock mpg123_outblock	(void*))

(define (pa:initialize)
  (let ([err (%pa:initialize)])
    (if (not (zero? err))
        (pa:handle-error err)
        (begin
          (display "portaudio initialized")
          (newline)))))

(define (mpg123:initialize)
  (let ([err (%mpg123:initialize)])
    (if (not (zero? err))
        (mpg123:handle-error err)
        (begin
          (display "mpg123 initialized")
          (newline)))))

(define (pa:handle-error err)
  (let ([error-text (pa:get-error-text err)])
    (display error-text)
    (newline)))

(define (mpg123:handle-error err)
  (let ([error-text (mpg123:get-error-text err)])
    (display error-text)))

(define (mpg123:decoders)
  (let ([mem-block-ptr (foreign-ref 'void* (%mpg123:decoders) 0)])
    (define (inner index list-of-decoders decoder-str prev-value)
      (let ([value (foreign-ref 'char mem-block-ptr index)])
        (if (eq? value #\nul)
            (if (eq? prev-value #\nul)
                (reverse list-of-decoders)
                (begin
                  (inner (+ 1 index)
                         (cons decoder-str list-of-decoders) "" value)))
            (inner (+ 1 index) list-of-decoders (string-append decoder-str (string value)) value))))
    (inner 0 '() "" #\nul)))
  
;; Initialize the Application
(pa:initialize)
(mpg123:initialize)

;;(display (mpg123:decoders))
;;(display (mpg123:get-current-decoder my-handle))

(define rate (foreign-alloc (ftype-sizeof long)))
(define channels (foreign-alloc (ftype-sizeof int)))
(define encoding (foreign-alloc (ftype-sizeof int)))

;; set up a mpg123:handle, pass 0 to use the default decoder.
(define handle (mpg123:new 0))

;; the filename is the first (and only) item in the list `command-line-arguments`
(define audio-file (car (command-line-arguments)))

;; open the audio file, passing in the handle to which you attach everything.
(mpg123:open handle audio-file)

;; once more, handle goes, as well as ADDRESSES to 
(mpg123:get-format handle rate channels encoding)
;; foreign-refs to these now show updated values

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
            (foreign-ref 'int channels 0))
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
                  (* (foreign-ref 'long rate  0) 1.0)  ; extract rate value from rate ptr
                  64                                  ;;
                  pa:clip-off 0 0))

(define stream-ptr (foreign-ref 'void* (ftype-pointer-address stream) 0)) ;;whats at the pointer to the pointer
(pa:handle-error (pa:start-stream stream-ptr))
(define buffer-size (mpg123:outblock handle))
(define output-buffer (foreign-alloc buffer-size))
;;(define output-buffer (make-ftype-pointer buffer-array (foreign-alloc (ftype-sizeof buffer-array))))
(let play ([sample 0])
  (let ([status (mpg123:read handle output-buffer buffer-size done-ptr)])
    (if (zero? status)
        (begin
          (pa:handle-error (pa:write-stream stream-ptr output-buffer (/ buffer-size 4)))
          (display "on sample ")
          (display sample)
          (newline)
          (play (+ 1 sample)))
        (display "done"))))


