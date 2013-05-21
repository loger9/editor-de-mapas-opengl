;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; OpenGL 2.1 2d skeleton

(define vertex-shader #<<end-of-shader

#version 120
attribute vec2 position;
attribute vec2 texCoord;

varying vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
  colorCoord = texCoord;
  gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 120

varying vec2 colorCoord;
uniform sampler2D colorTexture;

void main()
{
  gl_FragColor = texture2D(colorTexture, colorCoord);
}

end-of-shader
)

(define-type world (tile-vector unprintable:) vertex-vector key-state drawform)
(define tile-size 16.)

(define (make-world/init)
  (make-world
   (let* ((number-of-rows 47)
          (number-of-columns 80)
          (rows (make-vector number-of-rows)))
     (let recur ((i 0))
       (when (< i number-of-rows)
             (vector-set! rows i (make-vector number-of-columns 'none))
             (recur (++ i))))
     rows)
   '#f32()
   '0 'up))
#;
(define (insert-section-bmp x y width height section world)
  (world-vertex-vector-set! world 
                            (f32vector-append (world-vertex-vector world) 
                                    (f32vector x y (+ 0.1 (* 0.25 section)) 0.0
                                          x (+ y height) (+ 0.1 (* 0.25 section)) 1.0
                                          (+ x width) (+ y height) (+ 0.25  (* 0.25 section)) 1.0
                                          (+ x width) y (+ 0.25  (* 0.25 section)) 0.0 ))))

(define (insert-section-bmp x y width height section world)
  (world-vertex-vector-set! world 
                            (f32vector-append (world-vertex-vector world) 
                                    (f32vector x y (* 0.0625 section) 0.1875
                                          x (+ y height) (* 0.0625 section) 0.25
                                          (+ x width) (+ y height) (+ (* 0.0625 section) 0.0625) 0.25
                                          (+ x width) y (+ 0.25  (* 0.0625 section)) 0.1875))))

(define (set-column-vector world column type)
  (let recur ((counter 0))
    (cond
     ((eq? counter 47) #t)
     (else
      (vector-set! (vector-ref (world-tile-vector world) counter) column type)
      (recur (+ counter 1))))))

(define (print-map vec size world)
  (let recur ((counter1 0) (counter2 0))
    (cond
     ((and (eq? counter1 46) (eq? counter2 79))
      (insert-section-bmp (* (exact->inexact counter2) size) (* (exact->inexact counter1) size)  size size 2. world))
     ((eq? (vector-ref (vector-ref vec counter1) counter2) 'ground)
      (insert-section-bmp (* (exact->inexact counter2) size) (* (exact->inexact counter1) size)  size size 1. world)
      (if (eq? counter2 79) 
          (recur (+ counter1 1) 0)
          (recur counter1 (+ counter2 1))))
     ((eq? (vector-ref (vector-ref vec counter1) counter2) 'spikes)
      (insert-section-bmp (* (exact->inexact counter2) size) (* (exact->inexact counter1) size) size size 1. world)
      (if (eq? counter2 79)
          (recur (+ counter1 1) 0)
          (recur counter1 (+ counter2 1))))
     (else
      (if (eq? counter2 79) 
          (recur (+ counter1 1) 0) 
          (recur counter1 (+ counter2 1)))))))

(define (print-sector vec sector size world)
  (let ((print-sector-template
         (lambda (a b c d pos1 pos2)
           (let recur ((counter1 a) (counter2 b))
             (cond
              ((and (eq? counter1 c) (eq? counter2 d))
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 2. world) )
              ((eq? (vector-ref (vector-ref vec counter1) counter2) 'ground)
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 2. world)
               (if (eq? counter2 d) 
                   (recur (+ counter1 1) 0)
                   (recur counter1 (+ counter2 1))))
              ((eq? (vector-ref (vector-ref vec counter1) counter2) 'spikes)
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 1. world)
               (if (eq? counter2 d)
                   (recur (+ counter1 1) 0)
                   (recur counter1 (+ counter2 1))))
              (else
               (if (eq? counter2 d) 
                   (recur (+ counter1 1) 0) 
                   (recur counter1 (+ counter2 1)))))))))
   (case sector
     ((1)
      (print-sector-template 0 0 24 39
                             (lambda (x) (* (exact->inexact x) size))
                             (lambda (x) (* (exact->inexact x) size))))
     ((2)
      (print-sector-template 0 39 24 79
                             (lambda (x) (-(* (exact->inexact x) size) 1280.))
                             (lambda (x) (* (exact->inexact x) size))))
     ((3)
      (print-sector-template 23 0 46 40
                             (lambda (x) (* (exact->inexact x) size))
                             (lambda (x) (-(* (exact->inexact x) size) 752.))))
     ((4)
      (print-sector-template 23 39 46 79
                             (lambda (x) (-(* (exact->inexact x) size) 1280.))
                             (lambda (x) (-(* (exact->inexact x) size) 752.)))))))
(define (vector-copy vector vertex-data world)
  (let recur ((counter 0))
    (cond
     ((>= counter (f32vector-length vector))
      (GLfloat*-set! vertex-data counter 0.))
     (else
      (GLfloat*-set! vertex-data counter (f32vector-ref vector counter))
      (recur (+ counter 1))))))

(define (vector-copy-sector vector vertex-data world)
  (let recur ((counter 0))
    (cond
     ((= counter 27000))
     ((>= counter (f32vector-length vector))
      (GLfloat*-set! vertex-data counter 0.)
      (recur (+ counter 1)))
     (else
      (GLfloat*-set! vertex-data counter (f32vector-ref vector counter))
      (recur (+ counter 1))))))

(define (map-updater vertex-data world)
  (world-vertex-vector-set! world '#f32())
  (print-map (world-tile-vector world) 16. world)
  ;;(println (string-append "Size: " (number->string (f32vector-length (world-vertex-vector world)))))
  ;;(set! vertex-data (f32vector->GLfloat* (world-vertex-vector world)))
  (vector-copy (world-vertex-vector world) vertex-data world))

(define (map-divider sector vertex-data world)
  (world-vertex-vector-set! world '#f32())
  (print-sector (world-tile-vector world) sector 32. world)
  ;;(println (string-append "Size: " (number->string (f32vector-length (world-vertex-vector world)))))
  (vector-copy-sector (world-vertex-vector world) vertex-data world))

(define (main)
  (let ((init-screen-width 1280)
        (init-screen-height 752)
        (screen-width* (alloc-int* 1))
        (screen-height* (alloc-int* 1))
        (world (make-world/init)))
    (when (< (SDL_Init SDL_INIT_VIDEO) 0) report: (fusion:error "Couldn't initialize SDL!"))
    ;; SDL
    (let ((win (SDL_CreateWindow
                ""
                SDL_WINDOWPOS_CENTERED
                SDL_WINDOWPOS_CENTERED
                (cond-expand (mobile 0) (else init-screen-width))
                (cond-expand (mobile 0) (else init-screen-height))
                SDL_WINDOW_OPENGL)))
      (unless win (fusion:error "Unable to create render window" (SDL_GetError)))
      (SDL_GetWindowSize win screen-width* screen-height*)
      (let ((screen-width (*->int screen-width*))
            (screen-height (*->int screen-height*))
            (ctx (SDL_GL_CreateContext win)))
        (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
        ;; OpenGL
        (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)

        (world-tile-vector-set! world (read (open-input-file "level-one.dat")))
        (print-map (world-tile-vector world) 16. world)
        
        ;; Generate programs, buffers, textures
        (let* ((perspective-matrix (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                                             (matrix:* (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)
                                                       (make-identity-matrix))))
               (position-buffer-object-id* (alloc-GLuint* 1))
               (main-vao-id* (alloc-GLuint* 1))
               (surface-id* (alloc-GLuint* 1))
               (texture-id* (alloc-GLuint* 1))
               (texture-unit 0)
               (sampler-id* (alloc-GLuint* 1))
               (vertex-data-vector (make-f32vector 66000 0.))
               (vertex-data (f32vector->GLfloat* vertex-data-vector))
               (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                              (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
               (shader-program (fusion:create-program shaders))
               (texture-image* (or (IMG_Load "assets/PlantillaPF1.png")
                                   (fusion:error (string-append "Unable to load texture image -- " (IMG_GetError))))))
          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)

          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 3
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_BGR GL_UNSIGNED_BYTE
                        (SDL_Surface-pixels texture-image*))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
          (glBindTexture GL_TEXTURE_2D 0)
          (SDL_FreeSurface texture-image*)

          ;; Uniforms
          (glUseProgram shader-program)
          (glUniformMatrix4fv (glGetUniformLocation shader-program "perspectiveMatrix")
                              1 GL_FALSE
                              (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           perspective-matrix)))
          (glUniform1i (glGetUniformLocation shader-program "colorTexture") texture-unit)
          (glUseProgram 0)

          ;; Sampler
          (glGenSamplers 1 sampler-id*)
          (let ((sampler-id (*->GLuint sampler-id*)))
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST))
          
          ;; Vertex Array Object
          (glGenBuffers 1 position-buffer-object-id*)
          (let ((position-buffer-object-id (*->GLuint position-buffer-object-id*)))
            ;; Upload buffer
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            (glBufferData GL_ARRAY_BUFFER
                          (* (f32vector-length vertex-data-vector) GLfloat-size)
                          vertex-data
                          GL_DYNAMIC_DRAW)
            ;; Create VAO
            (glGenVertexArrays 1 main-vao-id*)
            (glBindVertexArray (*->GLuint main-vao-id*))
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            
            (let ((position-attr (glGetAttribLocation shader-program "position"))
                  (texture-coordinates-attr (glGetAttribLocation shader-program "texCoord")))
              (glEnableVertexAttribArray position-attr)
              (glVertexAttribPointer position-attr 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray texture-coordinates-attr)
              (glVertexAttribPointer texture-coordinates-attr 2
                                     GL_FLOAT GL_FALSE
                                     (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size))))
            
            (glBindBuffer GL_ARRAY_BUFFER 0)
            (glBindVertexArray 0)
            (map-updater vertex-data world)
            ;; Game loop
            (let ((event* (alloc-SDL_Event)))
              (call/cc
               (lambda (quit)
                 (let main-loop ()
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event*)))
                             (cond
                              ((= event-type SDL_MOUSEBUTTONDOWN)
                               ;;(SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Button down")
                               (let* ((mouse-event* (SDL_Event-button event*))
                                      (mouse-x (SDL_MouseButtonEvent-x mouse-event*))
                                      (mouse-y (SDL_MouseButtonEvent-y mouse-event*)))
                                 ;;(println (string-append "Mouse x: " (number->string mouse-x) "; Mouse y: " (number->string mouse-y)))
                                 (cond
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '0))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'none)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '1))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'ground)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '2))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'spikes)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '3))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'goal)
                                  (map-updater vertex-data world) )
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '4))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'start)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '5))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'key)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'up) (eq? (world-key-state world) '6))
                                   (vector-set! (vector-ref (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size)))) (inexact->exact(floor (/ mouse-x tile-size)))  'inverter)
                                   (map-updater world))
                                  ((and (eq? (world-drawform world) 'left) (eq? (world-key-state world) '0))
                                   (vector-set! (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size))) (make-vector 80 'none))
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'left) (eq? (world-key-state world) '1))
                                   (vector-set! (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size))) (make-vector 80 'ground))
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'left) (eq? (world-key-state world) '2))
                                   (vector-set! (world-tile-vector world) (inexact->exact(floor (/ mouse-y tile-size))) (make-vector 80 'spikes))
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'right) (eq? (world-key-state world) '0))
                                   (set-column-vector world (inexact->exact(floor (/ mouse-x tile-size))) 'none)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'right) (eq? (world-key-state world) '1))
                                   (set-column-vector world (inexact->exact(floor (/ mouse-x tile-size))) 'ground)
                                   (map-updater vertex-data world))
                                  ((and (eq? (world-drawform world) 'right) (eq? (world-key-state world) '2))
                                   (set-column-vector world (inexact->exact(floor (/ mouse-x tile-size))) 'spikes)
                                   (map-updater vertex-data world)))))
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))                                       
                                       ((= key SDLK_UP)
                                        (world-drawform-set! world 'up))
                                       ((= key SDLK_DOWN)
                                        (world-drawform-set! world 'down))
                                       ((= key SDLK_LEFT)
                                        (world-drawform-set! world 'left))
                                       ((= key SDLK_RIGHT)
                                        (world-drawform-set! world 'right))
                                       ((= key SDLK_0)
                                        (world-key-state-set! world '0))
                                       ((= key SDLK_1)
                                        (world-key-state-set! world '1))
                                       ((= key SDLK_2)
                                        (world-key-state-set! world '2))
                                       ((= key SDLK_3)
                                        (world-key-state-set! world '3))
                                       ((= key SDLK_4)
                                        (world-key-state-set! world '4))
                                       ((= key SDLK_5)
                                        (world-key-state-set! world '5))
                                       ((= key SDLK_6)
                                        (world-key-state-set! world '6))
                                       ((= key SDLK_F9)
                                        (world-tile-vector-set! world (read (open-input-file "level-3/level-3.dat")))
                                        (map-updater vertex-data world))
                                       ((= key SDLK_F5)
                                        (create-directory "level-3")
                                        (call-with-output-file "level-3/level-3.dat" (lambda (f) (display (world-tile-vector world) f))))
                                       ((= key SDLK_F1)
                                        (map-divider '1 vertex-data world)
                                        (call-with-output-file "level-3/level-3-sector1.dat" (lambda (f) (display (world-tile-vector world) f))))
                                       ((= key SDLK_F2)
                                        (map-divider '2 vertex-data world)
                                        (call-with-output-file "level-3/level-3-sector2.dat" (lambda (f) (display (world-tile-vector world) f))))
                                       ((= key SDLK_F3)
                                        (map-divider '3 vertex-data world)
                                        (call-with-output-file "level-3/level-3-sector3.dat" (lambda (f) (display (world-tile-vector world) f))))
                                       ((= key SDLK_F4)
                                        (map-divider '4 vertex-data world)
                                        (call-with-output-file "level-3/level-3-sector4.dat" (lambda (f) (display (world-tile-vector world) f))))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))

                   ;; -- Game logic --
                   
                   
                   ;; -- Draw --
                   (glClearColor 0.0 0.0 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)
                   
                   (glActiveTexture (+ GL_TEXTURE0 texture-unit))
                   (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
                   (glBindSampler texture-unit (*->GLuint sampler-id*))

                   ;; Begin VAO
                   (glBindVertexArray (*->GLuint main-vao-id*))
                   ;; Update vertex data buffer
                   (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
                   (glBufferSubData GL_ARRAY_BUFFER
                                    0
                                    (* (f32vector-length vertex-data-vector) GLfloat-size)
                                    vertex-data)
                   
                   (glUseProgram shader-program)
                   (glDrawArrays GL_QUADS 0 (/(f32vector-length vertex-data-vector) 4))
                   (glUseProgram 0)
                   (glBindVertexArray 0)
                   ;; End VAO
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop))))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

