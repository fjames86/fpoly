
To compile the library use:

nvcc --compiler-options '-fPIC' -o libfpoly.so --shared matrix.cu

To link to a C file, you must rename it as a .cu file, even though it's C! Use

nvcc -L/home/fjames/fpoly/libfpoly/ test.cu -lfpoly -o test

To load from Lisp use the standard procedure, i.e. define a CFFI library

(define-foreign-library libfpoly (t "libfpoly.so"))

Define the functions

(defcfun ("ffge_list" ffge-list) :int
  (mats :pointer)
  (vecs :pointer)
  (num :int)
  (n :int))

Call it using (with-foreign-object ...)

It works!

The pitfall is name mangling

NVCC seems to not insert correct symbols, but rather some strange extra
characters are put on either end. e.g. ffge_list -> _Z9ffge_listPiS_ii
CFFI only knows about this symbol, not "ffge_list" so you need to change the
definition to

(defcfun ("_Z9ffge_listPiS_ii" libfpoly-ffge-list) :int (mats :pointer) (vecs :pointer) (num :int) (n :int))

Now have this function in the interface.lisp, giving the name libfpoly-ffge-list-gpu (it also returns an :int instead of :void)

Possibly can change the compilation options to avoid this naming issue?

