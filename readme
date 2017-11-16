**Flame Compiler**
This is an old project from U Chicago CS226, spring 2000.
It compiles a small imperative programming language called
Flame to Sparc assembly.

--

Josh Joyce
jnjoyce@uchicago.edu

Implementation Notes:

* Parser generates random "encouragement" to help motivate error-prone coders.
* The compiler does not do array bounds checking, which is not worth the
  performance hit.
* Optimizations: Not really.
    - Does algebraic identity stuff in the parser.
    - Does mul/div by powers of 2 using shifts.
    - Always keeps DISPLAY in %o5.
