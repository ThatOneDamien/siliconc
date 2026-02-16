# Silicon Compiler (a.k.a siliconc or sic)
Among the many random programming projects I have taken up over the years,
a compiler has never been something that I have tried to do. Modern compilers
almost exclusively have huge, complex, and unreadable codebases that make it
hard for new learners to get a grasp on the things they do. Despite that,
there are many resources online that help beginners learn the basics of
compilers, and I wanted to try putting my learning to the test by making
a programming language that suits MY preferences.

## My Expectations
I know much better than to believe that I have the ability to create a
compiler that can reach even somewhat close to the quality of modern-day
compilers, but I don't want that to stop me from trying to make something
usable.

My goal is to make a functioning compiler that will allow me to create
programs at a small to intermediate scale, mostly ignoring optimizations.

## Future Feature Plans (sorted from highest to lowest priority)
- Defer statements
- More test cases
- Add proper dead-code checking and only add function declarations as needed to the codegen
- Methods for structs
- Generics
- Macros/constant-eval functions
- Self hosting
- Build system
- Update README with current information about the language
- Add support for float80/float128 (refactoring)
- Optimization of compiler memory usage
