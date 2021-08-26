Disciplina Style Guide
============================
Disciplina style guide uses
[Serokell Haskell Style Guide](https://github.com/serokell/serokell-util/blob/master/serokell-style.md)
as a base, providing slight modifications only:

Imports grouping is simpler:

0. Prelude import. Disciplina uses a current project-wide prelude that
   is a reexport of `universum`, so usually this section is omitted.
1. Everything from hackage packages or from your packages outside current project.
2. Everything from the current project.
3. Everything from the current target (like `Bench.*` or `Test.*`), but this may be merged with (2).

We also allow using implicit imports for internal project modules
(sections 2 and 3), though section 1 is strictly qualified.

Everything in the serokell style guide related to the `cabal` applies
to `hpack` in the same way (`-Werror`, sorting inputs, etc).
