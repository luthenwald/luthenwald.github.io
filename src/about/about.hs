-- title       = About
-- pubDate     = 2036-11-18
-- tags        = about, personal
-- description = The typical about me page.

-- | Lμthenwałd/Luth, He/Him
--
-- @img me.webp
--
-- Student working at the intersection of |Programming Language Theory|, |Category Theory|, and |high-performance computing|.
-- Currently based in Wuhan/Shanghai, China.

module About where

-- | On Research
--
-- My work sits at the junction of the theoretical and the applied. I am interested in how |Categorical Abstractions| can be used
-- not merely as design patterns but as structuring principles that carry semantic guarantees all the way through compilation.

-- || Programming Language Theory

-- || Category Theory

-- || GPU & Parallel Computing

-- | (Current) Projects

-- || Slyz
--
-- Slyz is a GPU-accelerated programming language I am developing in Racket. It targets Apple Silicon via Metal and is organized
-- around a |categorical intermediate representation| (CIR) that makes data-parallel structure explicit and amenable to formal
-- reasoning. The design draws on applicative programming, Naperian-functor-based array combinators, and an algebraic effect system
-- for managing memory and execution context.
--
-- The central ambition is a language in which high-level, shape-safe array programs can be written without the programmer reasoning
-- about thread layout, memory coalescing, or warp divergence — and where correctness of the compilation to Metal shaders can be
-- argued structurally.

-- || Splined
--
-- Iterative image reconstruction using random [cubic bézier](https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Cubic_B%C3%A9zier_curves) strokes, accelerated on [metal](https://en.wikipedia.org/wiki/Metal_(API)).
--
-- [src](https://tangled.org/luthenwald.tngl.sh/splined) | [bad apple](https://youtu.be/oVTDzle9qz8?si=DIWqILNn21JHVsnL)
--
-- > The image used here is under open access by [The Met](https://www.metmuseum.org/hubs/open-access).
--
-- @img splined-i.webp
-- @img splined-o.webp

-- | Education
--
-- I'm always the worst student in the grade.

-- | Technical Environment
--
-- || Programming Language

data Language = Language
   { langName   :: String
   , langDomain :: String
   } deriving (Show)

myStack :: [Language]
myStack =
   [ Language "Haskell" "type theory, research tooling, proofs"
   , Language "Racket"  "compiler implementation, DSLs, macros"
   , Language "Rust"    "systems programming"
   , Language "Julia"   "Prototyping, GPU computing, scientific computing"
   , Language "Zig"     "low-level systems, build tooling, gamedev"
   ]

-- ||

-- | Contact
--
-- I am reachable by [email](mailto:luthenwald@pm.me). If you are working on programming language theory, categorical semantics, GPU
-- computing, or formal verification, I am glad to hear from you.
--
--
-- https://tangled.org/luthenwald.tngl.sh/
