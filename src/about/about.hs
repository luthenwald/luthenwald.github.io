{-
title       = About
pubDate     = 2036-11-18
tags        = about, personal
description = The typical about me page.
-}

-- | Lμthenwałd/Luth, He/Him
--
-- Student working at the intersection of |Programming Language Theory|, |Category Theory|, and |high-performance computing|.
-- I am currently based in Wuhan/Shanghai, China.

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

-- | Education

-- | Technical Environment

data Language = Language
  { langName   :: String
  , langDomain :: String
  } deriving (Show)

myStack :: [Language]
myStack =
  [ Language "Haskell" "type theory, research tooling, proofs"
  , Language "Racket"  "compiler implementation, DSLs, macros"
  , Language "Rust"    "systems programming, game engines"
  , Language "Julia"   "GPU computing, scientific computing"
  , Language "Zig"     "low-level systems, build tooling"
  , Language "OCaml"   "type systems, formal methods"
  ]

-- | Contact
--
-- I am reachable by [email](mailto:luthenwald@pm.me). If you are working on programming language theory, categorical semantics, GPU
-- computing, or formal verification, I am glad to hear from you.
