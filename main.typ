#import "@preview/touying:0.5.3": *
#import themes.metropolis: *
#import "@preview/cetz:0.3.1"
#import "@preview/fletcher:0.5.2" as fletcher: node, edge
#import "@preview/ctheorems:1.1.3": *
#import "@preview/curryst:0.3.0": rule, proof-tree
#import "@preview/numbly:0.1.0": numbly
#import "./catt.typ": *
#import "./dtt.typ": *
#show: metropolis-theme.with(aspect-ratio: "16-9")

// Pdfpc configuration
// typst query --root . ./example.typ --field value --one "<pdfpc-file>" > ./example.pdfpc
#pdfpc.config(
  duration-minutes: 30,
  start-time: datetime(hour: 14, minute: 10, second: 0),
  end-time: datetime(hour: 14, minute: 40, second: 0),
  last-minutes: 5,
  note-font-size: 12,
  disable-markdown: false,
  default-transition: (
    type: "push",
    duration-seconds: 2,
    angle: ltr,
    alignment: "vertical",
    direction: "inward",
  ),
)

// Custom Table Styling
#let toptable = (..content) => {
  table(
    fill: (x, y) => if y == 0 {
      silver
    },
    stroke: (x, y) => if y == 0 {
      (
        top: (thickness: 1pt, paint: silver),
      )
    } else if y > 0 {
      (
        top: (thickness: 1pt, paint: silver),
        left: (thickness: 1pt, paint: silver),
        right: (thickness: 1pt, paint: silver),
        bottom: (thickness: 1pt, paint: silver),
      )
    },
    inset: 7pt,
    ..content,
  )
}

#let lefttable = (..content) => {
  table(
    fill: (x, y) => if x == 0 {
      silver
    },
    stroke: (x, y) => if x == 0 {
      (
        right: (thickness: 1pt, paint: silver),
      )
    } else if x > 0 {
      (
        top: (thickness: 1pt, paint: silver),
        left: (thickness: 1pt, paint: silver),
        right: (thickness: 1pt, paint: silver),
        bottom: (thickness: 1pt, paint: silver),
      )
    },
    inset: 7pt,
    ..content
  )
}


// Theorems configuration by ctheorems
#show: thmrules.with(qed-symbol: $square$)
#let theorem = thmbox("theorem", "Theorem", fill: rgb("#eeffee"))
#let corollary = thmplain(
  "corollary",
  "Corollary",
  base: "theorem",
  titlefmt: strong
)
#let definition = thmbox("definition", "Definition", inset: (x: 1.2em, top: 1em))
#let example = thmbox("example", "example", fill: rgb("#eeeeee")).with(numbering: none)
#let proof = thmproof("proof", "Proof")

#let boxify = (content) => box(fill: silver, inset: 0.5em, [#content])

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  // config-common(handout: true),
  config-info(
    title: [Equality and Isomorphisms in Type Theory],
    subtitle: [H.O.T.T. from scratch],
    author: [Abdul Haliq],
    date: datetime.today(),
    institution: [Aarhus University],
  ),
)

#set heading(numbering: numbly("{1}.", default: "1.1"))

#title-slide()

== Outline <touying:hidden>

#components.adaptive-columns(outline(title: none, indent: 1em))

= HoTT from scratch

== Deductive Systems

- *Deductive System*: a collection of rules #pause
- *Rule*: takes hypotheses and gives a conclusion
#figure(proof-tree(rule(
  $cal(C)$,
  $cal(H_1)$,
  $cal(H_2)$,
  $...$,
  $cal(H_n)$
)))
#pause
- *Axiom*: a rule with no hypotheses
#figure(proof-tree(rule($cal(C)$)))
#pause
- *Judgement*: hypotheses or conclusions

== Judgements

#slide(repeat: 10, self => [
  #let (uncover, only, alternatives) = utils.methods(self)
#align(center)[#smallcaps("Substitution Calculus Judgements")]

#{
figure(toptable(
  columns: 4,
  align: (center + horizon, center + horizon, center + horizon, center + horizon),
  [Context], [Type], [Term], [Substitution],
  [$Gamma bold("cx")$#pause],
  [$Gamma hy A Ty$#pause ],
  [$Gamma hy a : A$#pause],
  [$Delta hy gamma : Gamma$ #pause]
))
}
- *Context*: lists of dependent terms
#alternatives[][][][][
$
  a_1 : A_1
$][
$
  a_1 : A_1,
  a_2 : A_2(a_1)
$][
$
  a_1 : A_1,
  a_2 : A_2(a_1),
  a_3 : A_3(a_1, a_2)
$][
$
  a_1 : A_1,
  a_2 : A_2(a_1),
  a_3 : A_3(a_1, a_2),
  ... a_n : A_n (a_1, a_2, ..., a_(n-1))
$][
$
 A_1.A_2.A_3. #h(0.25em)... #h(0.25em) .A_n
$]

#pause#pause#pause#pause#pause
- *Substitutions*: shifts judgements from one context to another
#figure(boxify(proof-tree(rule(
  $Delta hy A[gamma] Ty$,
  $Gamma hy A Ty$,
  $Delta hy gamma : Gamma$
))))
])
== *Definitional Equality*


#align(center)[#smallcaps("Substitution Calculus Equality Judgements")]
#figure(toptable(
  columns: 3,
  align: (center + horizon, center + horizon, center + horizon),
  [Type], [Term], [Substitution],
  [$Gamma hy A = B Ty$#pause],
  [$Gamma hy a = b : A$#pause ],
  [$Delta hy gamma = delta : Gamma$ #pause]
))

#align(center)[
_definitionally equal symbols can be replaced by each other_
]
#pause


#figure(boxify(proof-tree(
  rule(
    $Gamma hy a : B$,
    $Gamma hy a : A$,
    $Gamma hy A = B Ty$,
  )
)))


== Natural Models


#slide(repeat: 12, self => [
  #let (uncover, only, alternatives) = utils.methods(self)

#align(center)[#smallcaps("Substitution Calculus Rules")]
#{
figure(lefttable(
  columns: 4,
  align: (center + horizon, center + horizon, center + horizon, center + horizon),
  [$gamma$ Application],
  alternatives[*term*][term],
  alternatives[][*type*][type],
  [],
  [Morphism],
  alternatives[][][*composition*][composition],
  alternatives[][][][*identity*][*identity*][identity],
  alternatives[][][][][][*associativity*][associativity],
  [Pullback],
  alternatives[][][][][][][*weakening*][weakening],
  alternatives[][][][][][][][*variable*][*variable*][*variable*][*variable*][variable],
  alternatives[][][][][][][][][][][][*substitution extension*][substitution extension],
))
}
#alternatives[
  #figure(proof-tree(
    rule(
      name: [substitution-term],
      $Delta hy a[gamma] : A[gamma]$,
      $Gamma hy a : A$,
      $Delta hy gamma : Gamma$
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [substitution-type],
      $Delta hy A[gamma] Ty$,
      $Gamma hy A Ty$,
      $Delta hy gamma : Gamma$
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [composition],
      $Gamma_2 hy gamma comp gamma' : Gamma_0$,
      $Gamma_1 hy gamma : Gamma_0$,
      $Gamma_2 hy gamma' : Gamma_1$
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [identity],
      $Gamma hy bold(id) : Gamma$,
      $Gamma bold("cx")$,
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [unital],
      $Delta hy gamma comp bold(id) = bold(id) comp gamma = gamma : Gamma$,
      $Delta hy gamma : Gamma$,
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [associativity],
      $Gamma_3 hy gamma_0 comp (gamma_1 comp gamma_2) = (gamma_0 comp gamma_1) comp gamma_2 : Gamma_0$,
      $Gamma_1 hy gamma_0 : Gamma_0$,
      $Gamma_2 hy gamma_1 : Gamma_1$,
      $Gamma_3 hy gamma_2 : Gamma_2$
    )
  ))
][
  #figure(proof-tree(
    rule(
      name: [weakening],
      $Gamma. A hy upright(bold(p)) : Gamma$,
      $Gamma hy A Ty$,
    )
  ))
  _think as adding terms to context_
][
  #figure(proof-tree(
    rule(
      name: [variable],
      $Gamma. A hy upright(bold(q)) : A[upright(bold(p))]$,
      $Gamma hy A Ty$,
    )
  ))
  _think as debruijn indices at index number of $bold(upright(p))$_
][
  #figure(proof-tree(
    rule(
      name: [variable],
      $Gamma. A. B_1 hy upright(bold(q))[upright(bold(p))] : A[upright(bold(p))^2]$,
      $Gamma hy A Ty$,
      $Gamma . A hy B_1 Ty$
    )
  ))
  _think as debruijn indices at index number of $bold(upright(p))$_
][
  #figure(proof-tree(
    rule(
      $Gamma. A. B_1. B_2 hy upright(bold(q))[upright(bold(p))^2] : A[upright(bold(p))^3]$,
      $Gamma hy A Ty$,
      $Gamma . A hy B_1 Ty$,
      $Gamma . A. B_1 hy B_2 Ty$
    )
  ))
  _think as debruijn indices at index number of $bold(upright(p))$_
][
  #figure(proof-tree(
    rule(
      $Gamma. A. B_1... B_n hy upright(bold(q))[upright(bold(p))^n] : A[upright(bold(p))^(n+1)]$,
      $Gamma hy A Ty$,
      $Gamma . A hy B_1 Ty$,
      $...$,
      $Gamma . A. B_1 ... B_(n-1) hy B_n Ty$
    )
  ))
  _notationally informally we still write $a:A$_
][
  #figure(proof-tree(
    rule(
      name: [substitution-extension],
      $Delta hy gamma. a : Gamma. A$,
      $Delta hy gamma : Gamma$,
      $Gamma hy A Ty$,
      $Delta hy a:A[gamma]$
    )
  ))
  _think removing / dispensing terms from context_
]
])

_the rules are justified / motivated by the natural model for dependent types_
#figure(diagram(cell-size: 30mm,
    $
      Delta
        #edge("rr", $script(Delta hy a: A[gamma])$, "->", bend: 45deg)
        #edge("dr", $script(Delta hy gamma : Gamma)$, "->", bend: -30deg)
        #edge("r", $script(Delta hy gamma . a : Gamma. A)$, "-->")
      & Gamma . A
        #edge("r", $script(Gamma . A hy upright(bold(q)) : A[bold(upright(p))])$, "->") 
        #edge("d", $script(Gamma . A hy upright(bold(p)) : Gamma)$, "->")
      & Tm
        #edge("d", $script(p)$, "->") \
      & Gamma
        #edge("r", $script(Gamma hy A Ty)$, "->")
      & Ty
    $
  ))
_we notate $Tm(Gamma, A)$ for set of terms and $Ty(Gamma)$ for set of types_

== Mapping In Types

$
  iota_(Gamma, X) : Tm(Gamma, Upsilon(X)) iso Y
$
- Type $Upsilon$ is defined by the isomorphism with some set $Y$
#pause
- *Type Constructor*: $Upsilon : (X:H) -> Ty(Gamma)$
#pause
#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], $Upsilon$, pause,
  [Introduction], $iota_(Gamma,X)^(-1)$, pause,
  [Elimination], $iota_(Gamma,X)$, pause,
  [Computation / $beta$], $iota_(Gamma,X) comp iota_(Gamma,X)^(-1) = id$, pause,
  [Uniqueness / $eta$], $iota_(Gamma,X)^(-1) comp iota_(Gamma,X) = id$
))

#pagebreak()

$
  iota_Gamma : Tm(Gamma, Pi(A,B)) iso Tm(Gamma. A, B)
$
#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], 
  figure(proof-tree(
    rule(
      name: [$Pi$-formation],
      $Gamma hy Pi(A,B) Ty$,
      $Gamma hy A Ty$,
      $Gamma. A hy B Ty$
    )
  )), pause,
  [Introduction],
  figure(proof-tree(
    rule(
      name: [$Pi$-intro],
      $Gamma hy lambda (b) : Pi(A,B)$,
      $Gamma. A hy b : B$
    )
  )), pause,
  [Elimination],
  figure(proof-tree(
    rule(
      name: [$Pi$-elim],
      $Gamma hy upright(bold("app"))(f,a) : B[id. a]$,
      $Gamma hy a : A$,
      $Gamma hy f : Pi(A,B)$,
    )
  )), pause,
  [Computation / $beta$],
  figure(proof-tree(
    rule(
      name: [$Pi-beta$],
      $upright(bold("app"))(lambda(b), a) = b[id. a] : B[id. a]$,
      $Gamma hy a : A$,
      $Gamma. A hy b : B$
    )
  )), pause,
  [Uniqueness / $eta$],
  figure(proof-tree(
    rule(
      name: [$Pi-eta$],
      $lambda(upright(bold("app"))(f[sp], sq)) = f : Pi(A,B)$,
      $Gamma hy f : Pi(A,B)$
    )
  ))
))

#pagebreak()


$
  iota_Gamma : Tm(Gamma, Sigma(A,B)) iso bold(upright(Sigma))_(a:Tm(Gamma, A)) Tm(Gamma, B[id. a])
$
#align(center)[_think of $bold(upright(Sigma))$ as metatheory $Sigma$, thus set of dependent pairs_]

#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], 
  figure(proof-tree(
    rule(
      name: [$Sigma$-formation],
      $Gamma hy Sigma(A,B) Ty$,
      $Gamma hy A Ty$,
      $Gamma. A hy B Ty$
    )
  )), pause,
  [Introduction],
  figure(proof-tree(
    rule(
      name: [$Sigma$-intro],
      $Gamma hy upright(bold("pair"))(a,b) : Sigma(A,B)$,
      $Gamma hy a : A$,
      $Gamma hy b : B[id. a]$
    )
  )), pause,
  [Elimination 1],
  figure(proof-tree(
    rule(
      name: [$Sigma$-elim$\ _1$],
      $Gamma hy upright(bold("fst"))(p) : A$,
      $Gamma hy p : Sigma(A,B)$,
    )
  )), pause,
  [Elimination 2],
  figure(proof-tree(
    rule(
      name: [$Sigma$-elim$\ _2$],
      $Gamma hy upright(bold("snd"))(p) : B[id. upright(bold("fst"))(p)]$,
      $Gamma hy p : Sigma(A,B)$,
    )
  ))
))

#pagebreak()

$
  iota_Unit : Tm(Gamma, Unit) iso {star}
$

#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], 
  figure(proof-tree(
    rule(
      name: [Unit-formation],
      $Gamma hy Unit Ty$
    )
  )), pause,
  [Introduction],
  figure(proof-tree(
    rule(
      name: [Unit-intro],
      $Gamma hy tt : Unit$,
      $Gamma hy Unit Ty$
    )
  )), pause,
  [Elimination],
  [_what about the elimination rule?_]
))

#align(center)[_we will see in mapping out types_]

== Naturality

_these isomorphisms respect substitution_

#figure(diagram(cell-size: 50mm,
$
  Tm(Gamma, Upsilon(X))
    #edge("r", $iota_Gamma$, "<->")
    #edge("d", $gamma$, "->")
  & Y
    #edge("d", $gamma^*$, "->") \
  Tm(Delta, Upsilon(X))
    #edge("r", $iota_Delta$, "<->")
  & Y[gamma]
$))

_we have to define rules for these as well for the types and terms of  $Upsilon$_

== *Extensional Equality*

$
  iota_Gamma : Tm(Gamma, Eq(A,a,b)) iso {star | a = b}
$

#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], 
  figure(proof-tree(
    rule(
      name: [Eq-formation],
      $Gamma hy Eq(A,a,b) Ty$,
      $Gamma hy A Ty$,
      $Gamma hy a : A$,
      $Gamma hy b : A$
    )
  )), pause,
  [Introduction],
  figure(proof-tree(
    rule(
      name: [Eq-intro],
      $Gamma hy refl : Eq(A,a,a)$,
      $Gamma hy a : A$
    )
  )), pause,
  [Elimination],
  figure(proof-tree(
    rule(
      name: [Eq-reflection],
      $Gamma hy a = b : A$,
      $Gamma hy p : Eq(A,a,b)$
    )
  ))
))

#pagebreak()

#figure(proof-tree(
  rule(
    name: [Eq-reflection],
    $Gamma hy a = b : A$,
    $Gamma hy p : Eq(A,a,b)$
  )
))
#align(center)[_Notice how elimination concludes a definitional equality judgement of terms rather than a term judgement. This departs from the usual elimination rules we have seen before._]
#pause
- *Propositional Equality*: equality is *internalized* within the theory e.g.
$
  a = b : A arrow.b p:Eq(A,a,b) arrow.t a = b : A
$
#pause
- if we have a proof of equality, we can swap terms (definitionally)
#pause
- e.g. with $p:Eq(bb(N), a + b, b + a)$ we can swap $a + b$ for $b + a$ in lets say an argument for length of a vector without having to evaluate $a$ and $b$

#pagebreak()
- *Uniqueness of Identity Proofs (UIP)*: there is only one proof of equality between two terms due to equality reflection, i.e. $p,q:Eq(A,a,b)$ means $p=q$
#pause
- *Injectivity*: distinct terms are mapped to distinct terms. But in UIP, all proofs of equality are mapped to $star$ i.e. $Gamma hy a = b : A$
#pause
- *Normalization*: with UIP our types aren't injective, without injective types we can't have normalization#pause; (we won't explore why here, but lets take it for granted for now)
#pause
#align(center)[_Without normalization we can't have decidable type checking!_]

#pause
#align(center)[_We must do propositional equality differently_]

== Mapping Out Types
$
  { c in Tm(Gamma. Upsilon, C) | rec } iso {star}
$
#align(center)[_we define $Upsilon$ by the terms it maps out to in $C$, hence the name_]
#pause
#figure(toptable(
  columns: 3,
  align: (center + horizon, center + horizon, center + horizon),
  $Upsilon(X)$, [signature], [initial algebra],
  $Void$, $X |-> 0$, $absurd$, pause,
  $Unit$, $X |-> 1$, $tt$, pause,
  $Bool$, $X |-> 1 + 1$, $btrue, bfalse$, pause,
  $A + B$, $X |-> A + B$, $inl, inr$, pause,
  $bb(N)$, $X |-> 1 + X$, $zero, succ$, pause
))
- *Formation Rule*: $Upsilon(X)$
- *Intro*: initial algebra constructors
- *Elim*: $rec$

#pagebreak()

#align(center)[_the $Unit$ type now defined as a mapping out type_]

$
  { c in Tm(Gamma. Unit, C) | c = c[id. tt] } iso {star}
$
#pause
$
  rec(tt, c) = c: C[id. tt]
$
#pause
#figure(proof-tree(
  rule(
    name: [Unit-elim],
    $Gamma hy rec(u, c) : C[id. u]$,
    $Gamma hy u : Unit$,
    $Gamma hy c : C[id. tt]$,
  )
))

#pagebreak()

#align(center)[_the $Bool$ type is a mapping out type_]

$
  { c in Tm(Gamma. Bool, C) | c_btrue = c[id. btrue] and c_bfalse = c[id. bfalse] } iso {star}
$
#pause
$
  rec(btrue, c_btrue, c_bfalse) &= c_btrue: C[id. btrue] \
  rec(bfalse, c_btrue, c_bfalse) &= c_bfalse: C[id. bfalse]
$
#pause
#figure(proof-tree(
  rule(
    name: [Bool-elim],
    $Gamma hy rec(b, c_btrue, c_bfalse) : C[id. b]$,
    $Gamma hy b : Bool$,
    $Gamma hy c_btrue : C[id. btrue]$,
    $Gamma hy c_bfalse : C[id. bfalse]$
  )
))
#pause
#align(center)[_for $Bool$ often $rec$ is also written as $upright(bold("if"))$_]

#pagebreak()

#align(center)[_the disjoint sum type; $A+B$, is like a $Bool$ with arguments_]
$
  { c in Tm(Gamma. A + B, C) | c_A = c[id. inl a] and c_B = c[id. inr b] } iso {star}
$
$
  rec(inl a, c_A, c_B) &= c_A : C[id. inl a] \
  rec(inr b, c_A, c_B) &= c_B : C[id. inr b]
$
#figure(proof-tree(
  rule(
    name: [$A+B$-elim],
    $Gamma hy rec(o, c_A, c_B) : C[id. b]$,
    $Gamma hy o : A + B$,
    $Gamma hy c_A : C[id. inl a]$,
    $Gamma hy c_B : C[id. inr b]$
  )
))

#pagebreak()

#align(center)[_$bb(N)$ motivates why we call the elimination rule a recursor $rec$_]
$
  { c in Tm(Gamma. bb(N), C) | c_z = c[id. zero] and c_s [sp. sq. c] = c[sp. succ(sq)] } iso {star}
$
$
rec(zero, c_z, c_s) &= c_z : C[id. zero] \
rec(succ(n),c_z,c_s) &= c_s [id. n. rec(n, c_z, c_s)] : C[id. succ(n)]
$
#figure(proof-tree(
  rule(
    name: [$bb(N)$-elim],
    $Gamma hy rec(n, c_z, c_s) : C[id. n]$,
    $Gamma hy n : bb(N)$,
    $Gamma. bb(N) hy C Ty$,
    $Gamma hy c_z : C[id. zero]$,
    $Gamma. bb(N). C hy c_s : C[sp^2. succ(sq[sp])]$
  )
))

#pagebreak()

#align(center)[_$Void$ has no recursor arguments in $C$_]
$
  Tm(Gamma. Void, C) iso {star}
$

#align(center)[$rec(v) : C[id. v]$ #h(2em) _often written as_ #h(2em) $absurd(v) : C[id. v]$]

#figure(proof-tree(
  rule(
    name: [Void-elim],
    $Gamma hy absurd(v) : C[id. v]$,
    $Gamma hy v : Void$,
    $Gamma. Void hy C Ty$
  )
))
#align(center)[_notice how *ANY* term in $C$ that depends on $Void$ is uniquely $absurd(v)$_]


== *Intensional Equality*

_We now try to define Propositional Equality as a mapping out type_
$
  {c in Tm(Gamma. Id(A,a,b), C) | jrule } iso {star}
$
- *Introduction Rule*: still the same $refl : Id(A,a,a)$
- *Elimination Rule*: $jrule$


// #figure(table(
//   columns: 3,
// ))

#pagebreak()
- construct subst
- construct uniq

#pagebreak()
- construct sym
- construct trans
- construct cong

#pagebreak()
- no UIP; briefly on Hoffman-Streicher Groupoid Model
- no funext; briefly on J being trivial in extensional TT
- hoffman conservativity theorem

== Universes

- isomorphism
  - code
  - El
- lift
- uni
- cumulative universes; to avoid impredicativity; paradoxes

== *Univalence*

- a specific $C$ in $"Tm"(Gamma."Id"(U,A,B),C)$
- bimaps / iso : HProp
  - construction
- contractible fibres / equiv : U
  - construction
- axioms break canonicity

= HOTT from HoTT

== Observational Equality

== Parametricity

== *Parametric Univalence*