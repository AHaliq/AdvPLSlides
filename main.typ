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
    institution: text(
            size: 1.2em,
            font: ("AU Passata"),
          )[#upper[Aarhus Universitet]],
  ),
)

#set heading(numbering: numbly("{1}.", default: "1.1"))

#title-slide()

== Motivation <touying:hidden>

- *Types* in programming are a "compiler enforced" discipline to ensure well formed programs. i.e. $1 + 2$ would be legal unlike $1 + quote"a"quote$ #pause
- *Strong Types* with features such as generics are able to express more general and also more specific guarantees i.e. vector of some type variable $X$, vector of ints vs vector of untyped values #pause
- *Dependent Types* gives us a framework to express mathematics where terms are proofs and types are propositions. This automates verification of mathematical proofs and results, instead of relying on human experts. We can also now do formal methods that ensure correctness of programs rigorously using types. #pause
- *Homotopy Type Theory* and univalence asserts that when types are equal we automatically have a map for terms from one type to the other. Thus given a proof / term in one type, for free we get the proof in the other. #pause
- *Higher Observational Type Theory*: is one attempt at making HoTT practical (on a computer) using ideas from logical relations and parametricity.

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
#figure(
alternatives[][][][][
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
$])

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
#figure(alternatives[
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
  #align(center)[_think as adding terms to context_]
][
  #figure(proof-tree(
    rule(
      name: [variable],
      $Gamma. A hy upright(bold(q)) : A[upright(bold(p))]$,
      $Gamma hy A Ty$,
    )
  ))
  #align(center)[_think as debruijn indices at index number of $bold(upright(p))$_]
][
  #figure(proof-tree(
    rule(
      name: [variable],
      $Gamma. A. B_1 hy upright(bold(q))[upright(bold(p))] : A[upright(bold(p))^2]$,
      $Gamma hy A Ty$,
      $Gamma . A hy B_1 Ty$
    )
  ))
  #align(center)[_think as debruijn indices at index number of $bold(upright(p))$_]
][
  #figure(proof-tree(
    rule(
      $Gamma. A. B_1. B_2 hy upright(bold(q))[upright(bold(p))^2] : A[upright(bold(p))^3]$,
      $Gamma hy A Ty$,
      $Gamma . A hy B_1 Ty$,
      $Gamma . A. B_1 hy B_2 Ty$
    )
  ))
  #align(center)[_think as debruijn indices at index number of $bold(upright(p))$_]
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
  #align(center)[_notationally informally we still write $a:A$_]
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
  #align(center)[_think removing / dispensing terms from context_]
])
])

#align(center)[_the rules are justified / motivated by the natural model for dependent types_]
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
#align(center)[_we notate $Tm(Gamma, A)$ for set of terms and $Ty(Gamma)$ for set of types_]

== Mapping In Types

$
  iota_Gamma : Tm(Gamma, Upsilon(X)) iso Y
$
- Type $Upsilon$ is defined by the isomorphism with some set $Y$
#pause
- *Type Constructor*: $Upsilon : (X:H) -> Ty(Gamma)$
#pause
#figure(lefttable(
  columns: 2,
  align: (center + horizon, center + horizon),
  [Formation], $Upsilon$, pause,
  [Introduction], $iota_Gamma^(-1)$, pause,
  [Elimination], $iota_Gamma$, pause,
  [Computation / $beta$], $iota_Gamma comp iota_Gamma^(-1) = id$, pause,
  [Uniqueness / $eta$], $iota_Gamma^(-1) comp iota_Gamma = id$
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
  iota_Gamma : Tm(Gamma, Unit) iso {star}
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

#align(center)[_these isomorphisms respect substitution_]

#figure(diagram(cell-size: 50mm,
$
  Tm(Gamma, Upsilon(X))
    #edge("r", $iota_Gamma$, "<->")
    #edge("d", $gamma^*$, "->")
  & Y
    #edge("d", $psi_gamma^*$, "->") \
  Tm(Delta, Upsilon(X))
    #edge("r", $iota_Delta$, "<->")
  & Y[gamma]
$))

#align(center)[_we have to define rules for these as well for the types and terms of  $Upsilon$_

_but we omit them in this presentation_]

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
    $Gamma hy rec(o, c_A, c_B) : C[id. o]$,
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
- *Elimination Rule*: $jrule$ when presented as a "functional program" is as follows:
$
  jrule :& {A:UU}\
  &(C : (a, b: A) -> Id(A,a,b) -> Ty(Gamma)) \ #pause
  -> & (a : A -> C(a,a,refl_a)) \ #pause
  -> & a, b : A -> p : Id(A,a,b) \ #pause
  -> & C(a,b,p)
$
#align(center)[_we will see why $jrule$ is justified to model equality as follows_]

#pagebreak()

#align(center)[_to aid visualization, we introduce the homotopy interpretation of types_]
- types are spaces
- terms are points
- identifications are paths
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((1.5,-2), radius: (2.5,2), name: "B")
  fill(black)
  fill(black)
  circle((0,-2), radius: 0.15, name: "a")
  circle((3,-2), radius: 0.15, name: "b")
  content("B.north", $A$, anchor: "south")
  stroke(black)
  line("a", "b", name: "line3", mark: (end: ">"))
  stroke((paint: gray, dash: "dotted"))
  content("a.south", [$a$], anchor: "north")
  content("b.south", [$b$], anchor: "north")
  content("line3.mid", $p$, anchor: "north")
}))


#pagebreak()
$
  jrule => subst
$
#figure(grid(
  columns: 2,
  align: (left + top, left + top),
  gutter: 1em,
  $
  jrule :& {A : UU} \
  &(C : (a, b: A) -> Id(A,a,b) -> Ty(Gamma)) \
  -> & (a : A -> C#h(0.25em) a#h(0.25em) a#h(0.25em) refl_a) \
  -> & a, b : A -> p : Id(A,a,b) \ 
  -> & C#h(0.25em) a#h(0.25em) b#h(0.25em) p
$,
$
  &subst {a, b: A} B #h(0.25em) p = jrule \ #pause
  &#h(1em) lambda a#h(0.25em) b #h(0.25em) \_. (B#h(0.25em)  a -> B#h(0.25em)  b) \ #pause
  &#h(1em) lambda\_. lambda b.b \ #pause
  &#h(1em) a#h(0.25em) b#h(0.25em) p
$
))
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((0,0), name: "L")
  circle((3,0), name: "R")
  circle((1.5,-2), radius: (2.5,0.7), name: "B")
  fill(black)
  circle((-0.5,0.3), radius: 0.15, name: "a1")
  circle((0.4,-0.5), radius: 0.15, name: "a2")
  circle((2.8,-0.5), radius: 0.15, name: "b2")
  circle((3,0.3), radius: 0.15, name: "b1")
  fill(black)
  circle((3.7,-0.2), radius: 0.15)
  circle((0,-2), radius: 0.15, name: "a")
  circle((3,-2), radius: 0.15, name: "b")
  content("L.north", [$B(a)$], anchor: "south")
  content("R.north", [$B(b)$], anchor: "south")
  content("B.north", $A$, anchor: "south")
  stroke(black)
  line("a", "b", name: "line3", mark: (end: ">"))
  stroke((paint: purple, dash: "dashed"))
  line("a1", "b1", name: "line1", mark: (end: ">"))
  line("a2", "b2", name: "line2", mark: (end: ">"))
  stroke((paint: gray, dash: "dotted"))
  line("a", "L.west")
  line("a", "L.east")
  line("b", "R.west")
  line("b", "R.east")
  content("a.south", [$a$], anchor: "north")
  content("b.south", [$b$], anchor: "north")
  content("line3.mid", $p$, anchor: "north")
}))
#align(center)[_think $subst$ gives a function that "transports" terms in $B a$ to $B b$_]

#pagebreak()
$
jrule => subst => sym
$
#figure(grid(
  columns: 2,
  align: (left + top, left + top),
  gutter: 1em,
  $
  subst :& {a, b: A} \
  & B : A -> UU \
  -> & Id(A,a,b) \
  -> & B #h(0.25em) a \
  -> & B #h(0.25em) b \
  $,
  $
    &sym p = subst \
    &#h(1em) lambda x. Id(A,x,a) \
    &#h(1em) p \
    &#h(1em) refl_a
  $
))
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((1.5,-2), radius: (2.5,1.3), name: "B")
  fill(black)
  circle((0,-2), radius: 0.15, name: "a")
  circle((3,-2), radius: 0.15, name: "b")
  stroke(black)
  fill(none)
  bezier("a.east", "b.west", (1.75,-3) ,name: "line3", mark: (end: (symbol: ">", fill: black)))
  content("line3.mid", $p$, anchor: "north")
  arc("a", start: 0deg, stop: 300deg, radius: 0.3, mark: (end: (symbol: ">", fill: purple)), name: "arc1")
  stroke((paint: purple, dash: "dashed"))
  bezier("b.north", "a.north", (1.75,-0.6), mark: (end: (symbol: ">", fill: purple)))
  stroke((paint: gray, dash: "dotted"))
  bezier("a", "b",(1.75,-1.7))
  content("a.south", [$a$], anchor: "north")
  content("b.south", [$b$], anchor: "north")
  content("B.north", $A$, anchor: "south")
}))
#align(center)[_think $sym$ "drags" the origin of $refl_a$ along $p$ to get its "inverse"_]

#pagebreak()
$
  jrule => subst => trans
$
#figure(grid(
  columns: 2,
  align: (left + top, left + top),
  gutter: 1em,
  $
  subst :& {a, b: A} \
  & B : A -> UU \
  -> & Id(A,a,b) \
  -> & B #h(0.25em) a \
  -> & B #h(0.25em) b \
  $,
  $
    &trans p#h(0.25em) q = subst\
    &#h(1em) lambda x. Id(A,a,x) \
    &#h(1em) p \
    &#h(1em) q
  $
))
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((1.5,-1.5), radius: (2.5,2), name: "B")
  fill(black)
  circle((0,-2), radius: 0.15, name: "a")
  circle((3,-2), radius: 0.15, name: "b")
  circle((3,-0.5), radius:0.15, name: "c")
  stroke(black)
  fill(none)
  bezier("b.north", "c.south", (3.5,-1.5), name: "line1", mark: (end: (symbol: ">", fill: black)))
  line("a.east", "b.west",name: "line3", mark: (end: (symbol: ">", fill: black)))
  content("line3.mid", $p$, anchor: "north")
  content("line1.mid", $q$, anchor: "west")
  fill(none)
  stroke((paint: purple, dash: "dashed"))
  line("a", "c.west", mark: (end: (symbol: ">", fill: purple)))
  stroke((paint: gray, dash: "dotted"))
  bezier("b", "c",(2.1,-1.5))
  content("a.south", [$a$], anchor: "north")
  content("b.south", [$b$], anchor: "north")
  content("c.north", [$c$], anchor: "south")
  content("B.north", $A$, anchor: "south")
}))
#align(center)[_think $trans$ "drags" end of $p$ along $q$ to get the transitive_]

#pagebreak()
$
  jrule => subst => cong
$
#figure(grid(
  columns: 2,
  align: (left + top, left + top),
  gutter: 1em,
  $
  subst :& {a, b: A} \
  & B : A -> UU \
  -> & Id(A,a,b) \
  -> & B #h(0.25em) a \
  -> & B #h(0.25em) b \
  $,
  $
    &cong f#h(0.25em) p = subst \
    &#h(1em) lambda x. Id(B,f#h(0.25em) a, f#h(0.25em) x) \
    &#h(1em) p \
    &#h(1em) refl_(f#h(0.25em) a)
  $
))
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((1.5,0.2), radius: (2.5,1), name: "A")
  circle((1.5,-2), radius: (2.5,1), name: "B")
  fill(black)
  circle((0,-2), radius: 0.15, name: "fa")
  circle((3,-2), radius: 0.15, name: "fb")
  circle((0,0.2), radius: 0.15, name: "a")
  circle((3,0.2), radius: 0.15, name: "b")
  stroke(black)
  line("a.east", "b.west", name: "line1", mark: (end: (symbol: ">", fill: black)))
  stroke((paint: purple, dash: "dashed"))
  line("fa.east", "fb.west", name: "line3", mark: (end: (symbol: ">", fill: purple)))
  stroke((paint: gray, dash: "dotted"))
  line("a", "fa", name: "line2", mark: (end: (symbol: ">", fill: gray)))
  line("b", "fb", name: "line4", mark: (end: (symbol: ">", fill: gray)))
  stroke(black)
  fill(none)
  arc("fa", start: 0deg, stop: 300deg, radius: 0.3, mark: (end: (symbol: ">", fill: black)), name: "arc1")
  content("fa.south", [$f gap a$], anchor: "north-west")
  content("fb.south", [$f gap b$], anchor: "north-east")
  content("a.north", [$a$], anchor: "south-west")
  content("b.north", [$b$], anchor: "south-east")
  content("line1.mid", $p$, anchor: "north")
  content("B.east", $B$, anchor: "west")
  content("A.east", $A$, anchor: "west")
}))
#align(center)[_think $cong$ "drags" end of $refl_(f #h(0.25em) a)$ along a path in $B$ "parallel" to $p$_]

#pagebreak()
$
  jrule => uniq
$
#align(center)[_let $[z] = Sigma(z : A, Id(A,x,z))$; points idetnfied with $z$_]
#figure(grid(
  columns: 2,
  align: (left + top, left + top),
  gutter: 1em,
  $
  jrule :& {A : UU} \
  &(C : (a, b: A) -> Id(A,a,b) -> Ty(Gamma)) \
  -> & (a : A -> C#h(0.25em) a#h(0.25em) a#h(0.25em) refl_a) \
  -> & a, b : A -> p : Id(A,a,b) \ 
  -> & C#h(0.25em) a#h(0.25em) b#h(0.25em) p
  $,
  $
    &uniq {a : A} b #h(0.25em) p = jrule \ #pause
    &#h(1em) lambda x, y, p'. Id([z], (x,refl_x), (y,p'))\ #pause
    &#h(1em) lambda x. refl_(x, refl_x) \ #pause
    &#h(1em) a#h(0.25em) b#h(0.25em) p
  $
))
#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 100%, y: 100%)
  circle((1.5,-2), radius: (2.5,1.3), name: "B")
  fill(black)
  circle((0,-2), radius: 0.15, name: "a")
  circle((3,-2), radius: 0.15, name: "b")
  stroke(black)
  line("a", "b", name: "line3", mark: (end: (symbol: ">", fill: black)))
  content("line3.mid", $p$, anchor: "north")
  fill(none)
  arc("a", start: 0deg, stop: 300deg, radius: 0.3, mark: (end: (symbol: ">", fill: black)), name: "arc1")
  stroke((paint: purple, dash: "dashed"))
  bezier("line3.mid", "arc1.north", (0.7,-0.6), mark: (end: (symbol: ">", fill: purple)))
  content("a.south", [$a$], anchor: "north")
  content("b.south", [$b$], anchor: "north")
  content("B.north", $A$, anchor: "south")
}))
#align(center)[_think $uniq$ identifies all identities from $a$ to $b$ with $refl_a$_]


#pagebreak()
- *J is Independent*: $jrule$ can still be defined in an $Eq$ with reflection, but it will be a trivial structure due to UIP
- *Function Extensionality (funext)*: We can't construct a term for such a type, assuming it as an axiom breaks canonicity (i.e. a bool term is judgementally neither true or false)
$
  "Funext" = (A: UU) -> (B: A -> UU) -> (f, g: (a: A) -> B#h(0.25em) a) \ -> ((a : A) -> Id(B#h(0.25em) a, f#h(0.25em) a, g #h(0.25em) a)) -> Id((a: A) -> B#h(0.25em) a, f, g)
$
- *No UIP*: We can't construct a term for such a type
$
  "UIP" &= (A: UU) -> (a, b: A) -> (p, q : Id(A,a,b)) -> Id(Id(A,a,b),p,q) \
$
- *Axiom K*: but we can add a second eliminator that identifies identifications of a term to itself with refl to recover UIP whilst still having canonicity and normalization
- *Hoffman Conservativity Theorem*: all propositions that are provable (construct terms for a type) in ETT but not in ITT boils down to funext and UIP

== Universes

- *Full Spectrum*: To use types in our theory, we need a notion of a universe whose terms are types, thus we can omit the formation rules of $Upsilon$ for introduction rules of $UU$
#figure(diagram(cell-size: 10mm, $
Tm(Gamma, UU) 
  edge("r", El, "->", bend: #30deg) 
  edge("r", code, "<-", bend: #{-30deg}) &
Ty(Gamma)
$))

- *Recursive Types*: i.e. $Pi(UU,-)$ can't be made without having a code for $UU : UU$, but doing so causes impredicative paradoxes making the theory inconsistent (we won't prove it here), thus we make a infinite hierarchy of cumulative universes
#figure(grid(columns: (1fr, 1fr), align: (center + horizon, center + horizon),
proof-tree(rule(
  name: [Lifting],
  $Gamma hy lift_i(c) : UU_(i+1)$,
  $Gamma hy c : UU_i$
)),
proof-tree(rule(
  name: [Universe],
  $Gamma hy uni_(i,j) : UU_i$,
  $j < i$,
  $Gamma hy UU_i type$,
  $Gamma hy UU_j type$
))
))

== *Univalence*

$
  { p in Tm(Gamma. Id(UU, A, B), C) | ua } iso {star}
$
- we want $C$ to work as a "bridge" that brings proofs in $A$ to $B$ #pause
- naively we might consider the isomorphisms or bimaps, but this is too strong a condition that it only works on a subset of $UU$ called $HProp$ which are homotopy propositions, types with only one term

#pagebreak()
$
  Tm(Gamma. Id(HProp, A, B), iso) iso {star} \ #pause
  (A =_HProp B) iso (A iso B) #pause
$
$
  A iso B &= Sigma((f,g) : A <-> B, "areIso"(f,g)) \ #pause
  A <-> B &= (A -> B) times (B -> A) \ #pause
  "areIso"(f,g) &= Id(A -> A, g comp f, id) times Id(B -> B, f comp g, id) #pause
$
#align(center)[_think both types need to have the same amount of terms; this is too restrictive_]

#figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 60%, y: 60%)
  circle((0,0), name: "L")
  circle((3,0), name: "R")
  circle((1.5,0), radius: (3,2), name: "HProp")
  fill(black)
  circle((0,0), radius: 0.15, name: "a1")
  circle((3,0), radius: 0.15, name: "b1")
  fill(black)
  content("L.north", [$A$], anchor: "south")
  content("R.north", [$B$], anchor: "south")
  content("HProp.north", $HProp$, anchor: "south")
  stroke(black)
  stroke((paint: purple, dash: "dashed"))
  line("a1.east", "b1.west", name: "line1", mark: (end: ">", start: ">"))
}))
#align(center)[_elim as $p |->(subst id p, subst id (sym p))$ and intro an identification given an iso_]

#pagebreak()
$
  Tm(Gamma. Id(UU, A, B), equiv) equiv {star} \ #pause
  (A = B) equiv (A equiv B) #pause
$
$
  "fib"(f,b) &= Sigma(a: A, Id(B, f#h(0.25em) a, b)) \ #pause
  "isContr"(X) &= Sigma(x: X, Pi(y: X, Id(X, x, y))) \ #pause
  "isEquiv"(f) &= Pi(b: B, "isContr"("fib"(f,b))) \ #pause
  A equiv B &= Sigma(f: A -> B, "isEquiv"(f))
$
#meanwhile
#figure(toptable(columns: 4, align: (center + horizon, center + horizon, center + horizon, center + horizon),
[fibre], [contractible], [contractible fibre], [univalence],
figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 60%, y: 60%)
  circle((0,0), radius:(1,1.5), name: "L")
  circle((3,0), radius:(1,1.5), name: "R")
  fill(black)
  circle((0,1), radius: 0.15, name: "a1")
  circle((0,0.25), radius: 0.15, name: "a2")
  circle((0,-0.5), radius: 0.15, name: "a3")
  circle((0,-1.2), radius: 0.15, name: "a4")
  circle((2.75,0.8), radius: 0.15, name: "b1")
  circle((2.75,0), radius: 0.15, name: "b2")
  circle((2.75,-0.8), radius: 0.15, name: "b3")
  circle((3.75,0), radius: 0.15, name: "b4")
  circle((3.2,-1), radius: 0.15, name: "b5")
  fill(black)
  content("L.north", [$A$], anchor: "south")
  content("R.north", [$B$], anchor: "south")
  stroke(black)
  stroke((paint: gray, dash: "dotted"))
  line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
  content("line1.mid", $f$, anchor: "south")
  line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
  line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
  stroke((paint: purple, dash: "solid"))
  line("b1.east", "b4.north", name: "line4", mark: (end: ">"))
  line("b2.east", "b4.west", name: "line5", mark: (end: ">"))
  line("b3.east", "b4.south", name: "line6", mark: (end: ">"))
})),
figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 60%, y: 60%)
  circle((0,0), radius:(1.5,1.5), name: "X")
  content("X.north", $X$, anchor: "south")
  fill(black)
  circle((0,0.75), radius: 0.15, name: "x1")
  content("x1.east", $x$, anchor: "west")
  circle((-0.75,-0.5), radius: 0.15, name: "x2")
  circle((0,-0.75), radius: 0.15, name: "x3")
  circle((0.75,-0.5), radius: 0.15, name: "x4")

  stroke((paint: purple, dash: "solid"))
  line("x1.south", "x2.north", name: "line1", mark: (end: ">"))
  line("x1.south", "x3.north", name: "line1", mark: (end: ">"))
  line("x1.south", "x4.north", name: "line1", mark: (end: ">"))
})),
figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 60%, y: 60%)
  circle((0,0), radius:(1,1.5), name: "L")
  circle((3,0), radius:(1,1.5), name: "R")
  fill(black)
  circle((0,0.8), radius: 0.15, name: "a1")
  fill(purple)
  stroke(purple)
  circle((0,0), radius: 0.15, name: "a2")
  fill(black)
  stroke(black)
  circle((0,-0.8), radius: 0.15, name: "a3")
  circle((2.75,0.8), radius: 0.15, name: "b1")
  circle((2.75,0), radius: 0.15, name: "b2")
  circle((2.75,-0.8), radius: 0.15, name: "b3")
  circle((3.75,0), radius: 0.15, name: "b4")
  fill(black)
  content("L.north", [$A$], anchor: "south")
  content("R.north", [$B$], anchor: "south")
  stroke(black)
  stroke((paint: gray, dash: "dotted"))
  line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
  content("line1.mid", $f$, anchor: "south")
  line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
  line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
  stroke((paint: black, dash: "solid"))
  line("b1.east", "b4.north", name: "line4", mark: (end: ">"))
  line("b2.east", "b4.west", name: "line5", mark: (end: ">"))
  line("b3.east", "b4.south", name: "line6", mark: (end: ">"))
  stroke((paint: purple, dash: "solid"))
  line("a2.north", "a1.south", mark: (end: ">"))
  line("a2.south", "a3.north", mark: (end: ">"))
  line("line5.25%", "line4.25%", mark: (end: ">"))
  line("line5.25%", "line6.25%", mark: (end: ">"))
})),
figure(cetz.canvas({
  import cetz.draw: *
  scale(x: 60%, y: 60%)
  circle((0,0), radius:(1,1.5), name: "L")
  circle((3,0), radius:(1,1.5), name: "R")
  circle((1.5,0), radius: (3,2.5), name: "UU")
  circle((0,0), radius: (0.3,0.5), name: "a1")
  circle((-0.2,0.5), radius: (0.3,0.5), name: "a2")
  circle((0,-0.7), radius: (0.3,0.5), name: "a3")
  circle((3,0), radius: (0.3,0.5), name: "b1")
  circle((2.7,0.8), radius: (0.3,0.5), name: "b2")
  circle((3.1,-0.5), radius: (0.3,0.5), name: "b3")
  fill(black)
  content("L.north", [$A$], anchor: "south")
  content("R.north", [$B$], anchor: "south")
  content("UU.north", $UU$, anchor: "south")
  stroke(black)
  stroke((paint: purple, dash: "dashed"))
  line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
  line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
  line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
}))
))

== Concluding Example

- term $bb(N)$ of isEven
- use $ua$ to get isEven for big integer
- note on ua as an axiom, thus we can't compute it, we don't have it in a theorem prover
- thus higher observational type theeory

= HOTT from HoTT

== Observational Equality

== Parametricity

== *Parametric Univalence*