#import "@preview/touying:0.5.3": *
#import themes.university: *
#import "@preview/cetz:0.2.2"
#import "@preview/fletcher:0.5.1" as fletcher: node, edge
#import "@preview/ctheorems:1.1.2": *
#import "@preview/numbly:0.1.0": numbly

#show: university-theme.with(aspect-ratio: "16-9")

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
#let example = thmplain("example", "Example").with(numbering: none)
#let proof = thmproof("proof", "Proof")

#show: university-theme.with(
  aspect-ratio: "16-9",
  // config-common(handout: true),
  config-info(
    title: [H.O.T.T. from scratch],
    subtitle: [Equality and Isomorphisms in Type Theory],
    author: [Abdul Haliq Abdul Latiff (202303466)],
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

- a collection of rules that take in hypothetical judgements and conclude a new judgement

== Judgements

- term judgement
- type judgement
- substitution judgement

- contexts are lists of dependent term judgements
- substitutions shifts judgements from one context to another
- *example*

== *Definitional Equality*

- definitional equality of terms judgement
- definitional equality of types judgement

- an axiom of the theory i.e. equality by syntax
- *example*

== Natural Models

- substitution application
  - substitution of term rule
  - substitution of type rule
- morphism structure rules
  - identity substitution axiom
  - substitution composition rule
  - associativity of substitution composition rule
- pullback structure rules
  - variable rule
    - debruijn indices
    - *example*
  - weakening rule
    - *example*
  - context extension rule
    - *example*

== Mapping In Types

- to make a type we need
  - formation rule
  - introduction rule
  - elimination rule
  - computation rule / beta rule
  - uniqueness rule / eta rule
- type former
- isomorphism
- *example unit type*
- *example pi type*
- *example sigma type*

== Naturality

- naturality square

== *Extensional Equality*

- *example Eq type*
- equality reflection rule as elimination rule
- propositional equality
  - internalization of definitional equality
- *example Eq(N,a+b,b+a)*
- UIP
- on metatheory; normalization of extensional type theory

== Mapping Out Types

- isomorphism
- type, signature, initial algebra table
- *example unit type*
- *example bool type*
- *example A + B type*
- *example Nat type*
- *example void type*
- elim rule as recursor
- intro rule as initial algebra constructors

== *Intensional Equality*

- *example Id type*
- elim rule as J rule
  - construct subst
    - construct sym
    - construct trans
    - construct cong
  - construct uniq
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