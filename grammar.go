package kowhai

import "fmt"

// A grammar is a set of rules that the parser should recognize
// The usage is create a grammar,
// add some rules,
// set a start rule (top-level definition)
// generate a state machine for the parser
type Grammar struct {
	rules map[string]*Rule
	start *Rule
}

func (g *Grammar) CreateRule(name string, terms ...Term) {
	rule := g.Lookup(name)
	prod := Production{}
	for _, t := range terms {
		prod = append(prod, t)
	}
	rule.Productions = append(rule.Productions, prod)
	g.rules[name] = rule
}

// handle common right-hand side of "t1 | t2 | t3"
// same as top-level OR
func (g *Grammar) UnionSingleTerms(name string, terms ...Term) {
	rule := g.Lookup(name)
	for _, t := range terms {
		rule.Productions = append(rule.Productions, Production{t})
	}
	g.rules[name] = rule
}

// Expand a ? operator (zero-or-one)
func (g *Grammar) Optional(terms ...Term) (out Term) {
	name := fmt.Sprintf("OPT_%v", terms)
	g.CreateRule(name, terms...)
	g.CreateRule(name)
	return g.rules[name]
}

// Expand a * operator (zero-or-more)
func (g *Grammar) Star(terms ...Term) (out Term) {
	name := fmt.Sprintf("STAR_%v", terms)
	g.CreateRule(name)
	r := g.rules[name]
	terms = append(terms, r)
	g.CreateRule(name, terms...)
	return r
}

// Expand a + operator (one-or-more)
func (g *Grammar) Plus(terms ...Term) (out Term) {
	name := fmt.Sprintf("PLUS_%v", terms)
	s := g.Star(terms...)
	terms = append(terms, s)
	g.CreateRule(name, terms...)
	return g.rules[name]
}

// Create an OR rule where each term is a different branch
func (g *Grammar) Or(terms ...Term) (out Term) {
	name := fmt.Sprintf("OR_%v", terms)
	rule := g.Lookup(name)
	for _, t := range terms {
		rule.Productions = append(rule.Productions, Production{t})
	}
	g.rules[name] = rule
	return rule
}

// Create a TypedTerm
func (g *Grammar) Type(tokenType int) (out Term) {
	return TypedTerm(tokenType)
}

// Lookup a rule
func (g *Grammar) Lookup(name string) (out *Rule) {
	rule, ok := g.rules[name]
	if !ok {
		rule = &Rule{name, nil}
		g.rules[name] = rule
	}
	return rule
}

func (g *Grammar) Symbol(name string) Term {
	return Symbol(name)
}

func (g *Grammar) SetStart(name string) {
	g.start = g.rules[name]
}

func (g *Grammar) DumpRules() {
	for key := range g.rules {
		rule := g.rules[key]
		fmt.Println(rule.name, rule.Productions)
	}
}

func (g *Grammar) BuildStateMachine() *AhfaMachine {
	return buildStateMachine(g.start)
}

func (g *Grammar) GetStartRule() *Rule {
	return g.start
}

func CreateGrammar() *Grammar {
	g := &Grammar{}
	g.rules = make(map[string]*Rule)
	return g
}

// Name is the left hand side
// Productions are the various right hand sides
type Rule struct {
	name        string
	Productions []Production
}

func (r *Rule) IsRule() bool {
	return true
}

func (r *Rule) MatchesToken(token Token) bool {
	return false
}

func (r *Rule) String() string {
	return r.name
}

func (r *Rule) Add(s Production) {
	r.Productions = append(r.Productions, s)
}

func (r *Rule) IsRightRecursive() bool {
	for _, prod := range r.Productions {
		last := len(prod) - 1
		if last >= 0 && prod[last] == r {
			return true
		}
	}
	return false
}

// this is used when building parse trees to
// prune out alternative shorter parse possibilities
func (r *Rule) IsAllowedChild(term Term) bool {
	for _, prod := range r.Productions {
		for _, t := range prod {
			if t == term {
				return true
			}
		}
	}
	return false
}

//Production is the set of terms that make up the right side of a rule
type Production []Term

// Symbol looks at a token and matches the value
// Generally used for keywords and punctuation referenced directly
// in rule definitions.
type Symbol string

func (s Symbol) IsRule() bool {
	return false
}

func (s Symbol) MatchesToken(token Token) bool {
	return s == Symbol(token.AsValue())
}

// A TypedTerm looks at the token type for a match
// Generally used for recognizing things produced by a lexer
// such as an int literal, string literal, etc.
type TypedTerm int

func (t TypedTerm) IsRule() bool {
	return false
}

func (t TypedTerm) MatchesToken(token Token) bool {
	return int(t) == token.TokenType()
}
