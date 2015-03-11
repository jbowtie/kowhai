package kowhai

import "fmt"

//import "strings"

//xpathLexer - generic-ish lexer with xpathTokens
//parser - consume tokens until error or EOF
//if input accepted, build parse tree
//parse tree -> AST maybe?

// EarleySet = []EarleyItem
// EIM = {dottedRule, originLocation}
// AH = {AHFAstate, originLocation}  AFHA is group of dotted rules; either predicted or confirmed
// LIM = {topAH, SYMBOL, originLocation}

// A term is anything that can appear on the RHS of a rule
// Here we define Symbol (for literal terminals that appear in a rule definition),
// Rule (for non-terminals), TypedTerm (for matching type of token produced by a lexer)
type Term interface {
	IsRule() bool
	MatchesToken(token Token) bool
}

//needs to expose a string value (for symbol match)
//needs to expose a token type (for type match; ie, int literal)
type Token interface {
	AsValue() string
	TokenType() int
}

//adding a literal token type for testing
type LiteralToken string

func (l LiteralToken) AsValue() string {
	return string(l)
}

func (l LiteralToken) TokenType() int {
	if l.AsValue() == "1" {
		return 1
	}
	return 0
}

//rule as fit for usage in the state machine
type AhfaRule struct {
	name     string     //name of the rule (LHS)
	prod     Production // RHS
	dotIndex int        // dot location
	orig     *Rule
}

//convert to a term for convenience
func (r *AhfaRule) AsTerm() Term {
	return r.orig
}

func (rule *AhfaRule) String() string {
	return fmt.Sprint(rule.name, rule.prod, rule.dotIndex)
}

func (rule *AhfaRule) nextTerm() Term {
	if rule.IsCompleted() {
		return nil
	}
	return rule.prod[rule.dotIndex]
}

func (rule *AhfaRule) IsCompleted() bool {
	return rule.dotIndex >= len(rule.prod)
}

func (r *AhfaRule) derivesNull() bool {
	return r.prod == nil
}

//group of dotted rules
type AhfaState []*AhfaRule

//check to see if a rule is present in a state
func (state AhfaState) Contains(rule *AhfaRule) bool {
	for _, r := range state {
		if r.String() == rule.String() {
			return true
		}
	}
	return false
}

func (state AhfaState) transitions() (terms map[Term]int) {
	terms = make(map[Term]int)
	for j := 0; j < len(state); j++ {
		rule := state[j]
		term := rule.nextTerm()
		if term == nil {
			continue
		}
		terms[term] = -1
	}
	return terms
}

func (state AhfaState) advance(key Term) (newstate AhfaState) {
	for j := 0; j < len(state); j++ {
		rule := state[j]
		term := rule.nextTerm()
		if term == nil {
			continue
		}
		if term == key {
			newRule := &AhfaRule{rule.name, rule.prod, rule.dotIndex + 1, rule.orig}
			if !state.Contains(newRule) {
				newstate = append(newstate, newRule)
			}
		}
	}
	return
}

func (state AhfaState) closure() AhfaState {
	//not using range as we can add more rules while iterating
	for j := 0; j < len(state); j++ {
		rule := state[j]
		term := rule.nextTerm()
		if term == nil {
			continue
		}
		if term.IsRule() {
			r := term.(*Rule)
			for _, prod := range r.Productions {
				newRule := &AhfaRule{r.name, prod, 0, r}
				if !state.Contains(newRule) {
					state = append(state, newRule)
				}
			}
		}
	}
	return state
}

func (state AhfaState) split() (kernel AhfaState, nonkernel AhfaState) {
	for _, rule := range state {
		if rule.dotIndex == 0 && rule.name != "GAMMA" {
			nonkernel = append(nonkernel, rule)
		} else if rule.derivesNull() {
			//see AH paper for details on rules that can derive null
			//right now our derivesNull() check is primitive
			nonkernel = append(nonkernel, rule)
		} else {
			kernel = append(kernel, rule)
		}
	}
	return
}

//determine if two states are equal
func (state AhfaState) isEqual(other AhfaState) bool {
	if len(state) != len(other) {
		return false
	}
	for _, r := range state {
		if !other.Contains(r) {
			return false
		}
	}
	return true
}

// the state machine
// TODO: transitions should be part of corresponding state
type AhfaMachine struct {
	states      []AhfaState
	transitions map[int]map[Term]int
}

// given a start rule, build a state machine
func buildStateMachine(start *Rule) (machine *AhfaMachine) {
	machine = &AhfaMachine{}
	machine.transitions = make(map[int]map[Term]int)

	//add the start rule
	rule0 := &AhfaRule{"GAMMA", Production{start}, 0, nil}
	k0, nk0 := AhfaState{rule0}.closure().split()
	machine.states = append(machine.states, k0)
	if nk0 != nil {
		machine.states = append(machine.states, nk0)
		machine.transitions[0] = map[Term]int{nil: 1}
	}

	//not using range as we can add more states while iterating
	for j := 0; j < len(machine.states); j++ {
		state := machine.states[j]
		if machine.transitions[j] == nil {
			machine.transitions[j] = make(map[Term]int)
		}
		newTerms := state.transitions()
		for key, _ := range newTerms {
			// for each new term, work out the new state
			newState := state.advance(key).closure()
			//split into kernel and non-kernel states
			k, nk := newState.split()
			kIndex := machine.IndexOf(k)
			if kIndex == -1 {
				kIndex = len(machine.states)
				machine.states = append(machine.states, k)
			}
			machine.transitions[j][key] = kIndex
			if nk != nil {
				//if we have a non-kernel state
				newIndex := machine.IndexOf(nk)
				if newIndex == -1 {
					newIndex = len(machine.states)
					machine.states = append(machine.states, nk)
				}
				//add a NIL transition from k -> nk
				if machine.transitions[kIndex] == nil {
					machine.transitions[kIndex] = make(map[Term]int)
				}
				machine.transitions[kIndex][nil] = newIndex
			}
		}
	}
	return
}

// used during building of machine to deduplicate states
func (machine *AhfaMachine) IndexOf(state AhfaState) int {
	for i, s := range machine.states {
		if s.isEqual(state) {
			return i
		}
	}
	return -1
}

// transition to the next state
func (machine *AhfaMachine) Goto(state int, symbol Term) int {
	if state < 0 || state >= len(machine.transitions) {
		return -1
	}
	t := machine.transitions[state]
	dest, ok := t[symbol]
	if !ok {
		return -1
	}
	return dest
}

func (machine *AhfaMachine) String() string {
	var output string
	for i, state := range machine.states {
		output += fmt.Sprint("State", i, state, "\n")
		output += fmt.Sprint("     ", machine.transitions[i], "\n")
	}
	return output
}

func (ah *AhfaMachine) AcceptedState(state int) bool {
	s := ah.states[state]
	for _, rule := range s {
		if rule.IsCompleted() {
			return rule.name == "GAMMA"
		}
	}
	return false
}

func (ah *AhfaMachine) Completed(state int) (rules []Term) {
	s := ah.states[state]
	for _, rule := range s {
		if rule.IsCompleted() {
			rules = append(rules, rule.AsTerm())
		}
	}
	return rules
}

type AhfaCompletion struct {
	start int
	end   int
	term  Term
}

//used to implement sort interface
type ParseNodes []AhfaCompletion

func (x ParseNodes) Len() int {
	return len(x)
}
func (x ParseNodes) Swap(i, j int) {
	x[i], x[j] = x[j], x[i]
}
func (x ParseNodes) Less(i, j int) bool {
	if x[i].start < x[j].start {
		return true
	}
	if x[i].start > x[j].start {
		return false
	}
	return x[i].end > x[j].end
}

// Tracks AFHA state and parent state
// should parent be a pointer to the parent EI?
// symbol is used when memoizing leo items
type EarleyItem struct {
	state  int
	parent int
	symbol Term
}

type EarleyItemSet struct {
	pos         int             // position in the string
	token       string          //the symbol we are moving over
	items       []EarleyItem    // items in the set
	ustates     map[string]bool //used to dedupe when adding
	transitions map[Term][]EarleyItem
}

//add an item to the EIS
func (set *EarleyItemSet) AddItem(state int, parent int) {
	//force uniqueness
	hash := fmt.Sprint(state, parent)
	if set.ustates[hash] {
		return
	}
	set.ustates[hash] = true
	set.items = append(set.items, EarleyItem{state, parent, nil})
}

func (item EarleyItem) String() string {
	if item.symbol == nil {
		return fmt.Sprint("{", item.state, " ", item.parent, "}")
	}
	return fmt.Sprint("{", item.state, " ", item.parent, " ", item.symbol, "}")
}

func (set *EarleyItemSet) String() string {
	return fmt.Sprint(set.pos, ": ", set.token, set.items)
}

type MarpaParser struct {
	machine *AhfaMachine
	table   []*EarleyItemSet
	cnodes  ParseNodes
}

// adds an Earley item for the confirmed state,
// plus any items predicted by the presence of a null transition
func (parser *MarpaParser) addEIM(i int, confirmedAH int, origin int) {
	//add the confirmed state
	parser.table[i].AddItem(confirmedAH, origin)
	predictedAH := parser.machine.Goto(confirmedAH, nil)
	//add predicted state, if any
	if predictedAH > -1 {
		parser.table[i].AddItem(predictedAH, i)
	}
}

func (parser *MarpaParser) ScanToken(token Token) (err error) {
	col := len(parser.table)
	set := &EarleyItemSet{col, token.AsValue(), nil, make(map[string]bool), make(map[Term][]EarleyItem)}
	parser.table = append(parser.table, set)
	parser.scan_pass(col, token)
	// if there are no items after the scan pass,
	// there's a syntax error!
	if set.items == nil {
		return fmt.Errorf("SYNTAX ERROR: %s at position %d", set.token, col)
	}
	parser.reduce_pass(col)
	return
}

func CreateParser(machine *AhfaMachine) MarpaParser {
	table := []*EarleyItemSet{}
	table = append(table, &EarleyItemSet{0, "", nil, make(map[string]bool), make(map[Term][]EarleyItem)})

	parser := MarpaParser{machine, table, nil}
	parser.initial()
	return parser
}

func (parser *MarpaParser) DumpTable() {
	fmt.Println(parser.table)
}

func (parser *MarpaParser) PrintAcceptedTree() bool {
	final_set := parser.table[len(parser.table)-1]
	for _, item := range final_set.items {
		if item.parent == 0 {
			if parser.machine.AcceptedState(item.state) {
				fmt.Println("===========")
				parser.DumpTable()
				//dumpTree(item.parseNode, 0)
				//sort.Sort(parser.cnodes)
				//for _, node := range parser.cnodes {
				//	if node.end > node.start {
				//		fmt.Println(node.start, node.end, node.term) //, tokens[node.start:node.end])
				//	}
				/*if node.end-node.start == 1 {
					fmt.Println("\t", tokens[node.start])
				}*/
				//}
				fmt.Println("===========")
				//build parse tree
				return true
			}
		}
	}
	// if the last Earley Set contains an accepted state
	// we have valid input
	// otherwise we have an incomplete expression
	//reject input
	fmt.Println("===========")
	fmt.Println("ERROR: INCOMPLETE EXPRESSION")
	parser.DumpTable()
	fmt.Println("===========")
	return false
}

func dumpTree(parseNode *AhfaCompletion, depth int) {
	if parseNode == nil {
		fmt.Println("<nil>")
	}
	if depth > 0 {
		fmts := fmt.Sprintf("%%%ds", depth*2)
		fmt.Printf(fmts, " ")
	}
	fmt.Println(parseNode.start, parseNode.end, parseNode.term)
	/*if parseNode.left != nil {
		dumpTree(parseNode.left, depth+1)
	}
	if parseNode.right != nil {
		dumpTree(parseNode.right, depth+1)
	}*/
}

// initialize the parser
func (parser *MarpaParser) initial() {
	parser.addEIM(0, 0, 0)
	parser.reduce_pass(0)
	return
}

func (parser *MarpaParser) scan_pass(location int, token Token) {
	if location == 0 {
		return
	}
	s := Symbol(token.AsValue())

	// lookup by symbol
	set := parser.table[location-1].transitions[s]
	for _, item := range set {
		toAH := parser.machine.Goto(item.state, s)
		if toAH > -1 {
			parser.addEIM(location, toAH, item.parent)
		}
	}

	//lookup by token type
	set = parser.table[location-1].transitions[TypedTerm(token.TokenType())]
	for _, item := range set {
		toAH := parser.machine.Goto(item.state, TypedTerm(token.TokenType()))
		if toAH > -1 {
			parser.addEIM(location, toAH, item.parent)
		}
	}

	return
}

func (parser *MarpaParser) recordCompletion(start, end int, term Term) {
	c := AhfaCompletion{start, end, term}
	parser.cnodes = append(parser.cnodes, c)
}

func (parser *MarpaParser) reduce_pass(location int) {
	eset := parser.table[location]
	//for each EIM in location table
	for j := 0; j < len(eset.items); j++ {
		item := eset.items[j]
		for _, rule := range parser.machine.Completed(item.state) {
			parser.reduceOneLHS(location, item.parent, rule)
			parser.recordCompletion(item.parent, location, rule)
		}

	}
	parser.memoize_transitions(location)
	return
}

// this builds a transition table for postdot symbols
// this will be used as a lookup when future columns
// try a reduction (can also be used to speed up scans)
func (parser *MarpaParser) memoize_transitions(location int) {
	current_set := parser.table[location]
	current_items := current_set.items

	trans := make(map[Term][]EarleyItem)
	//construct sym -> []EIM
	for _, item := range current_items {
		// postdot symbols are the keys in the transitions table
		for postdot, _ := range parser.machine.transitions[item.state] {
			trans[postdot] = append(trans[postdot], item)
		}
	}

	for postdot, items := range trans {
		// only worry about unique postdots
		if len(items) == 1 && postdot != nil && postdot.IsRule() {
			r := postdot.(*Rule)
			// only bother with leo handling of right recursive rules
			if r.IsRightRecursive() {
				leo := EarleyItem{items[0].state, items[0].parent, postdot}
				current_set.transitions[postdot] = append(current_set.transitions[postdot], leo)
			} else {
				current_set.transitions[postdot] = items
			}
		} else {
			current_set.transitions[postdot] = items
		}
	}
	//fmt.Println("MEMO", location, current_set.transitions)
	//for each postdot in iES
	// if leo_eligible  // right recursive, unique postdot
	//   transitions(location, postdot) = LIM
	// else
	//   transitions(location, postdot) = EIMs.contains(postdot)
	return
}

func (parser *MarpaParser) reduceOneLHS(location int, origin int, term Term) {
	//get all the postDOTs in this location
	// in future, lookup transition table for this term
	set := parser.table[origin]
	postDOTs := set.transitions[term]

	// term is a COMPLETED rule
	// recognize a right recursive rule
	/*r := term.(*Rule)
	if r != nil && r.IsRightRecursive() {
		fmt.Println(r)
	}*/

	//fmt.Println("COMPLETE", term, "STARTS", origin, "ENDS", location)

	// loop through the postdots from the original location
	for _, item := range postDOTs {
		if item.symbol != nil {
			//fmt.Println("Leo reduction for", term, origin, location)
			parser.leoReduce(location, item)
		} else {
			parser.earleyReduce(location, item, term)
		}
	}

	for _, item := range set.items {
		if !inSlice(item, postDOTs) {
			parser.earleyReduce(location, item, term)
		}
	}

}

func inSlice(item EarleyItem, set []EarleyItem) bool {
	for _, i := range set {
		if i == item {
			return true
		}
	}
	return false
}

func (parser *MarpaParser) leoReduce(location int, item EarleyItem) {
	toAH := parser.machine.Goto(item.state, item.symbol)
	if toAH > -1 {
		//fmt.Println("Leo reduction to", item.parent, location)
		parser.addEIM(location, toAH, item.parent)
	}
}

func (parser *MarpaParser) earleyReduce(location int, item EarleyItem, term Term) {
	toAH := parser.machine.Goto(item.state, term)
	if toAH > -1 {
		parser.addEIM(location, toAH, item.parent)
	}
}

/*
// init a grammar for testing
func grammar() (start *Rule) {
	n := &Rule{"N", []Production{CreateSymbol("boy")}}
	n.AddSymbol("telescope")

	d := &Rule{"D", []Production{}}
	d.AddSymbol("a")
	d.AddSymbol("an")
	d.AddSymbol("the")
	//this makes D optional
	//d.Add(Production{})

	v := &Rule{"V", []Production{}}
	v.AddSymbol("saw")

	p := &Rule{"P", []Production{}}
	p.AddSymbol("with")

	np := &Rule{"NP", []Production{Production{d, n}}}
	np.AddSymbol("john")

	pp := &Rule{"PP", []Production{Production{p, np}}}
	np.Add(Production{np, pp})

	vp := &Rule{"VP", []Production{Production{v, np}}}
	vp.Add(Production{vp, pp})

	s := &Rule{"S", []Production{Production{np, vp}}}
	return s
}

func grammar() (start *Rule) {
	g := &Grammar{}
	g.rules = make(map[string]*Rule)
	// S = a b? c
	g.CreateRule("S", Symbol("a"), g.Optional(Symbol("b")), Symbol("c"))
	// S = d e* f
	g.CreateRule("S", Symbol("d"), g.Star(Symbol("e")), Symbol("f"))
	// S = g h+ i
	g.CreateRule("S", Symbol("g"), g.Plus(Symbol("h")), Symbol("i"))
	// S = j INT k
	g.CreateRule("S", Symbol("j"), g.Type(1), Symbol("k"))
	g.SetStart("S")
	//g.DumpRules()
	return g.GetStartRule()
}
*/
/*
func main() {
	g := grammar()

	machine := BuildStateMachine(g)
	//fmt.Println(machine)
	//fmt.Println("===========")

	//TODO: turn into proper tests
	marpa(machine, strings.Split("a b c", " "))
	marpa(machine, strings.Split("a c", " "))
	marpa(machine, strings.Split("d f", " "))
	marpa(machine, strings.Split("d e f", " "))
	marpa(machine, strings.Split("d e e e e e f", " "))
	//marpa(machine, strings.Split("d g f", " ")) test

	marpa(machine, strings.Split("j 1 k", " "))

	//marpa(machine, strings.Split("john saw a boy", " "))
	//marpa(machine, strings.Split("john saw the boy with the telescope", " "))
	//marpa(machine, strings.Split("a boy john saw", " ")) //deliberate syntax error
	//marpa(machine, strings.Split("john saw", " ")) //incomplete

}*/
