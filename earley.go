package kowhai

import "fmt"

// A term is anything that can appear on the RHS of a rule
// Here we define Symbol (for literal terminals that appear in a rule definition),
// Rule (for non-terminals), TypedTerm (for matching type of token produced by a lexer)
type Term interface {
	IsRule() bool
	MatchesToken(token Token) bool
}

// A trivial interface for the tokens comsumed by the parser
type Token interface {
	AsValue() string //exposes a string value for symbol matches
	TokenType() int  // exposes a token type for a type match
}

// Simple token type that can be delivered to the parser
type LiteralToken string

func (l LiteralToken) AsValue() string {
	return string(l)
}

func (l LiteralToken) TokenType() int {
	return 0
}

// Used only in building the parse tree
// will need to refactor away later
type SppfTerm string

func (l SppfTerm) String() string {
	return fmt.Sprintf("_%v_", string(l))
}

func (l SppfTerm) IsRule() bool {
	return false
}
func (l SppfTerm) MatchesToken(token Token) bool {
	return false
}

// this will hopefully become a SPPF node
type AhfaCompletion struct {
	start int
	end   int
	term  Term
}

type SppfNode struct {
	start int
	end   int
	//rule  *AhfaRule //revisit in future
	rule  Term
	left  *SppfNode
	right *SppfNode
}

func (s *SppfNode) Label() string {
	return fmt.Sprintf("%v %v %v", s.rule, s.start, s.end)
}

func (s *SppfNode) String() string {
	if s.right == nil && s.left == nil {
		return fmt.Sprintf("<%v, %v, %v>", s.rule, s.start, s.end)
	}
	if s.right == nil {
		return fmt.Sprintf("<%v, %v, %v left(%v)>", s.rule, s.start, s.end, s.left)
	}
	return fmt.Sprintf("<%v, %v, %v left(%v) right(%v)>", s.rule, s.start, s.end, s.left, s.right)
}

//type SppfNodeSet map[string]*SppfNode

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
	state     int
	parent    int
	symbol    Term
	parseNode *SppfNode
}

type EarleyItemSet struct {
	pos         int             // position in the string
	token       string          //the symbol we are moving over
	items       []EarleyItem    // items in the set
	ustates     map[string]bool //used to dedupe when adding
	transitions map[Term][]EarleyItem
}

//add an item to the EIS
func (set *EarleyItemSet) AddItem(state int, parent int, parseNode *SppfNode) {
	//force uniqueness
	hash := fmt.Sprint(state, parent)
	if set.ustates[hash] {
		return
	}
	set.ustates[hash] = true
	set.items = append(set.items, EarleyItem{state, parent, nil, parseNode})
}

func (item EarleyItem) String() string {
	if item.parseNode == nil {
		return fmt.Sprint("{State ", item.state, " Parent ", item.parent, "}")
	}
	return fmt.Sprint("{State ", item.state, " Parent ", item.parent, " Node ", item.parseNode, "}")
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
func (parser *MarpaParser) addEIM(i int, confirmedAH int, origin int, parseNode *SppfNode) {
	//add the confirmed state
	parser.table[i].AddItem(confirmedAH, origin, parseNode)
	predictedAH := parser.machine.Goto(confirmedAH, nil)
	//add predicted state, if any
	if predictedAH > -1 {
		parser.table[i].AddItem(predictedAH, i, parseNode)
	}
}

// this handles the next token delivered by the lexer
func (parser *MarpaParser) ScanToken(token Token) (err error) {
	col := len(parser.table)
	set := &EarleyItemSet{col, token.AsValue(), nil, make(map[string]bool), make(map[Term][]EarleyItem)}
	parser.table = append(parser.table, set)
	nodes := make(map[string]*SppfNode)
	parser.scan_pass(col, token, nodes)
	// if there are no items after the scan pass,
	// there's a syntax error!
	if set.items == nil {
		return fmt.Errorf("SYNTAX ERROR: %s at position %d", set.token, col)
	}
	parser.reduce_pass(col, nodes)
	return
}

// create a new parser that uses the machine
func CreateParser(machine *AhfaMachine) MarpaParser {
	table := []*EarleyItemSet{}
	table = append(table, &EarleyItemSet{0, "", nil, make(map[string]bool), make(map[Term][]EarleyItem)})

	parser := MarpaParser{machine, table, nil}
	parser.initial()
	return parser
}

// dump the Earley sets for inspection
func (parser *MarpaParser) DumpMachine() {
	fmt.Println(parser.machine)
}

// dump the Earley sets for inspection
func (parser *MarpaParser) DumpTable() {
	fmt.Println(parser.table)
}

func (parser *MarpaParser) MakeParseNode(rule Term, origin int, location int, w *SppfNode, v *SppfNode, nodes map[string]*SppfNode) (y *SppfNode) {
	s := rule
	/*if origin == location {
		return
	}*/
	if location == origin+1 {
		y = v
		return
	}
	y = &SppfNode{origin, location, s, w, v}
	existing := nodes[y.Label()]
	if existing == nil {
		nodes[y.Label()] = y
	} else {
		y = existing
	}
	return
}

func (parser *MarpaParser) BuildParseTree() *ParseTreeNode {
	// if the last Earley Set contains an accepted state
	// we have valid input
	final_set := parser.table[len(parser.table)-1]
	for _, item := range final_set.items {
		if item.parent == 0 {
			if parser.machine.AcceptedState(item.state) {
				return parser.buildTree()
			}
		}
	}
	// otherwise we have an incomplete expression
	//reject input
	fmt.Println("===========")
	fmt.Println("ERROR: INCOMPLETE EXPRESSION")
	parser.DumpTable()
	fmt.Println("===========")
	return nil
}

func (parser *MarpaParser) buildTree() *ParseTreeNode {
	var top *ParseTreeNode
	var curr *ParseTreeNode
	for i := len(parser.cnodes); i > 0; i-- {
		n := parser.cnodes[i-1]
		tn := &ParseTreeNode{n.start, n.end, n.term, nil, nil}
		//init top if needed
		if top == nil {
			top = tn
			curr = tn
			continue
		}
		for curr != nil {
			if tn.start >= curr.start && tn.end <= curr.end {
				tn.parent = curr
				curr.children = append([]*ParseTreeNode{tn}, curr.children...)
				break
			} else {
				curr = curr.parent
			}
		}
		curr = tn
	}
	//top should be GAMMA node so expect actual top node as only child
	return top.children[0]
}

// placeholder function where we can look at the parse tree once we are building one!
func (parser *MarpaParser) PrintAcceptedTree() bool {
	// if the last Earley Set contains an accepted state
	// we have valid input
	final_set := parser.table[len(parser.table)-1]
	for _, item := range final_set.items {
		if item.parent == 0 {
			if parser.machine.AcceptedState(item.state) {
				fmt.Println("===========")
				parser.PrintCNodes()
				//dumpTree(item.parseNode, 0)
				fmt.Println("===========")
				return true
			}
		}
	}
	// otherwise we have an incomplete expression
	//reject input
	fmt.Println("===========")
	fmt.Println("ERROR: INCOMPLETE EXPRESSION")
	parser.DumpTable()
	fmt.Println("===========")
	return false
}

type ParseTreeNode struct {
	start    int
	end      int
	term     Term
	parent   *ParseTreeNode
	children []*ParseTreeNode
}

func (parser *MarpaParser) PrintCNodes() {
	top := parser.BuildParseTree()
	DumpTreeNode(top, 0)
}

func DumpTreeNode(parseNode *ParseTreeNode, depth int) {
	if depth > 0 {
		fmts := fmt.Sprintf("%%%ds", depth*2)
		fmt.Printf(fmts, " ")
	}
	if parseNode == nil {
		fmt.Println("<nil>")
		return
	}
	fmt.Println(parseNode.start, parseNode.end, parseNode.term)
	if parseNode.children != nil {
		for _, n := range parseNode.children {
			DumpTreeNode(n, depth+1)
		}
	}
}

func dumpTree(parseNode *SppfNode, depth int) {
	if depth > 0 {
		fmts := fmt.Sprintf("%%%ds", depth*2)
		fmt.Printf(fmts, " ")
	}
	if parseNode == nil {
		fmt.Println("<nil>")
		return
	}
	fmt.Println(parseNode.start, parseNode.end, parseNode.rule)
	if parseNode.left != nil {
		dumpTree(parseNode.left, depth+1)
	}
	if parseNode.right != nil {
		dumpTree(parseNode.right, depth+1)
	}
}

// initialize the parser
func (parser *MarpaParser) initial() {
	parser.addEIM(0, 0, 0, nil)
	nodes := make(map[string]*SppfNode)
	parser.reduce_pass(0, nodes)
	return
}

func (parser *MarpaParser) scan_pass(location int, token Token, nodes map[string]*SppfNode) {
	if location == 0 {
		return
	}
	s := Symbol(token.AsValue())
	v := &SppfNode{location - 1, location, s, nil, nil}
	//record the symbol itself in the completions list
	//helps build a parse tree later
	parser.recordCompletion(location-1, location, s)

	// lookup by symbol
	set := parser.table[location-1].transitions[s]
	for _, item := range set {
		toAH := parser.machine.Goto(item.state, s)
		if toAH > -1 {
			h := item.parent
			w := item.parseNode
			lbl := fmt.Sprintf("%v-%v-%v", h, s, location)
			y := parser.MakeParseNode(SppfTerm(lbl), h, location, w, v, nodes)
			//fmt.Println("SCAN", y)
			//fmt.Println("    ", w)
			//fmt.Println("    ", v)
			parser.addEIM(location, toAH, item.parent, y)
		}
	}

	//lookup by token type
	t := TypedTerm(token.TokenType())
	set = parser.table[location-1].transitions[t]
	for _, item := range set {
		toAH := parser.machine.Goto(item.state, t)
		if toAH > -1 {
			h := item.parent
			w := item.parseNode
			y := parser.MakeParseNode(s, h, location, w, v, nodes)
			parser.addEIM(location, toAH, item.parent, y)
		}
	}

	return
}

// for now simply record a rule completion
// in future we should be building a tree
func (parser *MarpaParser) recordCompletion(start, end int, term Term) {
	c := AhfaCompletion{start, end, term}
	parser.cnodes = append(parser.cnodes, c)
}

func (parser *MarpaParser) reduce_pass(location int, nodes map[string]*SppfNode) {
	eset := parser.table[location]
	//for each EIM in location table
	for j := 0; j < len(eset.items); j++ {
		item := eset.items[j]
		for _, rule := range parser.machine.Completed(item.state) {
			parser.reduceOneLHS(location, item.parent, rule, item, nodes)
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
				leo := EarleyItem{items[0].state, items[0].parent, postdot, nil}
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

func (parser *MarpaParser) reduceOneLHS(location int, origin int, term Term, trigger EarleyItem, nodes map[string]*SppfNode) {
	//get all the postDOTs in this location
	// is Eh in SPPF terms!
	set := parser.table[origin]
	postDOTs := set.transitions[term]

	// term is a COMPLETED rule
	// recognize a right recursive rule
	/*r := term.(*Rule)
	if r != nil && r.IsRightRecursive() {
		fmt.Println(r)
	}*/

	//fmt.Println("COMPLETE", term, "STARTS", origin, "ENDS", location)
	if origin != location {
		parser.recordCompletion(origin, location, term)
	}

	// loop through the postdots from the original location
	for _, item := range postDOTs {
		if item.symbol != nil {
			//fmt.Println("Leo reduction for", term, origin, location)
			parser.leoReduce(location, item)
		} else {
			parser.earleyReduce(location, item, term, trigger, nodes)
		}
	}

	for _, item := range set.items {
		if !inSlice(item, postDOTs) {
			parser.earleyReduce(location, item, term, trigger, nodes)
		}
	}

}

// must be some slice utils somewhere
func inSlice(item EarleyItem, set []EarleyItem) bool {
	for _, i := range set {
		if i == item {
			return true
		}
	}
	return false
}

//perform a leo reduction per marpa paper
func (parser *MarpaParser) leoReduce(location int, item EarleyItem) {
	toAH := parser.machine.Goto(item.state, item.symbol)
	if toAH > -1 {
		//fmt.Println("Leo reduction to", item.parent, location)
		parser.addEIM(location, toAH, item.parent, nil)
	}
}

//perform an earley reduction per Marpa paper
func (parser *MarpaParser) earleyReduce(location int, item EarleyItem, term Term, trigger EarleyItem, nodes map[string]*SppfNode) {
	toAH := parser.machine.Goto(item.state, term)
	if toAH > -1 {
		k := item.parent
		z := item.parseNode
		w := trigger.parseNode
		y := parser.MakeParseNode(term, k, location, z, w, nodes)
		/*fmt.Println("REDUCE", y)
		if y != w {
			fmt.Println("w     ", w)
		}
		if y != z {
			fmt.Println("z     ", z)
		}*/
		parser.addEIM(location, toAH, item.parent, y)
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
