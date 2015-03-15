package kowhai

import "fmt"

//rule as fit for usage in the state machine
type AhfaRule struct {
	name     string     //name of the rule (LHS)
	prod     Production // RHS
	dotIndex int        // dot location
	orig     *Rule      // underlying rule definition
}

//convert to a term for convenience
func (r *AhfaRule) AsTerm() Term {
	return r.orig
}

func (rule *AhfaRule) String() string {
	return fmt.Sprint(rule.name, rule.prod, rule.dotIndex)
}

// get the next term from this rule
func (rule *AhfaRule) nextTerm() Term {
	if rule.IsCompleted() {
		return nil
	}
	return rule.prod[rule.dotIndex]
}

func (rule *AhfaRule) IsCompleted() bool {
	return rule.dotIndex >= len(rule.prod)
}

// this should probably be flagged by the grammar
// for now we only handle most trivial case
//see AH paper for details on rules that can derive null
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

//creates a map of all possible transitions to a new state
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

// used during state machine construction
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

// calculate the closure
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

// split a state into kernel and nonkernel states
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

// gives us a way to dump and inspect the machine
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
