package kowhai

import "strings"

type ParseTreeOptimizer interface {
	Preprocess(node *ParseTreeNode) *ParseTreeNode
	Postprocess(node *ParseTreeNode) *ParseTreeNode
}

// This optimization can be used to remove intermediate nodes that have
// only a single child.
type TrimWhenSingle struct {
	AppliesTo []string
}

func (o TrimWhenSingle) Preprocess(node *ParseTreeNode) *ParseTreeNode {
	return node
}

func (o TrimWhenSingle) Postprocess(node *ParseTreeNode) *ParseTreeNode {
	if len(node.Children) != 1 {
		return node
	}
	if node.Term.IsRule() {
		r := node.Term.(*Rule)
		if r == nil {
			return node
		}
		for _, a := range o.AppliesTo {
			if a == r.String() {
				node.Children[0].Parent = node.Parent
				return node.Children[0]
			}
		}
	}

	return node
}

// The optimization removes parse nodes thar derive from rules
// that were created by the grammar operators (+, ?, *, |)
type RemoveSyntheticNodes struct {
}

func (o RemoveSyntheticNodes) Preprocess(node *ParseTreeNode) *ParseTreeNode {
	return node
}

func (o RemoveSyntheticNodes) Postprocess(node *ParseTreeNode) *ParseTreeNode {
	if len(node.Children) == 0 {
		return node
	}

	node.Children = o.GetNaturalChildren(node)

	return node
}

func (o RemoveSyntheticNodes) GetNaturalChildren(node *ParseTreeNode) (children []*ParseTreeNode) {
	for _, c := range node.Children {
		if c.Term.IsRule() {
			r := c.Term.(*Rule)
			if r == nil {
				children = append(children, c)
				continue
			}
			if strings.HasPrefix(r.name, "OR_") || strings.HasPrefix(r.name, "STAR_") || strings.HasPrefix(r.name, "OPT_") || strings.HasPrefix(r.name, "PLUS_") {
				for _, rp := range c.Children {
					rp.Parent = node
				}
				children = append(children, o.GetNaturalChildren(c)...)
				continue
			}
		}
		children = append(children, c)
	}
	return
}

// This optimization handles child nodes that overlap (due to partial parse trees)
// Generally applying this optimization makes the resulting parse tree easier to understand
type OverlappingChildren struct {
}

func (o OverlappingChildren) Preprocess(node *ParseTreeNode) *ParseTreeNode {
	if len(node.Children) < 2 {
		return node
	}

	curr := node.Children[0]
	for _, c := range node.Children[1:] {
		if c.Overlaps(curr) {
			for _, rp := range curr.Children {
				rp.Parent = c
			}
			c.Children = append(curr.Children, c.Children...)
			curr.Children = nil
		}
		curr = c
	}
	return node
}

func (o OverlappingChildren) Postprocess(node *ParseTreeNode) *ParseTreeNode {
	if node.Term.IsRule() && node.Children == nil {
		return nil
	}
	return node
}
