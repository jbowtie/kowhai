package kowhai

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
				return node.Children[0]
			}
		}
	}

	return node
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
