package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"
	"unicode/utf8"
)

// Node
type Node struct {
	Parent   *Node
	Children []*Node
	Heading  *string
	Body     []string
}

func (n *Node) HasHeading() bool {
	return n.Heading != nil
}

func (n *Node) IsRoot() bool {
	return n.Parent == nil
}

func (n *Node) HasBody() bool {
	return n.Parent == nil
}

// IsEmpty returns true if this is a filler node with no own content (no heading and no body)
func (n *Node) HasContent() bool {
	return n.Parent != nil || len(n.Body) != 0
}

func (n *Node) AddChildren(children ...*Node) {
	if n == nil {
		log.Fatal("cannot add chiltren to nil Node")
	}
	for _, c := range children {
		if c.Parent != nil {
			log.Fatalln("parent already set, reattach not supported")
		}
		c.Parent = n
	}
	n.Children = append(n.Children, children...)
}

func (n *Node) AppendBody(lines ...string) {
	n.Body = append(n.Body, lines...)
}

func String(s string) *string {
	return &s
}

func NewNode() *Node {
	return &Node{
		Body:     make([]string, 0),
		Children: make([]*Node, 0),
	}

}

func (n *Node) Depth() int {
	if n == nil {
		return 0
	}
	var d int
	node := n
	for node.Parent != nil {
		d++
		node = node.Parent
	}
	return d
}

func (n *Node) Distance(child *Node) int {
	return n.distance(child, 0)
}

func (n *Node) distance(descendant *Node, depth int) int {
	if n == descendant {
		return depth
	}
	for _, c := range n.Children {
		v := c.distance(descendant, depth+1)
		if v != -1 {
			return v
		}
	}
	return -1
}

func (n *Node) Write(w io.Writer) error {
	var err error
	n.Walk(func(node, parent *Node) bool {
		depth := n.Distance(node)
		headFmt := strings.Repeat(";", 2+depth)
		if node.Heading != nil {
			fmt.Fprintf(w, "%s %s\n", headFmt, *node.Heading)
		}
		for _, l := range node.Body {
			fmt.Fprintln(w, l)
		}
		return true
	})
	return err
}

func (n *Node) Walk(callback func(node, parent *Node) bool) {
	nodes := []*Node{n}
	for len(nodes) > 0 {
		node := nodes[len(nodes)-1]
		nodes = nodes[:len(nodes)-1]
		if !callback(node, node.Parent) {
			continue
		}
		for index := len(node.Children) - 1; index >= 0; index-- {
			nodes = append(nodes, node.Children[index])
		}
	}
}

func (n *Node) FindNodePath(headings ...string) *Node {
	node := n
	for _, fh := range headings {
		if node == nil {
			return node
		}
		node = node.ChildByHeading(fh)
	}
	return node
}

func (n *Node) ChildByHeading(heading string) *Node {
	for _, c := range n.Children {
		if c.HasHeading() {
			if *c.Heading == heading {
				return c
			}
		}
	}
	return nil
}

func Parse(r io.Reader) (*Node, error) {
	rootNode := NewNode()
	currentNode := rootNode
	var currentDepth int

	scanner := bufio.NewScanner(r)
scan:
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, ";;;") {
			newDepth := (utf8.RuneCountInString(line) - utf8.RuneCountInString(strings.TrimLeft(line, ";"))) - 2
			heading := strings.TrimSpace(strings.TrimLeft(line, ";"))

			// create bridge nodes
			for newDepth > currentDepth+1 {
				currentDepth++
				newNode := NewNode()
				currentNode.AddChildren(newNode)
				currentNode = newNode
			}

			// new intermediate emtpy child nodes
			if newDepth > currentDepth {
				newNode := NewNode()
				newNode.Heading = &heading
				currentNode.AddChildren(newNode)
				currentNode = newNode
				currentDepth = newDepth
				continue scan
			}

			// step down to right node
			for newDepth < currentDepth {
				currentNode = currentNode.Parent
				currentDepth--
			}

			if newDepth == currentDepth {
				newNode := NewNode()
				newNode.Heading = &heading
				currentNode.Parent.AddChildren(newNode)
				currentNode = newNode
				continue scan
			}
			panic("illegal state")
		}
		currentNode.AppendBody(line)
	}
	return rootNode, nil
}
