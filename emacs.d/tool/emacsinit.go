package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"sort"
	"strings"
)

// EmacsInit .
type EmacsInit struct {
	root *Node
	data []byte
}

func loadEmacsInit() (*EmacsInit, error) {
	data, err := ioutil.ReadFile("../init.el")
	if err != nil {
		return nil, err
	}
	bf := bytes.NewReader(data)
	root, err := Parse(bf)
	if err != nil {
		return nil, err
	}
	return &EmacsInit{
		root: root,
		data: data,
	}, nil
}

func (e *EmacsInit) Save() error {
	out := e.Render()
	if err := ioutil.WriteFile("../init.el", []byte(out), 0775); err != nil {
		return err
	}
	return nil
}

func (e *EmacsInit) Render() string {
	var buf bytes.Buffer
	w := bufio.NewWriter(&buf)
	e.root.Write(w)
	w.Flush()
	out := buf.String()
	return out
}
func (e *EmacsInit) Diff(w io.Writer) error {
	out := e.Render()
	diffs := NewDiffs(string(e.data), out)
	fmt.Fprint(w, DiffPrettyText(diffs))
	return nil
}

func (e *EmacsInit) FixLineSpacing() error {
	log.Println("Adjusting line spacing")
	last := e.root.Children[len(e.root.Children)-1]
	e.root.Walk(func(node *Node, parent *Node) bool {
		if node == e.root || node == last {
			return true
		}
		const leadingBlankLines = 1
		const trailingBlankLines = 2
		if node.HasContent() {
			{
				var leadingEmpty int
				for i := 0; i < len(node.Body)-1; i++ {
					if strings.TrimSpace(node.Body[i]) != "" {
						break
					}
					leadingEmpty++
				}
				if leadingEmpty > leadingBlankLines {
					node.Body = node.Body[:len(node.Body)-leadingEmpty+leadingBlankLines]
				}

				if leadingEmpty < leadingBlankLines {
					for i := 0; i < leadingBlankLines-leadingEmpty; i++ {
						node.Body = append([]string{""}, node.Body...)
					}
				}
			}
			{
				var trailingEmpty int
				for i := len(node.Body) - 1; i >= 0; i-- {
					if strings.TrimSpace(node.Body[i]) != "" {
						break
					}
					trailingEmpty++
				}
				if trailingEmpty == len(node.Body) {
					node.Body = []string{""}
					return true
				}
				if trailingEmpty > trailingBlankLines {
					node.Body = node.Body[:len(node.Body)-trailingEmpty+trailingBlankLines]
				}
				if trailingEmpty < trailingBlankLines {
					for i := 0; i < trailingBlankLines-trailingEmpty; i++ {
						node.Body = append(node.Body, "")
					}
				}
			}
		}
		return true
	})
	return nil
}

func (e *EmacsInit) Sort() error {
	for _, headingpath := range [][]string{
		{"use-package"},
		{"core", "defined in elisp source"},
		{"core", "defined in c source"},
	} {
		node := e.root.FindNodePath(headingpath...)
		if node == nil {
			return fmt.Errorf("could not find: %v", strings.Join(headingpath, " / "))
		}
		log.Println("sorting", *node.Heading)
		sort.SliceStable(node.Children, func(i, j int) bool {
			if !node.Children[i].HasHeading() || !node.Children[j].HasHeading() {
				return false
			}
			return *node.Children[i].Heading < *node.Children[j].Heading
		})
	}
	return nil
}
