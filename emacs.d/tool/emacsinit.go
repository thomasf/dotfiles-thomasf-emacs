package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"slices"
	"sort"
	"strings"
)

// initFile is one of the emacs init files the tool keeps tidy.
type initFile struct {
	path string
	// sorted lists the heading paths whose child sections are kept in
	// alphabetical order.
	sorted [][]string
}

var initFiles = []initFile{
	{path: "../init.el"},
	{path: "../lisp/init/init-lib.el"},
	{path: "../lisp/init/init-core.el", sorted: [][]string{
		{"core", "defined in elisp source"},
		{"core", "defined in c source"},
	}},
	{path: "../lisp/init/init-ui.el"},
	{path: "../lisp/init/init-commands.el"},
	{path: "../lisp/init/init-security.el", sorted: [][]string{{"use-package"}}},
	{path: "../lisp/init/init-packages.el", sorted: [][]string{{"use-package"}}},
	{path: "../lisp/init/init-org.el", sorted: [][]string{{"use-package"}}},
}

// EmacsInit .
type EmacsInit struct {
	file initFile
	root *Node
	data []byte
}

func loadEmacsInit(file initFile) (*EmacsInit, error) {
	data, err := ioutil.ReadFile(file.path)
	if err != nil {
		return nil, err
	}
	bf := bytes.NewReader(data)
	root, err := Parse(bf)
	if err != nil {
		return nil, err
	}
	return &EmacsInit{
		file: file,
		root: root,
		data: data,
	}, nil
}

func (e *EmacsInit) Save() error {
	out := e.Render()
	if err := ioutil.WriteFile(e.file.path, []byte(out), 0775); err != nil {
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
				for _, v := range slices.Backward(node.Body) {
					if strings.TrimSpace(v) != "" {
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
	for _, headingpath := range e.file.sorted {
		node := e.root.FindNodePath(headingpath...)
		if node == nil {
			return fmt.Errorf("%s: could not find: %v", e.file.path, strings.Join(headingpath, " / "))
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
