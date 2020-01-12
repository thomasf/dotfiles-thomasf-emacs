package main

import (
	"bufio"
	"bytes"
	"io/ioutil"
	"testing"
)

func TestInit(t *testing.T) {
	t.Parallel()
	emacs, err := loadEmacsInit()
	if err != nil {
		t.Fatal(err)
	}
	if err := emacs.Sort(); err != nil {
		t.Fatal(err)
	}

	if err := emacs.FixLineSpacing(); err != nil {
		t.Fatal(err)
	}

	emacs.Diff(ioutil.Discard)
}

func TestEmacsInitFile(t *testing.T) {
	t.Parallel()
	data, err := ioutil.ReadFile("../init.el")
	if err != nil {
		t.Fatal(err)

	}
	bf := bytes.NewReader(data)
	root, err := Parse(bf)
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	w := bufio.NewWriter(&buf)

	root.Write(w)
	w.Flush()
	out := buf.String()

	t.Run("diff equal", func(t *testing.T) {
		assertTextEqual(t, out, string(data))
	})

	t.Run("node depths", func(t *testing.T) {
		root.Walk(func(node *Node, parent *Node) bool {
			depth := node.Depth()
			distance := root.Distance(node)
			if depth != distance {
				t.Fail()
				t.Logf("distance and depths are not the same: %v, %v", depth, distance)
			}
			return true
		})
	})

	t.Run("find heading", func(t *testing.T) {
		nodepath := []string{"use-package", "lsp-mode", "lsp-ui"}
		n := root.FindNodePath(nodepath...)
		if n == nil {
			t.Fail()
			t.Logf("could not find node: %v", nodepath)
		}
	})

	t.Run("not find heading", func(t *testing.T) {
		nodepath := []string{"use-package packages: unsorted packages", "lsp-mode", "invalid"}
		n := root.FindNodePath(nodepath...)
		if n != nil {
			t.Fail()
			t.Logf("should not return result: %v", nodepath)
		}
	})

}
