package main

import (
	"bufio"
	"bytes"
	"io/ioutil"
	"testing"
)

func TestInit(t *testing.T) {
	t.Parallel()
	for _, file := range initFiles {
		t.Run(file.path, func(t *testing.T) {
			emacs, err := loadEmacsInit(file)
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
		})
	}
}

func parseFile(t *testing.T, path string) (*Node, []byte) {
	t.Helper()
	data, err := ioutil.ReadFile(path)
	if err != nil {
		t.Fatal(err)

	}
	bf := bytes.NewReader(data)
	root, err := Parse(bf)
	if err != nil {
		t.Fatal(err)
	}
	return root, data
}

func TestEmacsInitFile(t *testing.T) {
	t.Parallel()
	for _, file := range initFiles {
		t.Run(file.path, func(t *testing.T) {
			root, data := parseFile(t, file.path)

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
		})
	}

	root, _ := parseFile(t, "../lisp/init/init-packages.el")

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
