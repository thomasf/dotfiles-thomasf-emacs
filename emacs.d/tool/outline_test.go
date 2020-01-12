package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strings"
	"testing"

	"github.com/davecgh/go-spew/spew"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

func TestWalk(t *testing.T) {
	root := &Node{Heading: String("root"), Body: []string{"root body"}}
	n10 := &Node{Heading: String("1.0")}
	n11 := &Node{Heading: String("1.1")}
	n12 := &Node{Heading: String("1.2"), Body: []string{"one", "two"}}
	n122 := &Node{Heading: String("1.2.2")}
	n20 := &Node{Heading: String("2.0"), Body: []string{"end"}}
	root.AddChildren(n10)
	root.AddChildren(n20)
	n10.AddChildren(n11)
	n10.AddChildren(n12)
	n12.AddChildren(n122)

	var count int
	var headlines []string
	root.Walk(func(node, parent *Node) bool {
		count++
		headlines = append(headlines, *node.Heading)
		return true
	})

	t.Run("count", func(t *testing.T) {
		if count != 6 {
			t.Fail()
			t.Logf("expected 6, got %v", count)
		}
	})

	t.Run("order", func(t *testing.T) {
		expected := []string{"root", "1.0", "1.1", "1.2", "1.2.2", "2.0"}
		if diff := cmp.Diff(headlines, expected); diff != "" {
			t.Fail()
			t.Logf("mismatch (-want +got):\n%s", diff)
		}
	})

}

func TestParent(t *testing.T) {
	root := &Node{Heading: String("root"), Body: []string{"root body"}}
	n10 := &Node{Heading: String("1.0")}
	n11 := &Node{Heading: String("1.1")}
	n12 := &Node{Heading: String("1.2"), Body: []string{"one", "two"}}
	n122 := &Node{Heading: String("1.2.2")}
	n20 := &Node{Heading: String("2.0"), Body: []string{"end"}}
	root.AddChildren(n10)
	root.AddChildren(n20)
	n10.AddChildren(n11)
	n10.AddChildren(n12)
	n12.AddChildren(n122)

	nc := &Node{Heading: String("not connected")}

	// direct parents
	for n, v := range []struct {
		n1     *Node
		n2     *Node
		isSame bool
	}{
		{root, nc, false},
		{nc, root, false},
		{root, root.Parent, false},
		{root, n10.Parent, true},
		{root, n10, false},
		{n10, root, false},
		{root, n122.Parent.Parent.Parent, true},
		{root, n122.Parent.Parent, false},
		{n10, n122.Parent.Parent, true},
	} {
		t.Run(fmt.Sprintf("%v", n), func(t *testing.T) {
			if (v.isSame && v.n1 != v.n2) || (!v.isSame && v.n1 == v.n2) {
				t.Fail()
				t.Logf("test %v failed: expected issame: %v,  '%s'  '%s'", n,
					v.isSame, *v.n1.Heading, *v.n2.Heading)
			}
		})
	}
}

func TestWriteTree(t *testing.T) {
	root := &Node{Body: []string{"root body"}}
	n10 := &Node{Heading: String("1.0")}
	n11 := &Node{Heading: String("1.1")}
	n12 := &Node{Heading: String("1.2"), Body: []string{"one", "two"}}
	n122 := &Node{Heading: String("1.2.2")}
	n20 := &Node{Heading: String("2.0"), Body: []string{"end"}}
	root.AddChildren(n10)
	root.AddChildren(n20)
	n10.AddChildren(n11)
	n10.AddChildren(n12)
	n12.AddChildren(n122)
	var buf bytes.Buffer
	w := bufio.NewWriter(&buf)
	root.Write(w)
	w.Flush()
	res := buf.String()

	expected := `root body
;;; 1.0
;;;; 1.1
;;;; 1.2
one
two
;;;;; 1.2.2
;;; 2.0
end
`
	if res != expected {
		t.Fatalf("not equal:\n--\n%s\n--\n%s\n--", res, expected)
	}

}

func TestDistance(t *testing.T) {
	root := &Node{}
	n10 := &Node{Heading: String("1.0")}
	n11 := &Node{Heading: String("1.1")}
	n12 := &Node{Heading: String("1.2")}
	n122 := &Node{Heading: String("1.2.2")}
	n20 := &Node{Heading: String("2.4")}
	root.AddChildren(n10)
	root.AddChildren(n20)
	n10.AddChildren(n11)
	n10.AddChildren(n12)
	n12.AddChildren(n122)
	// log.Println("ch", len(root.Children), root, root.Children[0].Parent, root.Children[1].Parent)

	nc := &Node{Heading: String("not connected")}
	root.Distance(n12)

	for n, v := range []struct {
		n1 *Node
		n2 *Node
		d  int
	}{
		{root, nc, -1},
		{nc, root, -1},
		{root, root, 0},
		{root, n10, 1},
		{n10, root, -1},
		{root, n122, 3},
		{n10, n122, 2},
	} {
		t.Run(fmt.Sprintf("%v", n), func(t *testing.T) {
			depth := v.n1.Distance(v.n2)
			if depth != v.d {
				t.Fail()
				t.Logf("test %v failed: got %v, expected %v", n, depth, v.d)
			}
		})
	}
}

func TestParseSimple(t *testing.T) {

	emptyInput := ``
	emptyOutput := &Node{}

	simpleInput := strings.TrimSpace(`
root
;;; 1
one
`)
	var simpleOutput *Node
	{
		root := &Node{
			Body: []string{"root"},
		}

		n1 := &Node{
			Heading: String("1"),
			Body:    []string{"one"},
			Parent:  root,
		}

		root.Children = []*Node{n1}
		simpleOutput = root

	}

	sequenceInput := strings.TrimSpace(`
;;; 1
;;; 2
;;; 3
`)
	var sequenceOutput *Node
	{
		root := &Node{}
		n1 := &Node{
			Heading: String("1"),
			Parent:  root,
		}
		n2 := &Node{
			Heading: String("2"),
			Parent:  root,
		}
		n3 := &Node{
			Heading: String("3"),
			Parent:  root,
		}
		root.Children = []*Node{n1, n2, n3}
		sequenceOutput = root
	}

	for n, v := range []struct {
		input string
		want  *Node
	}{
		{emptyInput, emptyOutput},
		{simpleInput, simpleOutput},
		{sequenceInput, sequenceOutput},
	} {
		t.Run(fmt.Sprintf("%v", n), func(t *testing.T) {
			bf := bytes.NewBuffer([]byte(v.input))
			output, err := Parse(bf)
			if err != nil {
				t.Logf("test %v failed", n)
				t.Fail()
				t.Logf("test %v failed: error: %v", n, err)
				output.Write(os.Stderr)
				return
			}
			if diff := cmp.Diff(output, v.want, cmpopts.EquateEmpty()); diff != "" {
				t.Logf("test %v failed", n)
				t.Fail()
				t.Logf("mismatch (-want +got):\n%s", diff)
				log.Print("\n\nOUT\n\n")
				spew.Dump(output)
				log.Print("\n\nWANT\n\n")
				spew.Dump(v.want)
				output.Write(os.Stdout)
			}
		})
	}
}

func TestDistanceSkip(t *testing.T) {
	input := strings.TrimSpace(`
;;;; skip
`)
	var want *Node
	{
		root := &Node{}
		skip := &Node{Heading: String("skip")}
		s1 := &Node{Children: []*Node{skip}}
		skip.Parent = s1
		s1.Parent = root
		root.Children = []*Node{s1}
		want = root
	}

	bf := bytes.NewBuffer([]byte(input))
	output, err := Parse(bf)
	if err != nil {
		t.Logf("error: %v", err)
		t.Fail()
		output.Write(os.Stderr)
		return
	}

	if diff := cmp.Diff(output, want, cmpopts.EquateEmpty()); diff != "" {
		t.Fail()
		t.Logf("mismatch (-want +got):\n%s", diff)
		output.Write(os.Stdout)
	}
}

func TestDistanceSkip2(t *testing.T) {
	input := strings.TrimSpace(`
000
;;; a
aaa
;;;;;; b
bb
;;;; c
ccC
;;
`)

	var want *Node
	{
		root := &Node{Body: []string{"000"}}
		a := &Node{Heading: String("a"), Body: []string{"aaa"}}
		b := &Node{Heading: String("b"), Body: []string{"bb"}}
		c := &Node{Heading: String("c"), Body: []string{"ccC", ";;"}}
		ab1 := &Node{}
		ab2 := &Node{}
		root.AddChildren(a)
		a.AddChildren(ab1)
		ab1.AddChildren(ab2)
		ab2.AddChildren(b)
		a.AddChildren(c)
		want = root
	}

	bf := bytes.NewBuffer([]byte(input))
	output, err := Parse(bf)
	if err != nil {
		t.Logf("error: %v", err)
		t.Fail()
		output.Write(os.Stderr)
		return
	}
	if diff := cmp.Diff(output, want, cmpopts.EquateEmpty()); diff != "" {
		t.Fail()
		t.Logf("mismatch (-want +got):\n%s", diff)
		output.Write(os.Stdout)
	}
}

func count(n *Node) int {
	var c int
	n.Walk(func(node *Node, parent *Node) bool {
		c++
		return true
	})
	return c
}

func assertTextEqual(t *testing.T, got, want string) {
	t.Helper()
	if !cmp.Equal(got, want) {
		t.Fail()
		diffs := NewDiffs(got, want)
		t.Log(DiffPrettyText(diffs))
	}
}
