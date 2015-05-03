package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
)

var (
	allTasks []func()
	home     string
)

func init() {
	allTasks = []func(){all, emacs, usage}
	usr, err := user.Current()
	fail(err)
	home = usr.HomeDir
}

// usage task
func usage() {
	fmt.Println("Tasks:")
	fmt.Print(" ")
	for _, task := range allTasks {
		name := strings.TrimPrefix(runtime.FuncForPC(reflect.ValueOf(task).Pointer()).Name(), "main.")
		fmt.Print(" ", name)
	}
	fmt.Println("")
	fmt.Println("Switches:")
	flag.PrintDefaults()
}

// emacs task
func emacs() {
	fail(os.MkdirAll(filepath.Join(home, ".opt"), 0775))
	src := filepath.Join(home, ".opt", "emacs")
	info, err := os.Stat(src)
	fail(err)
	if info.IsDir() {
		cmdChain(
			newCmd("git", "pull", "origin", "emacs-24").dir(src),
			newCmd("make").dir(src),
			newCmd("make", "info").dir(src))
	} else {
		cmdChain(
			newCmd("git", "clone",
				"-b", "emacs-24",
				"git://git.sv.gnu.org/emacs.git", "emacs").dir(src),
			newCmd("./autogen.sh").dir(src),
			newCmd("./configure",
				"--with-x-toolkit=lucid",
				"--with-file-notification=inotify",
				"--without-pop",
				"--enable-link-time-optimization",
				"--without-toolkit-scroll-bars").dir(src),
			newCmd("make bootstrap").dir(src),
			newCmd("make info").dir(src),
		)
	}
}

func main() {
	defer func() {
		r := recover()
		if r != nil {
			fmt.Println("Build paniced:", r)
		}
		// do cleanup
		if r != nil {
			panic(r)
			os.Exit(1)
		}
		os.Exit(0)
	}()
	use := "usage"
	if len(os.Args) > 1 {
		if !strings.HasPrefix(os.Args[1], "-") {
			use = os.Args[1]
			var args []string
			for k, v := range os.Args {
				if k == 1 {
					continue
				}
				args = append(args, v)
			}
			os.Args = args
		}
	}
	t, err := task(use)
	if err != nil {
		log.Printf("Task %s not registered", use)
		os.Exit(1)
	}
	t()
	flag.Parse()
}

// Cmd is a wrapper around exec.Cmd
type Cmd struct {
	c       *exec.Cmd // command
	newEnv  []string  // env
	newArgs []string  // args
	workdir string
}

func (c *Cmd) env(entry ...string) *Cmd {
	c.newEnv = append(c.newEnv, entry...)
	return c
}

func (c *Cmd) arg(arg ...string) *Cmd {
	c.newArgs = append(c.newArgs, arg...)
	return c
}

func (c *Cmd) dir(dir string) *Cmd {
	c.workdir = dir
	return c
}

func (c *Cmd) run() error {
	newEnv := os.Environ()
	newEnv = append(newEnv, c.newEnv...)
	cmd := exec.Command(c.newArgs[0], c.newArgs[1:]...)
	cmd.Stdout = os.Stdout
	cmd.Stdin = os.Stdin
	if c.workdir != "" {
		cmd.Dir = c.workdir
	}
	log.Println(cmd)
	return cmd.Run()
}

func newCmd(arg ...string) *Cmd {
	c := &Cmd{
		newArgs: arg,
	}
	return c
}

func task(name string) (func(), error) {
	log.Println("entering task with name: ", name)
	for _, task := range allTasks {
		tname := strings.TrimPrefix(runtime.FuncForPC(reflect.ValueOf(task).Pointer()).Name(), "main.")
		if strings.HasSuffix(tname, name) {
			return task, nil
		}
	}
	return nil, errors.New("No")
}

func run(task func()) {
	name := strings.TrimPrefix(runtime.FuncForPC(reflect.ValueOf(task).Pointer()).Name(), "main.")
	log.Printf(">>> [%s]", name)
	defer func() {
		if r := recover(); r != nil {
			panic(fmt.Sprintf("!!! task %s paniced: %+v", name, r))
		}
		log.Printf("<<< [%s]", name)
	}()
	task()
}

func all() {
	run(emacs)
}

func fail(err error) {
	if err != nil {
		panic(err)
	}
}

// maybe...
func cmdChain(cmds ...*Cmd) {
	for _, cmd := range cmds {
		fail(cmd.run())
	}
}

// get_emacs() {
//     set -e
//     mkdir -p ~/.opt
//     cd ~/.opt
//     if [ -d emacs ]; then
//       cd emacs
//       git pull origin emacs-24
//       make
//       make info
//     else
//       git clone -b emacs-24 git://git.sv.gnu.org/emacs.git emacs
//       cd emacs
//       ./autogen.sh
//       ./configure \
//         --with-x-toolkit=gtk3 \
//         --with-file-notification=inotify \
//         --without-pop \
//         --enable-link-time-optimization \
//         --without-toolkit-scroll-bars
//       make bootstrap
//       make info
//     fi
// }

// # Clone or update a git repository
// getGit() {
//     repo="${REPOS}/${name}"
//     if [ -d "$repo" ]; then
//         (
//             cd ${repo}
//             git pull
//         )
//     else
//         git clone "$url" "${repo}"
//     fi
//     cd ${repo}
// }

func logEnterTask(name string) {
	log.Printf(">>> [%s]", name)
}
