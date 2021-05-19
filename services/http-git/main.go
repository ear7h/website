package main

import (
	"path"
	"errors"
	"fmt"
	"flag"
	"os"
	"net/http"
	"encoding/json"
	"io"
	"log"
	"io/fs"

	"github.com/go-git/go-git/v5"
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	gitfs "github.com/ear7h/go-git-fs"
)

type UserConfig struct {
	Name     string `tag:"name"`
	RepoPath string `tag:"repoPath"`
}

var (
	flagConfig = flag.String("config", "config.json", "path to the config file")
)

func exitError(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

const (
	UserNameToken = RouteToken("user_name")
	RepoToken     = RouteToken("repo")
	WildcardToken = RouteToken("*")
)

type RouteToken string

func (token RouteToken) ToPath() string {
	return "{" + string(token) + "}"
}

func (token RouteToken) GetString(r *http.Request) string {
	return chi.URLParam(r, string(token))
}

type ApiProvider struct {
	repos map[[2]string]*git.Repository
}

func (srv *ApiProvider) GetUser(w http.ResponseWriter, r *http.Request) {
	userName := UserNameToken.GetString(r)

	w.Header().Set("Content-Type", "text/plain")
	for k := range srv.repos {
		if k[0] == userName {
			fmt.Fprintln(w, k[1])
		}
	}
}

func (srv *ApiProvider) GetUserRepoFile(w http.ResponseWriter, r *http.Request) {
	userName := UserNameToken.GetString(r)
	repoName := RepoToken.GetString(r)
	p := WildcardToken.GetString(r)


	repo := srv.repos[[2]string{userName, repoName}]
	if repo == nil {
		http.Error(w, "404 page not found", http.StatusNotFound)
		return
	}

	repoFs, err := gitfs.NewFS(repo, "HEAD")
	if err != nil {
		http.Error(w, "could not open repo", http.StatusInternalServerError)
		return
	}

	log.Println(p)
	log.Println(path.Clean(p))

	file, err := repoFs.Open(path.Clean(p))
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			http.Error(w, "404 page not found", http.StatusNotFound)
			return
		}
		http.Error(w, "could not open file", http.StatusNotFound)
		return
	}
	defer file.Close()

	fi, err := file.Stat()
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			http.Error(w, "404 page not found", http.StatusNotFound)
			return
		}
		http.Error(w, "could not stat file", http.StatusNotFound)
		return
	}

	if fi.IsDir() {
		dirs, ok := file.(fs.ReadDirFile)
		if !ok {
			http.Error(w, "bad directory", http.StatusInternalServerError)
			return
		}

		de, err := dirs.ReadDir(-1)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				http.Error(w, "404 page not found", http.StatusNotFound)
				return
			}
			http.Error(w, "could not stat file", http.StatusNotFound)
			return
		}

		w.Header().Set("Content-Type", "text/plain")
		for _, v := range de {
			fmt.Fprintln(w, v.Name())
		}
		return
	}

	w.Header().Set("Content-Type", "text/plain")
	_, err = io.Copy(w, file)
	log.Println(err)
}

func main() {
	flag.Parse()

	configFile, err := os.Open(*flagConfig)
	if err != nil {
		exitError(err)
	}

	var config []UserConfig
	err = json.NewDecoder(configFile).Decode(&config)
	if err != nil {
		exitError(err)
	}
	err = configFile.Close()
	if err != nil {
		exitError(err)
	}

	repos := make(map[[2]string]*git.Repository)
	for _, v := range config {
		_, ok := repos[[2]string{"julio", v.Name}]
		if ok {
			exitError(errors.New("duplicate repository " + v.Name))
		}

		repos[[2]string{"julio", v.Name}], err = git.PlainOpen(v.RepoPath)
		if err != nil {
			exitError(err)
		}
	}

	srv := ApiProvider{repos}

	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get(
		"/~"+UserNameToken.ToPath()+"/"+RepoToken.ToPath(),
		srv.GetUserRepoFile)
	r.Get(
		"/~"+UserNameToken.ToPath()+"/"+RepoToken.ToPath()+"/",
		srv.GetUserRepoFile)
	r.Get(
		"/~"+UserNameToken.ToPath()+"/"+RepoToken.ToPath()+"/"+WildcardToken.ToPath(),
		srv.GetUserRepoFile)

	r.Get(
		"/~"+UserNameToken.ToPath(),
		srv.GetUser)
	r.Get(
		"/~"+UserNameToken.ToPath()+"/",
		srv.GetUser)

	http.ListenAndServe(":8080", r)

	fmt.Println("hello world")
}
