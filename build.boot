(set-env! :dependencies
          '[[adzerk/bootlaces "0.1.13" :scope "test"]
            [adzerk/boot-test "1.2.0" :scope "test"]
            [camel-snake-kebab "0.4.0"]
            [cheshire "5.7.0" :exclusions [org.clojure/clojure]]
            [degree9/boot-semgit "0.2.1"
             :exclusions [cheshire
                          clj-time
                          degree9/boot-semver
                          org.clojure/clojure
                          org.tcrawley/dynapath]
             :scope "test"]
            [degree9/boot-semver "1.4.4"
             :exclusions [org.clojure/clojure]
             :scope      "test"]
            [http-kit "2.3.0-alpha1"]
            [inflections "0.13.0" :exclusions [org.clojure/clojure]]
            [org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
            [org.clojure/test.check "0.9.0"
             :exclusions [org.clojure/clojure]
             :scope      "test"]
            [slamhound "1.5.5" :exclusions [org.clojure/clojure], :scope "test"]
            [tolitius/boot-check "0.1.4"
             :exclusions [org.tcrawley/dynapath]
             :scope      "test"]]

          :source-paths
          #{"src" "test"})

(require '[adzerk.bootlaces :refer :all]
         '[adzerk.boot-test :refer :all]
         '[degree9.boot-semver :refer :all]
         '[slam.hound :as slamhound]
         '[tolitius.boot-check :as check])

(task-options! pom  {:project 'plaid-clj
                     :scm     {:url "https://github.com/PennyProfit/plaid-clj"}
                     :version (get-version)}
               push {:repo "deploy-clojars"})

(deftask deploy
  [t type        TYPE kw   "type of release (:major, :minor, or :patch)"
   r release          bool "whether the pushed artifact is a release"]
  (assert (#{:major :minor :patch} type))
  (comp #_(test)
        (apply version (cond-> (case type
                                 :major [:major 'inc, :minor 'zero, :patch 'zero]
                                 :minor [:minor 'inc, :patch 'zero]
                                 :patch [:patch 'inc])
                         (not release) (conj :pre-release 'snapshot
                                             :develop     true)))
        (build-jar)
        (if release (push-release) (push-snapshot))))
