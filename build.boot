(set-env! :dependencies
          '[[adzerk/bootlaces "0.1.13" :scope "test"]
            [adzerk/boot-test "1.2.0" :scope "test"]
            [camel-snake-kebab "0.4.0"]
            [cheshire "5.7.0" :exclusions [org.clojure/clojure]]
            [com.fzakaria/slf4j-timbre "0.3.4" :scope "test"]
            [com.taoensso/timbre "4.8.0" :scope "test"]
            [http-kit "2.3.0-alpha1"]
            [inflections "0.13.0" :exclusions [org.clojure/clojure]]
            [org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
            [org.clojure/test.check "0.9.0"
             :exclusions [org.clojure/clojure]
             :scope      "test"]
            [penny-profit/boot-flow "0.1.0-SNAPSHOT" :scope "test"]
            [robert/hooke "1.3.0" :scope "test"]
            [slamhound "1.5.5" :exclusions [org.clojure/clojure], :scope "test"]
            [tolitius/boot-check "0.1.4"
             :exclusions [org.tcrawley/dynapath]
             :scope      "test"]]

          :resource-paths
          #{"src"}

          :source-paths
          #{"test"})

(require '[adzerk.bootlaces :refer :all]
         '[adzerk.boot-test :refer :all]
         '[penny-profit.boot-flow :as flow]
         '[robert.hooke :refer [add-hook]]
         '[slam.hound :as slamhound]
         '[tolitius.boot-check :as check])

(task-options!
 pom  {:description "Clojure bindings for the Plaid API"
       :project     'plaid-clj
       :scm         {:url "https://github.com/PennyProfit/plaid-clj"}}
 push {:repo "deploy-clojars"})

(defn finish-check [handler _]
  (comp #_(test) handler))
(add-hook #'flow/finish-check #'finish-check)

(defn master-deploy [handler _]
  (comp (build-jar) (push-release) handler))
(add-hook #'flow/master-deploy #'master-deploy)

(defn snapshot-deploy [handler _]
  (comp (build-jar) (push-snapshot) handler))
(add-hook #'flow/snapshot-deploy #'snapshot-deploy)
