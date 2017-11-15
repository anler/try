(defproject anler/try "0.3.0"
  :description "Handle exceptions as values in Clojure."
  :url "https://github.com/anler/try"
  :license {:name "BSD (2-Clause)"
            :url  "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]]
  :source-paths ["src/clj" "src/clc" "src/cljs"]
  :test-paths ["test/clj" "test/clc" "test/cljs"]
  :min-lein-version "2.0.0"
  :plugins [[funcool/codeina "0.4.0"
             :exclusions [org.clojure/clojure]]]
  :codeina {:sources ["src/clj"]
            :reader :clojure})
