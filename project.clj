(defproject me.arrdem/commonmark-hiccup "_"
  :description "Library to render CommonMark markdown to customizable HTML."
  :url "https://github.com/bitterblue/commonmark-hiccup"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.atlassian.commonmark/commonmark "0.8.0"]
                 [hiccup "1.0.5"]]

  :plugins [[me.arrdem/lein-git-version "LATEST"]]
  :git-version {:status-to-version
                (fn [{:keys [tag version ahead ahead? dirty?] :as git}]
                  (if (and tag (not ahead?) (not dirty?))
                    tag
                    (str tag
                         (when ahead? (str "." ahead))
                         (when dirty? "-SNAPSHOT"))))})
