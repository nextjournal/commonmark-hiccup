(ns commonmark-hiccup.core
  "Library for converting markdown to HTML.

  Uses [commonmark-java](https://github.com/atlassian/commonmark-java) for parsing and
  renders to [hiccup](https://github.com/weavejester/hiccup) data structures.

  The renderer itself is quite configurable."
  (:require [hiccup.core :as hiccup]
            [clojure.walk :as walk])
  (:import org.commonmark.parser.Parser
           [org.commonmark.node
            ,,Node
            ,,Document
            ,,Block
            ,,Heading
            ,,Paragraph
            ,,Text
            ,,BulletList
            ,,OrderedList
            ,,ListItem
            ,,ListBlock
            ,,BlockQuote
            ,,HtmlBlock
            ,,HtmlInline
            ,,FencedCodeBlock
            ,,IndentedCodeBlock
            ,,Code
            ,,Link
            ,,Image
            ,,Emphasis
            ,,StrongEmphasis
            ,,ThematicBreak
            ,,SoftLineBreak
            ,,HardLineBreak]))

(set! *warn-on-reflection* true)

(declare text-content)

(def default-config
  "An options map.

  The `:normalize` key may be a boolean indicating whether generated
  Hiccup structures should be normalized. Normalization flattens lists
  into tag nodes, and ensures the presence of an attributes map.

  The `:render :nodes` key is a map from `org.commonmark.node.Node`
  instance classes to specifications used to render that node class to
  Hiccup. See `#'render-node` for an exhaustive discussion of
  supported render specifications." 
  {;; Attempt normalization of hiccup structures?
   :normalize
   true

   ;; Map of nodes to either node -> hiccup functions, or specs comprised of keywords naming element
   ;; selectors, literal strings, maps from keys to specs or Hiccup tags with specs as bodies.
   :renderer
   {:nodes
    {Document          :content
     Heading           (fn [^Heading node]
                         [(keyword (str "h" (.getLevel node)))
                          (text-content node)])
     Paragraph         [:p :content]
     Text              :node-literal
     BulletList        [:ul :content]
     OrderedList       [:ol {:start :node-startNumber} :content]
     ListItem          [:li :content]
     BlockQuote        [:blockquote :content]
     HtmlBlock         :node-literal
     HtmlInline        :node-literal
     FencedCodeBlock   [:pre [:code {:class :node-info} :node-literal]]
     IndentedCodeBlock [:pre [:code {} :node-literal]]
     Code              [:code :node-literal]
     Link              [:a {:href :node-destination} :content]
     Image             [:img {:src   :node-destination
                              :alt   :text-content
                              :title :node-title}]
     Emphasis          [:em :content]
     StrongEmphasis    [:strong :content]
     ThematicBreak     [:hr]
     SoftLineBreak     " "
     HardLineBreak     [:br]}}})

(defn- children
  "Returns a seq of the children of a commonmark-java AST node."
  [^Node node]
  (->> (.getFirstChild node)
       (iterate #(.getNext ^Node %))
       (take-while some?)))

(defn- text-content
  "Recursively walks over the given commonmark-java AST node depth-first,
  extracting and concatenating literals from any text nodes it visits."
  [^Block node]
  (->> (tree-seq (constantly true) children node)
       (keep #(when (instance? Text %)
                (.getLiteral ^Text %)))
       (apply str)))

;; FIXME (arrdem 2017-12-21):
;;   Good grief this is slow. Can almost certainly be replaced with something less naive.
(defn property-map [node]
  (into {} (for [[k v] (dissoc (bean node) :class)]
             [(keyword (str "node-" (name k))) v])))

(defmulti node-properties
  "Returns the map representation of a commonmark-java AST node. Property names
  are prefixed with \"node-\"."
  class)

(defmethod node-properties :default [node] (property-map node))

(defmethod node-properties FencedCodeBlock [node]
  (-> (property-map node)
      (update :node-literal hiccup.util/escape-html)
      (update :node-info not-empty)))

(defmethod node-properties IndentedCodeBlock [node]
  (update (property-map node) :node-literal hiccup.util/escape-html))

(defmethod node-properties Code [node]
  (update (property-map node) :node-literal hiccup.util/escape-html))

(defmethod node-properties OrderedList [node]
  (update (property-map node) :node-startNumber #(when (< 1 %) %)))

(defmethod node-properties ListItem [^ListItem node]
  (let [parent (.getParent node)
        tight? (and (instance? ListBlock parent)
                    (.isTight ^ListBlock parent))]
    (assoc (property-map node)
           :content (if tight? :content-tight :content))))

(defn- normalize-hiccup
  "Attempts to normalize a Hiccup style tag.

  Ensures the presence of an attributes map.
  Recursively flattens any lists in the tail.
  Recursively normalizes any tags."
  [e]
  (cond (vector? e)
        (let [[tag & e]      e
              [attrs & tail] (if (map? (first e))
                               [(first e) (rest e)]
                               [{} e])]
          (apply vector tag
                 (into {}
                       (map (fn [[k v]]
                              [k (normalize-hiccup v)])
                            attrs))
                 (mapcat (fn lflat [e] 
                           (if-not (or (vector? e) (string? e))
                             (mapcat lflat e)
                             (if (vector? e)
                               [(normalize-hiccup e)]
                               [e])))
                         tail)))
        (and (seq? e)
             (every? string? e))
        (apply str e)
        
        :else e))

(defn- fix [f x]
  (let [x' (f x)]
    (if-not (= x x')
      (recur f x')
      x)))

(defn render-node
  "Renders a single CommonMark document node to Hiccup structures, using
  the renderers specified in the configuration.

  If `:normalize` is truthy in the configuration, further attempts to
  normalize all produced nodes."
  [config node]
  (let [spec             (get-in config [:renderer :nodes (class node)])
        render-children  (fn [n] (map (partial render-node config) (children n)))
        props            (node-properties node)
        d-children       (delay (render-children node))
        d-children-tight (delay (render-children (first (children node))))
        d-text           (delay (text-content node))]
    (->> spec
         ;; Do the render
         (walk/postwalk (partial fix
                                 (comp #(cond (= :content %)       @d-children
                                              (= :content-tight %) @d-children-tight
                                              (= :text-content %)  @d-text 
                                              :else                %)
                                       #(get props % %)
                                       #(if (and (fn? %) (not (keyword? %)))
                                          (% node) %))))
         ;; Clean up the output some
         (#(if (:normalize config) (normalize-hiccup %) %)))))

(defn ^Document parse-markdown
  "Parses a string of markdown, returning the parsed CommonMark Document."
  [s]
  (let [parser (.build (Parser/builder))]
    (.parse parser s)))

(defn markdown->hiccup
  "Parses the markdown and converts to a hiccup-compatible data structure.

  If no configurable is provided, the default configuration is used."
  ([s]
   (markdown->hiccup default-config s))
  ([config s]
   (render-node config (parse-markdown s))))

(defn markdown->html
  "Parses the markdown and renders to HTML via Hiccup.

  Optionally takes a configuration map, allowing customization of the HTML output.
  If no configuration is provided, the default configuration is used."
  ([s]
   (markdown->html default-config s))
  ([config s]
   (hiccup/html (markdown->hiccup config s))))

