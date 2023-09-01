(ns soft.core
  (:require [hiccup.core :as hiccup]
            [stylo.core :refer [c]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [markdown.core :as markdown]
            [cheshire.core :as json]))


(defn menu [cases]
  (->>
   cases
   (map
    (fn [tcase]
      [:a {:class (c [:px 4] [:py 2] {:display "block"})
           :href (:page tcase)}
       (get-in tcase [:content :name])]))))

(defn layout [ctx content]
  (hiccup/html
   [:html
    [:head
     [:title "SQL on FHIR Tests"]
     [:link {:href "soft.css", :rel "stylesheet"}]]
    [:body
     [:div {:class (c [:flex])}
      [:div {:class (c [:px 4]
                       [:py 4]
                       [:bg :gray-100]
                       :h-full
                       {:width "16em"})}
       (menu (:cases ctx))]
      [:div {:class (c [:px 4] :flex-1)}
       content]]]]))


(defn hl-json [o & [kk]]
  (cond
    (map? o)
    [:div {:class (c [:ml 2])}
     (let [last (dec (count o))]
       (->> o
            (map-indexed (fn [i [k v]]
                           [:div {:class (if (= i 0)
                                           (c [:ml 0] :flex [:space-x 0.5])
                                           (c [:ml 0] :flex [:space-x 0.5]))}
                            (if (and (= i 0) (not (= i last)))
                              [:div {:class (c  [:text :gray-400])} "{"]
                              [:div {:class (c  {:width ".6rem"})} "&nbsp"])

                            [:b {:class (c [:text :gray-600])} (name k) ]
                            [:span ":"]
                            (hl-json v k)
                            (if (= i last)
                              [:div {:class (c :flex [:text :gray-400] :flex-col {:vertical-align "bottom"})}
                               [:div {:class (c :flex-1)}]
                               [:div "}"]]
                              [:div {:class (c :flex :flex-col [:text :gray-400] {:vertical-align "bottom"})}
                               [:div {:class (c :flex-1)}]
                               [:div ","]])]))))]

    (sequential? o)
    [:div {:class (c [:ml 2])}
     (let [last (dec (count o))]
       (->> o
            (map-indexed (fn [i v]
                           [:div {:class (c :flex)}
                            (if (and (= i 0) (not (= i last)))
                              [:div {:class (c  [:text :gray-400])} "["]
                              [:div " "])
                            (hl-json v)
                            (if (= i last)
                              [:div {:class (c :flex :flex-col [:text :gray-400]  {:vertical-align "bottom"})}
                               [:div {:class (c :flex-1)}]
                               [:div "]"]]
                              [:div {:class (c :flex :flex-col [:text :gray-400] {:vertical-align "bottom"})}
                               [:div {:class (c :flex-1)}]
                               [:div " "]])]))))]
    (string? o)  [:span {:class (if (= :expression kk)
                                  (c [:text :pink-700])
                                  (c [:text :green-700]))} (pr-str o)]
    (number? o)  [:span {:class (c [:text :red-700])} (str o)]
    (boolean? o) [:span {:class (c [:text :blue-700])} (str o)]
    :else (pr-str o)))

(defn hl-view [v]
  [:div {:class (c [:mt 6] :text-sm [:bg :gray-100] :border [:py 3] :rounded :shadow-sm
                   {:font-family "SFMono-Regular, Consolas, \"Liberation Mono\", Menlo, monospace"}
                   )}
   (hl-json v)])

(def thc (c [:py 1] [:px 2] :border [:bg :gray-100]))
(def tdc (c [:py 1] [:px 2] :border))

(defn render-page [ctx fcase]
  [:div {:class (c [:p 4] [:text :gray-700])}
   [:h1 {:class (c :text-3xl :font-bold :border-b [:py 1])}
    (or (:name fcase) "name is missed")]
   [:p (when-let [d (:desc fcase)]
         (markdown/md-to-html-string d))]

   [:h2 {:class (c :text-xl :font-bold [:pt 1] [:mt 2])} "Resources:"]
   [:div {:class (c )}
    (for [r (:resource fcase)]
      [:div {:class (c [:py 0])}
       [:details
        [:summary {:class (c [:py 1] :border-b :cursor-pointer [:hover [:bg :gray-100]])} [:b (:id r)]]
        (hl-view r)]])]

   [:hr]

   (for [v (:views fcase)]
     [:div {:class (c [:my 4])}
      [:h2 {:class (c :font-bold :border-b [:my 1] :text-2xl)} (:title v)]
      [:p (:desc v)]
      (hl-view (:view v))
      [:br]
      (let [cols (->> (:result v)
                      (mapcat (fn [x] (when (map? x) (keys x))))
                      (into #{}))]
        [:table {:class (c :text-sm :w-full)}
         [:thead
          (->> cols
               (map (fn [c]
                      [:th {:class thc} (name c)])))]
         [:tbody
          (->> (:result v)
               (map (fn [r]
                      [:tr
                       (->> cols
                            (map (fn [col]
                                   [:td {:class tdc}
                                    (let [rv (get r col "NULL")]
                                      (if (string? rv)
                                        rv
                                        [:div {:class (c  [:text :red-700])}
                                         [:pre
                                          (json/generate-string rv {:pretty true})]]))])))])))]])])])

(defn render-file [ctx {f :file c :content p :page}]
  (let [content (layout ctx (render-page ctx c))
        out (str "build/" p)]
    (println :> f out)
    (spit out content)))

(defn main [path]
  (let [dir (io/file "build/")]
    (when-not (.exists dir)
      (.mkdirs dir))
    (spit "build/soft.css" (stylo.core/compile-styles @stylo.core/styles)))

  (def cases
    (->>
     (file-seq (io/file path))
     (mapv (fn [f]
             (when (str/ends-with? (.getPath f) ".json")
               {:file (.getName f)
                :page (str/replace (.getName f) #"\.json$" ".html")
                :content (json/parse-string (slurp (.getPath f)) keyword)})))
     (remove nil?)
     (sort-by :file)))

  (doseq [c cases]
    (render-file {:cases cases} c)))

(main "/Users/niquola/sql-on-fhir-2/input/tests")
