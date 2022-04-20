(ns shadow.test.env
  (:require
   [cljs.env :as env]
   [cljs.analyzer :as ana]
   [clojure.string :as s]))

(defn get-namespaces []
  (vals (::ana/namespaces @env/*compiler*)))

(defn get-namespaces-or-wait []
  (let [namespaces (get-namespaces)
        unanalyzed-namespaces (remove (fn [the-ns]
                                        (or (-> the-ns :name nil?)
                                            (-> the-ns
                                                :name
                                                name
                                                (s/split #"[.]" 2)
                                                #{"clojure" "cljs"})))
                                      namespaces)]
    (if (seq unanalyzed-namespaces)
      (do
        (println "waiting for" (count unanalyzed-namespaces) "of" (count namespaces) "namespaces")
        (Thread/sleep 250))
      namespaces)))

(defmacro get-test-data []
  (let [namespaces (or (->> get-namespaces-or-wait
                            repeatedly
                            (take 20)
                            (some identity))
                       (get-namespaces))]
    (reduce
     (fn [m {:keys [name defs] :as the-ns}]
       (let [{:syms [cljs-test-once-fixtures cljs-test-each-fixtures]}
             defs

             vars
             (->> (vals defs)
                  (filter :test)
                  (sort-by #(-> % :meta :line))
                  (map (fn [{:keys [name]}]
                         (list 'var name)))
                  (into []))

             ns-info
             (-> {}
                 (cond->
                   cljs-test-once-fixtures
                   (assoc-in [:fixtures :once] (:name cljs-test-once-fixtures))

                   cljs-test-each-fixtures
                   (assoc-in [:fixtures :each] (:name cljs-test-each-fixtures))

                   (seq vars)
                   (assoc :vars vars)))]

         (if-not (seq ns-info)
           m
           (assoc m `(quote ~name) ns-info))))
     {} namespaces)))
