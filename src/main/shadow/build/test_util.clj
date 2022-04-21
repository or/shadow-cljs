(ns shadow.build.test-util
  (:require [shadow.build.data :as data]
            [shadow.build.classpath :as cp]
            [shadow.cljs.util :as util]
            [clojure.java.io :as io]))

(defn find-test-namespaces [{:keys [classpath] :as state} config]
  (let [{:keys [namespaces ns-regexp exclude test-paths] :or {ns-regexp "-test$" exclude #{}}}
        config

        ns-regexp
        (re-pattern ns-regexp)]

    (if (seq namespaces)
      namespaces
      (->> (cp/find-cljs-namespaces-in-files
            classpath
            (when (seq test-paths)
              (map io/file test-paths)))
           (filter (fn [ns]
                     (re-find ns-regexp (str ns))))
           (remove exclude)
           (sort)
           (into [])))))

(defn inject-extra-requires
  [{::keys [runner-ns test-namespaces] :as state}]
  {:pre [(symbol? runner-ns)
         (coll? test-namespaces)
         (every? symbol? test-namespaces)]}

  ;; since the runner doesn't explicitly depend on the test namespaces
  ;; it may start compiling before they actually complete
  ;; which is a problem when the runner-ns uses macros that inspect the
  ;; analyzer data to discover tests since they may still be pending
  (let [runner-rc-id (data/get-source-id-by-provide state runner-ns)]

    (-> state
        (update-in [:sources runner-rc-id] assoc :extra-requires (set test-namespaces)))))

(defn configure-common [state]
  (assoc-in state [:compiler-options :load-tests] true))
