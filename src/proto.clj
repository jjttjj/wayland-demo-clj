(ns proto
  (:require [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as zxml]))

(defn parse-interfaces [root]
  (for [iface (zxml/xml-> root :interface)]
    (let [iname (zxml/attr iface :name)]
      {:name     iname
       :requests
       (->> (zxml/xml-> iface :request)
            (map-indexed (fn [ix x]
                           (let [rname (zxml/attr x :name)]
                             {:name rname
                              :opcode ix
                              :args
                              (->> (zxml/xml-> x :arg)
                                   (mapv
                                     (fn [a] (-> a zip/node :attrs))))})))
            (map (juxt :name identity))
            (into {}))
       :events
       (->> (zxml/xml-> iface :event)
            (map-indexed (fn [ix x] {:name (zxml/attr x :name)
                                     :opcode ix
                                     :args
                                     (->> (zxml/xml-> x :arg)
                                          (mapv
                                            (fn [a] (-> a zip/node :attrs))))}))
            (map (juxt :opcode identity))
            (into {}))})))

(defn parse-resource [r]
  (-> r
      io/resource io/reader xml/parse zip/xml-zip parse-interfaces))

(defn parse-all [xml-paths]
  (mapcat parse-resource xml-paths))

(def interface-spec
  (-> (->> (parse-all ["wayland.xml" "xdg-shell.xml"])
           (map (juxt :name identity))
           (into {}))
      ;; hack to "fix" the arg specs for bind
      (update-in ["wl_registry" :requests "bind" :args]
        (fn [[a1 a2]]
          [a1
           {:name    "interface",
            :type    "string",
            :summary "undocumented interface name"}
           {:name    "version",
            :type    "uint",
            :summary "undocumented version"}
           a2]))))
