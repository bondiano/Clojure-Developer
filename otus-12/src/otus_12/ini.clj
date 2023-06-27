(ns otus-12.ini
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defmacro with-conformer
  [[bind] & body]
  `(s/conformer
    (fn [~bind]
      (try
        ~@body
        (catch Exception e#
          ::s/invalid)))
    identity))

(s/def ::->keyword
  (with-conformer [value]
    (keyword value)))

(s/def ::keyword
  (s/or :keyword keyword?
        :string ::->keyword))

(s/def ::->number
  (with-conformer [value]
    (Float/parseFloat value)))

(s/def ::->str
  (with-conformer [value]
    (str value)))

(s/def ::->boolean
  (with-conformer [value]
    (let [lower-case-value (str/lower-case value)]
      (condp = lower-case-value
        "true" true
        "false" false
        ::s/invalid))))

(s/def ::->value
  (s/or
   :number ::->number
   :boolean ::->boolean
   :str ::->str))

(def ^:private section-name-re
  (re-pattern "\\[(.*)\\]"))

(s/def ::->section-name
  (s/and (with-conformer [value]
           (second (re-find section-name-re value)))
         ::->keyword))

(s/def ::->kv-pair
  (s/and
   (with-conformer [value]
     (str/split value #"="))
   (s/tuple ::->keyword ::->value)))

(s/def ::section
  (s/cat
   :name ::->section-name
   :body (s/* ::->kv-pair)))

(s/def ::ini
  (s/* ::section))

(s/def ::->ini
  (s/and
   (with-conformer [value]
     (->> value
         str/split-lines
         (mapv str/trim)
         (remove empty?)))
   ::ini))

(defn ast->map
  [ast]
  (reduce (fn [acc section]
            (let [name (:name section)
                  body (:body section)]
            (reduce (fn [acc [k v]]
                      (assoc-in acc [name k] (s/unform ::value v)))
                    acc
                    body)))
          {}
          ast))

(defn parse-ini
  [input]
  (let [ast (s/conform ::->ini input)]
    (if-let [error (s/invalid? ast)]
      error
      (ast->map ast))))

(def ini-example
  "[owner]\nname=John Doe\norganization=Acme Widgets Inc.\n[database]\nserver=192.0.2.62\nport=143\nfile=\"payroll.dat\"\nencrypted=false")

(defn -main
  [& args]
  (println (parse-ini ini-example)))
