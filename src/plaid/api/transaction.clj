(ns plaid.api.transaction
  (:require [clojure.spec :as s] [clojure.spec.gen :as gen])
  (:import (java.text SimpleDateFormat) (java.util Date)))

(s/def ::account string?)
(s/def ::amount number?)
(s/def ::date
  (s/with-gen (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %))
    #(let [date-format (SimpleDateFormat. "yyyy-MM-dd")]
       (gen/fmap (fn [x] (.format date-format (Date. (* x 86400000))))
                 (s/gen (s/int-in -719528 2932896))))))
(s/def ::id string?)
(s/def ::name string?)
