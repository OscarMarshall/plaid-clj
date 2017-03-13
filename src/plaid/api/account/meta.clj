(ns plaid.api.account.meta
  (:require [clojure.spec :as s] [clojure.spec.gen :as gen]))

(s/def ::name string?)
(s/def ::number
  (s/with-gen (s/and string? #(re-matches #"\d{4}" %))
    #(gen/fmap (fn [x] (format "%04d" x)) (s/gen (s/int-in 0 9999)))))
