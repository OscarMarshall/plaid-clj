(ns plaid.api.mfa
  (:require [clojure.spec :as s]))

(s/def ::mask string?)
(s/def ::type string?)
(s/def ::question string?)
(s/def ::answers (s/coll-of string?))
