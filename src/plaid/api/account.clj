(ns plaid.api.account
  (:require [clojure.spec :as s] [plaid.api.account.meta :as meta]))

(s/def ::id string?)
(s/def ::meta (s/keys :req [::meta/name ::meta/number]))
