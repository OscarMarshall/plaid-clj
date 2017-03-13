(ns plaid.api
  (:require [clojure.spec :as s]
            [plaid.api.account :as account]
            [plaid.api.mfa :as mfa]
            [plaid.api.transaction :as transaction]))

(s/def ::access-token string?)
(s/def ::account (s/keys :req [::account/id ::account/meta]))
(s/def ::accounts (s/coll-of ::account))
(s/def ::mfa
  (s/coll-of (s/or :list       (s/keys :req [::mfa/mask ::mfa/type])
                   :selections (s/keys :req [::mfa/question ::mfa/answers])
                   :questions  (s/keys :req [::mfa/question]))))
(s/def ::transaction
  (s/keys :req [::transaction/account
                ::transaction/amount
                ::transaction/date
                ::transaction/id
                ::transaction/name]))
(s/def ::transactions (s/coll-of ::transaction))
(s/def ::type string?)
