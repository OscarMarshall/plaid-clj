(ns plaid.core
  (:require [camel-snake-kebab.core :as csk]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [inflections.core :as inflections]
            [org.httpkit.client :as http]
            [plaid.api :as api]))

(def environments
  {:tartan "https://tartan.plaid.com", :production "https://api.plaid.com"})

(s/def ::access-token ::api/access-token)
(s/def ::authenticated-response
  (s/merge ::response (s/keys :req [::request-id])))
(s/def ::body (s/keys :opt [::options]))
(s/def ::client (s/keys :req [::client-id ::env ::secret]))
(s/def ::client-id string?)
(s/def ::credentials (s/keys :req [::username ::password]))
(s/def ::env (set (vals environments)))
(s/def ::error
  (s/with-gen #(instance? Exception %)
    #(gen/fmap (fn [x] (Exception. x)) (s/gen string?))))
(s/def ::exchange-token-response
  (s/merge ::authenticated-response (s/keys :req [::api/access-token])))
(s/def ::get-connect-user-response
  (s/merge ::get-user-response (s/keys :req [::api/transactions])))
(s/def ::get-user-response
  (s/merge ::authenticated-response
           (s/keys :req [::api/access-token ::api/accounts])))
(s/def ::id string?)
(s/def ::include-mfa-response boolean?)
(s/def ::method #{:delete :get :patch :post})
(s/def ::mfa string?)
(s/def ::mfa-response
  (s/merge ::authenticated-response
           (s/keys :req [::api/access-token ::api/mfa ::api/type])))
(s/def ::options (s/map-of keyword? string?))
(s/def ::password string?)
(s/def ::product #{"auth" "connect" "income" "info" "risk"})
(s/def ::public-token string?)
(s/def ::query string?)
(s/def ::request-id string?)
(s/def ::response (s/keys :req [::status]))
(s/def ::secret string?)
(s/def ::status
  (s/with-gen pos-int? #(gen/one-of [(s/gen #{200 201}) (s/gen pos-int?)])))
(s/def ::type string?)
(s/def ::url string?)
(s/def ::username string?)

(s/fdef ex-spec
  :args (s/and (s/cat :spec (s/with-gen (s/and keyword? s/get-spec)
                              #(s/gen (set (filter keyword?
                                                   (keys (s/registry))))))
                      :x    any?)
               #(not (s/valid? (:spec %) (:x %))))
  :ret  ::error)
(defn ex-spec [spec x] (ex-info (s/explain-str spec x) (s/explain-data spec x)))

(s/fdef client
  :args (s/cat :client-id ::client-id
               :secret    ::secret
               :env       ::env)
  :ret  ::client)
(defn client [client-id secret env]
  {:pre [(s/valid? ::client-id client-id)
         (s/valid? ::secret secret)
         (s/valid? ::env env)]}
  {::client-id client-id, ::env env, ::secret secret})

(s/fdef hierarchically-namespace-keys
  :args (s/cat :x any? :ns string?)
  :ret  any?)
(defn hierarchically-namespace-keys [x top-ns]
  (cond
    (map? x)
    (into {}
          (map (fn [[k v]]
                 (let [k        (name k)
                       child-ns (str top-ns "." (cond-> k
                                                  (sequential? v)
                                                  inflections/singular))]
                   [(keyword top-ns k)
                    (hierarchically-namespace-keys v child-ns)])))
          x)

    (sequential? x)
    (into [] (map #(hierarchically-namespace-keys % top-ns)) x)

    :else
    x))

(s/fdef authenticated-request!
  :args (s/cat :client  ::client
               :options (s/keys :req [::body ::method ::url]
                                :opt [::include-mfa-response]))
  :ret  (s/or :error                  (s/keys :req [::error])
              :authenticated-response (s/keys :req [::authenticated-response])
              :mfa-response           (s/keys :req [::mfa-response]))
  :fn   (fn [{{{:keys [::include-mfa-response]} :options} :args
              {:keys [mfa-response]}                      :ret}]
          (or (not mfa-response) include-mfa-response)))
(defn- authenticated-request!
  [{:keys [::client-id ::secret]}
   {:keys [::body ::include-mfa-response ::method ::url]}]
  (let [{:keys [body error headers status]}
        @(http/request
          {:as      :stream
           :body    (-> (cond-> body
                          (:options body)
                          (update :options
                                  json/generate-string
                                  {:key-fn (comp csk/->snake_case name)}))
                        (assoc :client-id client-id, :secret secret)
                        (json/generate-string {:key-fn (comp csk/->snake_case
                                                             name)}))
           :headers {"Content-Type" "application/json"}
           :method  method
           :url     url})]
    (if error
      {::error error}
      (try (let [response (some-> body
                                  (io/reader :encoding "UTF-8")
                                  (json/parse-stream csk/->kebab-case)
                                  (hierarchically-namespace-keys "plaid.api")
                                  (assoc ::request-id (:x-request-id headers)
                                         ::status     status))]
             (cond
               (= status 200)
               (if (s/valid? ::authenticated-response response)
                 {::authenticated-response response}
                 {::error (ex-spec ::authenticated-response response)})

               (and include-mfa-response (= status 201))
               (if (s/valid? ::mfa-response response)
                 {::mfa-response response}
                 {::error (ex-spec ::mfa-response response)})

               :else
               {::error (ex-info (or (:message response) (str response))
                                 response)}))
           (catch Exception error
             {::error error})))))

#_
(s/fdef add-user!
  :args (s/cat :product     ::product
               :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn- add-user!
  ([product client type credentials options]
   (authenticated-request!
    client
    {::body                 {::credentials credentials
                             ::options     options
                             ::type        type}
     ::include-mfa-response true
     ::method               :post
     ::url                  (str (::env client) "/" product)}))
  ([product client type credentials]
   (add-user! product client type credentials {})))

(s/fdef get-user!
  :args (s/cat :product      ::product
               :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error             (s/keys :req [::error])
              :get-user-response (s/keys :req [::get-user-response])))
(defn- get-user!
  ([product client access-token options]

   (let [{:keys [::authenticated-response ::error]}
         (authenticated-request!
          client
          {::body   {::access-token access-token, ::options options}
           ::method :post
           ::url    (str (::env client) "/" product "/get")})]
     (cond
       error
       {::error error}

       (s/valid? ::get-user-response authenticated-response)
       {::get-user-response authenticated-response}

       :else
       {::error (ex-spec ::get-user-response authenticated-response)})))
  ([product client access-token] (get-user! product client access-token {})))

#_
(s/fdef step-user!
  :args (s/cat :product      ::product
               :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn- step-user!
  ([product client access-token mfa-response options]
   (authenticated-request!
    client
    {::body                 {::access-token access-token
                             ::mfa          mfa-response
                             ::options      options}
     ::include-mfa-response true
     ::method               :post
     ::url                  (str (::env client) "/" product "/step")}))
  ([product client access-token mfa-response]
   (step-user! product client access-token mfa-response {})))

#_
(s/fdef patch-user!
  :args (s/cat :product      ::product
               :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn- patch-user!
  ([product client access-token credentials options]
   (authenticated-request!
    client
    {::body                 {::access-token access-token
                             ::credentials  credentials
                             ::options      options}
     ::include-mfa-response true
     ::method               :patch
     ::url                  (str (::env client) "/" product)}))
  ([product client access-token credentials]
   (patch-user! product client access-token credentials {})))

#_
(s/fdef delete-user!
  :args (s/cat :product      ::product
               :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn- delete-user!
  ([product client access-token options]
   (authenticated-request! client
                           {::body   {::access-token access-token
                                      ::options      options}
                            ::method :delete
                            ::url    (str (::env client) "/" product)}))
  ([product client access-token] (delete-user! product client access-token {})))

#_
(s/fdef add-auth-user!
  :args (s/cat :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn add-auth-user!
  ([client type credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::type type)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (add-user! "auth" client type credentials options))
  ([client type credentials] (add-auth-user! client type credentials {})))
#_
(s/fdef get-auth-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-auth-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (get-user! "auth" client access-token options))
  ([client access-token] (get-auth-user! client access-token {})))
#_
(s/fdef step-auth-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn step-auth-user!
  ([client access-token mfa-response options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::mfa-response mfa-response)
          (s/valid? ::options options)]}
   (step-user! "auth" client access-token mfa-response options))
  ([client access-token mfa-response]
   (step-auth-user! client access-token mfa-response {})))
#_
(s/fdef patch-auth-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn patch-auth-user!
  ([client access-token credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (patch-user! "auth" client access-token credentials options))
  ([client access-token credentials]
   (patch-auth-user! client access-token credentials {})))
#_
(s/fdef delete-auth-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn delete-auth-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (delete-user! "auth" client access-token options))
  ([client access-token] (delete-auth-user! client access-token {})))

#_
(s/fdef add-connect-user!
  :args (s/cat :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn add-connect-user!
  ([client type credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::type type)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (add-user! "connect" client type credentials options))
  ([client type credentials] (add-connect-user! client type credentials {})))
(s/fdef get-connect-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      ::options)
  :ret  (s/or :error
              (s/keys :req [::error])

              :get-connect-user-response
              (s/keys :req [::get-connect-user-response])))
(defn get-connect-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (let [{:keys [::error ::get-user-response]}
         (get-user! "connect" client access-token options)]
     (cond
       error
       {::error error}

       (s/valid? ::get-connect-user-response get-user-response)
       {::get-connect-user-response get-user-response}

       :else
       {::error (ex-spec ::get-connect-user-response get-user-response)})))
  ([client access-token] (get-connect-user! client access-token {})))
#_
(s/fdef step-connect-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn step-connect-user!
  ([client access-token mfa-response options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::mfa-response mfa-response)
          (s/valid? ::options options)]}
   (step-user! "connect" client access-token mfa-response options))
  ([client access-token mfa-response]
   (step-connect-user! client access-token mfa-response {})))
#_
(s/fdef patch-connect-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn patch-connect-user!
  ([client access-token credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (patch-user! "connect" client access-token credentials options))
  ([client access-token credentials]
   (patch-connect-user! client access-token credentials {})))
#_
(s/fdef delete-connect-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn delete-connect-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (delete-user! "connect" client access-token options))
  ([client access-token] (delete-connect-user! client access-token {})))

#_
(s/fdef add-income-user!
  :args (s/cat :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn add-income-user!
  ([client type credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::type type)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (add-user! "income" client type credentials options))
  ([client type credentials] (add-income-user! client type credentials {})))
#_
(s/fdef get-income-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-income-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (get-user! "income" client access-token options))
  ([client access-token] (get-income-user! client access-token {})))
#_
(s/fdef step-income-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn step-income-user!
  ([client access-token mfa-response options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::mfa-response mfa-response)
          (s/valid? ::options options)]}
   (step-user! "income" client access-token mfa-response options))
  ([client access-token mfa-response]
   (step-income-user! client access-token mfa-response {})))
#_
(s/fdef patch-income-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn patch-income-user!
  ([client access-token credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (patch-user! "income" client access-token credentials options))
  ([client access-token credentials]
   (patch-income-user! client access-token credentials {})))
#_
(s/fdef delete-income-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn delete-income-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (delete-user! "income" client access-token options))
  ([client access-token] (delete-income-user! client access-token {})))

#_
(s/fdef add-info-user!
  :args (s/cat :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn add-info-user!
  ([client type credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::type type)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (add-user! "info" client type credentials options))
  ([client type credentials] (add-info-user! client type credentials {})))
#_
(s/fdef get-info-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-info-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (get-user! "info" client access-token options))
  ([client access-token] (get-info-user! client access-token {})))
#_
(s/fdef step-info-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn step-info-user!
  ([client access-token mfa-response options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::mfa-response mfa-response)
          (s/valid? ::options options)]}
   (step-user! "info" client access-token mfa-response options))
  ([client access-token mfa-response]
   (step-info-user! client access-token mfa-response {})))
#_
(s/fdef patch-info-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn patch-info-user!
  ([client access-token credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (patch-user! "info" client access-token credentials options))
  ([client access-token credentials]
   (patch-info-user! client access-token credentials {})))
#_
(s/fdef delete-info-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn delete-info-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (delete-user! "info" client access-token options))
  ([client access-token] (delete-info-user! client access-token {})))

#_
(s/fdef add-risk-user!
  :args (s/cat :client      ::client
               :type        ::type
               :credentials ::credentials
               :options     (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn add-risk-user!
  ([client type credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::type type)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (add-user! "risk" client type credentials options))
  ([client type credentials] (add-risk-user! client type credentials {})))
#_
(s/fdef get-risk-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-risk-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (get-user! "risk" client access-token options))
  ([client access-token] (get-risk-user! client access-token {})))
#_
(s/fdef step-risk-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :mfa-response ::mfa
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn step-risk-user!
  ([client access-token mfa-response options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::mfa-response mfa-response)
          (s/valid? ::options options)]}
   (step-user! "risk" client access-token mfa-response options))
  ([client access-token mfa-response]
   (step-risk-user! client access-token mfa-response {})))
#_
(s/fdef patch-risk-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :credentials  ::credentials
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn patch-risk-user!
  ([client access-token credentials options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::credentials credentials)
          (s/valid? ::options options)]}
   (patch-user! "risk" client access-token credentials options))
  ([client access-token credentials]
   (patch-risk-user! client access-token credentials {})))
#_
(s/fdef delete-risk-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :options      (s/? ::options))
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn delete-risk-user!
  ([client access-token options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::options options)]}
   (delete-user! "risk" client access-token options))
  ([client access-token] (delete-risk-user! client access-token {})))

(s/fdef exchange-token!
  :args (s/cat :client       ::client
               :public-token ::public-token
               :account-id   (s/? (s/nilable ::id)))
  :ret  (s/or :error
              (s/keys :req [::error])

              :exchange-token-response
              (s/keys :req [::exchange-token-response])))
(defn exchange-token!
  ([client public-token account-id]
   {:pre [(s/valid? ::client client)
          (s/valid? ::public-token public-token)
          (s/valid? (s/nilable ::id) account-id)]}
   (let [{:keys [::error ::authenticated-response]}
         (authenticated-request! client
                                 {::body   {::account-id   account-id
                                            ::public-token public-token}
                                  ::method :post
                                  ::url    (str (::env client)
                                                "/exchange_token")})]
     (cond
       error
       {::error error}

       (s/valid? ::exchange-token-response authenticated-response)
       {::exchange-token-response authenticated-response}

       :else
       {::error (ex-spec ::exchange-token-response authenticated-response)})))
  ([client public-token] (exchange-token! client public-token nil)))

#_
(s/fdef get-balance!
  :args (s/cat :client ::client, :access-token ::access-token)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-balance! [client access-token]
  {:pre [(s/valid? ::client client)
         (s/valid? ::access-token access-token)]}
  (authenticated-request! client
                          {::body   {::access-token access-token}
                           ::method :post
                           ::url    (str (::env client) "/balance")}))

#_
(s/fdef upgrade-user!
  :args (s/cat :client       ::client
               :access-token ::access-token
               :upgrade-to   ::product
               :options      (s/? ::options))
  :ret  (s/or :error        (s/keys :req [::error])
              :response     (s/keys :req [::response])
              :mfa-response (s/keys :req [::mfa-response])))
#_
(defn upgrade-user!
  ([client access-token upgrade-to options]
   {:pre [(s/valid? ::client client)
          (s/valid? ::access-token access-token)
          (s/valid? ::product upgrade-to)
          (s/valid? ::options options)]}
   (authenticated-request! client
                           {::body                 {::access-token access-token
                                                    ::upgrade-to   upgrade-to
                                                    ::options      options}
                            ::include-mfa-response true
                            ::method               :post
                            ::url                  (str (::env client)
                                                        "/upgrade")}))
  ([client access-token upgrade-to]
   (upgrade-user! client access-token upgrade-to {})))

#_
(s/fdef get-all-institutions!
  :args (s/cat :client ::client, :options ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-all-institutions! [client options]
  {:pre [(s/valid? ::client client) (s/valid? ::options options)]}
  (authenticated-request! client
                          {::body   options
                           ::method :post
                           ::url    (str (:env client) "/institutions/all")}))

#_
(s/fdef public-request!
  :args (s/cat :options ::options)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn- public-request! [{:keys [::query-params ::url]}]
  (let [{:keys [body error headers status]}
        @(http/request {:method :get, :query-params query-params, :url url})]
    (if error
      {::error error}
      (try (let [response (some->
                           body
                           (io/reader :encoding "UTF-8")
                           (json/parse-stream #(keyword "plaid.response"
                                                        (csk/->kebab-case %))))]
             (if (= status 200)
               (if (s/valid? ::response response)
                 {::response response}
                 {::error (ex-spec ::response response)})
               {::error (ex-info (or (:message response)
                                     (str response))
                                 response)}))
           (catch Exception error
             {::error error})))
    (cond error             {::error error}
          (not= status 200) {::error (assoc body ::status status)}
          :else             {::response body})))

#_
(s/fdef get-category!
  :args (s/cat :category-id ::id, :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-category! [category-id env]
  {:pre [(s/valid? ::id category-id) (s/valid? ::env env)]}
  (public-request! {::url (str env "/categories/" category-id)}))

#_
(s/fdef get-categories!
  :args (s/cat :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-categories! [env]
  {:pre [(s/valid? ::env env)]}
  (public-request! {::url (str env "/categories")}))

#_
(s/fdef get-institution!
  :args (s/cat :institution-id ::id, :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-institution! [institution-id env]
  {:pre [(s/valid? ::id institution-id) (s/valid? ::env env)]}
  (public-request! {::url (str env "/institutions/" institution-id)}))

#_
(s/fdef get-institutions!
  :args (s/cat :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn get-institutions! [env]
  {:pre [(s/valid? ::env env)]}
  (public-request! {::url (str env "/institutions")}))

#_
(s/fdef search-institutions!
  :args (s/cat :options (s/keys :opt [::id ::product ::query]), :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn search-institutions! [options env]
  {:pre [(s/valid? (s/keys :opt [::id ::product ::query]) options)
         (s/valid? ::env env)]}
  (let [{:keys [::id ::product ::query]} options]
    (public-request! {::url          (str env "/institutions/search")
                      ::query-params {:id id, :p product, :q query}})))

#_
(s/fdef search-all-institutions!
  :args (s/cat :options (s/keys :opt [::id ::product ::query]), :env ::env)
  :ret  (s/or :error    (s/keys :req [::error])
              :response (s/keys :req [::response])))
#_
(defn search-all-institutions! [options env]
  {:pre [(s/valid? (s/keys :opt [::id ::product ::query]) options)
         (s/valid? ::env env)]}
  (let [{:keys [id product query]} options]
    (public-request! {::url          (str env "/institutions/all/search")
                      ::query-params {:id id, :p product, :q query}})))
