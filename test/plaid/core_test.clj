(ns plaid.core-test
  (:require [cheshire.core :as json]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clojure.test :as t :refer [deftest is testing]]
            [clojure.test.check]
            [org.httpkit.client :as http]
            [plaid.core :as sut])
  (:import (java.io Reader StringReader)))

(s/def ::http/as #{:stream})
(s/def ::http/body
  (s/with-gen (s/or :string string?, :stream (partial instance? Reader))
    (fn []
      (let [json-doc (gen/fmap json/generate-string
                               (s/gen (s/map-of keyword? any?)))]
        (gen/one-of [json-doc (gen/fmap #(StringReader. %) json-doc)])))))
(s/def ::http/error
  (s/with-gen (partial instance? Exception)
    (fn [] (gen/fmap #(Exception. %) (s/gen string?)))))
(s/def ::http/headers (s/map-of keyword? string?))
(s/def ::http/method #{:delete :get :patch :post})
(s/def ::http/query-params
  (s/map-of keyword? (s/or :string string?
                           :coll   (s/coll-of string?)
                           :map    ::http/query-params)))
(s/def ::http/request
  (s/keys :req-un [::http/method ::http/url]
          :opt-un [::http/as ::http/body ::http/query-params]))
(s/def ::http/response
  (s/or :response (s/keys :req-un [::http/body ::http/headers ::http/status])
        :error    (s/keys :req-un [::http/error])))
(s/def ::http/status #{200 201 500})
(s/def ::http/url string?)

(t/use-fixtures :once (fn [f] (stest/instrument) (f) (stest/unstrument)))

(deftest ex-spec
  (is (empty? (filter :failure (stest/check `sut/ex-spec)))))

(deftest client-test
  (is (empty? (filter :failure (stest/check `sut/client)))))

(deftest authenticated-request!-test
  (stest/instrument
   `http/request
   {:spec {`http/request
           (s/fspec :args (s/cat :options ::http/request)
                    :ret  (s/with-gen future?
                            (fn []
                              (gen/fmap #(future %)
                                        (s/gen (s/and ::http/response
                                                      (fn [[_ {[x] :body}]]
                                                        (not= x :string))))))))}
    :stub #{`http/request}})
  (is (empty? (filter :failure (stest/check `sut/authenticated-request!))))
  (stest/unstrument `http/request)
  (testing "http/request error"
    (let [request-atom (atom nil)]
      (with-redefs [http/request (fn [& args]
                                   (reset! request-atom args)
                                   (future {:error (Exception. "error")}))]
        (let [response  (#'sut/authenticated-request!
                         (sut/client "client-id"
                                     "secret"
                                     "https://tartan.plaid.com")
                         {::sut/body   {:body "body"}
                          ::sut/method :post
                          ::sut/url    "http://example.com"})
              [request] @request-atom]
          (is (= {:body "body", :client_id "client-id", :secret "secret"}
                 (json/parse-string (:body request) keyword)))
          (is (= :post (:method request)))
          (is (= "http://example.com" (:url request)))
          (is (instance? Exception (::sut/error response)))))))
  (testing "Status is 200"
    (let [request-atom (atom nil)]
      (with-redefs [http/request
                    (fn [& args]
                      (reset! request-atom args)
                      (future {:body    (StringReader. "{\"body\":\"body\"}")
                               :headers {:x-request-id "id"}
                               :status  200}))]
        (let [response  (#'sut/authenticated-request!
                         (sut/client "client-id"
                                     "secret"
                                     "https://tartan.plaid.com")
                         {::sut/body   {:body "body"}
                          ::sut/method :post
                          ::sut/url    "http://example.com"})
              [request] @request-atom]
          (is (= {:body "body", :client_id "client-id", :secret "secret"}
                 (json/parse-string (:body request) keyword)))
          (is (= :post (:method request)))
          (is (= "http://example.com" (:url request)))
          (is (= #::sut {:authenticated-response
                         #:plaid.api {:body            "body"
                                      ::sut/request-id "id"
                                      ::sut/status     200}}
                 response))))))
  (testing "Status is 201"
    (testing "and :plaid.request/include-mfa-response is false"
      (let [request-atom (atom nil)]
        (with-redefs [http/request
                      (fn [& args]
                        (reset! request-atom args)
                        (future {:body    (StringReader. "{\"body\":\"body\"}")
                                 :headers {:x-request-id "id"}
                                 :status  201}))]
          (let [response  (#'sut/authenticated-request!
                           (sut/client "client-id"
                                       "secret"
                                       "https://tartan.plaid.com")
                           {::sut/body   {:body "body"}
                            ::sut/method :post
                            ::sut/url    "http://example.com"})
                [request] @request-atom]
            (is (= {:body "body", :client_id "client-id", :secret "secret"}
                   (json/parse-string (:body request) keyword)))
            (is (= :post (:method request)))
            (is (= "http://example.com" (:url request)))
            (is (contains? response ::sut/error))
            (is (instance? Exception (::sut/error response)))))))
    (testing "and :plaid.request/include-mfa-response is true"
      (let [request-atom (atom nil)]
        (with-redefs [http/request
                      (fn [& args]
                        (reset! request-atom args)
                        (future
                          {:body    (-> {:access_token "access-token"
                                         :mfa          [{:question "question"}]
                                         :type         "questions"}
                                        json/generate-string
                                        StringReader.)
                           :headers {:x-request-id "id"}
                           :status  201}))]
          (let [response  (#'sut/authenticated-request!
                           (sut/client "client-id"
                                       "secret"
                                       "https://tartan.plaid.com")
                           {::sut/body                 {:body "body"}
                            ::sut/include-mfa-response true
                            ::sut/method               :post
                            ::sut/url                  "http://example.com"})
                [request] @request-atom]
            (is (= {:body "body", :client_id "client-id", :secret "secret"}
                   (json/parse-string (:body request) keyword)))
            (is (= :post (:method request)))
            (is (= "http://example.com" (:url request)))
            (is (= #::sut
                   {:mfa-response
                    #:plaid.api
                    {:access-token    "access-token"
                     :mfa             [#:plaid.api.mfa{:question "question"}]
                     ::sut/request-id "id"
                     ::sut/status     201
                     :type            "questions"}}
                   response)))))))
  (testing "Status is 500"
    (testing "http/request error"
      (let [request-atom (atom nil)]
        (with-redefs [http/request
                      (fn [& args]
                        (reset! request-atom args)
                        (future {:body    (StringReader. "{\"body\":\"body\"}")
                                 :headers {:x-request-id "id"}
                                 :status  500}))]
          (let [response  (#'sut/authenticated-request!
                           (sut/client "client-id"
                                       "secret"
                                       "https://tartan.plaid.com")
                           {::sut/body   {:body "body"}
                            ::sut/method :post
                            ::sut/url    "http://example.com"})
                [request] @request-atom]
            request
            (is (= {:body "body", :client_id "client-id", :secret "secret"}
                   (json/parse-string (:body request) keyword)))
            (is (= :post (:method request)))
            (is (= "http://example.com" (:url request)))
            (is (instance? Exception (::sut/error response)))))))))

(deftest exchange-token!-test
  (stest/instrument
   `sut/authenticated-request
   {:spec {`sut/authenticated-request
           (s/fspec :args (s/cat :client  ::client
                                 :options (s/keys :req [::sut/body
                                                        ::sut/method
                                                        ::sut/url]))
                    :ret  (s/or :error
                                (s/keys :req [::sut/error])

                                :response
                                (s/keys :req [::sut/authenticated-response])))}
    :stub #{`sut/authenticated-request}})
  (is (empty? (filter :failure (stest/check `sut/authenticated-request)))))
