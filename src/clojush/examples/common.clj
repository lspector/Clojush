;; The only clojush namespace
(ns clojush.examples.common
  (:require [clojush.pushgp.pushgp :as pushgp]
            [clojush.pushstate :as pushstate]
            [clojush.interpreter :as interpreter]
            ))

(def define-push-argmap pushgp/define-push-argmap)
(defmacro define-registered [a b] `(pushstate/define-registered ~a ~b))
(def make-push-state pushstate/make-push-state)
(def push-item pushstate/push-item) 
(def top-item pushstate/top-item)
(def stack-ref pushstate/stack-ref)
(def top-item pushstate/top-item)
(def run-push interpreter/run-push)
(def pushgp pushgp/pushgp)
