(ns hooks.test-hooks
  (:require
    [clj-kondo.hooks-api :as api]))


(defn with-server [{:keys [node] :as exp}]
  (let [[s-name handler opts & body] (-> node :children rest)
        underscore (api/token-node '_)
        new-children (list*
                       (api/token-node 'let)
                       (api/vector-node
                         [s-name handler
                          underscore opts])
                       body)
        new-node (assoc node :children new-children)]
    (assoc exp :node new-node)))

