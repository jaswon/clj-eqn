(ns clj-eqn.core
  (:require [reagent.core :as r]
            [clj-eqn.calc :as calc]))

(defn app []
  [:div {:id "app"}
    [:div {:id "instructions"} "only natural numbers, basic arithmetic operators (+, -, /, *), parentheses, and one x."]
    [:div {:id "warnings" :style {:color "red"}} "all operations must be explicit! (eg. "
      [:tt "5*x"]
      " not "
      [:tt "5x"]
      ")."]
    [:div {:id "examples-header"} "Examples:"]
    [:ul {:id "examples"}
      [:li>tt "4+x=5"]
      [:li>tt "5*(3+x)=25"]
      [:li>tt "7/(5+3*x)=31"]]
    [:input {:id "query"
             :type "text"
             :onKeyDown (fn [e]
                          (when (= (aget e "key") "Enter")
                            (.click (js/document.querySelector "#solve"))))}]
    [:button {:id "solve"
              :onClick (fn [e]
                        (set!
                          (.-innerHTML (js/document.querySelector "#result"))
                          (try
                            (calc/solve (.-value (js/document.querySelector "#query")))
                            (catch js/Error e "error or no solution"))))}
      "solve"]
    [:div {:id "result-container"}
      "Solution: "
      [:span {:id "result"}]]])

(r/render [app] (js/document.querySelector "#app"))
