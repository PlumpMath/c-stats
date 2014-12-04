(ns cycling-stats.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [<! put! chan]])
  (:import [goog.net Jsonp]
           [goog Uri]))

(enable-console-print!)

(defonce app-state (atom {:athlete {} :activities []}))

(defn request
  [uri]
  (let [out (chan)
        req (Jsonp. (Uri. uri))]
    ;; f7a342587c6772f089ad88b8d8074ed1180c1e72 <- crystal
    ;; 24ccf2dc6d31c090c427df95f591a4b3d53496b2 <- ryan
    (.send req #js {:access_token "24ccf2dc6d31c090c427df95f591a4b3d53496b2"} 
               (fn [res] (put! out res)))
    out))

(def css-trans-group (aget js/React "addons" "CSSTransitionGroup"))

(defn request-athlete
  [app]
  (go
    (let [result (<! (request "https://www.strava.com/api/v3/athlete"))
          clj-result (js->clj result :keywordize-keys true)]
      (om/update! app :athlete clj-result))))

(defn request-activities
  [app]
  (go
    (let [result (<! (request "https://www.strava.com/api/v3/activities"))
          clj-result (js->clj result :keywordize-keys true)]
      (om/update! app :activities clj-result))))

(defn request-laps
  [activity-id]
  (request (str "https://www.strava.com/api/v3/activities/"
                activity-id
                "/laps")))

(defn athlete-view [athlete owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/a #js {:href (str "http://www.strava.com/athletes/" (:id athlete))
                    :target "_blank"
                    :className "profile-container"}
          (dom/h2 #js {:style #js {:display "inline-block"
                                   :font-weight "normal"}}
                  (:firstname athlete))
          (dom/h2 #js {:style #js {:display "inline-block"
                                   :font-weight "bold"}}
                  (:lastname athlete))
          (dom/figure #js {:className "avatar-container"}
            (dom/img #js {:src (:profile athlete)
                          :style #js {:display "block"}})))))))

(defn slider [[min max step] owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/input #js {:type "range"
                        :min min
                        :max max
                        :step step
                        :defaultValue (om/get-state owner :value)
                        :onChange
                        (fn [e]
                          (let [val (.. e -target -value)]
                            (om/set-state! owner :value val)))})
        (dom/span nil (om/get-state owner :value))))))

(defn analyzer [{:keys [target interval-count laps type interval-operator out]}]
  (let [accessor (case type
                    "watts" :average_watts
                    "mph"   :average_speed
                    "rpm"   :average_cadence
                    "bpm"   :average_heartrate)
        op (case interval-operator
             "or more" >
             "or less" <)
        result (filter (fn [current] (op (accessor current) target)) laps)
        reason (str (count result) " of " interval-count
                    " expected intervals attained a target of " target
                    " " type " " interval-operator ".")
        return-result (if (< (count result) interval-count)
                        {:status :fail
                         :reason reason}
                        {:status :success
                         :reason reason})]
    (put! out return-result)
    out))

(defn plan-view [activity-id owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [analysis-chan]}]
      (dom/form nil
        (dom/div nil
          (dom/span nil "I expect")
          (apply dom/select #js {:ref "interval-count"}
            (map (fn [n] (dom/option nil (inc n))) (range 10)))
          (dom/span nil "intervals"))
        (dom/div nil
          (dom/span nil "with an average of")
          (dom/div #js {:ref "slider-container"}
            (om/build slider [0 500 10] {:init-state {:value 50}}))
          (apply dom/select #js {:ref "interval-type"}
            (map (fn [type] (dom/option nil type)) ["watts" "rpm" "mph" "bpm"]))
          (dom/select #js {:ref "interval-operator"
                           :defaultValue "or more"}
            (dom/option nil "or more")
            (dom/option nil "or less")))
        (dom/button #js {:className "big-bertha"
                         :onClick
                         (fn [e]
                           (.preventDefault e)
                           (go (let [laps (js->clj
                                            (<! (request-laps activity-id))
                                            :keywordize-keys true)
                                     slider-container (om/get-node owner "slider-container")
                                     interval-value (. (.querySelector slider-container "input") -value)
                                     interval-count (. (om/get-node owner "interval-count") -value)
                                     interval-operator (. (om/get-node owner "interval-operator") -value)
                                     interval-type (. (om/get-node owner "interval-type") -value)]
                                (analyzer {:laps laps
                                           :interval-operator interval-operator
                                           :type interval-type
                                           :target interval-value
                                           :interval-count interval-count
                                           :out analysis-chan}))))}
                      "Analyze ride")))))

(defn activity-view [activity owner]
  (reify
    om/IInitState
    (init-state [_]
      {:analysis-chan (chan)
       :status :pending
       :message "Ride not analyzed yet."})
    om/IWillMount
    (will-mount [_]
      (let [analysis-chan (om/get-state owner :analysis-chan)]
        (go (while true
          (let [analysis-result (<! analysis-chan)]
            (om/set-state! owner :status (:status analysis-result))
            (om/set-state! owner :message (:reason analysis-result)))))))
    om/IRenderState
    (render-state [this {:keys [analysis-chan]}]
      (let [color (case (om/get-state owner :status)
                    :pending "rgba(255, 255, 0, 0.1)"
                    :success "rgba(0, 255, 0, 0.1)"
                    :fail    "rgba(255, 0, 0, 0.1)")
            message (dom/p nil "Analysis: " (om/get-state owner :message))]
        (dom/div #js {:style #js {:background-color color}
                      :className "activity-container cf"}
          message
          (dom/div #js {:className "details"}
            (dom/a #js {:href (str "http://www.strava.com/activities/"
                                   (:id activity))
                        :target "_blank"}
                   (:name activity))
            (dom/p nil (:start_date_local activity)))
          (om/build plan-view (:id activity) {:init-state {:analysis-chan analysis-chan}}))))))

(defn main-view [app owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (request-athlete app)
      (request-activities app))
    om/IRender
    (render [_]
      (dom/div nil
        (om/build athlete-view (:athlete app))
        (apply css-trans-group #js {:transitionName "example"}
          (om/build-all activity-view (:activities app)))))))

(defn main []
  (om/root main-view app-state 
           {:target (.getElementById js/document "app")}))

