(ns transit.response)

(defn schedule-for [route ta tm sa sm]
  (map (fn [tix]
         (let [trip (get ta tix)]
           {:trip-id (:trip-id trip)
            :service-id (:service-id trip)
            :route-id (:route-id trip)
            :schedules (map (fn [six]
                              (let [st (get sa six)]
                                {:stop-id (:stop-id st)
                                 :arrival (:arrival st)
                                 :departure (:departure st)}))
                            (get sm (:trip-id trip)))}))
       (get tm route)))
