(ns transit.run
  (:require [transit.parser :as parser]
            [transit.response :as response]
            [cheshire.core :refer :all]
            [org.httpkit.server :refer :all]
            [compojure.core :refer :all]))


(defn -main
  []
  (let [td (parser/read-trips "../MBTA_GTFS/trips.txt")
        sd (parser/read-stoptimes "../MBTA_GTFS/stop_times.txt")
        schedule-handler (fn [req]
                           {:status 200
                            :headers {"Content-Type" "application/json"}
                            :body (cheshire.core/encode (response/schedule-for
                                                         (last (clojure.string/split (:uri req) #"/"))
                                                         (:a td) (:m td) (:a sd) (:m sd)))})
        sch-route (GET "/schedules/*" [] schedule-handler)]

    (run-server sch-route {:port 4000})))
