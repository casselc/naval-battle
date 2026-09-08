(ns naval-battle.instrumentation-test-runner
  (:require [clojure.test :as test]
            [naval-battle.instrumentation-test]
            [naval-battle.instrumentation-report-test]))

(defn -main [& _]
  (let [result (test/run-tests 'naval-battle.instrumentation-test
                               'naval-battle.instrumentation-report-test)]
    (System/exit (if (zero? (+ (:fail result) (:error result))) 0 1))))
