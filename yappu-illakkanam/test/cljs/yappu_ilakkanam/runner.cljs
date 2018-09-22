(ns cljs-reframe.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [cljs-reframe.core-test]))

(doo-tests 'cljs-reframe.core-test)
