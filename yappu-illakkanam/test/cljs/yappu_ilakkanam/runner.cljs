(ns yappu-ilakkanam.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [yappu-ilakkanam.core-test]))

(doo-tests 'yappu-ilakkanam.core-test)
