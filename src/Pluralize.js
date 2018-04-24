'use strict';

// module Pluralize

exports.pluralize = function(word) {
    return function(count) {
        return function(inclusive) {
            return require('pluralize')(word, count, inclusive)
        }
    }
}

