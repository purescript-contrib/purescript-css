/* global exports */
"use strict";

// module Main


exports.addStyleSheet = function(s){
  return function () {
    var e = document.createElement('style');
    e.appendChild(document.createTextNode(s));
    document.head.appendChild(e);
  };
}

exports.titleWidth = function(){
  return document.getElementById('title').offsetWidth;
}

exports.titleHeight = function(){
  return document.getElementById('title').offsetHeight;
}

exports.titleStyle = function(s){
  return function () {
    document.getElementById('title').setAttribute('style', s);
  };
}
