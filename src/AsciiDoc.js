"use strict";

// module AsciiDoc

exports.convert = function (content) {
    var options = Opal.hash({doctype: 'inline', attributes: ['showtitle']});
    var html = Opal.Asciidoctor.$convert(content, options);
    return html;
};

exports.mkViewer = function (content) {
    return function (node) {
        return function () {
            node.innerHTML = exports.convert(content);
        };
    };
};

exports.mkEditor = function (content) {
    return function (node) {
        return function () {
            var editor = ace.edit(node);
            editor.session.setMode("ace/mode/asciidoc");
            editor.setValue(content);
        };
    };
};
