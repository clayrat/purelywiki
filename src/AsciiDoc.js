"use strict";

// module AsciiDoc

exports.convert = function (content) {
    var options = Opal.hash({doctype: 'inline', attributes: ['showtitle']});
    var html = Opal.Asciidoctor.$convert(content, options);
    return html;
};

exports.unsafeConvertToNode = function (content) {
    return function (node) {
        return function () {
            node.innerHTML = exports.convert(content);
        }
    };
};
