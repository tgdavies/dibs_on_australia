"use strict";

// module Control.Monad.Eff.AUI

exports.dialog = function(p) {
    var dlg = new AJS.Dialog({
        width: p.width,
        height: p.height,
        id: p.id,
        closeOnOutsideClick: p.closeOnOutsideClick
    });
    dlg.addButton("Cancel", function (d) {
        d.hide();
    });
    dlg.addPanel("SinglePanel", "<p>" + p.contents + "</p>", "singlePanel");

    dlg.show();
};