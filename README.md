
# D2L Tweaks

Adds a few Quality of Life tweaks to the online educational platform Brightspace Desire2Learn (D2L).

Currently:
* Makes the content view page larger to accomodate zoomed out pages/taller displays.
* Replaces custom pdf/mp4 media players with the Web browser's native players
* Uses native PDF viewer for PDF-rendered MS Word/Excel documents.
* Adds non-dropdown easy download buttons for compatible files.
* Adds non-dropdown 'open in new tab' buttons for compatible files.
* Renders "external pages" inside the content view page (embedded within an iframe) rather than just showing a link.

## Installing:
Developed against the [Tampermonkey Chrome extension](https://chrome.google.com/webstore/detail/dhdgffkkebhmkfjojejmpbldmpobfkfo).

Simply open the raw version of [d2l-tweaks.user.js](https://github.com/csm123199/d2l-tweaks/raw/master/d2l-tweaks.user.js), and Tampermonkey should prompt you to install the script.

Tampermonkey should occasionally update the script as necessary, if enabled.

The same steps should work with the [Greasemonkey Firefox extension](https://www.greasespot.net/). Automatic updates seem to work with Greasemonkey, as well.

I recommend installing the [HTML Video Keyboard Shortcuts](https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae) Chrome extension.
Notably, this extension adds hotkeys ( `<` and `>` ) to speed-up/slow-down the video. There are many other hotkeys listed on the extension's store page.
