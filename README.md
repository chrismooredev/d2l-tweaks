
# D2L Tweaks

Adds a few Quality of Life tweaks to the online educational platform Brightspace Desire2Learn (D2L).

### Currently implemented (only affects pages under the Content section):
* Makes the content view page longer to accomodate zoomed out pages/taller displays.
* Replaces custom PDF/MP4 media viewers with the web browser's native viewers.
* Uses native PDF viewer for PDF-rendered MS Word/Excel documents.
	* (Google Chrome) Uses an in-browser document editor, if installed.
* If possible, replaces the title bar dropdown with direct links/buttons.
* For external pages, renders the page inside D2L rather than simply providing a link.
* Provides a link to go to the direct Quiz page.

### Planned:
* Enable hovering to show images, for the Table of Contents

## Installing:
1. Install Tampermonkey (for [Chrome](https://chrome.google.com/webstore/detail/dhdgffkkebhmkfjojejmpbldmpobfkfo) or [Firefox](https://addons.mozilla.org/en-US/firefox/addon/tampermonkey/)) or Greasemonkey (for [Firefox](https://addons.mozilla.org/en-US/firefox/addon/greasemonkey/))
	* Note that this script is not actively tested to work with Firefox/Greasemonkey, though it should be largely compatible. Bugs should be raised on Github as issues or pull-requests.
2. Open [d2l-tweaks.user.js](https://github.com/csm123199/d2l-tweaks/raw/master/d2l-tweaks.user.js)
3. Tampermonkey/Greasemonkey should prompt you to install the script.
	* If enabled, the script should occasionally auto-update if new versions are released.

## Recommended Extensions
* HTML Video Keyboard Shortcuts (for [Chrome](https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae)) for shortcuts on embedded videos.
	* Notably, it adds '<' and '>' to adjust video playback speed.
* Office Editing for Docs, Sheets & Slides (for [Chrome](https://chrome.google.com/webstore/detail/gbkeegbaiigmenfmjfclcdgdpimamgkj)) to enable in-browser interactive office documents.
	* If you have the extension but don't want this script to use it, find this line in the source and change it to `false`.
		* `OFFICE_DOCUMENTS_DIRECT_VIEW_USES_EXTENSION = true`
* Markdown Here (for [Chrome](https://chrome.google.com/webstore/detail/elifhakcjgalahccnjkneoccemfahfoa), [Firefox](https://addons.mozilla.org/en-US/firefox/addon/markdown-here/)) to enable authoring textboxes like Discussion board posts in markdown.

## Developing
* Clone the repository
* Run `npm install` in the repo's directory
* Edit code, and run `tsc` to compile it. (`tsc --watch` for it to auto-compile on changes)
* For quick iteration, you may want to install a [custom userscript](https://gist.github.com/csm123199/bdb49c7bd5973f99a41bb8532d34f055/raw/2bc4a93af6320abe0ff9642ee8813b3316fe87d9/test-d2l-tweaks.user.js) that loads the project from disk. (Change the path after the `@require`)
