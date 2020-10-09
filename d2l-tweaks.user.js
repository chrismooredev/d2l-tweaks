"use strict";
// Maybe these will be useful for a non-content page? Keeping around for now
function urlOfD2LAsset(cls, asset) {
    if (!Number.isFinite(Number.parseInt(cls))) {
        throw new Error(`D2L class ID isn't parsable to a number/ID: '${cls}'`);
    }
    if (!Number.isFinite(Number.parseInt(asset))) {
        throw new Error(`D2L asset ID isn't parsable to a number/ID: '${asset}'`);
    }
    // the ?stream=true tells D2L to use a response header for content to be viewed in the browser, rather than downloaded
    let assetURL = newURL(`/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`);
    return [
        assetURL,
        D2LAssetMeta(assetURL),
    ];
}
async function D2LAssetMeta(location) {
    let f = await fetch(location.toString(), { method: 'HEAD' });
    // get mime and orig file name, if available
    let type = f.headers.get("content-type");
    let filename = f.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
    let size = Number.parseInt(f.headers.get("content-length"));
    return { type, filename, size };
}
/** Represents a D2L Content page. Distinguished by a `/d2l/le/content/$class/viewContent/$asset/View` url*/
class ContentPage {
    constructor(cls, asset) {
        let cv = document.querySelector("#ContentView");
        if (!cv) {
            throw new Error("Page doesn't have a #ContentView !");
        }
        let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h");
        if (!hdrBar)
            throw new Error("Unable to find content header bar!");
        this.contentView = cv;
        this.hdrBar = hdrBar;
        this.cls = cls;
        this.asset = asset;
        this.naiveAssetURL = newURL(`/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`);
        this.officeURL = null;
        this.interactiveURL = this._interactiveURL();
        this.downloadableURL = this._downloadableURL();
        this._naiveAssetMeta = undefined;
        this.replacedContent = false;
        console.log('Header dropdown actions: ', this.headerDropdownActions());
    }
    static initialize() {
        let ptype = getPageType();
        if (ptype.type == "content") {
            return new ContentPage(ptype.class, ptype.asset);
        }
        return null;
    }
    get title() {
        let title = this.hdrBar.querySelector('.d2l-page-title');
        return title.innerText;
    }
    normalizeContent() {
        if (this.interactiveURL && !this.replacedContent) {
            this.replaceContent(this.interactiveURL);
        }
    }
    addHeaderButtons() {
        // replace inner content
        // open in new tab
        // download
        const btnBar = document.createElement('span');
        btnBar.style.marginLeft = "0.3rem"; // arbitrary - looked alright for a few different title names
        const append = (ele) => btnBar.appendChild(ele);
        if (this.interactiveURL) {
            console.log("Adding interactive buttons");
            if (!this.replacedContent) {
                // only show button if we haven't replaced ourselves, and we have an interactive URL to show
                append(this.titleBtn_replaceContent("Use Native Viewer", this.interactiveURL));
            }
            append(this.titleLink("View Directly", this.interactiveURL));
            if (this.officeURL) {
                append(this.titleLink("View Interactive", this.officeURL));
            }
        }
        if (this.downloadableURL) {
            console.log("Adding downloadable buttons");
            append(this.titleLink("Download", this.downloadableURL, d2l_icon_download()));
            let hdrBtns = this.headerDropdownActions();
            if (hdrBtns && hdrBtns.length == 1 && hdrBtns[0] == "Download") {
                // remove dropdown if there is only 'Download' - we are supplying our own button above
                this.hdrBar.querySelector('d2l-dropdown[data-contextmenuid=d2l_pageTitleActions]')?.remove();
            }
        }
        this.hdrBar.style.flexWrap = 'wrap';
        this.hdrBar.style.flexDirection = 'row';
        this.hdrBar.appendChild(btnBar);
    }
    /** Returns a URL appropriate to embed in an IFrame, or open in a new browser tab.
     *
     * Most importantly, it doesn't serve the `Content-Disposition: attachment` header.
    */
    _interactiveURL() {
        // TODO: Get mime type and check against navigator.mimeTypes ?
        let asQuiz = this.quizUrl();
        if (asQuiz)
            return asQuiz;
        let asMP4 = this.mp4Url();
        if (asMP4)
            return asMP4;
        let asPDF = this.pdfUrl();
        // office should only show via button
        if (asPDF) {
            if (!asPDF[1])
                this.officeURL = this.officeUrl();
            return asPDF[0];
        }
        let asExtPage = this.extPageUrl();
        if (asExtPage)
            return asExtPage;
        console.log("Page not recognized as interactive");
        return null;
    }
    /** Returned URL should ideally serve the `Content-Disposition: attachment` header */
    _downloadableURL() {
        let asExtPage = this.extPageUrl();
        if (asExtPage)
            return null;
        //if(this.quizUrl()) return null;
        // If this page wraps D2L another D2L page?
        // eg) quizzes, assignments, etc placed in Content
        if (document.querySelector("#ContentView > .d2l-placeholder"))
            return null;
        if (this.extPageUrl())
            return null;
        return this.naiveFileURL(false);
    }
    /** Unneeded? */
    _assetMeta() {
        if (this._naiveAssetMeta)
            return this._naiveAssetMeta;
        return this._naiveAssetMeta = fetch(this.naiveAssetURL.toString(), { method: 'HEAD' })
            .then(res => {
            if (res.status == 404) {
                return null;
            }
            else {
                // get mime and orig file name, if available
                let type = res.headers.get("content-type");
                let filename = res.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
                let size = Number.parseInt(res.headers.get("content-length"));
                return { type, filename, size };
            }
        });
    }
    /** Returns the human-readable labels for the content's dropdown actions */
    headerDropdownActions() {
        // only works if the dropdown has been activated
        //Array.from(this.hdrBar.querySelectorAll('d2l-dropdown d2l-dropdown-menu d2l-menu d2l-menu-item'))
        //	.map(e => e.getAttribute('text'))
        // Access unrendered (templated) dropdown actions, and get their text.
        let templ = this.hdrBar.querySelector('template#d2l_pageTitleActions');
        if (!templ)
            return null;
        return Array.from(templ.content.querySelectorAll('d2l-menu-item[text]'))
            .map(e => e.getAttribute('text'));
        /* currently known:
            file content: "Download"
            quiz: "View Summary"
            quiz: "View Submissions"
            quiz: "View Reports"
        */
    }
    // Keep these organized roughly in the order of least->most expensive to run
    quizUrl() {
        let quiz_iframe = this.contentView.querySelector('#QuizContentViewPlaceHolder > iframe[src]');
        if (quiz_iframe) {
            let url = newURL(quiz_iframe.getAttribute('src'));
            // Ensure a regular D2L page, rather than without headers/margins/borders/etc
            url.searchParams.delete('cfql');
            url.searchParams.delete('dnb');
            url.searchParams.delete('contentTopicId');
            url.searchParams.delete('d2l_body_type');
            return url;
        }
        return null;
    }
    // Interactive only - no content-disposition
    mp4Url() {
        let players = this.contentView.querySelectorAll("div.vui-mediaplayer[data-mediaplayer-src]");
        if (players.length == 0) {
            return null;
        }
        else if (players.length > 1) {
            throw new Error('More than 1 media player found on the page!');
        }
        let player = players[0];
        return newURL(player.getAttribute('data-mediaplayer-src'));
    }
    /** Interactive. May be handled by an extension */
    officeUrl() {
        /*
         * There's currently a bug in the `Office Editing for Docs, Sheets, & Slides` extension that prevents it from parsing the 'Content-Disposition' header correctly.
         * While it doesn't affect functionality, the filename is mangled from what it is supposed to be.
         *
         * We could try to serve the file over `/content/enforced/${class_name}/${div[data-title]}` but there isn't an easy(?) way to get $class_name from the page (D2L JS api?)
         * MP4 files seem to be served from this folder, however.
        */
        if (CAPABILITIES.extensions.office_viewer) {
            let asPDF = this.pdfUrl();
            if (asPDF && !asPDF[1]) {
                // if we aren't a native PDF - we might be a converted office PDF?
                // check in pdfUrl ensures there is only 1 element that matches this
                let viewer = this.contentView.querySelectorAll("div[class^=d2l-fileviewer-pdf-][data-location]")[0];
                let ext = viewer.getAttribute('data-title')?.split('.').slice(-1)[0];
                if (['doc', 'dot', 'xls', 'xlt', 'csv', 'ppt', 'pot', 'pps', 'sld'].some(s => ext?.startsWith(s))) {
                    return this.naiveFileURL(true);
                    //return newURL(`/content/enforced/`)
                    let title = document.createElement('title');
                    title.innerText = this.title;
                    let body = document.createElement('body');
                    body.style.margin = '0';
                    let frame = document.createElement('iframe');
                    frame.src = this.naiveFileURL(true).toString();
                    frame.setAttribute('allowfullscreen', '1');
                    frame.style.border = '0';
                    frame.style.width = '100%';
                    frame.style.height = '100%';
                    let doc = `
						<html>
							<head>${title.outerHTML}</head>
							<body>${frame.outerHTML}</body>
						</html>
					`;
                    let asBase64 = btoa(doc);
                    return new URL('data:text/html;base64,' + asBase64);
                }
            }
        }
        return null;
    }
    /** Interactive only. - no content-disposition.
     *
     * May be downloadable iff `pdfUrl[1]`
     */
    pdfUrl() {
        // native pdf: .d2l-fileviewer-pdf-native
        // converted to pdf: .d2l-fileviewer-pdf-pdfjs
        // Could we determine if native/converted via the data-location URL?
        // Files converted to PDF tend to be served over AWS
        // Get PDF viewers on the page, and ensure there is only one for our content
        let pdfViewers = this.contentView.querySelectorAll("div[class^=d2l-fileviewer-pdf-][data-location]");
        if (pdfViewers.length == 0) {
            return null;
        }
        else if (pdfViewers.length > 1) {
            throw new Error('More than 1 PDF Fileviewer with a data location found on the page!');
        }
        let viewer = pdfViewers[0];
        // Extract the type of contents our PDF viewer is showing
        let type = Array.from(viewer.classList)
            .filter(s => s.startsWith('d2l-fileviewer-pdf-'))
            .map(s => s.split('-').slice(-1)[0]); // get last component
        if (type.length != 1) { // >1 because we filter to this class prefix
            throw new Error('More than one D2L PDF Fileviewers on the page');
        }
        // return a tuple of [url, isNativePDF]
        let location = newURL(viewer.getAttribute('data-location'));
        switch (type[0]) {
            case "native": return [location, true];
            case "pdfjs": return [location, false];
            default: throw new Error('Unknown PDF viewer type: ' + type[0]);
        }
    }
    /** Only interactive */
    extPageUrl() {
        let itext = this.contentView.innerText;
        if (!itext.includes("External Resource") || !itext.includes("Open in New Window")) {
            return null;
        }
        // reasonably sure this page's content is an external page - throw on any errors
        let urls = Object.values(D2L.OR.__g1)
            .map(s => {
            try {
                return JSON.parse(s);
            }
            catch (e) {
                throw new Error("D2L.OR.__g1 contains malformed JSON!");
            }
        })
            .filter((o) => {
            return o._type == "func" && o.N == "D2L.LE.Content.Desktop.Topic.OpenInNewWindow" && o.P.length == 1;
        })
            .map(o => o.P[0]);
        if (urls.length == 0) {
            throw new Error(`Content View showing "External Resource" without any OpenInNewWindow functions in D2L.OR.__g1!`);
        }
        else if (urls.length > 1) {
            throw new Error(`Multiple OpenInNewWindow commands for "External Resource" content. Not returning any. (${JSON.stringify(urls)})`);
        }
        else if (typeof urls[0] != 'string') {
            throw new Error(`Single OpenInNewWindow command parameter isn't a string! (${JSON.stringify(urls[0])})`);
        }
        let url = newURL(urls[0]);
        url.protocol = "https"; // otherwise iframe won't load b/c D2L is HTTPS
        if (url.host.includes("youtube.com") && url.pathname == "/watch") {
            let video_id = url.searchParams.get("v");
            if (video_id) {
                url.searchParams.delete("v");
                url.pathname = "/embed/" + video_id;
            }
        }
        return url;
    }
    /** Only downloadable
     *
     * Doesn't check for 404 - should be used after detection of external pages/etc
    */
    naiveFileURL(stream) {
        let url = newURL(this.naiveAssetURL.toString()); // clone URL
        url.searchParams.set("stream", stream.toString());
        return url;
    }
    /** DOM manipulation helpers */
    replaceContent(iframe_src) {
        let newContent;
        let ifram = document.createElement('iframe');
        ifram.src = iframe_src.toString();
        ifram.style.width = '100%';
        ifram.style.height = '90vh';
        ifram.style.resize = 'both';
        ifram.setAttribute('preload', 'auto');
        ifram.setAttribute('allowfullscreen', 'true'); // for media players (eg: embedded youtube)
        newContent = ifram;
        let cv = this.contentView;
        while (cv.lastChild) {
            cv.removeChild(cv.lastChild);
        } // remove existing content
        cv.appendChild(newContent); // add self to dom
        // Attempt to preserve aspect ratio
        // Must be done after appending to the DOM (.appendChild) so .offsetWidth works
        if (iframe_src.pathname.endsWith('.mp4')) {
            let width = ifram.offsetWidth;
            // assume videos are 16:9 aspect ratio
            ifram.style.height = (width * (9 / 16)) + 'px';
        }
        this.replacedContent = true;
        return ifram;
    }
    titleBtn(text, onclick) {
        let btn = document.createElement('button');
        btn.innerText = text;
        btn.classList.add('d2l-button');
        btn.style.marginLeft = '0.25rem'; // 5px matches .d2l-contextmenu-ph
        btn.style.marginRight = '0.25rem';
        btn.style.width = 'auto';
        btn.addEventListener('click', onclick);
        return btn;
    }
    titleLink(text, href, prependNode) {
        let link = document.createElement('a');
        link.innerText = text;
        if (prependNode)
            link.prepend(prependNode);
        link.href = href.toString();
        link.classList.add('d2l-button');
        link.style.marginLeft = '0.25rem';
        link.style.marginRight = '0.25rem';
        link.style.width = 'auto';
        return link;
    }
    /** Adds a button to use a native viewer for the provided URL. Automatically removes itself from the DOM when clicked. */
    titleBtn_replaceContent(label, src) {
        const that = this;
        function btnonclick() {
            // installed as onclick handler => this = `<button>...</button>`
            // remove ourselves (button) since we have served our purpose
            this.remove();
            that.replaceContent(src);
        }
        ;
        // Note: put button in immediatly, provide link after PDF was downloaded
        return this.titleBtn(label, btnonclick);
    }
}
const general_css = `
	/* eliminate some whitespace */
	/* reduce_toc_padding */
		html .daylight #D2L_LE_Content_TreeBrowser .d2l-le-TreeAccordionItem,
		html .daylight #ContentPluginTree .d2l-le-TreeAccordionItem {
			padding: 5px 0 5px 0;
		}

	/* section heading */
		html .daylight .d2l-header-top .d2l-box-h {
			margin-top: 1rem;
			margin-bottom: 1rem;
		}
		html .d2l-page-header {
			margin-bottom: 0.5rem;
		}
		html .d2l-typography .d2l-htmlblock  h2 {
			margin: 0.5rem 0;
		}

	/* section contents */
		html .d2l-datalist-style1 > .d2l-datalist .d2l-datalist-item-content {
			padding-top: 0.25rem;
			padding-bottom: 0.25rem; 
		}
		html .d2l-typography .d2l-datalist-item-content .d2l-htmlblock p {
			margin: 0.5em 0;
		}

	/* discussions */
		html #ForumsTopicsPlaceholder .d2l-sep {
			display: none;
		}
		html .d2l-forum-details {
			padding-top: 0;
		}
	html .d2l-page-header {
		margin-bottom: 0;
	}
`;
const csstweaks = {
    content: function () {
    },
    content_toc: function () {
        // include `html` to beat the specificity of the built-in rules
        // https://developer.mozilla.org/en-US/docs/Web/CSS/Specificity
        const str = `
			/* Make the ToC pointer more visibile, remove annoyingly styled gradient */
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor::before ,
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor:link::before {
					visibility: hidden;
				}
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor::after,
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor:link::after,
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor:visited::after,
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor:hover::after,
				html .daylight #ContentModuleTree .d2l-le-TreeAccordionItem.d2l-le-TreeAccordionItem-Selected > a.d2l-le-TreeAccordionItem-anchor:focus::after {
					filter: invert(50%); /* basically make it gray */
				}

	
			
		`;
        let style = document.createElement('style');
        style.setAttribute('type', 'text/css');
        style.innerHTML = str;
        document.head.appendChild(style);
        console.log("Installing css tweaks to: ", style);
    },
};
/** Traverses the DOM finding each parent element with `display: none` and toggles it off for the duration of the callback. */
function asVisible(target, f) {
    let isVisible = (target) => target.offsetParent !== null;
    if (isVisible(target)) {
        // If we're visible, we're done - call the function
        return f();
    }
    else {
        if (getComputedStyle(target).getPropertyValue('display') === 'none') {
            let [oldV, oldP] = [target.style.getPropertyValue('display'), target.style.getPropertyPriority('display')];
            target.style.setProperty('display', 'inherit', '!important'); // better value?
            // try again - maybe we're visible now?
            let rtn = asVisible(target, f);
            target.style.setProperty('display', oldV, oldP);
            return rtn;
        }
        else {
            // we aren't responsible for our invisibility - is our parent?
            return asVisible(target.parentElement, f);
        }
    }
}
// ==UserScript==
// @name         D2L Tweaks
// @namespace    https://github.com/csm123199/d2l-tweaks
// @version      0.9
// @description  Add QoL changes to D2L's user interface
// @author       Chris Moore
// @include      https://*.edu/d2l/*
// @grant        none
// @updateUrl    https://raw.githubusercontent.com/csm123199/d2l-tweaks/master/d2l-tweaks.user.js
// ==/UserScript==
// D2L rest api docs
// https://docs.valence.desire2learn.com/res/content.html
/// <reference path="./d2l-globals.d.ts" />
/* Config */
const MAKE_NATIVE_ON_LOAD = true;
const OFFICE_DOCUMENTS_DIRECT_VIEW_USES_EXTENSION = false;
/* Code */
// Ensure the page we're on is a valid D2L page.
// If your institution doesn't match, please submit a PR adding a generic check for yours.
let url = newURL(document.location);
if (url.protocol != 'https:' || !url.host.endsWith('.edu') || !url.pathname.startsWith('/d2l')) {
    throw new Error(`Bad host for D2L Script (exiting via exception): '${url.host}'`);
}
const CAPABILITIES = initCapabilities();
(async function () {
    'use strict';
    if (!withinIframe()) {
        suggestHTML5VideoKeyboardShortcuts();
        suggestOfficeEditing();
        try {
            let ptype = getPageType();
            if (ptype.type == "content") {
                csstweaks.content();
                let contentPage = new ContentPage(ptype.class, ptype.asset);
                console.log("Content page: ", contentPage);
                if (MAKE_NATIVE_ON_LOAD)
                    contentPage.normalizeContent();
                contentPage.addHeaderButtons();
            }
            else if (ptype.type == "content_toc") {
                csstweaks.content_toc();
            }
            else {
                console.warn("[D2L Tweaks] Unknown page type.");
            }
        }
        catch (e) {
            console.error("Error occured in userscript", e);
            //alert("Error occured in D2L bettering userscript, error in console.");
        }
    }
})();
function initCapabilities() {
    // I realize I shouldn't be using the userAgent to detect browser flavors.
    // It'll be good enough for this.
    let useragent = navigator.userAgent.toLowerCase();
    // @ts-ignore
    if (useragent.includes('chrome')) {
        const mt = (app) => navigator.mimeTypes.namedItem('application/' + app) !== null;
        return {
            extensions: {
                // detect if chrome supports viewing MS office documents (via extension/etc)
                office_viewer: mt('msword') && mt('msexcel') && mt('mspowerpoint') && mt('msword-template'),
                //office_viewer: chromeExtensionInstalled('gmpljdlgcdkljlppaekciacdmdlhfeon'),
                video_shortcuts: chromeExtensionInstalled('llhmaciggnibnbdokidmbilklceaobae'),
            },
            browser: "chrome",
        };
    }
    else if (useragent.includes('firefox')) {
        return {
            // no way to know
            extensions: {
                office_viewer: false,
                video_shortcuts: true,
            },
            browser: "firefox",
        };
    }
    return {
        extensions: { office_viewer: false, video_shortcuts: false, },
        browser: "unknown",
    };
}
function suggestHTML5VideoKeyboardShortcuts() {
    // Only suggest on chrome, b/c firefox users exist
    // (and firefox already has right-click, change speed controls)
    if (!chromeExtensionInstalled('llhmaciggnibnbdokidmbilklceaobae')) {
        console.warn(`D2L userscript recommends installing the "HTML5 Video Keyboard Shortcuts" extension for speeding up videos`, `https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae`);
    }
}
function suggestOfficeEditing() {
    // Only suggest on chrome, b/c firefox users exist
    if (!chromeExtensionInstalled('gbkeegbaiigmenfmjfclcdgdpimamgkj')) {
        console.warn(`D2L userscript recommends installing the "Office Editing for Docs, Sheets & Slides" extension for interactively viewing office documents`, `https://chrome.google.com/webstore/detail/gbkeegbaiigmenfmjfclcdgdpimamgkj`);
    }
}
function chromeExtensionInstalled(id) {
    try {
        // @ts-ignore
        if (window.chrome) {
            window.chrome.runtime.sendMessage(id, null);
            return true;
        }
    }
    catch (e) {
        if (!e.message.includes("Invalid extension id")) {
            throw e;
        }
    }
    return false;
}
/** Useful for early exit if we aren't the top frame */
function withinIframe() {
    // https://stackoverflow.com/a/326076/11536614
    try {
        return window.self !== window.top;
    }
    catch (e) {
        return true; // access denied error
    }
}
function d2l_icon(backgroundImage) {
    let i = document.createElement('span');
    i.classList.add('d2l-icon-custom');
    i.style.backgroundImage = backgroundImage;
    i.style.backgroundPosition = "0 0";
    i.style.backgroundRepeat = "no-repeat";
    i.style.width = "18px";
    i.style.height = "18px";
    i.style.backgroundSize = "18px 18px";
    return i;
}
function d2l_icon_download() {
    return d2l_icon("url('https://s.brightspace.com/lib/bsi/20.20.8-85/images/tier1/download.svg')");
}
/** The URL interface represents an object providing static methods used for creating object URLs. */
function newURL(url) {
    if (typeof url == 'string')
        return new URL(url, document.location.href);
    else if (url instanceof URL) {
        return new URL(url.href);
    }
    else if (url instanceof Location) {
        return new URL(url.href);
    }
    else {
        throw new Error('unknown parameter type');
    }
}
/** Return the type of D2L page we are on, with some appropriate metadata about it */
function getPageType() {
    let href = newURL(document.location);
    // slice to remove zero-length component at beginning (since pathnames start with '/')
    let components = href.pathname.split('/').slice(1);
    if (components.length == 7
        && components[0] == "d2l"
        && components[1] == "le"
        && components[2] == "content"
        // class ID
        && components[4] == "viewContent"
        // asset ID
        && components[6] == "View") {
        return { type: "content", class: components[3], asset: components[5], };
    }
    else if (components.length == 5
        && components[0] == "d2l"
        && components[1] == "le"
        && components[2] == "content"
        // class ID
        && components[4] == "Home") {
        return { type: "content_toc", class: components[3] };
    }
    return { type: "unknown" };
}
;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZDJsLXR3ZWFrcy51c2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic3JjL2NvbnRlbnQtcGFnZS50cyIsInNyYy9jc3MtdHdlYWtzLnRzIiwic3JjL2QybC10d2Vha3MudXNlci50cyIsInNyYy9oZWxwZXJzLnRzIiwic3JjL3R5cGVzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFDQSw0RUFBNEU7QUFDNUUsU0FBUyxhQUFhLENBQUMsR0FBVyxFQUFFLEtBQWE7SUFDaEQsSUFBSSxDQUFDLE1BQU0sQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxHQUFHLENBQUMsQ0FBQyxFQUFFO1FBQzNDLE1BQU0sSUFBSSxLQUFLLENBQUMsZ0RBQWdELEdBQUcsR0FBRyxDQUFDLENBQUM7S0FDeEU7SUFDRCxJQUFJLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxNQUFNLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxDQUFDLEVBQUU7UUFDN0MsTUFBTSxJQUFJLEtBQUssQ0FBQyxnREFBZ0QsS0FBSyxHQUFHLENBQUMsQ0FBQztLQUMxRTtJQUVELHNIQUFzSDtJQUN0SCxJQUFJLFFBQVEsR0FBRyxNQUFNLENBQUMsb0JBQW9CLEdBQUcsbUJBQW1CLEtBQUssT0FBTyxDQUFDLENBQUM7SUFDOUUsT0FBTztRQUNOLFFBQVE7UUFDUixZQUFZLENBQUMsUUFBUSxDQUFDO0tBQ3RCLENBQUE7QUFDRixDQUFDO0FBQ0QsS0FBSyxVQUFVLFlBQVksQ0FBQyxRQUFhO0lBQ3hDLElBQUksQ0FBQyxHQUFHLE1BQU0sS0FBSyxDQUFDLFFBQVEsQ0FBQyxRQUFRLEVBQUUsRUFBRSxFQUFFLE1BQU0sRUFBRSxNQUFNLEVBQUUsQ0FBQyxDQUFDO0lBRTdELDRDQUE0QztJQUM1QyxJQUFJLElBQUksR0FBRyxDQUFDLENBQUMsT0FBTyxDQUFDLEdBQUcsQ0FBQyxjQUFjLENBQUUsQ0FBQztJQUMxQyxJQUFJLFFBQVEsR0FBRyxDQUFDLENBQUMsT0FBTyxD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QUksS0FBSztXQUN0QixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksSUFBSTtXQUNyQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksU0FBUztRQUM3QixXQUFXO1dBQ1IsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLGFBQWE7UUFDakMsV0FBVztXQUNSLFVBQVUsQ0FBQyxDQUFDLENBQUMsSUFBSSxNQUFNLEVBQ3pCO1FBQ0QsT0FBTyxFQUFFLElBQUksRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFFLFVBQVUsQ0FBQyxDQUFDLENBQUMsRUFBRSxLQUFLLEVBQUUsVUFBVSxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUM7S0FDeEU7U0FBTSxJQUNOLFVBQVUsQ0FBQyxNQUFNLElBQUksQ0FBQztXQUNuQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksS0FBSztXQUN0QixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksSUFBSTtXQUNyQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksU0FBUztRQUM3QixXQUFXO1dBQ1IsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLE1BQU0sRUFDekI7UUFDRCxPQUFPLEVBQUUsSUFBSSxFQUFFLGFBQWEsRUFBRSxLQUFLLEVBQUUsVUFBVSxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUM7S0FDckQ7SUFFRCxPQUFPLEVBQUUsSUFBSSxFQUFFLFNBQVMsRUFBRSxDQUFDO0FBQzVCLENBQUM7QUN2SEEsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIlxyXG4vLyBNYXliZSB0aGVzZSB3aWxsIGJlIHVzZWZ1bCBmb3IgYSBub24tY29udGVudCBwYWdlPyBLZWVwaW5nIGFyb3VuZCBmb3Igbm93XHJcbmZ1bmN0aW9uIHVybE9mRDJMQXNzZXQoY2xzOiBzdHJpbmcsIGFzc2V0OiBzdHJpbmcpOiBbVVJMLCBQcm9taXNlPEQyTEFzc2V0TWV0YWRhdGE+XXtcclxuXHRpZiAoIU51bWJlci5pc0Zpbml0ZShOdW1iZXIucGFyc2VJbnQoY2xzKSkpIHtcclxuXHRcdHRocm93IG5ldyBFcnJvcihgRDJMIGNsYXNzIElEIGlzbid0IHBhcnNhYmxlIHRvIGEgbnVtYmVyL0lEOiAnJHtjbHN9J2ApO1xyXG5cdH1cclxuXHRpZiAoIU51bWJlci5pc0Zpbml0ZShOdW1iZXIucGFyc2VJbnQoYXNzZXQpKSkge1xyXG5cdFx0dGhyb3cgbmV3IEVycm9yKGBEMkwgYXNzZXQgSUQgaXNuJ3QgcGFyc2FibGUgdG8gYSBudW1iZXIvSUQ6ICcke2Fzc2V0fSdgKTtcclxuXHR9XHJcblxyXG5cdC8vIHRoZSA/c3RyZWFtPXRydWUgdGVsbHMgRDJMIHRvIHVzZSBhIHJlc3BvbnNlIGhlYWRlciBmb3IgY29udGVudCB0byBiZSB2aWV3ZWQgaW4gdGhlIGJyb3dzZXIsIHJhdGhlciB0aGFuIGRvd25sb2FkZWRcclxuXHRsZXQgYXNzZXRVUkwgPSBuZXdVUkwoYC9kMmwvYXBpL2xlLzEuMzQvJHtjbHN9L2NvbnRlbnQvdG9waWNzLyR7YXNzZXR9L2ZpbGVgKTtcclxuXHRyZXR1cm4gW1xyXG5cdFx0YXNzZXRVUkwsXHJcblx0XHREMkxBc3NldE1ldGEoYXNzZXRVUkwpLCAvLyByZXR1cm5zIHByb21pc2VcclxuXHRdXHJcbn1cclxuYXN5bmMgZnVuY3Rpb24gRDJMQXNzZXRNZXRhKGxvY2F0aW9uOiBVUkwpOiBQcm9taXNlPEQyTEFzc2V0TWV0YWRhdGE+IHtcclxuXHRsZXQgZiA9IGF3YWl0IGZldGNoKGxvY2F0aW9uLnRvU3RyaW5nKCksIHsgbWV0aG9kOiAnSEVBRCcgfSk7XHJcblxyXG5cdC8vIGdldCBtaW1lIGFuZCBvcmlnIGZpbGUgbmFtZSwgaWYgYXZhaWxhYmxlXHJcblx0bGV0IHR5cGUgPSBmLmhlYWRlcnMuZ2V0KFwiY29udGVudC10eXBlXCIpITtcclxuXHRsZXQgZmlsZW5hbWUgPSBmLmhlYWRlcnMuZ2V0KFwiY29udGVudC1kaXNwb3NpdGlvblwiKT8ubWF0Y2goL2ZpbGVuYW1lPVwiKC4rKVwiLyk/LlsxXSA/PyBudWxsOyAvL251bGwgaWYgaGVhZGVyIG1pc3NpbmcsIG5vIGZpbGVuYW1lLCBldGNcclxuXHRsZXQgc2l6ZSA9IE51bWJlci5wYXJzZUludChmLmhlYWRlcnMuZ2V0KFwiY29udGVudC1sZW5ndGhcIikhKTtcclxuXHJcblx0cmV0dXJuIHsgdHlwZSwgZmlsZW5hbWUsIHNpemUgfTtcclxufVxyXG5cclxuLyoqIFJlcHJlc2VudHMgYSBEMkwgQ29udGVudCBwYWdlLiBEaXN0aW5ndWlzaGVkIGJ5IGEgYC9kMmwvbGUvY29udGVudC8kY2xhc3Mvdmlld0NvbnRlbnQvJGFzc2V0L1ZpZXdgIHVybCovXHJcbmNsYXNzIENvbnRlbnRQYWdlIHtcclxuXHRwcm90ZWN0ZWQgY29udGVudFZpZXc6IEhUTUxFbGVtZW50O1xyXG5cdHByb3RlY3RlZCBoZHJCYXI6IEhUTUxFbGVtZW50O1xyXG5cdHByb3RlY3RlZCBjbHM6IHN0cmluZztcclxuXHRwcm90ZWN0ZWQgYXNzZXQ6IHN0cmluZztcclxuXHQvKiogTWF5IHBvaW50IHRvIGEgNDA0LiBTaG91bGQgYmUgYXZvaWRlZC4gKi9cclxuXHRwcm90ZWN0ZWQgbmFpdmVBc3NldFVSTDogVVJMO1xyXG5cclxuXHRwcm90ZWN0ZWQgaW50ZXJhY3RpdmVVUkw6IFVSTCB8IG51bGw7XHJcblx0cHJvdGVjdGVkIGRvd25sb2FkYWJsZVVSTDogVVJMIHwgbnVsbDtcclxuXHRwcm90ZWN0ZWQgb2ZmaWNlVVJMOiBVUkwgfCBudWxsO1xyXG5cclxuXHRwcm90ZWN0ZWQgX25haXZlQXNzZXRNZXRhPzogUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhIHwgbnVsbD47XHJcblx0cHJvdGVjdGVkIHJlcGxhY2VkQ29udGVudDogYm9vbGVhbjtcclxuXHJcblx0c3RhdGljIGluaXRpYWxpemUoKTogQ29udGVudFBhZ2UgfCBudWxsIHtcclxuXHRcdGxldCBwdHlwZSA9IGdldFBhZ2VUeXBlKCk7XHJcblx0XHRpZihwdHlwZS50eXBlID09IFwiY29udGVudFwiKSB7XHJcblx0XHRcdHJldHVybiBuZXcgQ29udGVudFBhZ2UocHR5cGUuY2xhc3MsIHB0eXBlLmFzc2V0KTtcclxuXHRcdH1cclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHRjb25zdHJ1Y3RvcihjbHM6IHN0cmluZywgYXNzZXQ6IHN0cmluZykge1xyXG5cdFx0bGV0IGN2ID0gZG9jdW1lbnQucXVlcnlTZWxlY3RvcihcIiNDb250ZW50Vmlld1wiKSBhcyBIVE1MRWxlbWVudCB8IG51bGw7XHJcblx0XHRpZighY3YpIHsgdGhyb3cgbmV3IEVycm9yKFwiUGFnZSBkb2Vzbid0IGhhdmUgYSAjQ29udGVudFZpZXcgIVwiKTsgfVxyXG5cclxuXHRcdGxldCBoZHJCYXIgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yKFwiLmQybC1wYWdlLXRpdGxlLWMgLmQybC1ib3gtaFwiKSBhcyBIVE1MRWxlbWVudCB8IG51bGw7XHJcblx0XHRpZighaGRyQmFyKSB0aHJvdyBuZXcgRXJyb3IoXCJVbmFibGUgdG8gZmluZCBjb250ZW50IGhlYWRlciBiYXIhXCIpO1xyXG5cdFx0XHJcblxyXG5cdFx0dGhpcy5jb250ZW50VmlldyA9IGN2O1xyXG5cdFx0dGhpcy5oZHJCYXIgPSBoZHJCYXI7XHJcblx0XHR0aGlzLmNscyA9IGNscztcclxuXHRcdHRoaXMuYXNzZXQgPSBhc3NldDtcclxuXHRcdHRoaXMubmFpdmVBc3NldFVSTCA9IG5ld1VSTChgL2QybC9hcGkvbGUvMS4zNC8ke2Nsc30vY29udGVudC90b3BpY3MvJHthc3NldH0vZmlsZWApO1xyXG5cclxuXHRcdHRoaXMub2ZmaWNlVVJMID0gbnVsbDtcclxuXHRcdHRoaXMuaW50ZXJhY3RpdmVVUkwgPSB0aGlzLl9pbnRlcmFjdGl2ZVVSTCgpO1xyXG5cdFx0dGhpcy5kb3dubG9hZGFibGVVUkwgPSB0aGlzLl9kb3dubG9hZGFibGVVUkwoKTtcclxuXHJcblx0XHR0aGlzLl9uYWl2ZUFzc2V0TWV0YSA9IHVuZGVmaW5lZDtcclxuXHRcdHRoaXMucmVwbGFjZWRDb250ZW50ID0gZmFsc2U7XHJcblxyXG5cdFx0Y29uc29sZS5sb2coJ0hlYWRlciBkcm9wZG93biBhY3Rpb25zOiAnLCB0aGlzLmhlYWRlckRyb3Bkb3duQWN0aW9ucygpKTtcclxuXHR9XHJcblxyXG5cdHByb3RlY3RlZCBnZXQgdGl0bGUoKTogc3RyaW5nIHtcclxuXHRcdGxldCB0aXRsZSA9IHRoaXMuaGRyQmFyLnF1ZXJ5U2VsZWN0b3IoJy5kMmwtcGFnZS10aXRsZScpISBhcyBIVE1MRWxlbWVudDtcclxuXHRcdHJldHVybiB0aXRsZS5pbm5lclRleHQ7XHJcblx0fVxyXG5cclxuXHRwdWJsaWMgbm9ybWFsaXplQ29udGVudCgpIHtcclxuXHRcdGlmKHRoaXMuaW50ZXJhY3RpdmVVUkwgJiYgIXRoaXMucmVwbGFjZWRDb250ZW50KSB7XHJcblx0XHRcdHRoaXMucmVwbGFjZUNvbnRlbnQodGhpcy5pbnRlcmFjdGl2ZVVSTCk7XHJcblx0XHR9XHJcblx0fVxyXG5cdFxyXG5cdHB1YmxpYyBhZGRIZWFkZXJCdXR0b25zKCkge1xyXG5cdFx0Ly8gcmVwbGFjZSBpbm5lciBjb250ZW50XHJcblx0XHQvLyBvcGVuIGluIG5ldyB0YWJcclxuXHRcdC8vIGRvd25sb2FkXHJcblxyXG5cdFx0Y29uc3QgYnRuQmFyID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnc3BhbicpO1xyXG5cdFx0YnRuQmFyLnN0eWxlLm1hcmdpbkxlZnQgPSBcIjAuM3JlbVwiOyAvLyBhcmJpdHJhcnkgLSBsb29rZWQgYWxyaWdodCBmb3IgYSBmZXcgZGlmZmVyZW50IHRpdGxlIG5hbWVzXHJcblx0XHRjb25zdCBhcHBlbmQgPSAoZWxlOiBIVE1MRWxlbWVudCkgPT4gYnRuQmFyLmFwcGVuZENoaWxkKGVsZSk7XHJcblxyXG5cdFx0aWYodGhpcy5pbnRlcmFjdGl2ZVVSTCkge1xyXG5cdFx0XHRjb25zb2xlLmxvZyhcIkFkZGluZyBpbnRlcmFjdGl2ZSBidXR0b25zXCIpO1xyXG5cdFx0XHRpZighdGhpcy5yZXBsYWNlZENvbnRlbnQpIHtcclxuXHRcdFx0XHQvLyBvbmx5IHNob3cgYnV0dG9uIGlmIHdlIGhhdmVuJ3QgcmVwbGFjZWQgb3Vyc2VsdmVzLCBhbmQgd2UgaGF2ZSBhbiBpbnRlcmFjdGl2ZSBVUkwgdG8gc2hvd1xyXG5cdFx0XHRcdGFwcGVuZCh0aGlzLnRpdGxlQnRuX3JlcGxhY2VDb250ZW50KFwiVXNlIE5hdGl2ZSBWaWV3ZXJcIiwgdGhpcy5pbnRlcmFjdGl2ZVVSTCkpO1xyXG5cdFx0XHR9XHJcblx0XHRcdGFwcGVuZCh0aGlzLnRpdGxlTGluayhcIlZpZXcgRGlyZWN0bHlcIiwgdGhpcy5pbnRlcmFjdGl2ZVVSTCkpO1xyXG5cclxuXHRcdFx0aWYodGhpcy5vZmZpY2VVUkwpIHtcclxuXHRcdFx0XHRhcHBlbmQodGhpcy50aXRsZUxpbmsoXCJWaWV3IEludGVyYWN0aXZlXCIsIHRoaXMub2ZmaWNlVVJMKSk7XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHJcblx0XHRpZih0aGlzLmRvd25sb2FkYWJsZVVSTCkge1xyXG5cdFx0XHRjb25zb2xlLmxvZyhcIkFkZGluZyBkb3dubG9hZGFibGUgYnV0dG9uc1wiKTtcclxuXHRcdFx0YXBwZW5kKHRoaXMudGl0bGVMaW5rKFwiRG93bmxvYWRcIiwgdGhpcy5kb3dubG9hZGFibGVVUkwsIGQybF9pY29uX2Rvd25sb2FkKCkpKTtcclxuXHJcblx0XHRcdGxldCBoZHJCdG5zID0gdGhpcy5oZWFkZXJEcm9wZG93bkFjdGlvbnMoKTtcclxuXHRcdFx0aWYoaGRyQnRucyAmJiBoZHJCdG5zLmxlbmd0aCA9PSAxICYmIGhkckJ0bnNbMF0gPT0gXCJEb3dubG9hZFwiKSB7XHJcblx0XHRcdFx0Ly8gcmVtb3ZlIGRyb3Bkb3duIGlmIHRoZXJlIGlzIG9ubHkgJ0Rvd25sb2FkJyAtIHdlIGFyZSBzdXBwbHlpbmcgb3VyIG93biBidXR0b24gYWJvdmVcclxuXHRcdFx0XHR0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yKCdkMmwtZHJvcGRvd25bZGF0YS1jb250ZXh0bWVudWlkPWQybF9wYWdlVGl0bGVBY3Rpb25zXScpPy5yZW1vdmUoKTtcclxuXHRcdFx0fVxyXG5cdFx0fVxyXG5cclxuXHRcdHRoaXMuaGRyQmFyLnN0eWxlLmZsZXhXcmFwID0gJ3dyYXAnO1xyXG5cdFx0dGhpcy5oZHJCYXIuc3R5bGUuZmxleERpcmVjdGlvbiA9ICdyb3cnO1xyXG5cdFx0dGhpcy5oZHJCYXIuYXBwZW5kQ2hpbGQoYnRuQmFyKTtcclxuXHR9XHJcblx0XHJcblxyXG5cdC8qKiBSZXR1cm5zIGEgVVJMIGFwcHJvcHJpYXRlIHRvIGVtYmVkIGluIGFuIElGcmFtZSwgb3Igb3BlbiBpbiBhIG5ldyBicm93c2VyIHRhYi5cclxuXHQgKiBcclxuXHQgKiBNb3N0IGltcG9ydGFudGx5LCBpdCBkb2Vzbid0IHNlcnZlIHRoZSBgQ29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVudGAgaGVhZGVyLlxyXG5cdCovXHJcblx0cHJvdGVjdGVkIF9pbnRlcmFjdGl2ZVVSTCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdC8vIFRPRE86IEdldCBtaW1lIHR5cGUgYW5kIGNoZWNrIGFnYWluc3QgbmF2aWdhdG9yLm1pbWVUeXBlcyA/XHJcblxyXG5cdFx0bGV0IGFzUXVpeiA9IHRoaXMucXVpelVybCgpO1xyXG5cdFx0aWYoYXNRdWl6KSByZXR1cm4gYXNRdWl6O1xyXG5cclxuXHRcdGxldCBhc01QNCA9IHRoaXMubXA0VXJsKCk7XHJcblx0XHRpZihhc01QNCkgcmV0dXJuIGFzTVA0O1xyXG5cclxuXHRcdGxldCBhc1BERiA9IHRoaXMucGRmVXJsKCk7XHJcblxyXG5cdFx0Ly8gb2ZmaWNlIHNob3VsZCBvbmx5IHNob3cgdmlhIGJ1dHRvblxyXG5cdFx0aWYoYXNQREYpIHtcclxuXHRcdFx0aWYoIWFzUERGWzFdKVxyXG5cdFx0XHRcdHRoaXMub2ZmaWNlVVJMID0gdGhpcy5vZmZpY2VVcmwoKTtcclxuXHRcdFx0cmV0dXJuIGFzUERGWzBdO1xyXG5cdFx0fVxyXG5cclxuXHRcdGxldCBhc0V4dFBhZ2UgPSB0aGlzLmV4dFBhZ2VVcmwoKTtcclxuXHRcdGlmKGFzRXh0UGFnZSkgcmV0dXJuIGFzRXh0UGFnZTtcclxuXHJcblx0XHRjb25zb2xlLmxvZyhcIlBhZ2Ugbm90IHJlY29nbml6ZWQgYXMgaW50ZXJhY3RpdmVcIik7XHJcblxyXG5cdFx0cmV0dXJuIG51bGw7XHJcblx0fVxyXG5cclxuXHQvKiogUmV0dXJuZWQgVVJMIHNob3VsZCBpZGVhbGx5IHNlcnZlIHRoZSBgQ29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVudGAgaGVhZGVyICovXHJcblx0cHJvdGVjdGVkIF9kb3dubG9hZGFibGVVUkwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHRsZXQgYXNFeHRQYWdlID0gdGhpcy5leHRQYWdlVXJsKCk7XHJcblx0XHRpZihhc0V4dFBhZ2UpIHJldHVybiBudWxsO1xyXG5cclxuXHRcdC8vaWYodGhpcy5xdWl6VXJsKCkpIHJldHVybiBudWxsO1xyXG5cdFx0Ly8gSWYgdGhpcyBwYWdlIHdyYXBzIEQyTCBhbm90aGVyIEQyTCBwYWdlP1xyXG5cdFx0Ly8gZWcpIHF1aXp6ZXMsIGFzc2lnbm1lbnRzLCBldGMgcGxhY2VkIGluIENvbnRlbnRcclxuXHRcdGlmKGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoXCIjQ29udGVudFZpZXcgPiAuZDJsLXBsYWNlaG9sZGVyXCIpKSByZXR1cm4gbnVsbDtcclxuXHRcdGlmKHRoaXMuZXh0UGFnZVVybCgpKSByZXR1cm4gbnVsbDtcclxuXHJcblx0XHRyZXR1cm4gdGhpcy5uYWl2ZUZpbGVVUkwoZmFsc2UpO1xyXG5cdH1cclxuXHJcblx0LyoqIFVubmVlZGVkPyAqL1xyXG5cdHByaXZhdGUgX2Fzc2V0TWV0YSgpOiBQcm9taXNlPEQyTEFzc2V0TWV0YWRhdGEgfCBudWxsPiB7XHJcblx0XHRpZih0aGlzLl9uYWl2ZUFzc2V0TWV0YSkgcmV0dXJuIHRoaXMuX25haXZlQXNzZXRNZXRhO1xyXG5cclxuXHRcdHJldHVybiB0aGlzLl9uYWl2ZUFzc2V0TWV0YSA9IGZldGNoKHRoaXMubmFpdmVBc3NldFVSTC50b1N0cmluZygpLCB7IG1ldGhvZDogJ0hFQUQnIH0pXHJcblx0XHRcdC50aGVuKHJlcyA9PiB7XHJcblx0XHRcdFx0aWYocmVzLnN0YXR1cyA9PSA0MDQpIHtcclxuXHRcdFx0XHRcdHJldHVybiBudWxsO1xyXG5cdFx0XHRcdH0gZWxzZSB7XHJcblx0XHRcdFx0XHQvLyBnZXQgbWltZSBhbmQgb3JpZyBmaWxlIG5hbWUsIGlmIGF2YWlsYWJsZVxyXG5cdFx0XHRcdFx0bGV0IHR5cGUgPSByZXMuaGVhZGVycy5nZXQoXCJjb250ZW50LXR5cGVcIikhO1xyXG5cdFx0XHRcdFx0bGV0IGZpbGVuYW1lID0gcmVzLmhlYWRlcnMuZ2V0KFwiY29udGVudC1kaXNwb3NpdGlvblwiKT8ubWF0Y2goL2ZpbGVuYW1lPVwiKC4rKVwiLyk/LlsxXSA/PyBudWxsOyAvL251bGwgaWYgaGVhZGVyIG1pc3NpbmcsIG5vIGZpbGVuYW1lLCBldGNcclxuXHRcdFx0XHRcdGxldCBzaXplID0gTnVtYmVyLnBhcnNlSW50KHJlcy5oZWFkZXJzLmdldChcImNvbnRlbnQtbGVuZ3RoXCIpISk7XHJcblx0XHRcdFx0XHJcblx0XHRcdFx0XHRyZXR1cm4geyB0eXBlLCBmaWxlbmFtZSwgc2l6ZSB9IGFzIEQyTEFzc2V0TWV0YWRhdGE7XHJcblx0XHRcdFx0fVxyXG5cdFx0XHR9KTtcclxuXHR9XHJcblxyXG5cdC8qKiBSZXR1cm5zIHRoZSBodW1hbi1yZWFkYWJsZSBsYWJlbHMgZm9yIHRoZSBjb250ZW50J3MgZHJvcGRvd24gYWN0aW9ucyAqL1xyXG5cdHByb3RlY3RlZCBoZWFkZXJEcm9wZG93bkFjdGlvbnMoKTogc3RyaW5nW10gfCBudWxsIHtcclxuXHRcdC8vIG9ubHkgd29ya3MgaWYgdGhlIGRyb3Bkb3duIGhhcyBiZWVuIGFjdGl2YXRlZFxyXG5cdFx0Ly9BcnJheS5mcm9tKHRoaXMuaGRyQmFyLnF1ZXJ5U2VsZWN0b3JBbGwoJ2QybC1kcm9wZG93biBkMmwtZHJvcGRvd24tbWVudSBkMmwtbWVudSBkMmwtbWVudS1pdGVtJykpXHJcblx0XHQvL1x0Lm1hcChlID0+IGUuZ2V0QXR0cmlidXRlKCd0ZXh0JykpXHJcblxyXG5cdFx0Ly8gQWNjZXNzIHVucmVuZGVyZWQgKHRlbXBsYXRlZCkgZHJvcGRvd24gYWN0aW9ucywgYW5kIGdldCB0aGVpciB0ZXh0LlxyXG5cdFx0bGV0IHRlbXBsOiBIVE1MVGVtcGxhdGVFbGVtZW50IHwgbnVsbCA9IHRoaXMuaGRyQmFyLnF1ZXJ5U2VsZWN0b3IoJ3RlbXBsYXRlI2QybF9wYWdlVGl0bGVBY3Rpb25zJyk7XHJcblx0XHRpZighdGVtcGwpIHJldHVybiBudWxsO1xyXG5cdFx0cmV0dXJuIEFycmF5LmZyb20odGVtcGwuY29udGVudC5xdWVyeVNlbGVjdG9yQWxsKCdkMmwtbWVudS1pdGVtW3RleHRdJykpXHJcblx0XHRcdC5tYXAoZSA9PiBlLmdldEF0dHJpYnV0ZSgndGV4dCcpISk7XHJcblxyXG5cdFx0LyogY3VycmVudGx5IGtub3duOlxyXG5cdFx0XHRmaWxlIGNvbnRlbnQ6IFwiRG93bmxvYWRcIlxyXG5cdFx0XHRxdWl6OiBcIlZpZXcgU3VtbWFyeVwiXHJcblx0XHRcdHF1aXo6IFwiVmlldyBTdWJtaXNzaW9uc1wiXHJcblx0XHRcdHF1aXo6IFwiVmlldyBSZXBvcnRzXCJcclxuXHRcdCovXHJcblx0fVxyXG5cclxuXHQvLyBLZWVwIHRoZXNlIG9yZ2FuaXplZCByb3VnaGx5IGluIHRoZSBvcmRlciBvZiBsZWFzdC0+bW9zdCBleHBlbnNpdmUgdG8gcnVuXHJcblxyXG5cdHByaXZhdGUgcXVpelVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdGxldCBxdWl6X2lmcmFtZSA9IHRoaXMuY29udGVudFZpZXcucXVlcnlTZWxlY3RvcignI1F1aXpDb250ZW50Vmlld1BsYWNlSG9sZGVyID4gaWZyYW1lW3NyY10nKTtcclxuXHRcdGlmKHF1aXpfaWZyYW1lKSB7XHJcblx0XHRcdGxldCB1cmwgPSBuZXdVUkwocXVpel9pZnJhbWUuZ2V0QXR0cmlidXRlKCdzcmMnKSEpO1xyXG5cdFx0XHQvLyBFbnN1cmUgYSByZWd1bGFyIEQyTCBwYWdlLCByYXRoZXIgdGhhbiB3aXRob3V0IGhlYWRlcnMvbWFyZ2lucy9ib3JkZXJzL2V0Y1xyXG5cdFx0XHR1cmwuc2VhcmNoUGFyYW1zLmRlbGV0ZSgnY2ZxbCcpO1xyXG5cdFx0XHR1cmwuc2VhcmNoUGFyYW1zLmRlbGV0ZSgnZG5iJyk7XHJcblx0XHRcdHVybC5zZWFyY2hQYXJhbXMuZGVsZXRlKCdjb250ZW50VG9waWNJZCcpO1xyXG5cdFx0XHR1cmwuc2VhcmNoUGFyYW1zLmRlbGV0ZSgnZDJsX2JvZHlfdHlwZScpO1xyXG5cdFx0XHRyZXR1cm4gdXJsO1xyXG5cdFx0fVxyXG5cdFx0cmV0dXJuIG51bGw7XHJcblx0fVxyXG5cclxuXHQvLyBJbnRlcmFjdGl2ZSBvbmx5IC0gbm8gY29udGVudC1kaXNwb3NpdGlvblxyXG5cdHByaXZhdGUgbXA0VXJsKCk6IFVSTCB8IG51bGwge1xyXG5cdFx0bGV0IHBsYXllcnMgPSB0aGlzLmNvbnRlbnRWaWV3LnF1ZXJ5U2VsZWN0b3JBbGwoXCJkaXYudnVpLW1lZGlhcGxheWVyW2RhdGEtbWVkaWFwbGF5ZXItc3JjXVwiKTtcclxuXHRcdGlmKHBsYXllcnMubGVuZ3RoID09IDApIHtcclxuXHRcdFx0cmV0dXJuIG51bGw7XHJcblx0XHR9IGVsc2UgaWYocGxheWVycy5sZW5ndGggPiAxKSB7XHJcblx0XHRcdHRocm93IG5ldyBFcnJvcignTW9yZSB0aGFuIDEgbWVkaWEgcGxheWVyIGZvdW5kIG9uIHRoZSBwYWdlIScpO1xyXG5cdFx0fVxyXG5cdFx0bGV0IHBsYXllciA9IHBsYXllcnNbMF07XHJcblxyXG5cdFx0cmV0dXJuIG5ld1VSTChwbGF5ZXIuZ2V0QXR0cmlidXRlKCdkYXRhLW1lZGlhcGxheWVyLXNyYycpISk7XHJcblx0fVxyXG5cclxuXHQvKiogSW50ZXJhY3RpdmUuIE1heSBiZSBoYW5kbGVkIGJ5IGFuIGV4dGVuc2lvbiAqL1xyXG5cdHByaXZhdGUgb2ZmaWNlVXJsKCk6IFVSTCB8IG51bGwge1xyXG5cdFx0LypcclxuXHRcdCAqIFRoZXJlJ3MgY3VycmVudGx5IGEgYnVnIGluIHRoZSBgT2ZmaWNlIEVkaXRpbmcgZm9yIERvY3MsIFNoZWV0cywgJiBTbGlkZXNgIGV4dGVuc2lvbiB0aGF0IHByZXZlbnRzIGl0IGZyb20gcGFyc2luZyB0aGUgJ0NvbnRlbnQtRGlzcG9zaXRpb24nIGhlYWRlciBjb3JyZWN0bHkuXHJcblx0XHQgKiBXaGlsZSBpdCBkb2Vzbid0IGFmZmVjdCBmdW5jdGlvbmFsaXR5LCB0aGUgZmlsZW5hbWUgaXMgbWFuZ2xlZCBmcm9tIHdoYXQgaXQgaXMgc3VwcG9zZWQgdG8gYmUuXHJcblx0XHQgKiBcclxuXHRcdCAqIFdlIGNvdWxkIHRyeSB0byBzZXJ2ZSB0aGUgZmlsZSBvdmVyIGAvY29udGVudC9lbmZvcmNlZC8ke2NsYXNzX25hbWV9LyR7ZGl2W2RhdGEtdGl0bGVdfWAgYnV0IHRoZXJlIGlzbid0IGFuIGVhc3koPykgd2F5IHRvIGdldCAkY2xhc3NfbmFtZSBmcm9tIHRoZSBwYWdlIChEMkwgSlMgYXBpPylcclxuXHRcdCAqIE1QNCBmaWxlcyBzZWVtIHRvIGJlIHNlcnZlZCBmcm9tIHRoaXMgZm9sZGVyLCBob3dldmVyLlxyXG5cdFx0Ki9cclxuXHRcdGlmKENBUEFCSUxJVElFUy5leHRlbnNpb25zLm9mZmljZV92aWV3ZXIpIHtcclxuXHRcdFx0bGV0IGFzUERGID0gdGhpcy5wZGZVcmwoKTtcclxuXHRcdFx0aWYoYXNQREYgJiYgIWFzUERGWzFdKSB7XHJcblx0XHRcdFx0Ly8gaWYgd2UgYXJlbid0IGEgbmF0aXZlIFBERiAtIHdlIG1pZ2h0IGJlIGEgY29udmVydGVkIG9mZmljZSBQREY/XHJcblx0XHJcblx0XHRcdFx0Ly8gY2hlY2sgaW4gcGRmVXJsIGVuc3VyZXMgdGhlcmUgaXMgb25seSAxIGVsZW1lbnQgdGhhdCBtYXRjaGVzIHRoaXNcclxuXHRcdFx0XHRsZXQgdmlld2VyID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2W2NsYXNzXj1kMmwtZmlsZXZpZXdlci1wZGYtXVtkYXRhLWxvY2F0aW9uXVwiKVswXTtcclxuXHRcdFx0XHRcclxuXHRcdFx0XHRsZXQgZXh0ID0gdmlld2VyLmdldEF0dHJpYnV0ZSgnZGF0YS10aXRsZScpPy5zcGxpdCgnLicpLnNsaWNlKC0xKVswXTtcclxuXHRcdFx0XHRpZihbJ2RvYycsICdkb3QnLCAneGxzJywgJ3hsdCcsICdjc3YnLCAncHB0JywgJ3BvdCcsICdwcHMnLCAnc2xkJ10uc29tZShzID0+IGV4dD8uc3RhcnRzV2l0aChzKSkpIHtcclxuXHRcdFx0XHRcdHJldHVybiB0aGlzLm5haXZlRmlsZVVSTCh0cnVlKTtcclxuXHRcdFx0XHRcdC8vcmV0dXJuIG5ld1VSTChgL2NvbnRlbnQvZW5mb3JjZWQvYClcclxuXHJcblx0XHRcdFx0XHRsZXQgdGl0bGUgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCd0aXRsZScpO1xyXG5cdFx0XHRcdFx0dGl0bGUuaW5uZXJUZXh0ID0gdGhpcy50aXRsZTtcclxuXHRcdFx0XHRcdFxyXG5cdFx0XHRcdFx0bGV0IGJvZHkgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdib2R5Jyk7XHJcblx0XHRcdFx0XHRib2R5LnN0eWxlLm1hcmdpbiA9ICcwJztcclxuXHJcblx0XHRcdFx0XHRsZXQgZnJhbWUgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdpZnJhbWUnKTtcclxuXHRcdFx0XHRcdGZyYW1lLnNyYyA9IHRoaXMubmFpdmVGaWxlVVJMKHRydWUpLnRvU3RyaW5nKCk7XHJcblx0XHRcdFx0XHRmcmFtZS5zZXRBdHRyaWJ1dGUoJ2FsbG93ZnVsbHNjcmVlbicsICcxJyk7XHJcblx0XHRcdFx0XHRmcmFtZS5zdHlsZS5ib3JkZXIgPSAnMCdcclxuXHRcdFx0XHRcdGZyYW1lLnN0eWxlLndpZHRoID0gJzEwMCUnXHJcblx0XHRcdFx0XHRmcmFtZS5zdHlsZS5oZWlnaHQgPSAnMTAwJSdcclxuXHJcblx0XHRcdFx0XHRsZXQgZG9jID0gYFxyXG5cdFx0XHRcdFx0XHQ8aHRtbD5cclxuXHRcdFx0XHRcdFx0XHQ8aGVhZD4ke3RpdGxlLm91dGVySFRNTH08L2hlYWQ+XHJcblx0XHRcdFx0XHRcdFx0PGJvZHk+JHtmcmFtZS5vdXRlckhUTUx9PC9ib2R5PlxyXG5cdFx0XHRcdFx0XHQ8L2h0bWw+XHJcblx0XHRcdFx0XHRgO1xyXG5cclxuXHRcdFx0XHRcdGxldCBhc0Jhc2U2NCA9IGJ0b2EoZG9jKTtcclxuXHRcdFx0XHRcdHJldHVybiBuZXcgVVJMKCdkYXRhOnRleHQvaHRtbDtiYXNlNjQsJyArIGFzQmFzZTY0KTtcclxuXHRcdFx0XHR9XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHRcdFxyXG5cdFx0cmV0dXJuIG51bGw7XHJcblx0fVxyXG5cclxuXHQvKiogSW50ZXJhY3RpdmUgb25seS4gLSBubyBjb250ZW50LWRpc3Bvc2l0aW9uLlxyXG5cdCAqIFxyXG5cdCAqIE1heSBiZSBkb3dubG9hZGFibGUgaWZmIGBwZGZVcmxbMV1gXHJcblx0ICovXHJcblx0cHJpdmF0ZSBwZGZVcmwoKTogW1VSTCwgbmF0aXZlUERGOiBib29sZWFuXSB8IG51bGwge1xyXG5cdFx0Ly8gbmF0aXZlIHBkZjogLmQybC1maWxldmlld2VyLXBkZi1uYXRpdmVcclxuXHRcdC8vIGNvbnZlcnRlZCB0byBwZGY6IC5kMmwtZmlsZXZpZXdlci1wZGYtcGRmanNcclxuXHJcblx0XHQvLyBDb3VsZCB3ZSBkZXRlcm1pbmUgaWYgbmF0aXZlL2NvbnZlcnRlZCB2aWEgdGhlIGRhdGEtbG9jYXRpb24gVVJMP1xyXG5cdFx0Ly8gRmlsZXMgY29udmVydGVkIHRvIFBERiB0ZW5kIHRvIGJlIHNlcnZlZCBvdmVyIEFXU1xyXG5cclxuXHRcdC8vIEdldCBQREYgdmlld2VycyBvbiB0aGUgcGFnZSwgYW5kIGVuc3VyZSB0aGVyZSBpcyBvbmx5IG9uZSBmb3Igb3VyIGNvbnRlbnRcclxuXHRcdGxldCBwZGZWaWV3ZXJzID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2W2NsYXNzXj1kMmwtZmlsZXZpZXdlci1wZGYtXVtkYXRhLWxvY2F0aW9uXVwiKVxyXG5cdFx0aWYocGRmVmlld2Vycy5sZW5ndGggPT0gMCkge1xyXG5cdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdH0gZWxzZSBpZihwZGZWaWV3ZXJzLmxlbmd0aCA+IDEpIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdNb3JlIHRoYW4gMSBQREYgRmlsZXZpZXdlciB3aXRoIGEgZGF0YSBsb2NhdGlvbiBmb3VuZCBvbiB0aGUgcGFnZSEnKTtcclxuXHRcdH1cclxuXHRcdGxldCB2aWV3ZXIgPSBwZGZWaWV3ZXJzWzBdO1xyXG5cclxuXHRcdC8vIEV4dHJhY3QgdGhlIHR5cGUgb2YgY29udGVudHMgb3VyIFBERiB2aWV3ZXIgaXMgc2hvd2luZ1xyXG5cdFx0bGV0IHR5cGUgPSBBcnJheS5mcm9tKHZpZXdlci5jbGFzc0xpc3QpXHJcblx0XHRcdC5maWx0ZXIocyA9PiBzLnN0YXJ0c1dpdGgoJ2QybC1maWxldmlld2VyLXBkZi0nKSlcclxuXHRcdFx0Lm1hcChzID0+IHMuc3BsaXQoJy0nKS5zbGljZSgtMSlbMF0pOyAvLyBnZXQgbGFzdCBjb21wb25lbnRcclxuXHRcdGlmKHR5cGUubGVuZ3RoICE9IDEpIHsgLy8gPjEgYmVjYXVzZSB3ZSBmaWx0ZXIgdG8gdGhpcyBjbGFzcyBwcmVmaXhcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdNb3JlIHRoYW4gb25lIEQyTCBQREYgRmlsZXZpZXdlcnMgb24gdGhlIHBhZ2UnKTtcclxuXHRcdH1cclxuXHJcblx0XHQvLyByZXR1cm4gYSB0dXBsZSBvZiBbdXJsLCBpc05hdGl2ZVBERl1cclxuXHRcdGxldCBsb2NhdGlvbiA9IG5ld1VSTCh2aWV3ZXIuZ2V0QXR0cmlidXRlKCdkYXRhLWxvY2F0aW9uJykhKTtcclxuXHRcdHN3aXRjaCh0eXBlWzBdKSB7XHJcblx0XHRcdGNhc2UgXCJuYXRpdmVcIjogcmV0dXJuIFtsb2NhdGlvbiwgdHJ1ZV07XHJcblx0XHRcdGNhc2UgXCJwZGZqc1wiOiByZXR1cm4gW2xvY2F0aW9uLCBmYWxzZV07XHJcblx0XHRcdGRlZmF1bHQ6IHRocm93IG5ldyBFcnJvcignVW5rbm93biBQREYgdmlld2VyIHR5cGU6ICcgKyB0eXBlWzBdKTtcclxuXHRcdH1cclxuXHJcblx0fVxyXG5cclxuXHQvKiogT25seSBpbnRlcmFjdGl2ZSAqL1xyXG5cdHByaXZhdGUgZXh0UGFnZVVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdGxldCBpdGV4dCA9IHRoaXMuY29udGVudFZpZXcuaW5uZXJUZXh0O1xyXG5cdFx0aWYgKCFpdGV4dC5pbmNsdWRlcyhcIkV4dGVybmFsIFJlc291cmNlXCIpIHx8ICFpdGV4dC5pbmNsdWRlcyhcIk9wZW4gaW4gTmV3IFdpbmRvd1wiKSkge1xyXG5cdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdH1cclxuXHRcdC8vIHJlYXNvbmFibHkgc3VyZSB0aGlzIHBhZ2UncyBjb250ZW50IGlzIGFuIGV4dGVybmFsIHBhZ2UgLSB0aHJvdyBvbiBhbnkgZXJyb3JzXHJcblxyXG5cdFx0bGV0IHVybHMgPSBPYmplY3QudmFsdWVzKEQyTC5PUi5fX2cxKVxyXG5cdFx0XHQubWFwKHMgPT4geyB0cnkge1xyXG5cdFx0XHRcdHJldHVybiBKU09OLnBhcnNlKHMpIGFzIEQyTF9PUl9PYmplY3RcclxuXHRcdFx0fSBjYXRjaChlKSB7XHJcblx0XHRcdFx0dGhyb3cgbmV3IEVycm9yKFwiRDJMLk9SLl9fZzEgY29udGFpbnMgbWFsZm9ybWVkIEpTT04hXCIpXHJcblx0XHRcdH0gfSlcclxuXHRcdFx0LmZpbHRlcigobyk6IG8gaXMgT1JfT2JqZWN0cy5GdW5jID0+IHtcclxuXHRcdFx0XHRyZXR1cm4gby5fdHlwZSA9PSBcImZ1bmNcIiAmJiBvLk4gPT0gXCJEMkwuTEUuQ29udGVudC5EZXNrdG9wLlRvcGljLk9wZW5Jbk5ld1dpbmRvd1wiICYmIG8uUC5sZW5ndGggPT0gMVxyXG5cdFx0XHR9KVxyXG5cdFx0XHQubWFwKG8gPT4gby5QWzBdKTtcclxuXHRcdFxyXG5cdFx0aWYodXJscy5sZW5ndGggPT0gMCkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoYENvbnRlbnQgVmlldyBzaG93aW5nIFwiRXh0ZXJuYWwgUmVzb3VyY2VcIiB3aXRob3V0IGFueSBPcGVuSW5OZXdXaW5kb3cgZnVuY3Rpb25zIGluIEQyTC5PUi5fX2cxIWApO1xyXG5cdFx0fSBlbHNlIGlmKHVybHMubGVuZ3RoID4gMSkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoYE11bHRpcGxlIE9wZW5Jbk5ld1dpbmRvdyBjb21tYW5kcyBmb3IgXCJFeHRlcm5hbCBSZXNvdXJjZVwiIGNvbnRlbnQuIE5vdCByZXR1cm5pbmcgYW55LiAoJHtKU09OLnN0cmluZ2lmeSh1cmxzKX0pYCk7XHJcblx0XHR9IGVsc2UgaWYodHlwZW9mIHVybHNbMF0gIT0gJ3N0cmluZycpIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKGBTaW5nbGUgT3BlbkluTmV3V2luZG93IGNvbW1hbmQgcGFyYW1ldGVyIGlzbid0IGEgc3RyaW5nISAoJHtKU09OLnN0cmluZ2lmeSh1cmxzWzBdKX0pYClcclxuXHRcdH1cclxuXHJcblx0XHRsZXQgdXJsID0gbmV3VVJMKHVybHNbMF0pO1xyXG5cdFx0dXJsLnByb3RvY29sID0gXCJodHRwc1wiOyAvLyBvdGhlcndpc2UgaWZyYW1lIHdvbid0IGxvYWQgYi9jIEQyTCBpcyBIVFRQU1xyXG5cdFx0aWYodXJsLmhvc3QuaW5jbHVkZXMoXCJ5b3V0dWJlLmNvbVwiKSAmJiB1cmwucGF0aG5hbWUgPT0gXCIvd2F0Y2hcIikge1xyXG5cdFx0XHRsZXQgdmlkZW9faWQgPSB1cmwuc2VhcmNoUGFyYW1zLmdldChcInZcIik7XHJcblx0XHRcdGlmKHZpZGVvX2lkKSB7XHJcblx0XHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoXCJ2XCIpO1xyXG5cdFx0XHRcdHVybC5wYXRobmFtZSA9IFwiL2VtYmVkL1wiICsgdmlkZW9faWQ7XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHJcblx0XHRyZXR1cm4gdXJsO1xyXG5cdH1cclxuXHJcblx0LyoqIE9ubHkgZG93bmxvYWRhYmxlXHJcblx0ICogXHJcblx0ICogRG9lc24ndCBjaGVjayBmb3IgNDA0IC0gc2hvdWxkIGJlIHVzZWQgYWZ0ZXIgZGV0ZWN0aW9uIG9mIGV4dGVybmFsIHBhZ2VzL2V0Y1xyXG5cdCovXHJcblx0cHJpdmF0ZSBuYWl2ZUZpbGVVUkwoc3RyZWFtOiBib29sZWFuKTogVVJMIHtcclxuXHRcdGxldCB1cmwgPSBuZXdVUkwodGhpcy5uYWl2ZUFzc2V0VVJMLnRvU3RyaW5nKCkpIC8vIGNsb25lIFVSTFxyXG5cdFx0dXJsLnNlYXJjaFBhcmFtcy5zZXQoXCJzdHJlYW1cIiwgc3RyZWFtLnRvU3RyaW5nKCkpO1xyXG5cdFx0cmV0dXJuIHVybFxyXG5cdH1cclxuXHJcblx0LyoqIERPTSBtYW5pcHVsYXRpb24gaGVscGVycyAqL1xyXG5cdHByb3RlY3RlZCByZXBsYWNlQ29udGVudChpZnJhbWVfc3JjOiBVUkwpIHtcclxuXHRcdGxldCBuZXdDb250ZW50OiBIVE1MRWxlbWVudDtcclxuXHRcdGxldCBpZnJhbSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2lmcmFtZScpXHJcblx0XHRpZnJhbS5zcmMgPSBpZnJhbWVfc3JjLnRvU3RyaW5nKCk7XHJcblx0XHRpZnJhbS5zdHlsZS53aWR0aCA9ICcxMDAlJztcclxuXHRcdGlmcmFtLnN0eWxlLmhlaWdodCA9ICc5MHZoJztcclxuXHRcdGlmcmFtLnN0eWxlLnJlc2l6ZSA9ICdib3RoJztcclxuXHRcdGlmcmFtLnNldEF0dHJpYnV0ZSgncHJlbG9hZCcsICdhdXRvJyk7XHJcblx0XHRpZnJhbS5zZXRBdHRyaWJ1dGUoJ2FsbG93ZnVsbHNjcmVlbicsICd0cnVlJyk7IC8vIGZvciBtZWRpYSBwbGF5ZXJzIChlZzogZW1iZWRkZWQgeW91dHViZSlcclxuXHRcdG5ld0NvbnRlbnQgPSBpZnJhbTtcclxuXHJcblx0XHRsZXQgY3YgPSB0aGlzLmNvbnRlbnRWaWV3O1xyXG5cdFx0d2hpbGUgKGN2Lmxhc3RDaGlsZCkgeyBjdi5yZW1vdmVDaGlsZChjdi5sYXN0Q2hpbGQpIH0gLy8gcmVtb3ZlIGV4aXN0aW5nIGNvbnRlbnRcclxuXHRcdGN2LmFwcGVuZENoaWxkKG5ld0NvbnRlbnQpOyAvLyBhZGQgc2VsZiB0byBkb21cclxuXHJcblx0XHQvLyBBdHRlbXB0IHRvIHByZXNlcnZlIGFzcGVjdCByYXRpb1xyXG5cdFx0Ly8gTXVzdCBiZSBkb25lIGFmdGVyIGFwcGVuZGluZyB0byB0aGUgRE9NICguYXBwZW5kQ2hpbGQpIHNvIC5vZmZzZXRXaWR0aCB3b3Jrc1xyXG5cdFx0aWYoaWZyYW1lX3NyYy5wYXRobmFtZS5lbmRzV2l0aCgnLm1wNCcpKSB7XHJcblx0XHRcdGxldCB3aWR0aCA9IGlmcmFtLm9mZnNldFdpZHRoO1xyXG5cdFx0XHQvLyBhc3N1bWUgdmlkZW9zIGFyZSAxNjo5IGFzcGVjdCByYXRpb1xyXG5cdFx0XHRpZnJhbS5zdHlsZS5oZWlnaHQgPSAod2lkdGggKiAoOS8xNikpICsgJ3B4JztcclxuXHRcdH1cclxuXHJcblx0XHR0aGlzLnJlcGxhY2VkQ29udGVudCA9IHRydWU7XHJcblx0XHRyZXR1cm4gaWZyYW07XHJcblx0fVxyXG5cdHByb3RlY3RlZCB0aXRsZUJ0bih0ZXh0OiBzdHJpbmcsIG9uY2xpY2s6ICh0aGlzOiBIVE1MQnV0dG9uRWxlbWVudCwgZXY6IE1vdXNlRXZlbnQpID0+IGFueSk6IEhUTUxCdXR0b25FbGVtZW50IHtcclxuXHRcdGxldCBidG4gPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdidXR0b24nKTtcclxuXHRcdGJ0bi5pbm5lclRleHQgPSB0ZXh0O1xyXG5cdFx0YnRuLmNsYXNzTGlzdC5hZGQoJ2QybC1idXR0b24nKTtcclxuXHRcdGJ0bi5zdHlsZS5tYXJnaW5MZWZ0ID0gJzAuMjVyZW0nOyAvLyA1cHggbWF0Y2hlcyAuZDJsLWNvbnRleHRtZW51LXBoXHJcblx0XHRidG4uc3R5bGUubWFyZ2luUmlnaHQgPSAnMC4yNXJlbSc7XHJcblx0XHRidG4uc3R5bGUud2lkdGggPSAnYXV0byc7XHJcblx0XHRidG4uYWRkRXZlbnRMaXN0ZW5lcignY2xpY2snLCBvbmNsaWNrKTtcclxuXHRcdHJldHVybiBidG47XHJcblx0fVxyXG5cdHByb3RlY3RlZCB0aXRsZUxpbmsodGV4dDogc3RyaW5nLCBocmVmOiBVUkwsIHByZXBlbmROb2RlPzogSFRNTEVsZW1lbnQpOiBIVE1MQW5jaG9yRWxlbWVudCB7XHJcblx0XHRsZXQgbGluayA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2EnKTtcclxuXHRcdGxpbmsuaW5uZXJUZXh0ID0gdGV4dDtcclxuXHRcdGlmKHByZXBlbmROb2RlKSBsaW5rLnByZXBlbmQocHJlcGVuZE5vZGUpO1xyXG5cdFx0bGluay5ocmVmID0gaHJlZi50b1N0cmluZygpO1xyXG5cdFx0bGluay5jbGFzc0xpc3QuYWRkKCdkMmwtYnV0dG9uJyk7XHJcblx0XHRsaW5rLnN0eWxlLm1hcmdpbkxlZnQgPSAnMC4yNXJlbSc7XHJcblx0XHRsaW5rLnN0eWxlLm1hcmdpblJpZ2h0ID0gJzAuMjVyZW0nO1xyXG5cdFx0bGluay5zdHlsZS53aWR0aCA9ICdhdXRvJztcclxuXHRcdHJldHVybiBsaW5rO1xyXG5cdH1cclxuXHJcblx0LyoqIEFkZHMgYSBidXR0b24gdG8gdXNlIGEgbmF0aXZlIHZpZXdlciBmb3IgdGhlIHByb3ZpZGVkIFVSTC4gQXV0b21hdGljYWxseSByZW1vdmVzIGl0c2VsZiBmcm9tIHRoZSBET00gd2hlbiBjbGlja2VkLiAqL1xyXG5cdHByb3RlY3RlZCB0aXRsZUJ0bl9yZXBsYWNlQ29udGVudChsYWJlbDogc3RyaW5nLCBzcmM6IFVSTCk6IEhUTUxCdXR0b25FbGVtZW50IHtcclxuXHRcdGNvbnN0IHRoYXQgPSB0aGlzO1xyXG5cdFx0ZnVuY3Rpb24gYnRub25jbGljayh0aGlzOiBIVE1MQnV0dG9uRWxlbWVudCkge1xyXG5cdFx0XHQvLyBpbnN0YWxsZWQgYXMgb25jbGljayBoYW5kbGVyID0+IHRoaXMgPSBgPGJ1dHRvbj4uLi48L2J1dHRvbj5gXHJcblx0XHRcdC8vIHJlbW92ZSBvdXJzZWx2ZXMgKGJ1dHRvbikgc2luY2Ugd2UgaGF2ZSBzZXJ2ZWQgb3VyIHB1cnBvc2VcclxuXHRcdFx0dGhpcy5yZW1vdmUoKTtcclxuXHRcdFx0dGhhdC5yZXBsYWNlQ29udGVudChzcmMpO1xyXG5cdFx0fTtcclxuXHJcblx0XHQvLyBOb3RlOiBwdXQgYnV0dG9uIGluIGltbWVkaWF0bHksIHByb3ZpZGUgbGluayBhZnRlciBQREYgd2FzIGRvd25sb2FkZWRcclxuXHRcdHJldHVybiB0aGlzLnRpdGxlQnRuKGxhYmVsLCBidG5vbmNsaWNrKTtcclxuXHR9XHJcbn1cclxuIiwiXHJcbmNvbnN0IGdlbmVyYWxfY3NzID0gYFxyXG5cdC8qIGVsaW1pbmF0ZSBzb21lIHdoaXRlc3BhY2UgKi9cclxuXHQvKiByZWR1Y2VfdG9jX3BhZGRpbmcgKi9cclxuXHRcdGh0bWwgLmRheWxpZ2h0ICNEMkxfTEVfQ29udGVudF9UcmVlQnJvd3NlciAuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLFxyXG5cdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRQbHVnaW5UcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0ge1xyXG5cdFx0XHRwYWRkaW5nOiA1cHggMCA1cHggMDtcclxuXHRcdH1cclxuXHJcblx0Lyogc2VjdGlvbiBoZWFkaW5nICovXHJcblx0XHRodG1sIC5kYXlsaWdodCAuZDJsLWhlYWRlci10b3AgLmQybC1ib3gtaCB7XHJcblx0XHRcdG1hcmdpbi10b3A6IDFyZW07XHJcblx0XHRcdG1hcmdpbi1ib3R0b206IDFyZW07XHJcblx0XHR9XHJcblx0XHRodG1sIC5kMmwtcGFnZS1oZWFkZXIge1xyXG5cdFx0XHRtYXJnaW4tYm90dG9tOiAwLjVyZW07XHJcblx0XHR9XHJcblx0XHRodG1sIC5kMmwtdHlwb2dyYXBoeSAuZDJsLWh0bWxibG9jayAgaDIge1xyXG5cdFx0XHRtYXJnaW46IDAuNXJlbSAwO1xyXG5cdFx0fVxyXG5cclxuXHQvKiBzZWN0aW9uIGNvbnRlbnRzICovXHJcblx0XHRodG1sIC5kMmwtZGF0YWxpc3Qtc3R5bGUxID4gLmQybC1kYXRhbGlzdCAuZDJsLWRhdGFsaXN0LWl0ZW0tY29udGVudCB7XHJcblx0XHRcdHBhZGRpbmctdG9wOiAwLjI1cmVtO1xyXG5cdFx0XHRwYWRkaW5nLWJvdHRvbTogMC4yNXJlbTsgXHJcblx0XHR9XHJcblx0XHRodG1sIC5kMmwtdHlwb2dyYXBoeSAuZDJsLWRhdGFsaXN0LWl0ZW0tY29udGVudCAuZDJsLWh0bWxibG9jayBwIHtcclxuXHRcdFx0bWFyZ2luOiAwLjVlbSAwO1xyXG5cdFx0fVxyXG5cclxuXHQvKiBkaXNjdXNzaW9ucyAqL1xyXG5cdFx0aHRtbCAjRm9ydW1zVG9waWNzUGxhY2Vob2xkZXIgLmQybC1zZXAge1xyXG5cdFx0XHRkaXNwbGF5OiBub25lO1xyXG5cdFx0fVxyXG5cdFx0aHRtbCAuZDJsLWZvcnVtLWRldGFpbHMge1xyXG5cdFx0XHRwYWRkaW5nLXRvcDogMDtcclxuXHRcdH1cclxuXHRodG1sIC5kMmwtcGFnZS1oZWFkZXIge1xyXG5cdFx0bWFyZ2luLWJvdHRvbTogMDtcclxuXHR9XHJcbmA7XHJcblxyXG5jb25zdCBjc3N0d2Vha3MgPSB7XHJcblx0Y29udGVudDogZnVuY3Rpb24oKSB7XHJcblxyXG5cdH0sXHJcblx0Y29udGVudF90b2M6IGZ1bmN0aW9uKCkge1xyXG5cdFx0Ly8gaW5jbHVkZSBgaHRtbGAgdG8gYmVhdCB0aGUgc3BlY2lmaWNpdHkgb2YgdGhlIGJ1aWx0LWluIHJ1bGVzXHJcblx0XHQvLyBodHRwczovL2RldmVsb3Blci5tb3ppbGxhLm9yZy9lbi1VUy9kb2NzL1dlYi9DU1MvU3BlY2lmaWNpdHlcclxuXHJcblx0XHRjb25zdCBzdHIgPSBgXHJcblx0XHRcdC8qIE1ha2UgdGhlIFRvQyBwb2ludGVyIG1vcmUgdmlzaWJpbGUsIHJlbW92ZSBhbm5veWluZ2x5IHN0eWxlZCBncmFkaWVudCAqL1xyXG5cdFx0XHRcdGh0bWwgLmRheWxpZ2h0ICNDb250ZW50TW9kdWxlVHJlZSAuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1TZWxlY3RlZCA+IGEuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLWFuY2hvcjo6YmVmb3JlICxcclxuXHRcdFx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudE1vZHVsZVRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tU2VsZWN0ZWQgPiBhLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1hbmNob3I6bGluazo6YmVmb3JlIHtcclxuXHRcdFx0XHRcdHZpc2liaWxpdHk6IGhpZGRlbjtcclxuXHRcdFx0XHR9XHJcblx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOjphZnRlcixcclxuXHRcdFx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudE1vZHVsZVRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tU2VsZWN0ZWQgPiBhLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1hbmNob3I6bGluazo6YWZ0ZXIsXHJcblx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOnZpc2l0ZWQ6OmFmdGVyLFxyXG5cdFx0XHRcdGh0bWwgLmRheWxpZ2h0ICNDb250ZW50TW9kdWxlVHJlZSAuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1TZWxlY3RlZCA+IGEuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLWFuY2hvcjpob3Zlcjo6YWZ0ZXIsXHJcblx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOmZvY3VzOjphZnRlciB7XHJcblx0XHRcdFx0XHRmaWx0ZXI6IGludmVydCg1MCUpOyAvKiBiYXNpY2FsbHkgbWFrZSBpdCBncmF5ICovXHJcblx0XHRcdFx0fVxyXG5cclxuXHRcclxuXHRcdFx0XHJcblx0XHRgO1xyXG5cclxuXHRcdGxldCBzdHlsZSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ3N0eWxlJyk7XHJcblx0XHRzdHlsZS5zZXRBdHRyaWJ1dGUoJ3R5cGUnLCAndGV4dC9jc3MnKTtcclxuXHRcdHN0eWxlLmlubmVySFRNTCA9IHN0cjtcclxuXHRcdGRvY3VtZW50LmhlYWQuYXBwZW5kQ2hpbGQoc3R5bGUpO1xyXG5cdFx0Y29uc29sZS5sb2coXCJJbnN0YWxsaW5nIGNzcyB0d2Vha3MgdG86IFwiLCBzdHlsZSk7XHJcblx0fSxcclxufTtcclxuXHJcbi8qKiBUcmF2ZXJzZXMgdGhlIERPTSBmaW5kaW5nIGVhY2ggcGFyZW50IGVsZW1lbnQgd2l0aCBgZGlzcGxheTogbm9uZWAgYW5kIHRvZ2dsZXMgaXQgb2ZmIGZvciB0aGUgZHVyYXRpb24gb2YgdGhlIGNhbGxiYWNrLiAqL1xyXG5mdW5jdGlvbiBhc1Zpc2libGU8VD4odGFyZ2V0OiBIVE1MRWxlbWVudCwgZjogKCkgPT4gVCk6IFQge1xyXG5cdGxldCBpc1Zpc2libGUgPSAodGFyZ2V0OiBIVE1MRWxlbWVudCkgPT4gdGFyZ2V0Lm9mZnNldFBhcmVudCAhPT0gbnVsbDtcclxuXHJcblx0aWYoaXNWaXNpYmxlKHRhcmdldCkpIHtcclxuXHRcdC8vIElmIHdlJ3JlIHZpc2libGUsIHdlJ3JlIGRvbmUgLSBjYWxsIHRoZSBmdW5jdGlvblxyXG5cdFx0cmV0dXJuIGYoKTtcclxuXHR9IGVsc2Uge1xyXG5cdFx0aWYoZ2V0Q29tcHV0ZWRTdHlsZSh0YXJnZXQpLmdldFByb3BlcnR5VmFsdWUoJ2Rpc3BsYXknKSA9PT0gJ25vbmUnKSB7XHJcblx0XHRcdGxldCBbb2xkViwgb2xkUF0gPSBbdGFyZ2V0LnN0eWxlLmdldFByb3BlcnR5VmFsdWUoJ2Rpc3BsYXknKSwgdGFyZ2V0LnN0eWxlLmdldFByb3BlcnR5UHJpb3JpdHkoJ2Rpc3BsYXknKV07XHJcblx0XHRcdHRhcmdldC5zdHlsZS5zZXRQcm9wZXJ0eSgnZGlzcGxheScsICdpbmhlcml0JywgJyFpbXBvcnRhbnQnKTsgLy8gYmV0dGVyIHZhbHVlP1xyXG5cclxuXHRcdFx0Ly8gdHJ5IGFnYWluIC0gbWF5YmUgd2UncmUgdmlzaWJsZSBub3c/XHJcblx0XHRcdGxldCBydG4gPSBhc1Zpc2libGUodGFyZ2V0LCBmKTtcclxuXHJcblx0XHRcdHRhcmdldC5zdHlsZS5zZXRQcm9wZXJ0eSgnZGlzcGxheScsIG9sZFYsIG9sZFApO1xyXG5cclxuXHRcdFx0cmV0dXJuIHJ0bjtcclxuXHRcdH0gZWxzZSB7XHJcblx0XHRcdC8vIHdlIGFyZW4ndCByZXNwb25zaWJsZSBmb3Igb3VyIGludmlzaWJpbGl0eSAtIGlzIG91ciBwYXJlbnQ/XHJcblx0XHRcdHJldHVybiBhc1Zpc2libGUodGFyZ2V0LnBhcmVudEVsZW1lbnQhLCBmKTtcclxuXHRcdH1cclxuXHR9XHJcbn1cclxuIiwiLy8gPT1Vc2VyU2NyaXB0PT1cbi8vIEBuYW1lICAgICAgICAgRDJMIFR3ZWFrc1xuLy8gQG5hbWVzcGFjZSAgICBodHRwczovL2dpdGh1Yi5jb20vY3NtMTIzMTk5L2QybC10d2Vha3Ncbi8vIEB2ZXJzaW9uICAgICAgMC45XG4vLyBAZGVzY3JpcHRpb24gIEFkZCBRb0wgY2hhbmdlcyB0byBEMkwncyB1c2VyIGludGVyZmFjZVxuLy8gQGF1dGhvciAgICAgICBDaHJpcyBNb29yZVxuLy8gQGluY2x1ZGUgICAgICBodHRwczovLyouZWR1L2QybC8qXG4vLyBAZ3JhbnQgICAgICAgIG5vbmVcbi8vIEB1cGRhdGVVcmwgICAgaHR0cHM6Ly9yYXcuZ2l0aHVidXNlcmNvbnRlbnQuY29tL2NzbTEyMzE5OS9kMmwtdHdlYWtzL21hc3Rlci9kMmwtdHdlYWtzLnVzZXIuanNcbi8vID09L1VzZXJTY3JpcHQ9PVxuXG4vLyBEMkwgcmVzdCBhcGkgZG9jc1xuLy8gaHR0cHM6Ly9kb2NzLnZhbGVuY2UuZGVzaXJlMmxlYXJuLmNvbS9yZXMvY29udGVudC5odG1sXG5cbi8vLyA8cmVmZXJlbmNlIHBhdGg9XCIuL2QybC1nbG9iYWxzLmQudHNcIiAvPlxuXG4vKiBDb25maWcgKi9cbmNvbnN0IE1BS0VfTkFUSVZFX09OX0xPQUQgPSB0cnVlO1xuY29uc3QgT0ZGSUNFX0RPQ1VNRU5UU19ESVJFQ1RfVklFV19VU0VTX0VYVEVOU0lPTiA9IGZhbHNlO1xuXG4vKiBDb2RlICovXG4vLyBFbnN1cmUgdGhlIHBhZ2Ugd2UncmUgb24gaXMgYSB2YWxpZCBEMkwgcGFnZS5cbi8vIElmIHlvdXIgaW5zdGl0dXRpb24gZG9lc24ndCBtYXRjaCwgcGxlYXNlIHN1Ym1pdCBhIFBSIGFkZGluZyBhIGdlbmVyaWMgY2hlY2sgZm9yIHlvdXJzLlxubGV0IHVybCA9IG5ld1VSTChkb2N1bWVudC5sb2NhdGlvbik7XG5cbmlmICh1cmwucHJvdG9jb2wgIT0gJ2h0dHBzOicgfHwgIXVybC5ob3N0LmVuZHNXaXRoKCcuZWR1JykgfHwgIXVybC5wYXRobmFtZS5zdGFydHNXaXRoKCcvZDJsJykpIHtcblx0dGhyb3cgbmV3IEVycm9yKGBCYWQgaG9zdCBmb3IgRDJMIFNjcmlwdCAoZXhpdGluZyB2aWEgZXhjZXB0aW9uKTogJyR7dXJsLmhvc3R9J2ApO1xufVxuXG5jb25zdCBDQVBBQklMSVRJRVM6IENhcGFiaWxpdGllcyA9IGluaXRDYXBhYmlsaXRpZXMoKTtcblxuKGFzeW5jIGZ1bmN0aW9uICgpIHtcblx0J3VzZSBzdHJpY3QnO1xuXG5cdGlmICghd2l0aGluSWZyYW1lKCkpIHtcblx0XHRzdWdnZXN0SFRNTDVWaWRlb0tleWJvYXJkU2hvcnRjdXRzKCk7XG5cdFx0c3VnZ2VzdE9mZmljZUVkaXRpbmcoKTtcblxuXHRcdHRyeSB7XG5cdFx0XHRsZXQgcHR5cGUgPSBnZXRQYWdlVHlwZSgpO1xuXHRcdFx0aWYocHR5cGUudHlwZSA9PSBcImNvbnRlbnRcIikge1xuXHRcdFx0XHRjc3N0d2Vha3MuY29udGVudCgpO1xuXG5cdFx0XHRcdGxldCBjb250ZW50UGFnZSA9IG5ldyBDb250ZW50UGFnZShwdHlwZS5jbGFzcywgcHR5cGUuYXNzZXQpO1xuXHRcdFx0XHRjb25zb2xlLmxvZyhcIkNvbnRlbnQgcGFnZTogXCIsIGNvbnRlbnRQYWdlKTtcblx0XHRcdFx0aWYoTUFLRV9OQVRJVkVfT05fTE9BRClcblx0XHRcdFx0XHRjb250ZW50UGFnZS5ub3JtYWxpemVDb250ZW50KCk7XG5cdFx0XHRcdGNvbnRlbnRQYWdlLmFkZEhlYWRlckJ1dHRvbnMoKTtcblxuXHRcdFx0fSBlbHNlIGlmKHB0eXBlLnR5cGUgPT0gXCJjb250ZW50X3RvY1wiKSB7XG5cdFx0XHRcdGNzc3R3ZWFrcy5jb250ZW50X3RvYygpO1xuXG5cdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRjb25zb2xlLndhcm4oXCJbRDJMIFR3ZWFrc10gVW5rbm93biBwYWdlIHR5cGUuXCIpO1xuXHRcdFx0fVxuXHRcdH0gY2F0Y2ggKGUpIHtcblx0XHRcdGNvbnNvbGUuZXJyb3IoXCJFcnJvciBvY2N1cmVkIGluIHVzZXJzY3JpcHRcIiwgZSk7XG5cdFx0XHQvL2FsZXJ0KFwiRXJyb3Igb2NjdXJlZCBpbiBEMkwgYmV0dGVyaW5nIHVzZXJzY3JpcHQsIGVycm9yIGluIGNvbnNvbGUuXCIpO1xuXHRcdH1cblx0fVxufSkoKTtcbiIsIlxyXG5mdW5jdGlvbiBpbml0Q2FwYWJpbGl0aWVzKCk6IENhcGFiaWxpdGllcyB7XHJcblx0Ly8gSSByZWFsaXplIEkgc2hvdWxkbid0IGJlIHVzaW5nIHRoZSB1c2VyQWdlbnQgdG8gZGV0ZWN0IGJyb3dzZXIgZmxhdm9ycy5cclxuXHQvLyBJdCdsbCBiZSBnb29kIGVub3VnaCBmb3IgdGhpcy5cclxuXHRsZXQgdXNlcmFnZW50ID0gbmF2aWdhdG9yLnVzZXJBZ2VudC50b0xvd2VyQ2FzZSgpO1xyXG5cclxuXHQvLyBAdHMtaWdub3JlXHJcblx0aWYodXNlcmFnZW50LmluY2x1ZGVzKCdjaHJvbWUnKSkge1xyXG5cdFx0Y29uc3QgbXQgPSAoYXBwOiBzdHJpbmcpOiBib29sZWFuID0+IG5hdmlnYXRvci5taW1lVHlwZXMubmFtZWRJdGVtKCdhcHBsaWNhdGlvbi8nICsgYXBwKSAhPT0gbnVsbDtcclxuXHRcdHJldHVybiB7XHJcblx0XHRcdGV4dGVuc2lvbnM6IHtcclxuXHRcdFx0XHQvLyBkZXRlY3QgaWYgY2hyb21lIHN1cHBvcnRzIHZpZXdpbmcgTVMgb2ZmaWNlIGRvY3VtZW50cyAodmlhIGV4dGVuc2lvbi9ldGMpXHJcblx0XHRcdFx0b2ZmaWNlX3ZpZXdlcjogbXQoJ21zd29yZCcpICYmIG10KCdtc2V4Y2VsJykgJiYgbXQoJ21zcG93ZXJwb2ludCcpICYmIG10KCdtc3dvcmQtdGVtcGxhdGUnKSxcclxuXHRcdFx0XHQvL29mZmljZV92aWV3ZXI6IGNocm9tZUV4dGVuc2lvbkluc3RhbGxlZCgnZ21wbGpkbGdjZGtsamxwcGFla2NpYWNkbWRsaGZlb24nKSxcclxuXHRcdFx0XHR2aWRlb19zaG9ydGN1dHM6IGNocm9tZUV4dGVuc2lvbkluc3RhbGxlZCgnbGxobWFjaWdnbmlibmJkb2tpZG1iaWxrbGNlYW9iYWUnKSxcclxuXHRcdFx0fSxcclxuXHRcdFx0YnJvd3NlcjogXCJjaHJvbWVcIixcclxuXHRcdH1cclxuXHR9IGVsc2UgaWYodXNlcmFnZW50LmluY2x1ZGVzKCdmaXJlZm94JykpIHtcclxuXHRcdHJldHVybiB7XHJcblx0XHRcdC8vIG5vIHdheSB0byBrbm93XHJcblx0XHRcdGV4dGVuc2lvbnM6IHtcclxuXHRcdFx0XHRvZmZpY2Vfdmlld2VyOiBmYWxzZSxcclxuXHRcdFx0XHR2aWRlb19zaG9ydGN1dHM6IHRydWUsIC8vIEZGIGhhcyBidWlsdCBpbiBzcGVlZCBjb250cm9scyAtIHdoaWNoIEkgY2FyZSBhYm91dFxyXG5cdFx0XHR9LFxyXG5cdFx0XHRicm93c2VyOiBcImZpcmVmb3hcIixcclxuXHRcdH1cclxuXHR9XHJcblx0cmV0dXJuIHtcclxuXHRcdGV4dGVuc2lvbnM6IHsgb2ZmaWNlX3ZpZXdlcjogZmFsc2UsIHZpZGVvX3Nob3J0Y3V0czogZmFsc2UsIH0sXHJcblx0XHRicm93c2VyOiBcInVua25vd25cIixcclxuXHR9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIHN1Z2dlc3RIVE1MNVZpZGVvS2V5Ym9hcmRTaG9ydGN1dHMoKSB7XHJcblx0Ly8gT25seSBzdWdnZXN0IG9uIGNocm9tZSwgYi9jIGZpcmVmb3ggdXNlcnMgZXhpc3RcclxuXHQvLyAoYW5kIGZpcmVmb3ggYWxyZWFkeSBoYXMgcmlnaHQtY2xpY2ssIGNoYW5nZSBzcGVlZCBjb250cm9scylcclxuXHRpZighY2hyb21lRXh0ZW5zaW9uSW5zdGFsbGVkKCdsbGhtYWNpZ2duaWJuYmRva2lkbWJpbGtsY2Vhb2JhZScpKSB7XHJcblx0XHRjb25zb2xlLndhcm4oYEQyTCB1c2Vyc2NyaXB0IHJlY29tbWVuZHMgaW5zdGFsbGluZyB0aGUgXCJIVE1MNSBWaWRlbyBLZXlib2FyZCBTaG9ydGN1dHNcIiBleHRlbnNpb24gZm9yIHNwZWVkaW5nIHVwIHZpZGVvc2AsIGBodHRwczovL2Nocm9tZS5nb29nbGUuY29tL3dlYnN0b3JlL2RldGFpbC9sbGhtYWNpZ2duaWJuYmRva2lkbWJpbGtsY2Vhb2JhZWApO1xyXG5cdH1cclxufVxyXG5mdW5jdGlvbiBzdWdnZXN0T2ZmaWNlRWRpdGluZygpIHtcclxuXHQvLyBPbmx5IHN1Z2dlc3Qgb24gY2hyb21lLCBiL2MgZmlyZWZveCB1c2VycyBleGlzdFxyXG5cdGlmKCFjaHJvbWVFeHRlbnNpb25JbnN0YWxsZWQoJ2dia2VlZ2JhaWlnbWVuZm1qZmNsY2RnZHBpbWFtZ2tqJykpIHtcclxuXHRcdGNvbnNvbGUud2FybihgRDJMIHVzZXJzY3JpcHQgcmVjb21tZW5kcyBpbnN0YWxsaW5nIHRoZSBcIk9mZmljZSBFZGl0aW5nIGZvciBEb2NzLCBTaGVldHMgJiBTbGlkZXNcIiBleHRlbnNpb24gZm9yIGludGVyYWN0aXZlbHkgdmlld2luZyBvZmZpY2UgZG9jdW1lbnRzYCwgYGh0dHBzOi8vY2hyb21lLmdvb2dsZS5jb20vd2Vic3RvcmUvZGV0YWlsL2dia2VlZ2JhaWlnbWVuZm1qZmNsY2RnZHBpbWFtZ2tqYCk7XHJcblx0fVxyXG59XHJcblxyXG5mdW5jdGlvbiBjaHJvbWVFeHRlbnNpb25JbnN0YWxsZWQoaWQ6IHN0cmluZykge1xyXG5cdHRyeSB7XHJcblx0XHQvLyBAdHMtaWdub3JlXHJcblx0XHRpZih3aW5kb3cuY2hyb21lKSB7IHdpbmRvdy5jaHJvbWUucnVudGltZS5zZW5kTWVzc2FnZShpZCwgbnVsbCk7IHJldHVybiB0cnVlOyB9XHJcblx0XHRcclxuXHR9IGNhdGNoIChlKSB7XHJcblx0XHRpZiAoIWUubWVzc2FnZS5pbmNsdWRlcyhcIkludmFsaWQgZXh0ZW5zaW9uIGlkXCIpKSB7IHRocm93IGU7IH1cclxuXHR9XHJcblx0cmV0dXJuIGZhbHNlO1xyXG59XHJcblxyXG4vKiogVXNlZnVsIGZvciBlYXJseSBleGl0IGlmIHdlIGFyZW4ndCB0aGUgdG9wIGZyYW1lICovXHJcbmZ1bmN0aW9uIHdpdGhpbklmcmFtZSgpIHtcclxuXHQvLyBodHRwczovL3N0YWNrb3ZlcmZsb3cuY29tL2EvMzI2MDc2LzExNTM2NjE0XHJcblx0dHJ5IHtcclxuXHRcdHJldHVybiB3aW5kb3cuc2VsZiAhPT0gd2luZG93LnRvcDtcclxuXHR9IGNhdGNoIChlKSB7XHJcblx0XHRyZXR1cm4gdHJ1ZTsgLy8gYWNjZXNzIGRlbmllZCBlcnJvclxyXG5cdH1cclxufVxyXG5cclxuZnVuY3Rpb24gZDJsX2ljb24oYmFja2dyb3VuZEltYWdlOiBzdHJpbmcpOiBIVE1MU3BhbkVsZW1lbnQge1xyXG5cdGxldCBpID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnc3BhbicpO1xyXG5cdGkuY2xhc3NMaXN0LmFkZCgnZDJsLWljb24tY3VzdG9tJyk7XHJcblx0aS5zdHlsZS5iYWNrZ3JvdW5kSW1hZ2UgPSBiYWNrZ3JvdW5kSW1hZ2U7XHJcblx0aS5zdHlsZS5iYWNrZ3JvdW5kUG9zaXRpb24gPSBcIjAgMFwiO1xyXG5cdGkuc3R5bGUuYmFja2dyb3VuZFJlcGVhdCA9IFwibm8tcmVwZWF0XCI7XHJcblx0aS5zdHlsZS53aWR0aCA9IFwiMThweFwiO1xyXG5cdGkuc3R5bGUuaGVpZ2h0ID0gXCIxOHB4XCI7XHJcblx0aS5zdHlsZS5iYWNrZ3JvdW5kU2l6ZSA9IFwiMThweCAxOHB4XCI7XHJcblx0cmV0dXJuIGk7XHJcbn1cclxuXHJcbmZ1bmN0aW9uIGQybF9pY29uX2Rvd25sb2FkKCk6IEhUTUxFbGVtZW50IHtcclxuXHRyZXR1cm4gZDJsX2ljb24oXCJ1cmwoJ2h0dHBzOi8vcy5icmlnaHRzcGFjZS5jb20vbGliL2JzaS8yMC4yMC44LTg1L2ltYWdlcy90aWVyMS9kb3dubG9hZC5zdmcnKVwiKTtcclxufVxyXG5cclxuLyoqIFRoZSBVUkwgaW50ZXJmYWNlIHJlcHJlc2VudHMgYW4gb2JqZWN0IHByb3ZpZGluZyBzdGF0aWMgbWV0aG9kcyB1c2VkIGZvciBjcmVhdGluZyBvYmplY3QgVVJMcy4gKi9cclxuZnVuY3Rpb24gbmV3VVJMKHVybDogc3RyaW5nIHwgVVJMIHwgTG9jYXRpb24pOiBVUkwge1xyXG5cdGlmKHR5cGVvZiB1cmwgPT0gJ3N0cmluZycpXHJcblx0XHRyZXR1cm4gbmV3IFVSTCh1cmwsIGRvY3VtZW50LmxvY2F0aW9uLmhyZWYpO1xyXG5cdGVsc2UgaWYodXJsIGluc3RhbmNlb2YgVVJMKSB7XHJcblx0XHRyZXR1cm4gbmV3IFVSTCh1cmwuaHJlZik7XHJcblx0fSBlbHNlIGlmKHVybCBpbnN0YW5jZW9mIExvY2F0aW9uKSB7XHJcblx0XHRyZXR1cm4gbmV3IFVSTCh1cmwuaHJlZik7XHJcblx0fSBlbHNlIHtcclxuXHRcdHRocm93IG5ldyBFcnJvcigndW5rbm93biBwYXJhbWV0ZXIgdHlwZScpO1xyXG5cdH1cclxufVxyXG5cclxuLyoqIFJldHVybiB0aGUgdHlwZSBvZiBEMkwgcGFnZSB3ZSBhcmUgb24sIHdpdGggc29tZSBhcHByb3ByaWF0ZSBtZXRhZGF0YSBhYm91dCBpdCAqL1xyXG5mdW5jdGlvbiBnZXRQYWdlVHlwZSgpOiBQYWdlVHlwZSB7XHJcblx0bGV0IGhyZWYgPSBuZXdVUkwoZG9jdW1lbnQubG9jYXRpb24pO1xyXG5cdC8vIHNsaWNlIHRvIHJlbW92ZSB6ZXJvLWxlbmd0aCBjb21wb25lbnQgYXQgYmVnaW5uaW5nIChzaW5jZSBwYXRobmFtZXMgc3RhcnQgd2l0aCAnLycpXHJcblx0bGV0IGNvbXBvbmVudHMgPSBocmVmLnBhdGhuYW1lLnNwbGl0KCcvJykuc2xpY2UoMSk7XHJcblx0aWYoXHJcblx0XHRjb21wb25lbnRzLmxlbmd0aCA9PSA3XHJcblx0XHQmJiBjb21wb25lbnRzWzBdID09IFwiZDJsXCJcclxuXHRcdCYmIGNvbXBvbmVudHNbMV0gPT0gXCJsZVwiXHJcblx0XHQmJiBjb21wb25lbnRzWzJdID09IFwiY29udGVudFwiXHJcblx0XHQvLyBjbGFzcyBJRFxyXG5cdFx0JiYgY29tcG9uZW50c1s0XSA9PSBcInZpZXdDb250ZW50XCJcclxuXHRcdC8vIGFzc2V0IElEXHJcblx0XHQmJiBjb21wb25lbnRzWzZdID09IFwiVmlld1wiXHJcblx0KSB7XHJcblx0XHRyZXR1cm4geyB0eXBlOiBcImNvbnRlbnRcIiwgY2xhc3M6IGNvbXBvbmVudHNbM10sIGFzc2V0OiBjb21wb25lbnRzWzVdLCB9O1xyXG5cdH0gZWxzZSBpZihcclxuXHRcdGNvbXBvbmVudHMubGVuZ3RoID09IDVcclxuXHRcdCYmIGNvbXBvbmVudHNbMF0gPT0gXCJkMmxcIlxyXG5cdFx0JiYgY29tcG9uZW50c1sxXSA9PSBcImxlXCJcclxuXHRcdCYmIGNvbXBvbmVudHNbMl0gPT0gXCJjb250ZW50XCJcclxuXHRcdC8vIGNsYXNzIElEXHJcblx0XHQmJiBjb21wb25lbnRzWzRdID09IFwiSG9tZVwiXHJcblx0KSB7XHJcblx0XHRyZXR1cm4geyB0eXBlOiBcImNvbnRlbnRfdG9jXCIsIGNsYXNzOiBjb21wb25lbnRzWzNdIH07XHJcblx0fVxyXG5cclxuXHRyZXR1cm4geyB0eXBlOiBcInVua25vd25cIiB9O1xyXG59XHJcbiIsIlxyXG5pbnRlcmZhY2UgQ2FwYWJpbGl0aWVzIHtcclxuXHRleHRlbnNpb25zOiB7XHJcblx0XHRvZmZpY2Vfdmlld2VyOiBib29sZWFuLFxyXG5cdFx0dmlkZW9fc2hvcnRjdXRzOiBib29sZWFuLFxyXG5cdH0sXHJcblx0YnJvd3NlcjogXCJjaHJvbWVcIiB8IFwiZmlyZWZveFwiIHwgXCJ1bmtub3duXCIsXHJcbn07XHJcblxyXG5pbnRlcmZhY2UgRDJMQXNzZXRNZXRhZGF0YSB7XHJcblx0dHlwZTogc3RyaW5nLFxyXG5cdGZpbGVuYW1lOiBzdHJpbmd8bnVsbCxcclxuXHRzaXplOiBudW1iZXIsXHJcbn1cclxuXHJcbnR5cGUgUGFnZVR5cGUgPSBQYWdlLlVua25vd24gfCBQYWdlLkNvbnRlbnQgfCBQYWdlLkNvbnRlbnRUb2M7XHJcbm5hbWVzcGFjZSBQYWdlIHtcclxuXHRleHBvcnQgaW50ZXJmYWNlIFVua25vd24ge1xyXG5cdFx0dHlwZTogXCJ1bmtub3duXCI7XHJcblx0fVxyXG5cdGV4cG9ydCBpbnRlcmZhY2UgQ29udGVudCB7XHJcblx0XHR0eXBlOiBcImNvbnRlbnRcIjtcclxuXHRcdGNsYXNzOiBzdHJpbmc7XHJcblx0XHRhc3NldDogc3RyaW5nO1xyXG5cdH1cclxuXHRleHBvcnQgaW50ZXJmYWNlIENvbnRlbnRUb2Mge1xyXG5cdFx0dHlwZTogXCJjb250ZW50X3RvY1wiO1xyXG5cdFx0Y2xhc3M6IHN0cmluZztcclxuXHR9XHJcbn1cclxuXHJcbiJdfQ==