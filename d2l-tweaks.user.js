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
                append(this.titleBtn_useNativeFrame(this.interactiveURL));
            }
            append(this.titleLink("View Directly", this.interactiveURL));
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
        if (asPDF) {
            if (asPDF[1])
                return asPDF[0];
            let asOffice = this.officeUrl();
            if (asOffice)
                return asOffice;
            // The rendered PDF is not an office file. Fallback to browser PDF renderer.
            return asPDF[0];
        }
        let asExtPage = this.extPageUrl();
        if (asExtPage)
            return asExtPage;
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
        if (OFFICE_DOCUMENTS_DIRECT_VIEW_USES_EXTENSION && CAPABILITIES.extensions.office_viewer) {
            let asPDF = this.pdfUrl();
            if (asPDF && !asPDF[1]) {
                // if we aren't a native PDF - we might be a converted office PDF?
                // check in pdfUrl ensures there is only 1 element that matches this
                let viewer = this.contentView.querySelectorAll("div[class^=d2l-fileviewer-pdf-][data-location]")[0];
                let ext = viewer.getAttribute('data-title')?.split('.').slice(-1)[0];
                if (['doc', 'dot', 'xls', 'xlt', 'csv', 'ppt', 'pot', 'pps', 'sld'].some(s => ext?.startsWith(s))) {
                    return this.naiveFileURL(true);
                    //return newURL(`/content/enforced/`)
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
    titleBtn_useNativeFrame(src) {
        const that = this;
        function btnonclick() {
            // installed as onclick handler => this = `<button>...</button>`
            // remove ourselves (button) since we have served our purpose
            this.remove();
            that.replaceContent(src);
        }
        ;
        // Note: put button in immediatly, provide link after PDF was downloaded
        return this.titleBtn("Use Native Viewer", btnonclick);
    }
}
// ==UserScript==
// @name         D2L Tweaks
// @namespace    https://github.com/csm123199/d2l-tweaks
// @version      0.8
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
const OFFICE_DOCUMENTS_DIRECT_VIEW_USES_EXTENSION = true;
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
                let contentPage = new ContentPage(ptype.class, ptype.asset);
                console.log("Content page: ", contentPage);
                if (MAKE_NATIVE_ON_LOAD)
                    contentPage.normalizeContent();
                contentPage.addHeaderButtons();
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
        if (chrome) {
            chrome.runtime.sendMessage(id, null);
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
    return { type: "unknown" };
}
;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZDJsLXR3ZWFrcy51c2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic3JjL2NvbnRlbnQtcGFnZS50cyIsInNyYy9kMmwtdHdlYWtzLnVzZXIudHMiLCJzcmMvaGVscGVycy50cyIsInNyYy90eXBlcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQ0EsNEVBQTRFO0FBQzVFLFNBQVMsYUFBYSxDQUFDLEdBQVcsRUFBRSxLQUFhO0lBQ2hELElBQUksQ0FBQyxNQUFNLENBQUMsUUFBUSxDQUFDLE1BQU0sQ0FBQyxRQUFRLENBQUMsR0FBRyxDQUFDLENBQUMsRUFBRTtRQUMzQyxNQUFNLElBQUksS0FBSyxDQUFDLGdEQUFnRCxHQUFHLEdBQUcsQ0FBQyxDQUFDO0tBQ3hFO0lBQ0QsSUFBSSxDQUFDLE1BQU0sQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsQ0FBQyxFQUFFO1FBQzdDLE1BQU0sSUFBSSxLQUFLLENBQUMsZ0RBQWdELEtBQUssR0FBRyxDQUFDLENBQUM7S0FDMUU7SUFFRCxzSEFBc0g7SUFDdEgsSUFBSSxRQUFRLEdBQUcsTUFBTSxDQUFDLG9CQUFvQixHQUFHLG1CQUFtQixLQUFLLE9BQU8sQ0FBQyxDQUFDO0lBQzlFLE9BQU87UUFDTixRQUFRO1FBQ1IsWUFBWSxDQUFDLFFBQVEsQ0FBQztLQUN0QixDQUFBO0FBQ0YsQ0FBQztBQUNELEtBQUssVUFBVSxZQUFZLENBQUMsUUFBYTtJQUN4QyxJQUFJLENBQUMsR0FBRyxNQUFNLEtBQUssQ0FBQyxRQUFRLENBQUMsUUFBUSxFQUFFLEVBQUUsRUFBRSxNQUFNLEVBQUUsTUFBTSxFQUFFLENBQUMsQ0FBQztJQUU3RCw0Q0FBNEM7SUFDNUMsSUFBSSxJQUFJLEdBQUcsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxHQUFHLENBQUMsY0FBYyxDQUFFLENBQUM7SUFDMUMsSUFBSSxRQUFRLEdBQUcsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxHQUFHLENBQUMscUJBQXFC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Q3JGLFNBQVMsV0FBVztJQUNuQixJQUFJLElBQUksR0FBRyxNQUFNLENBQUMsUUFBUSxDQUFDLFFBQVEsQ0FBQyxDQUFDO0lBQ3JDLHNGQUFzRjtJQUN0RixJQUFJLFVBQVUsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFDLENBQUM7SUFDbkQsSUFDQyxVQUFVLENBQUMsTUFBTSxJQUFJLENBQUM7V0FDbkIsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLEtBQUs7V0FDdEIsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLElBQUk7V0FDckIsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLFNBQVM7UUFDN0IsV0FBVztXQUNSLFVBQVUsQ0FBQyxDQUFDLENBQUMsSUFBSSxhQUFhO1FBQ2pDLFdBQVc7V0FDUixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksTUFBTSxFQUN6QjtRQUNELE9BQU8sRUFBRSxJQUFJLEVBQUUsU0FBUyxFQUFFLEtBQUssRUFBRSxVQUFVLENBQUMsQ0FBQyxDQUFDLEVBQUUsS0FBSyxFQUFFLFVBQVUsQ0FBQyxDQUFDLENBQUMsR0FBRyxDQUFDO0tBQ3hFO0lBRUQsT0FBTyxFQUFFLElBQUksRUFBRSxTQUFTLEVBQUUsQ0FBQztBQUM1QixDQUFDO0FDOUdBLENBQUMiLCJzb3VyY2VzQ29udGVudCI6WyJcclxuLy8gTWF5YmUgdGhlc2Ugd2lsbCBiZSB1c2VmdWwgZm9yIGEgbm9uLWNvbnRlbnQgcGFnZT8gS2VlcGluZyBhcm91bmQgZm9yIG5vd1xyXG5mdW5jdGlvbiB1cmxPZkQyTEFzc2V0KGNsczogc3RyaW5nLCBhc3NldDogc3RyaW5nKTogW1VSTCwgUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhPl17XHJcblx0aWYgKCFOdW1iZXIuaXNGaW5pdGUoTnVtYmVyLnBhcnNlSW50KGNscykpKSB7XHJcblx0XHR0aHJvdyBuZXcgRXJyb3IoYEQyTCBjbGFzcyBJRCBpc24ndCBwYXJzYWJsZSB0byBhIG51bWJlci9JRDogJyR7Y2xzfSdgKTtcclxuXHR9XHJcblx0aWYgKCFOdW1iZXIuaXNGaW5pdGUoTnVtYmVyLnBhcnNlSW50KGFzc2V0KSkpIHtcclxuXHRcdHRocm93IG5ldyBFcnJvcihgRDJMIGFzc2V0IElEIGlzbid0IHBhcnNhYmxlIHRvIGEgbnVtYmVyL0lEOiAnJHthc3NldH0nYCk7XHJcblx0fVxyXG5cclxuXHQvLyB0aGUgP3N0cmVhbT10cnVlIHRlbGxzIEQyTCB0byB1c2UgYSByZXNwb25zZSBoZWFkZXIgZm9yIGNvbnRlbnQgdG8gYmUgdmlld2VkIGluIHRoZSBicm93c2VyLCByYXRoZXIgdGhhbiBkb3dubG9hZGVkXHJcblx0bGV0IGFzc2V0VVJMID0gbmV3VVJMKGAvZDJsL2FwaS9sZS8xLjM0LyR7Y2xzfS9jb250ZW50L3RvcGljcy8ke2Fzc2V0fS9maWxlYCk7XHJcblx0cmV0dXJuIFtcclxuXHRcdGFzc2V0VVJMLFxyXG5cdFx0RDJMQXNzZXRNZXRhKGFzc2V0VVJMKSwgLy8gcmV0dXJucyBwcm9taXNlXHJcblx0XVxyXG59XHJcbmFzeW5jIGZ1bmN0aW9uIEQyTEFzc2V0TWV0YShsb2NhdGlvbjogVVJMKTogUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhPiB7XHJcblx0bGV0IGYgPSBhd2FpdCBmZXRjaChsb2NhdGlvbi50b1N0cmluZygpLCB7IG1ldGhvZDogJ0hFQUQnIH0pO1xyXG5cclxuXHQvLyBnZXQgbWltZSBhbmQgb3JpZyBmaWxlIG5hbWUsIGlmIGF2YWlsYWJsZVxyXG5cdGxldCB0eXBlID0gZi5oZWFkZXJzLmdldChcImNvbnRlbnQtdHlwZVwiKSE7XHJcblx0bGV0IGZpbGVuYW1lID0gZi5oZWFkZXJzLmdldChcImNvbnRlbnQtZGlzcG9zaXRpb25cIik/Lm1hdGNoKC9maWxlbmFtZT1cIiguKylcIi8pPy5bMV0gPz8gbnVsbDsgLy9udWxsIGlmIGhlYWRlciBtaXNzaW5nLCBubyBmaWxlbmFtZSwgZXRjXHJcblx0bGV0IHNpemUgPSBOdW1iZXIucGFyc2VJbnQoZi5oZWFkZXJzLmdldChcImNvbnRlbnQtbGVuZ3RoXCIpISk7XHJcblxyXG5cdHJldHVybiB7IHR5cGUsIGZpbGVuYW1lLCBzaXplIH07XHJcbn1cclxuXHJcbi8qKiBSZXByZXNlbnRzIGEgRDJMIENvbnRlbnQgcGFnZS4gRGlzdGluZ3Vpc2hlZCBieSBhIGAvZDJsL2xlL2NvbnRlbnQvJGNsYXNzL3ZpZXdDb250ZW50LyRhc3NldC9WaWV3YCB1cmwqL1xyXG5jbGFzcyBDb250ZW50UGFnZSB7XHJcblx0cHJvdGVjdGVkIGNvbnRlbnRWaWV3OiBIVE1MRWxlbWVudDtcclxuXHRwcm90ZWN0ZWQgaGRyQmFyOiBIVE1MRWxlbWVudDtcclxuXHRwcm90ZWN0ZWQgY2xzOiBzdHJpbmc7XHJcblx0cHJvdGVjdGVkIGFzc2V0OiBzdHJpbmc7XHJcblx0LyoqIE1heSBwb2ludCB0byBhIDQwNC4gU2hvdWxkIGJlIGF2b2lkZWQuICovXHJcblx0cHJvdGVjdGVkIG5haXZlQXNzZXRVUkw6IFVSTDtcclxuXHJcblx0cHJvdGVjdGVkIGludGVyYWN0aXZlVVJMOiBVUkwgfCBudWxsO1xyXG5cdHByb3RlY3RlZCBkb3dubG9hZGFibGVVUkw6IFVSTCB8IG51bGw7XHJcblxyXG5cdHByb3RlY3RlZCBfbmFpdmVBc3NldE1ldGE/OiBQcm9taXNlPEQyTEFzc2V0TWV0YWRhdGEgfCBudWxsPjtcclxuXHRwcm90ZWN0ZWQgcmVwbGFjZWRDb250ZW50OiBib29sZWFuO1xyXG5cclxuXHRzdGF0aWMgaW5pdGlhbGl6ZSgpOiBDb250ZW50UGFnZSB8IG51bGwge1xyXG5cdFx0bGV0IHB0eXBlID0gZ2V0UGFnZVR5cGUoKTtcclxuXHRcdGlmKHB0eXBlLnR5cGUgPT0gXCJjb250ZW50XCIpIHtcclxuXHRcdFx0cmV0dXJuIG5ldyBDb250ZW50UGFnZShwdHlwZS5jbGFzcywgcHR5cGUuYXNzZXQpO1xyXG5cdFx0fVxyXG5cdFx0cmV0dXJuIG51bGw7XHJcblx0fVxyXG5cdGNvbnN0cnVjdG9yKGNsczogc3RyaW5nLCBhc3NldDogc3RyaW5nKSB7XHJcblx0XHRsZXQgY3YgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yKFwiI0NvbnRlbnRWaWV3XCIpIGFzIEhUTUxFbGVtZW50IHwgbnVsbDtcclxuXHRcdGlmKCFjdikgeyB0aHJvdyBuZXcgRXJyb3IoXCJQYWdlIGRvZXNuJ3QgaGF2ZSBhICNDb250ZW50VmlldyAhXCIpOyB9XHJcblxyXG5cdFx0bGV0IGhkckJhciA9IGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoXCIuZDJsLXBhZ2UtdGl0bGUtYyAuZDJsLWJveC1oXCIpIGFzIEhUTUxFbGVtZW50IHwgbnVsbDtcclxuXHRcdGlmKCFoZHJCYXIpIHRocm93IG5ldyBFcnJvcihcIlVuYWJsZSB0byBmaW5kIGNvbnRlbnQgaGVhZGVyIGJhciFcIik7XHJcblx0XHRcclxuXHJcblx0XHR0aGlzLmNvbnRlbnRWaWV3ID0gY3Y7XHJcblx0XHR0aGlzLmhkckJhciA9IGhkckJhcjtcclxuXHRcdHRoaXMuY2xzID0gY2xzO1xyXG5cdFx0dGhpcy5hc3NldCA9IGFzc2V0O1xyXG5cdFx0dGhpcy5uYWl2ZUFzc2V0VVJMID0gbmV3VVJMKGAvZDJsL2FwaS9sZS8xLjM0LyR7Y2xzfS9jb250ZW50L3RvcGljcy8ke2Fzc2V0fS9maWxlYCk7XHJcblxyXG5cdFx0dGhpcy5pbnRlcmFjdGl2ZVVSTCA9IHRoaXMuX2ludGVyYWN0aXZlVVJMKCk7XHJcblx0XHR0aGlzLmRvd25sb2FkYWJsZVVSTCA9IHRoaXMuX2Rvd25sb2FkYWJsZVVSTCgpO1xyXG5cclxuXHRcdHRoaXMuX25haXZlQXNzZXRNZXRhID0gdW5kZWZpbmVkO1xyXG5cdFx0dGhpcy5yZXBsYWNlZENvbnRlbnQgPSBmYWxzZTtcclxuXHJcblx0XHRjb25zb2xlLmxvZygnSGVhZGVyIGRyb3Bkb3duIGFjdGlvbnM6ICcsIHRoaXMuaGVhZGVyRHJvcGRvd25BY3Rpb25zKCkpO1xyXG5cdH1cclxuXHJcblx0cHVibGljIG5vcm1hbGl6ZUNvbnRlbnQoKSB7XHJcblx0XHRpZih0aGlzLmludGVyYWN0aXZlVVJMICYmICF0aGlzLnJlcGxhY2VkQ29udGVudCkge1xyXG5cdFx0XHR0aGlzLnJlcGxhY2VDb250ZW50KHRoaXMuaW50ZXJhY3RpdmVVUkwpO1xyXG5cdFx0fVxyXG5cdH1cclxuXHRcclxuXHRwdWJsaWMgYWRkSGVhZGVyQnV0dG9ucygpIHtcclxuXHRcdC8vIHJlcGxhY2UgaW5uZXIgY29udGVudFxyXG5cdFx0Ly8gb3BlbiBpbiBuZXcgdGFiXHJcblx0XHQvLyBkb3dubG9hZFxyXG5cclxuXHRcdGNvbnN0IGJ0bkJhciA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ3NwYW4nKTtcclxuXHRcdGJ0bkJhci5zdHlsZS5tYXJnaW5MZWZ0ID0gXCIwLjNyZW1cIjsgLy8gYXJiaXRyYXJ5IC0gbG9va2VkIGFscmlnaHQgZm9yIGEgZmV3IGRpZmZlcmVudCB0aXRsZSBuYW1lc1xyXG5cdFx0Y29uc3QgYXBwZW5kID0gKGVsZTogSFRNTEVsZW1lbnQpID0+IGJ0bkJhci5hcHBlbmRDaGlsZChlbGUpO1xyXG5cclxuXHRcdGlmKHRoaXMuaW50ZXJhY3RpdmVVUkwpIHtcclxuXHRcdFx0Y29uc29sZS5sb2coXCJBZGRpbmcgaW50ZXJhY3RpdmUgYnV0dG9uc1wiKTtcclxuXHRcdFx0aWYoIXRoaXMucmVwbGFjZWRDb250ZW50KSB7XHJcblx0XHRcdFx0Ly8gb25seSBzaG93IGJ1dHRvbiBpZiB3ZSBoYXZlbid0IHJlcGxhY2VkIG91cnNlbHZlcywgYW5kIHdlIGhhdmUgYW4gaW50ZXJhY3RpdmUgVVJMIHRvIHNob3dcclxuXHRcdFx0XHRhcHBlbmQodGhpcy50aXRsZUJ0bl91c2VOYXRpdmVGcmFtZSh0aGlzLmludGVyYWN0aXZlVVJMKSk7XHJcblx0XHRcdH1cclxuXHRcdFx0YXBwZW5kKHRoaXMudGl0bGVMaW5rKFwiVmlldyBEaXJlY3RseVwiLCB0aGlzLmludGVyYWN0aXZlVVJMKSk7XHJcblx0XHR9XHJcblxyXG5cdFx0aWYodGhpcy5kb3dubG9hZGFibGVVUkwpIHtcclxuXHRcdFx0Y29uc29sZS5sb2coXCJBZGRpbmcgZG93bmxvYWRhYmxlIGJ1dHRvbnNcIik7XHJcblx0XHRcdGFwcGVuZCh0aGlzLnRpdGxlTGluayhcIkRvd25sb2FkXCIsIHRoaXMuZG93bmxvYWRhYmxlVVJMLCBkMmxfaWNvbl9kb3dubG9hZCgpKSk7XHJcblxyXG5cdFx0XHRsZXQgaGRyQnRucyA9IHRoaXMuaGVhZGVyRHJvcGRvd25BY3Rpb25zKCk7XHJcblx0XHRcdGlmKGhkckJ0bnMgJiYgaGRyQnRucy5sZW5ndGggPT0gMSAmJiBoZHJCdG5zWzBdID09IFwiRG93bmxvYWRcIikge1xyXG5cdFx0XHRcdC8vIHJlbW92ZSBkcm9wZG93biBpZiB0aGVyZSBpcyBvbmx5ICdEb3dubG9hZCcgLSB3ZSBhcmUgc3VwcGx5aW5nIG91ciBvd24gYnV0dG9uIGFib3ZlXHJcblx0XHRcdFx0dGhpcy5oZHJCYXIucXVlcnlTZWxlY3RvcignZDJsLWRyb3Bkb3duW2RhdGEtY29udGV4dG1lbnVpZD1kMmxfcGFnZVRpdGxlQWN0aW9uc10nKT8ucmVtb3ZlKCk7XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHJcblx0XHR0aGlzLmhkckJhci5zdHlsZS5mbGV4V3JhcCA9ICd3cmFwJztcclxuXHRcdHRoaXMuaGRyQmFyLnN0eWxlLmZsZXhEaXJlY3Rpb24gPSAncm93JztcclxuXHRcdHRoaXMuaGRyQmFyLmFwcGVuZENoaWxkKGJ0bkJhcik7XHJcblx0fVxyXG5cdFxyXG5cclxuXHQvKiogUmV0dXJucyBhIFVSTCBhcHByb3ByaWF0ZSB0byBlbWJlZCBpbiBhbiBJRnJhbWUsIG9yIG9wZW4gaW4gYSBuZXcgYnJvd3NlciB0YWIuXHJcblx0ICogXHJcblx0ICogTW9zdCBpbXBvcnRhbnRseSwgaXQgZG9lc24ndCBzZXJ2ZSB0aGUgYENvbnRlbnQtRGlzcG9zaXRpb246IGF0dGFjaG1lbnRgIGhlYWRlci5cclxuXHQqL1xyXG5cdHByb3RlY3RlZCBfaW50ZXJhY3RpdmVVUkwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHQvLyBUT0RPOiBHZXQgbWltZSB0eXBlIGFuZCBjaGVjayBhZ2FpbnN0IG5hdmlnYXRvci5taW1lVHlwZXMgP1xyXG5cclxuXHRcdGxldCBhc1F1aXogPSB0aGlzLnF1aXpVcmwoKTtcclxuXHRcdGlmKGFzUXVpeikgcmV0dXJuIGFzUXVpejtcclxuXHJcblx0XHRsZXQgYXNNUDQgPSB0aGlzLm1wNFVybCgpO1xyXG5cdFx0aWYoYXNNUDQpIHJldHVybiBhc01QNDtcclxuXHJcblx0XHRsZXQgYXNQREYgPSB0aGlzLnBkZlVybCgpO1xyXG5cdFx0aWYoYXNQREYpIHtcclxuXHRcdFx0aWYoYXNQREZbMV0pIHJldHVybiBhc1BERlswXTtcclxuXHJcblx0XHRcdGxldCBhc09mZmljZSA9IHRoaXMub2ZmaWNlVXJsKCk7XHJcblx0XHRcdGlmKGFzT2ZmaWNlKSByZXR1cm4gYXNPZmZpY2U7XHJcblxyXG5cdFx0XHQgLy8gVGhlIHJlbmRlcmVkIFBERiBpcyBub3QgYW4gb2ZmaWNlIGZpbGUuIEZhbGxiYWNrIHRvIGJyb3dzZXIgUERGIHJlbmRlcmVyLlxyXG5cdFx0XHRyZXR1cm4gYXNQREZbMF07XHJcblx0XHR9XHJcblxyXG5cdFx0bGV0IGFzRXh0UGFnZSA9IHRoaXMuZXh0UGFnZVVybCgpO1xyXG5cdFx0aWYoYXNFeHRQYWdlKSByZXR1cm4gYXNFeHRQYWdlO1xyXG5cclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHJcblx0LyoqIFJldHVybmVkIFVSTCBzaG91bGQgaWRlYWxseSBzZXJ2ZSB0aGUgYENvbnRlbnQtRGlzcG9zaXRpb246IGF0dGFjaG1lbnRgIGhlYWRlciAqL1xyXG5cdHByb3RlY3RlZCBfZG93bmxvYWRhYmxlVVJMKCk6IFVSTCB8IG51bGwge1xyXG5cdFx0bGV0IGFzRXh0UGFnZSA9IHRoaXMuZXh0UGFnZVVybCgpO1xyXG5cdFx0aWYoYXNFeHRQYWdlKSByZXR1cm4gbnVsbDtcclxuXHJcblx0XHQvL2lmKHRoaXMucXVpelVybCgpKSByZXR1cm4gbnVsbDtcclxuXHRcdC8vIElmIHRoaXMgcGFnZSB3cmFwcyBEMkwgYW5vdGhlciBEMkwgcGFnZT9cclxuXHRcdC8vIGVnKSBxdWl6emVzLCBhc3NpZ25tZW50cywgZXRjIHBsYWNlZCBpbiBDb250ZW50XHJcblx0XHRpZihkb2N1bWVudC5xdWVyeVNlbGVjdG9yKFwiI0NvbnRlbnRWaWV3ID4gLmQybC1wbGFjZWhvbGRlclwiKSkgcmV0dXJuIG51bGw7XHJcblx0XHRpZih0aGlzLmV4dFBhZ2VVcmwoKSkgcmV0dXJuIG51bGw7XHJcblxyXG5cdFx0cmV0dXJuIHRoaXMubmFpdmVGaWxlVVJMKGZhbHNlKTtcclxuXHR9XHJcblxyXG5cdC8qKiBVbm5lZWRlZD8gKi9cclxuXHRwcml2YXRlIF9hc3NldE1ldGEoKTogUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhIHwgbnVsbD4ge1xyXG5cdFx0aWYodGhpcy5fbmFpdmVBc3NldE1ldGEpIHJldHVybiB0aGlzLl9uYWl2ZUFzc2V0TWV0YTtcclxuXHJcblx0XHRyZXR1cm4gdGhpcy5fbmFpdmVBc3NldE1ldGEgPSBmZXRjaCh0aGlzLm5haXZlQXNzZXRVUkwudG9TdHJpbmcoKSwgeyBtZXRob2Q6ICdIRUFEJyB9KVxyXG5cdFx0XHQudGhlbihyZXMgPT4ge1xyXG5cdFx0XHRcdGlmKHJlcy5zdGF0dXMgPT0gNDA0KSB7XHJcblx0XHRcdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdFx0XHR9IGVsc2Uge1xyXG5cdFx0XHRcdFx0Ly8gZ2V0IG1pbWUgYW5kIG9yaWcgZmlsZSBuYW1lLCBpZiBhdmFpbGFibGVcclxuXHRcdFx0XHRcdGxldCB0eXBlID0gcmVzLmhlYWRlcnMuZ2V0KFwiY29udGVudC10eXBlXCIpITtcclxuXHRcdFx0XHRcdGxldCBmaWxlbmFtZSA9IHJlcy5oZWFkZXJzLmdldChcImNvbnRlbnQtZGlzcG9zaXRpb25cIik/Lm1hdGNoKC9maWxlbmFtZT1cIiguKylcIi8pPy5bMV0gPz8gbnVsbDsgLy9udWxsIGlmIGhlYWRlciBtaXNzaW5nLCBubyBmaWxlbmFtZSwgZXRjXHJcblx0XHRcdFx0XHRsZXQgc2l6ZSA9IE51bWJlci5wYXJzZUludChyZXMuaGVhZGVycy5nZXQoXCJjb250ZW50LWxlbmd0aFwiKSEpO1xyXG5cdFx0XHRcdFxyXG5cdFx0XHRcdFx0cmV0dXJuIHsgdHlwZSwgZmlsZW5hbWUsIHNpemUgfSBhcyBEMkxBc3NldE1ldGFkYXRhO1xyXG5cdFx0XHRcdH1cclxuXHRcdFx0fSk7XHJcblx0fVxyXG5cclxuXHQvKiogUmV0dXJucyB0aGUgaHVtYW4tcmVhZGFibGUgbGFiZWxzIGZvciB0aGUgY29udGVudCdzIGRyb3Bkb3duIGFjdGlvbnMgKi9cclxuXHRwcm90ZWN0ZWQgaGVhZGVyRHJvcGRvd25BY3Rpb25zKCk6IHN0cmluZ1tdIHwgbnVsbCB7XHJcblx0XHQvLyBvbmx5IHdvcmtzIGlmIHRoZSBkcm9wZG93biBoYXMgYmVlbiBhY3RpdmF0ZWRcclxuXHRcdC8vQXJyYXkuZnJvbSh0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yQWxsKCdkMmwtZHJvcGRvd24gZDJsLWRyb3Bkb3duLW1lbnUgZDJsLW1lbnUgZDJsLW1lbnUtaXRlbScpKVxyXG5cdFx0Ly9cdC5tYXAoZSA9PiBlLmdldEF0dHJpYnV0ZSgndGV4dCcpKVxyXG5cclxuXHRcdC8vIEFjY2VzcyB1bnJlbmRlcmVkICh0ZW1wbGF0ZWQpIGRyb3Bkb3duIGFjdGlvbnMsIGFuZCBnZXQgdGhlaXIgdGV4dC5cclxuXHRcdGxldCB0ZW1wbDogSFRNTFRlbXBsYXRlRWxlbWVudCB8IG51bGwgPSB0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yKCd0ZW1wbGF0ZSNkMmxfcGFnZVRpdGxlQWN0aW9ucycpO1xyXG5cdFx0aWYoIXRlbXBsKSByZXR1cm4gbnVsbDtcclxuXHRcdHJldHVybiBBcnJheS5mcm9tKHRlbXBsLmNvbnRlbnQucXVlcnlTZWxlY3RvckFsbCgnZDJsLW1lbnUtaXRlbVt0ZXh0XScpKVxyXG5cdFx0XHQubWFwKGUgPT4gZS5nZXRBdHRyaWJ1dGUoJ3RleHQnKSEpO1xyXG5cclxuXHRcdC8qIGN1cnJlbnRseSBrbm93bjpcclxuXHRcdFx0ZmlsZSBjb250ZW50OiBcIkRvd25sb2FkXCJcclxuXHRcdFx0cXVpejogXCJWaWV3IFN1bW1hcnlcIlxyXG5cdFx0XHRxdWl6OiBcIlZpZXcgU3VibWlzc2lvbnNcIlxyXG5cdFx0XHRxdWl6OiBcIlZpZXcgUmVwb3J0c1wiXHJcblx0XHQqL1xyXG5cdH1cclxuXHJcblx0Ly8gS2VlcCB0aGVzZSBvcmdhbml6ZWQgcm91Z2hseSBpbiB0aGUgb3JkZXIgb2YgbGVhc3QtPm1vc3QgZXhwZW5zaXZlIHRvIHJ1blxyXG5cclxuXHRwcml2YXRlIHF1aXpVcmwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHRsZXQgcXVpel9pZnJhbWUgPSB0aGlzLmNvbnRlbnRWaWV3LnF1ZXJ5U2VsZWN0b3IoJyNRdWl6Q29udGVudFZpZXdQbGFjZUhvbGRlciA+IGlmcmFtZVtzcmNdJyk7XHJcblx0XHRpZihxdWl6X2lmcmFtZSkge1xyXG5cdFx0XHRsZXQgdXJsID0gbmV3VVJMKHF1aXpfaWZyYW1lLmdldEF0dHJpYnV0ZSgnc3JjJykhKTtcclxuXHRcdFx0Ly8gRW5zdXJlIGEgcmVndWxhciBEMkwgcGFnZSwgcmF0aGVyIHRoYW4gd2l0aG91dCBoZWFkZXJzL21hcmdpbnMvYm9yZGVycy9ldGNcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2NmcWwnKTtcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2RuYicpO1xyXG5cdFx0XHR1cmwuc2VhcmNoUGFyYW1zLmRlbGV0ZSgnY29udGVudFRvcGljSWQnKTtcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2QybF9ib2R5X3R5cGUnKTtcclxuXHRcdFx0cmV0dXJuIHVybDtcclxuXHRcdH1cclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHJcblx0Ly8gSW50ZXJhY3RpdmUgb25seSAtIG5vIGNvbnRlbnQtZGlzcG9zaXRpb25cclxuXHRwcml2YXRlIG1wNFVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdGxldCBwbGF5ZXJzID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2LnZ1aS1tZWRpYXBsYXllcltkYXRhLW1lZGlhcGxheWVyLXNyY11cIik7XHJcblx0XHRpZihwbGF5ZXJzLmxlbmd0aCA9PSAwKSB7XHJcblx0XHRcdHJldHVybiBudWxsO1xyXG5cdFx0fSBlbHNlIGlmKHBsYXllcnMubGVuZ3RoID4gMSkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoJ01vcmUgdGhhbiAxIG1lZGlhIHBsYXllciBmb3VuZCBvbiB0aGUgcGFnZSEnKTtcclxuXHRcdH1cclxuXHRcdGxldCBwbGF5ZXIgPSBwbGF5ZXJzWzBdO1xyXG5cclxuXHRcdHJldHVybiBuZXdVUkwocGxheWVyLmdldEF0dHJpYnV0ZSgnZGF0YS1tZWRpYXBsYXllci1zcmMnKSEpO1xyXG5cdH1cclxuXHJcblx0LyoqIEludGVyYWN0aXZlLiBNYXkgYmUgaGFuZGxlZCBieSBhbiBleHRlbnNpb24gKi9cclxuXHRwcml2YXRlIG9mZmljZVVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdC8qXHJcblx0XHQgKiBUaGVyZSdzIGN1cnJlbnRseSBhIGJ1ZyBpbiB0aGUgYE9mZmljZSBFZGl0aW5nIGZvciBEb2NzLCBTaGVldHMsICYgU2xpZGVzYCBleHRlbnNpb24gdGhhdCBwcmV2ZW50cyBpdCBmcm9tIHBhcnNpbmcgdGhlICdDb250ZW50LURpc3Bvc2l0aW9uJyBoZWFkZXIgY29ycmVjdGx5LlxyXG5cdFx0ICogV2hpbGUgaXQgZG9lc24ndCBhZmZlY3QgZnVuY3Rpb25hbGl0eSwgdGhlIGZpbGVuYW1lIGlzIG1hbmdsZWQgZnJvbSB3aGF0IGl0IGlzIHN1cHBvc2VkIHRvIGJlLlxyXG5cdFx0ICogXHJcblx0XHQgKiBXZSBjb3VsZCB0cnkgdG8gc2VydmUgdGhlIGZpbGUgb3ZlciBgL2NvbnRlbnQvZW5mb3JjZWQvJHtjbGFzc19uYW1lfS8ke2RpdltkYXRhLXRpdGxlXX1gIGJ1dCB0aGVyZSBpc24ndCBhbiBlYXN5KD8pIHdheSB0byBnZXQgJGNsYXNzX25hbWUgZnJvbSB0aGUgcGFnZSAoRDJMIEpTIGFwaT8pXHJcblx0XHQgKiBNUDQgZmlsZXMgc2VlbSB0byBiZSBzZXJ2ZWQgZnJvbSB0aGlzIGZvbGRlciwgaG93ZXZlci5cclxuXHRcdCovXHJcblx0XHRpZihPRkZJQ0VfRE9DVU1FTlRTX0RJUkVDVF9WSUVXX1VTRVNfRVhURU5TSU9OICYmIENBUEFCSUxJVElFUy5leHRlbnNpb25zLm9mZmljZV92aWV3ZXIpIHtcclxuXHRcdFx0bGV0IGFzUERGID0gdGhpcy5wZGZVcmwoKTtcclxuXHRcdFx0aWYoYXNQREYgJiYgIWFzUERGWzFdKSB7XHJcblx0XHRcdFx0Ly8gaWYgd2UgYXJlbid0IGEgbmF0aXZlIFBERiAtIHdlIG1pZ2h0IGJlIGEgY29udmVydGVkIG9mZmljZSBQREY/XHJcblx0XHJcblx0XHRcdFx0Ly8gY2hlY2sgaW4gcGRmVXJsIGVuc3VyZXMgdGhlcmUgaXMgb25seSAxIGVsZW1lbnQgdGhhdCBtYXRjaGVzIHRoaXNcclxuXHRcdFx0XHRsZXQgdmlld2VyID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2W2NsYXNzXj1kMmwtZmlsZXZpZXdlci1wZGYtXVtkYXRhLWxvY2F0aW9uXVwiKVswXTtcclxuXHRcdFx0XHRcclxuXHRcdFx0XHRsZXQgZXh0ID0gdmlld2VyLmdldEF0dHJpYnV0ZSgnZGF0YS10aXRsZScpPy5zcGxpdCgnLicpLnNsaWNlKC0xKVswXTtcclxuXHRcdFx0XHRpZihbJ2RvYycsICdkb3QnLCAneGxzJywgJ3hsdCcsICdjc3YnLCAncHB0JywgJ3BvdCcsICdwcHMnLCAnc2xkJ10uc29tZShzID0+IGV4dD8uc3RhcnRzV2l0aChzKSkpIHtcclxuXHRcdFx0XHRcdHJldHVybiB0aGlzLm5haXZlRmlsZVVSTCh0cnVlKTtcclxuXHRcdFx0XHRcdC8vcmV0dXJuIG5ld1VSTChgL2NvbnRlbnQvZW5mb3JjZWQvYClcclxuXHRcdFx0XHR9XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHRcdFxyXG5cdFx0cmV0dXJuIG51bGw7XHJcblx0fVxyXG5cclxuXHQvKiogSW50ZXJhY3RpdmUgb25seS4gLSBubyBjb250ZW50LWRpc3Bvc2l0aW9uLlxyXG5cdCAqIFxyXG5cdCAqIE1heSBiZSBkb3dubG9hZGFibGUgaWZmIGBwZGZVcmxbMV1gXHJcblx0ICovXHJcblx0cHJpdmF0ZSBwZGZVcmwoKTogW1VSTCwgbmF0aXZlUERGOiBib29sZWFuXSB8IG51bGwge1xyXG5cdFx0Ly8gbmF0aXZlIHBkZjogLmQybC1maWxldmlld2VyLXBkZi1uYXRpdmVcclxuXHRcdC8vIGNvbnZlcnRlZCB0byBwZGY6IC5kMmwtZmlsZXZpZXdlci1wZGYtcGRmanNcclxuXHJcblx0XHQvLyBDb3VsZCB3ZSBkZXRlcm1pbmUgaWYgbmF0aXZlL2NvbnZlcnRlZCB2aWEgdGhlIGRhdGEtbG9jYXRpb24gVVJMP1xyXG5cdFx0Ly8gRmlsZXMgY29udmVydGVkIHRvIFBERiB0ZW5kIHRvIGJlIHNlcnZlZCBvdmVyIEFXU1xyXG5cclxuXHRcdC8vIEdldCBQREYgdmlld2VycyBvbiB0aGUgcGFnZSwgYW5kIGVuc3VyZSB0aGVyZSBpcyBvbmx5IG9uZSBmb3Igb3VyIGNvbnRlbnRcclxuXHRcdGxldCBwZGZWaWV3ZXJzID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2W2NsYXNzXj1kMmwtZmlsZXZpZXdlci1wZGYtXVtkYXRhLWxvY2F0aW9uXVwiKVxyXG5cdFx0aWYocGRmVmlld2Vycy5sZW5ndGggPT0gMCkge1xyXG5cdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdH0gZWxzZSBpZihwZGZWaWV3ZXJzLmxlbmd0aCA+IDEpIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdNb3JlIHRoYW4gMSBQREYgRmlsZXZpZXdlciB3aXRoIGEgZGF0YSBsb2NhdGlvbiBmb3VuZCBvbiB0aGUgcGFnZSEnKTtcclxuXHRcdH1cclxuXHRcdGxldCB2aWV3ZXIgPSBwZGZWaWV3ZXJzWzBdO1xyXG5cclxuXHRcdC8vIEV4dHJhY3QgdGhlIHR5cGUgb2YgY29udGVudHMgb3VyIFBERiB2aWV3ZXIgaXMgc2hvd2luZ1xyXG5cdFx0bGV0IHR5cGUgPSBBcnJheS5mcm9tKHZpZXdlci5jbGFzc0xpc3QpXHJcblx0XHRcdC5maWx0ZXIocyA9PiBzLnN0YXJ0c1dpdGgoJ2QybC1maWxldmlld2VyLXBkZi0nKSlcclxuXHRcdFx0Lm1hcChzID0+IHMuc3BsaXQoJy0nKS5zbGljZSgtMSlbMF0pOyAvLyBnZXQgbGFzdCBjb21wb25lbnRcclxuXHRcdGlmKHR5cGUubGVuZ3RoICE9IDEpIHsgLy8gPjEgYmVjYXVzZSB3ZSBmaWx0ZXIgdG8gdGhpcyBjbGFzcyBwcmVmaXhcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdNb3JlIHRoYW4gb25lIEQyTCBQREYgRmlsZXZpZXdlcnMgb24gdGhlIHBhZ2UnKTtcclxuXHRcdH1cclxuXHJcblx0XHQvLyByZXR1cm4gYSB0dXBsZSBvZiBbdXJsLCBpc05hdGl2ZVBERl1cclxuXHRcdGxldCBsb2NhdGlvbiA9IG5ld1VSTCh2aWV3ZXIuZ2V0QXR0cmlidXRlKCdkYXRhLWxvY2F0aW9uJykhKTtcclxuXHRcdHN3aXRjaCh0eXBlWzBdKSB7XHJcblx0XHRcdGNhc2UgXCJuYXRpdmVcIjogcmV0dXJuIFtsb2NhdGlvbiwgdHJ1ZV07XHJcblx0XHRcdGNhc2UgXCJwZGZqc1wiOiByZXR1cm4gW2xvY2F0aW9uLCBmYWxzZV07XHJcblx0XHRcdGRlZmF1bHQ6IHRocm93IG5ldyBFcnJvcignVW5rbm93biBQREYgdmlld2VyIHR5cGU6ICcgKyB0eXBlWzBdKTtcclxuXHRcdH1cclxuXHJcblx0fVxyXG5cclxuXHQvKiogT25seSBpbnRlcmFjdGl2ZSAqL1xyXG5cdHByaXZhdGUgZXh0UGFnZVVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdGxldCBpdGV4dCA9IHRoaXMuY29udGVudFZpZXcuaW5uZXJUZXh0O1xyXG5cdFx0aWYgKCFpdGV4dC5pbmNsdWRlcyhcIkV4dGVybmFsIFJlc291cmNlXCIpIHx8ICFpdGV4dC5pbmNsdWRlcyhcIk9wZW4gaW4gTmV3IFdpbmRvd1wiKSkge1xyXG5cdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdH1cclxuXHRcdC8vIHJlYXNvbmFibHkgc3VyZSB0aGlzIHBhZ2UncyBjb250ZW50IGlzIGFuIGV4dGVybmFsIHBhZ2UgLSB0aHJvdyBvbiBhbnkgZXJyb3JzXHJcblxyXG5cdFx0bGV0IHVybHMgPSBPYmplY3QudmFsdWVzKEQyTC5PUi5fX2cxKVxyXG5cdFx0XHQubWFwKHMgPT4geyB0cnkge1xyXG5cdFx0XHRcdHJldHVybiBKU09OLnBhcnNlKHMpIGFzIEQyTF9PUl9PYmplY3RcclxuXHRcdFx0fSBjYXRjaChlKSB7XHJcblx0XHRcdFx0dGhyb3cgbmV3IEVycm9yKFwiRDJMLk9SLl9fZzEgY29udGFpbnMgbWFsZm9ybWVkIEpTT04hXCIpXHJcblx0XHRcdH0gfSlcclxuXHRcdFx0LmZpbHRlcigobyk6IG8gaXMgT1JfT2JqZWN0cy5GdW5jID0+IHtcclxuXHRcdFx0XHRyZXR1cm4gby5fdHlwZSA9PSBcImZ1bmNcIiAmJiBvLk4gPT0gXCJEMkwuTEUuQ29udGVudC5EZXNrdG9wLlRvcGljLk9wZW5Jbk5ld1dpbmRvd1wiICYmIG8uUC5sZW5ndGggPT0gMVxyXG5cdFx0XHR9KVxyXG5cdFx0XHQubWFwKG8gPT4gby5QWzBdKTtcclxuXHRcdFxyXG5cdFx0aWYodXJscy5sZW5ndGggPT0gMCkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoYENvbnRlbnQgVmlldyBzaG93aW5nIFwiRXh0ZXJuYWwgUmVzb3VyY2VcIiB3aXRob3V0IGFueSBPcGVuSW5OZXdXaW5kb3cgZnVuY3Rpb25zIGluIEQyTC5PUi5fX2cxIWApO1xyXG5cdFx0fSBlbHNlIGlmKHVybHMubGVuZ3RoID4gMSkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoYE11bHRpcGxlIE9wZW5Jbk5ld1dpbmRvdyBjb21tYW5kcyBmb3IgXCJFeHRlcm5hbCBSZXNvdXJjZVwiIGNvbnRlbnQuIE5vdCByZXR1cm5pbmcgYW55LiAoJHtKU09OLnN0cmluZ2lmeSh1cmxzKX0pYCk7XHJcblx0XHR9IGVsc2UgaWYodHlwZW9mIHVybHNbMF0gIT0gJ3N0cmluZycpIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKGBTaW5nbGUgT3BlbkluTmV3V2luZG93IGNvbW1hbmQgcGFyYW1ldGVyIGlzbid0IGEgc3RyaW5nISAoJHtKU09OLnN0cmluZ2lmeSh1cmxzWzBdKX0pYClcclxuXHRcdH1cclxuXHJcblx0XHRsZXQgdXJsID0gbmV3VVJMKHVybHNbMF0pO1xyXG5cdFx0dXJsLnByb3RvY29sID0gXCJodHRwc1wiOyAvLyBvdGhlcndpc2UgaWZyYW1lIHdvbid0IGxvYWQgYi9jIEQyTCBpcyBIVFRQU1xyXG5cdFx0aWYodXJsLmhvc3QuaW5jbHVkZXMoXCJ5b3V0dWJlLmNvbVwiKSAmJiB1cmwucGF0aG5hbWUgPT0gXCIvd2F0Y2hcIikge1xyXG5cdFx0XHRsZXQgdmlkZW9faWQgPSB1cmwuc2VhcmNoUGFyYW1zLmdldChcInZcIik7XHJcblx0XHRcdGlmKHZpZGVvX2lkKSB7XHJcblx0XHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoXCJ2XCIpO1xyXG5cdFx0XHRcdHVybC5wYXRobmFtZSA9IFwiL2VtYmVkL1wiICsgdmlkZW9faWQ7XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHJcblx0XHRyZXR1cm4gdXJsO1xyXG5cdH1cclxuXHJcblx0LyoqIE9ubHkgZG93bmxvYWRhYmxlXHJcblx0ICogXHJcblx0ICogRG9lc24ndCBjaGVjayBmb3IgNDA0IC0gc2hvdWxkIGJlIHVzZWQgYWZ0ZXIgZGV0ZWN0aW9uIG9mIGV4dGVybmFsIHBhZ2VzL2V0Y1xyXG5cdCovXHJcblx0cHJpdmF0ZSBuYWl2ZUZpbGVVUkwoc3RyZWFtOiBib29sZWFuKTogVVJMIHtcclxuXHRcdGxldCB1cmwgPSBuZXdVUkwodGhpcy5uYWl2ZUFzc2V0VVJMLnRvU3RyaW5nKCkpIC8vIGNsb25lIFVSTFxyXG5cdFx0dXJsLnNlYXJjaFBhcmFtcy5zZXQoXCJzdHJlYW1cIiwgc3RyZWFtLnRvU3RyaW5nKCkpO1xyXG5cdFx0cmV0dXJuIHVybFxyXG5cdH1cclxuXHJcblx0LyoqIERPTSBtYW5pcHVsYXRpb24gaGVscGVycyAqL1xyXG5cdHByb3RlY3RlZCByZXBsYWNlQ29udGVudChpZnJhbWVfc3JjOiBVUkwpIHtcclxuXHRcdGxldCBuZXdDb250ZW50OiBIVE1MRWxlbWVudDtcclxuXHRcdGxldCBpZnJhbSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2lmcmFtZScpXHJcblx0XHRpZnJhbS5zcmMgPSBpZnJhbWVfc3JjLnRvU3RyaW5nKCk7XHJcblx0XHRpZnJhbS5zdHlsZS53aWR0aCA9ICcxMDAlJztcclxuXHRcdGlmcmFtLnN0eWxlLmhlaWdodCA9ICc5MHZoJztcclxuXHRcdGlmcmFtLnN0eWxlLnJlc2l6ZSA9ICdib3RoJztcclxuXHRcdGlmcmFtLnNldEF0dHJpYnV0ZSgncHJlbG9hZCcsICdhdXRvJyk7XHJcblx0XHRpZnJhbS5zZXRBdHRyaWJ1dGUoJ2FsbG93ZnVsbHNjcmVlbicsICd0cnVlJyk7IC8vIGZvciBtZWRpYSBwbGF5ZXJzIChlZzogZW1iZWRkZWQgeW91dHViZSlcclxuXHRcdG5ld0NvbnRlbnQgPSBpZnJhbTtcclxuXHJcblx0XHRsZXQgY3YgPSB0aGlzLmNvbnRlbnRWaWV3O1xyXG5cdFx0d2hpbGUgKGN2Lmxhc3RDaGlsZCkgeyBjdi5yZW1vdmVDaGlsZChjdi5sYXN0Q2hpbGQpIH0gLy8gcmVtb3ZlIGV4aXN0aW5nIGNvbnRlbnRcclxuXHRcdGN2LmFwcGVuZENoaWxkKG5ld0NvbnRlbnQpOyAvLyBhZGQgc2VsZiB0byBkb21cclxuXHJcblx0XHQvLyBBdHRlbXB0IHRvIHByZXNlcnZlIGFzcGVjdCByYXRpb1xyXG5cdFx0Ly8gTXVzdCBiZSBkb25lIGFmdGVyIGFwcGVuZGluZyB0byB0aGUgRE9NICguYXBwZW5kQ2hpbGQpIHNvIC5vZmZzZXRXaWR0aCB3b3Jrc1xyXG5cdFx0aWYoaWZyYW1lX3NyYy5wYXRobmFtZS5lbmRzV2l0aCgnLm1wNCcpKSB7XHJcblx0XHRcdGxldCB3aWR0aCA9IGlmcmFtLm9mZnNldFdpZHRoO1xyXG5cdFx0XHQvLyBhc3N1bWUgdmlkZW9zIGFyZSAxNjo5IGFzcGVjdCByYXRpb1xyXG5cdFx0XHRpZnJhbS5zdHlsZS5oZWlnaHQgPSAod2lkdGggKiAoOS8xNikpICsgJ3B4JztcclxuXHRcdH1cclxuXHJcblx0XHR0aGlzLnJlcGxhY2VkQ29udGVudCA9IHRydWU7XHJcblx0XHRyZXR1cm4gaWZyYW07XHJcblx0fVxyXG5cdHByb3RlY3RlZCB0aXRsZUJ0bih0ZXh0OiBzdHJpbmcsIG9uY2xpY2s6ICh0aGlzOiBIVE1MQnV0dG9uRWxlbWVudCwgZXY6IE1vdXNlRXZlbnQpID0+IGFueSk6IEhUTUxCdXR0b25FbGVtZW50IHtcclxuXHRcdGxldCBidG4gPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdidXR0b24nKTtcclxuXHRcdGJ0bi5pbm5lclRleHQgPSB0ZXh0O1xyXG5cdFx0YnRuLmNsYXNzTGlzdC5hZGQoJ2QybC1idXR0b24nKTtcclxuXHRcdGJ0bi5zdHlsZS5tYXJnaW5MZWZ0ID0gJzAuMjVyZW0nOyAvLyA1cHggbWF0Y2hlcyAuZDJsLWNvbnRleHRtZW51LXBoXHJcblx0XHRidG4uc3R5bGUubWFyZ2luUmlnaHQgPSAnMC4yNXJlbSc7XHJcblx0XHRidG4uc3R5bGUud2lkdGggPSAnYXV0byc7XHJcblx0XHRidG4uYWRkRXZlbnRMaXN0ZW5lcignY2xpY2snLCBvbmNsaWNrKTtcclxuXHRcdHJldHVybiBidG47XHJcblx0fVxyXG5cdHByb3RlY3RlZCB0aXRsZUxpbmsodGV4dDogc3RyaW5nLCBocmVmOiBVUkwsIHByZXBlbmROb2RlPzogSFRNTEVsZW1lbnQpOiBIVE1MQW5jaG9yRWxlbWVudCB7XHJcblx0XHRsZXQgbGluayA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2EnKTtcclxuXHRcdGxpbmsuaW5uZXJUZXh0ID0gdGV4dDtcclxuXHRcdGlmKHByZXBlbmROb2RlKSBsaW5rLnByZXBlbmQocHJlcGVuZE5vZGUpO1xyXG5cdFx0bGluay5ocmVmID0gaHJlZi50b1N0cmluZygpO1xyXG5cdFx0bGluay5jbGFzc0xpc3QuYWRkKCdkMmwtYnV0dG9uJyk7XHJcblx0XHRsaW5rLnN0eWxlLm1hcmdpbkxlZnQgPSAnMC4yNXJlbSc7XHJcblx0XHRsaW5rLnN0eWxlLm1hcmdpblJpZ2h0ID0gJzAuMjVyZW0nO1xyXG5cdFx0bGluay5zdHlsZS53aWR0aCA9ICdhdXRvJztcclxuXHRcdHJldHVybiBsaW5rO1xyXG5cdH1cclxuXHJcblx0LyoqIEFkZHMgYSBidXR0b24gdG8gdXNlIGEgbmF0aXZlIHZpZXdlciBmb3IgdGhlIHByb3ZpZGVkIFVSTC4gQXV0b21hdGljYWxseSByZW1vdmVzIGl0c2VsZiBmcm9tIHRoZSBET00gd2hlbiBjbGlja2VkLiAqL1xyXG5cdHByb3RlY3RlZCB0aXRsZUJ0bl91c2VOYXRpdmVGcmFtZShzcmM6IFVSTCk6IEhUTUxCdXR0b25FbGVtZW50IHtcclxuXHRcdGNvbnN0IHRoYXQgPSB0aGlzO1xyXG5cdFx0ZnVuY3Rpb24gYnRub25jbGljayh0aGlzOiBIVE1MQnV0dG9uRWxlbWVudCkge1xyXG5cdFx0XHQvLyBpbnN0YWxsZWQgYXMgb25jbGljayBoYW5kbGVyID0+IHRoaXMgPSBgPGJ1dHRvbj4uLi48L2J1dHRvbj5gXHJcblx0XHRcdC8vIHJlbW92ZSBvdXJzZWx2ZXMgKGJ1dHRvbikgc2luY2Ugd2UgaGF2ZSBzZXJ2ZWQgb3VyIHB1cnBvc2VcclxuXHRcdFx0dGhpcy5yZW1vdmUoKTtcclxuXHRcdFx0dGhhdC5yZXBsYWNlQ29udGVudChzcmMpO1xyXG5cdFx0fTtcclxuXHJcblx0XHQvLyBOb3RlOiBwdXQgYnV0dG9uIGluIGltbWVkaWF0bHksIHByb3ZpZGUgbGluayBhZnRlciBQREYgd2FzIGRvd25sb2FkZWRcclxuXHRcdHJldHVybiB0aGlzLnRpdGxlQnRuKFwiVXNlIE5hdGl2ZSBWaWV3ZXJcIiwgYnRub25jbGljayk7XHJcblx0fVxyXG59XHJcbiIsIi8vID09VXNlclNjcmlwdD09XG4vLyBAbmFtZSAgICAgICAgIEQyTCBUd2Vha3Ncbi8vIEBuYW1lc3BhY2UgICAgaHR0cHM6Ly9naXRodWIuY29tL2NzbTEyMzE5OS9kMmwtdHdlYWtzXG4vLyBAdmVyc2lvbiAgICAgIDAuOFxuLy8gQGRlc2NyaXB0aW9uICBBZGQgUW9MIGNoYW5nZXMgdG8gRDJMJ3MgdXNlciBpbnRlcmZhY2Vcbi8vIEBhdXRob3IgICAgICAgQ2hyaXMgTW9vcmVcbi8vIEBpbmNsdWRlICAgICAgaHR0cHM6Ly9kMmwuKi5lZHUvZDJsL2xlL2NvbnRlbnQvKi92aWV3Q29udGVudC8qL1ZpZXdcbi8vIEBncmFudCAgICAgICAgbm9uZVxuLy8gQHVwZGF0ZVVybCAgICBodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vY3NtMTIzMTk5L2QybC10d2Vha3MvbWFzdGVyL2QybC10d2Vha3MudXNlci5qc1xuLy8gPT0vVXNlclNjcmlwdD09XG5cbi8vIEQyTCByZXN0IGFwaSBkb2NzXG4vLyBodHRwczovL2RvY3MudmFsZW5jZS5kZXNpcmUybGVhcm4uY29tL3Jlcy9jb250ZW50Lmh0bWxcblxuLy8vIDxyZWZlcmVuY2UgcGF0aD1cIi4vZDJsLWdsb2JhbHMuZC50c1wiIC8+XG5cbi8qIENvbmZpZyAqL1xuY29uc3QgTUFLRV9OQVRJVkVfT05fTE9BRCA9IHRydWU7XG5jb25zdCBPRkZJQ0VfRE9DVU1FTlRTX0RJUkVDVF9WSUVXX1VTRVNfRVhURU5TSU9OID0gdHJ1ZTtcblxuLyogQ29kZSAqL1xuLy8gRW5zdXJlIHRoZSBwYWdlIHdlJ3JlIG9uIGlzIGEgdmFsaWQgRDJMIHBhZ2UuXG4vLyBJZiB5b3VyIGluc3RpdHV0aW9uIGRvZXNuJ3QgbWF0Y2gsIHBsZWFzZSBzdWJtaXQgYSBQUiBhZGRpbmcgYSBnZW5lcmljIGNoZWNrIGZvciB5b3Vycy5cbmxldCB1cmwgPSBuZXdVUkwoZG9jdW1lbnQubG9jYXRpb24pO1xuXG4vL1RPRE86IEp1c3QgY2hlY2sgZm9yIC9kMmwvKiBwYXRoP1xuaWYgKHVybC5wcm90b2NvbCAhPSAnaHR0cHM6JyB8fCAhdXJsLmhvc3QubWF0Y2goL15kMmwuW2EtekEtWjAtOV8tXSsuZWR1LykpIHtcblx0dGhyb3cgbmV3IEVycm9yKGBCYWQgaG9zdCBmb3IgRDJMIFNjcmlwdCAoZXhpdGluZyB2aWEgZXhjZXB0aW9uKTogJyR7dXJsLmhvc3R9J2ApO1xufVxuXG5jb25zdCBDQVBBQklMSVRJRVM6IENhcGFiaWxpdGllcyA9IGluaXRDYXBhYmlsaXRpZXMoKTtcblxuKGFzeW5jIGZ1bmN0aW9uICgpIHtcblx0J3VzZSBzdHJpY3QnO1xuXG5cdGlmICghd2l0aGluSWZyYW1lKCkpIHtcblx0XHRzdWdnZXN0SFRNTDVWaWRlb0tleWJvYXJkU2hvcnRjdXRzKCk7XG5cdFx0c3VnZ2VzdE9mZmljZUVkaXRpbmcoKTtcblxuXHRcdHRyeSB7XG5cdFx0XHRsZXQgcHR5cGUgPSBnZXRQYWdlVHlwZSgpO1xuXHRcdFx0aWYocHR5cGUudHlwZSA9PSBcImNvbnRlbnRcIikge1xuXHRcdFx0XHRsZXQgY29udGVudFBhZ2UgPSBuZXcgQ29udGVudFBhZ2UocHR5cGUuY2xhc3MsIHB0eXBlLmFzc2V0KTtcblx0XHRcdFx0Y29uc29sZS5sb2coXCJDb250ZW50IHBhZ2U6IFwiLCBjb250ZW50UGFnZSk7XG5cdFx0XHRcdGlmKE1BS0VfTkFUSVZFX09OX0xPQUQpXG5cdFx0XHRcdFx0Y29udGVudFBhZ2Uubm9ybWFsaXplQ29udGVudCgpO1xuXHRcdFx0XHRjb250ZW50UGFnZS5hZGRIZWFkZXJCdXR0b25zKCk7XG5cdFx0XHR9XG5cdFx0fSBjYXRjaCAoZSkge1xuXHRcdFx0Y29uc29sZS5lcnJvcihcIkVycm9yIG9jY3VyZWQgaW4gdXNlcnNjcmlwdFwiLCBlKTtcblx0XHRcdC8vYWxlcnQoXCJFcnJvciBvY2N1cmVkIGluIEQyTCBiZXR0ZXJpbmcgdXNlcnNjcmlwdCwgZXJyb3IgaW4gY29uc29sZS5cIik7XG5cdFx0fVxuXG5cdH1cbn0pKCk7XG4iLCJcclxuZnVuY3Rpb24gaW5pdENhcGFiaWxpdGllcygpOiBDYXBhYmlsaXRpZXMge1xyXG5cdC8vIEkgcmVhbGl6ZSBJIHNob3VsZG4ndCBiZSB1c2luZyB0aGUgdXNlckFnZW50IHRvIGRldGVjdCBicm93c2VyIGZsYXZvcnMuXHJcblx0Ly8gSXQnbGwgYmUgZ29vZCBlbm91Z2ggZm9yIHRoaXMuXHJcblx0bGV0IHVzZXJhZ2VudCA9IG5hdmlnYXRvci51c2VyQWdlbnQudG9Mb3dlckNhc2UoKTtcclxuXHJcblx0Ly8gQHRzLWlnbm9yZVxyXG5cdGlmKHVzZXJhZ2VudC5pbmNsdWRlcygnY2hyb21lJykpIHtcclxuXHRcdGNvbnN0IG10ID0gKGFwcDogc3RyaW5nKTogYm9vbGVhbiA9PiBuYXZpZ2F0b3IubWltZVR5cGVzLm5hbWVkSXRlbSgnYXBwbGljYXRpb24vJyArIGFwcCkgIT09IG51bGw7XHJcblx0XHRyZXR1cm4ge1xyXG5cdFx0XHRleHRlbnNpb25zOiB7XHJcblx0XHRcdFx0Ly8gZGV0ZWN0IGlmIGNocm9tZSBzdXBwb3J0cyB2aWV3aW5nIE1TIG9mZmljZSBkb2N1bWVudHMgKHZpYSBleHRlbnNpb24vZXRjKVxyXG5cdFx0XHRcdG9mZmljZV92aWV3ZXI6IG10KCdtc3dvcmQnKSAmJiBtdCgnbXNleGNlbCcpICYmIG10KCdtc3Bvd2VycG9pbnQnKSAmJiBtdCgnbXN3b3JkLXRlbXBsYXRlJyksXHJcblx0XHRcdFx0Ly9vZmZpY2Vfdmlld2VyOiBjaHJvbWVFeHRlbnNpb25JbnN0YWxsZWQoJ2dtcGxqZGxnY2RrbGpscHBhZWtjaWFjZG1kbGhmZW9uJyksXHJcblx0XHRcdFx0dmlkZW9fc2hvcnRjdXRzOiBjaHJvbWVFeHRlbnNpb25JbnN0YWxsZWQoJ2xsaG1hY2lnZ25pYm5iZG9raWRtYmlsa2xjZWFvYmFlJyksXHJcblx0XHRcdH0sXHJcblx0XHRcdGJyb3dzZXI6IFwiY2hyb21lXCIsXHJcblx0XHR9XHJcblx0fSBlbHNlIGlmKHVzZXJhZ2VudC5pbmNsdWRlcygnZmlyZWZveCcpKSB7XHJcblx0XHRyZXR1cm4ge1xyXG5cdFx0XHQvLyBubyB3YXkgdG8ga25vd1xyXG5cdFx0XHRleHRlbnNpb25zOiB7XHJcblx0XHRcdFx0b2ZmaWNlX3ZpZXdlcjogZmFsc2UsXHJcblx0XHRcdFx0dmlkZW9fc2hvcnRjdXRzOiB0cnVlLCAvLyBGRiBoYXMgYnVpbHQgaW4gc3BlZWQgY29udHJvbHMgLSB3aGljaCBJIGNhcmUgYWJvdXRcclxuXHRcdFx0fSxcclxuXHRcdFx0YnJvd3NlcjogXCJmaXJlZm94XCIsXHJcblx0XHR9XHJcblx0fVxyXG5cdHJldHVybiB7XHJcblx0XHRleHRlbnNpb25zOiB7IG9mZmljZV92aWV3ZXI6IGZhbHNlLCB2aWRlb19zaG9ydGN1dHM6IGZhbHNlLCB9LFxyXG5cdFx0YnJvd3NlcjogXCJ1bmtub3duXCIsXHJcblx0fVxyXG59XHJcblxyXG5mdW5jdGlvbiBzdWdnZXN0SFRNTDVWaWRlb0tleWJvYXJkU2hvcnRjdXRzKCkge1xyXG5cdC8vIE9ubHkgc3VnZ2VzdCBvbiBjaHJvbWUsIGIvYyBmaXJlZm94IHVzZXJzIGV4aXN0XHJcblx0Ly8gKGFuZCBmaXJlZm94IGFscmVhZHkgaGFzIHJpZ2h0LWNsaWNrLCBjaGFuZ2Ugc3BlZWQgY29udHJvbHMpXHJcblx0aWYoIWNocm9tZUV4dGVuc2lvbkluc3RhbGxlZCgnbGxobWFjaWdnbmlibmJkb2tpZG1iaWxrbGNlYW9iYWUnKSkge1xyXG5cdFx0Y29uc29sZS53YXJuKGBEMkwgdXNlcnNjcmlwdCByZWNvbW1lbmRzIGluc3RhbGxpbmcgdGhlIFwiSFRNTDUgVmlkZW8gS2V5Ym9hcmQgU2hvcnRjdXRzXCIgZXh0ZW5zaW9uIGZvciBzcGVlZGluZyB1cCB2aWRlb3NgLCBgaHR0cHM6Ly9jaHJvbWUuZ29vZ2xlLmNvbS93ZWJzdG9yZS9kZXRhaWwvbGxobWFjaWdnbmlibmJkb2tpZG1iaWxrbGNlYW9iYWVgKTtcclxuXHR9XHJcbn1cclxuZnVuY3Rpb24gc3VnZ2VzdE9mZmljZUVkaXRpbmcoKSB7XHJcblx0Ly8gT25seSBzdWdnZXN0IG9uIGNocm9tZSwgYi9jIGZpcmVmb3ggdXNlcnMgZXhpc3RcclxuXHRpZighY2hyb21lRXh0ZW5zaW9uSW5zdGFsbGVkKCdnYmtlZWdiYWlpZ21lbmZtamZjbGNkZ2RwaW1hbWdraicpKSB7XHJcblx0XHRjb25zb2xlLndhcm4oYEQyTCB1c2Vyc2NyaXB0IHJlY29tbWVuZHMgaW5zdGFsbGluZyB0aGUgXCJPZmZpY2UgRWRpdGluZyBmb3IgRG9jcywgU2hlZXRzICYgU2xpZGVzXCIgZXh0ZW5zaW9uIGZvciBpbnRlcmFjdGl2ZWx5IHZpZXdpbmcgb2ZmaWNlIGRvY3VtZW50c2AsIGBodHRwczovL2Nocm9tZS5nb29nbGUuY29tL3dlYnN0b3JlL2RldGFpbC9nYmtlZWdiYWlpZ21lbmZtamZjbGNkZ2RwaW1hbWdramApO1xyXG5cdH1cclxufVxyXG5cclxuZnVuY3Rpb24gY2hyb21lRXh0ZW5zaW9uSW5zdGFsbGVkKGlkOiBzdHJpbmcpIHtcclxuXHR0cnkge1xyXG5cdFx0Ly8gQHRzLWlnbm9yZVxyXG5cdFx0aWYoY2hyb21lKSB7IGNocm9tZS5ydW50aW1lLnNlbmRNZXNzYWdlKGlkLCBudWxsKTsgcmV0dXJuIHRydWU7IH1cclxuXHRcdFxyXG5cdH0gY2F0Y2ggKGUpIHtcclxuXHRcdGlmICghZS5tZXNzYWdlLmluY2x1ZGVzKFwiSW52YWxpZCBleHRlbnNpb24gaWRcIikpIHsgdGhyb3cgZTsgfVxyXG5cdH1cclxuXHRyZXR1cm4gZmFsc2U7XHJcbn1cclxuXHJcbi8qKiBVc2VmdWwgZm9yIGVhcmx5IGV4aXQgaWYgd2UgYXJlbid0IHRoZSB0b3AgZnJhbWUgKi9cclxuZnVuY3Rpb24gd2l0aGluSWZyYW1lKCkge1xyXG5cdC8vIGh0dHBzOi8vc3RhY2tvdmVyZmxvdy5jb20vYS8zMjYwNzYvMTE1MzY2MTRcclxuXHR0cnkge1xyXG5cdFx0cmV0dXJuIHdpbmRvdy5zZWxmICE9PSB3aW5kb3cudG9wO1xyXG5cdH0gY2F0Y2ggKGUpIHtcclxuXHRcdHJldHVybiB0cnVlOyAvLyBhY2Nlc3MgZGVuaWVkIGVycm9yXHJcblx0fVxyXG59XHJcblxyXG5mdW5jdGlvbiBkMmxfaWNvbihiYWNrZ3JvdW5kSW1hZ2U6IHN0cmluZyk6IEhUTUxTcGFuRWxlbWVudCB7XHJcblx0bGV0IGkgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdzcGFuJyk7XHJcblx0aS5jbGFzc0xpc3QuYWRkKCdkMmwtaWNvbi1jdXN0b20nKTtcclxuXHRpLnN0eWxlLmJhY2tncm91bmRJbWFnZSA9IGJhY2tncm91bmRJbWFnZTtcclxuXHRpLnN0eWxlLmJhY2tncm91bmRQb3NpdGlvbiA9IFwiMCAwXCI7XHJcblx0aS5zdHlsZS5iYWNrZ3JvdW5kUmVwZWF0ID0gXCJuby1yZXBlYXRcIjtcclxuXHRpLnN0eWxlLndpZHRoID0gXCIxOHB4XCI7XHJcblx0aS5zdHlsZS5oZWlnaHQgPSBcIjE4cHhcIjtcclxuXHRpLnN0eWxlLmJhY2tncm91bmRTaXplID0gXCIxOHB4IDE4cHhcIjtcclxuXHRyZXR1cm4gaTtcclxufVxyXG5cclxuZnVuY3Rpb24gZDJsX2ljb25fZG93bmxvYWQoKTogSFRNTEVsZW1lbnQge1xyXG5cdHJldHVybiBkMmxfaWNvbihcInVybCgnaHR0cHM6Ly9zLmJyaWdodHNwYWNlLmNvbS9saWIvYnNpLzIwLjIwLjgtODUvaW1hZ2VzL3RpZXIxL2Rvd25sb2FkLnN2ZycpXCIpO1xyXG59XHJcblxyXG4vKiogVGhlIFVSTCBpbnRlcmZhY2UgcmVwcmVzZW50cyBhbiBvYmplY3QgcHJvdmlkaW5nIHN0YXRpYyBtZXRob2RzIHVzZWQgZm9yIGNyZWF0aW5nIG9iamVjdCBVUkxzLiAqL1xyXG5mdW5jdGlvbiBuZXdVUkwodXJsOiBzdHJpbmcgfCBVUkwgfCBMb2NhdGlvbik6IFVSTCB7XHJcblx0aWYodHlwZW9mIHVybCA9PSAnc3RyaW5nJylcclxuXHRcdHJldHVybiBuZXcgVVJMKHVybCwgZG9jdW1lbnQubG9jYXRpb24uaHJlZik7XHJcblx0ZWxzZSBpZih1cmwgaW5zdGFuY2VvZiBVUkwpIHtcclxuXHRcdHJldHVybiBuZXcgVVJMKHVybC5ocmVmKTtcclxuXHR9IGVsc2UgaWYodXJsIGluc3RhbmNlb2YgTG9jYXRpb24pIHtcclxuXHRcdHJldHVybiBuZXcgVVJMKHVybC5ocmVmKTtcclxuXHR9IGVsc2Uge1xyXG5cdFx0dGhyb3cgbmV3IEVycm9yKCd1bmtub3duIHBhcmFtZXRlciB0eXBlJyk7XHJcblx0fVxyXG59XHJcblxyXG4vKiogUmV0dXJuIHRoZSB0eXBlIG9mIEQyTCBwYWdlIHdlIGFyZSBvbiwgd2l0aCBzb21lIGFwcHJvcHJpYXRlIG1ldGFkYXRhIGFib3V0IGl0ICovXHJcbmZ1bmN0aW9uIGdldFBhZ2VUeXBlKCk6IFBhZ2VUeXBlIHtcclxuXHRsZXQgaHJlZiA9IG5ld1VSTChkb2N1bWVudC5sb2NhdGlvbik7XHJcblx0Ly8gc2xpY2UgdG8gcmVtb3ZlIHplcm8tbGVuZ3RoIGNvbXBvbmVudCBhdCBiZWdpbm5pbmcgKHNpbmNlIHBhdGhuYW1lcyBzdGFydCB3aXRoICcvJylcclxuXHRsZXQgY29tcG9uZW50cyA9IGhyZWYucGF0aG5hbWUuc3BsaXQoJy8nKS5zbGljZSgxKTtcclxuXHRpZihcclxuXHRcdGNvbXBvbmVudHMubGVuZ3RoID09IDdcclxuXHRcdCYmIGNvbXBvbmVudHNbMF0gPT0gXCJkMmxcIlxyXG5cdFx0JiYgY29tcG9uZW50c1sxXSA9PSBcImxlXCJcclxuXHRcdCYmIGNvbXBvbmVudHNbMl0gPT0gXCJjb250ZW50XCJcclxuXHRcdC8vIGNsYXNzIElEXHJcblx0XHQmJiBjb21wb25lbnRzWzRdID09IFwidmlld0NvbnRlbnRcIlxyXG5cdFx0Ly8gYXNzZXQgSURcclxuXHRcdCYmIGNvbXBvbmVudHNbNl0gPT0gXCJWaWV3XCJcclxuXHQpIHtcclxuXHRcdHJldHVybiB7IHR5cGU6IFwiY29udGVudFwiLCBjbGFzczogY29tcG9uZW50c1szXSwgYXNzZXQ6IGNvbXBvbmVudHNbNV0sIH07XHJcblx0fVxyXG5cclxuXHRyZXR1cm4geyB0eXBlOiBcInVua25vd25cIiB9O1xyXG59XHJcbiIsIlxyXG5pbnRlcmZhY2UgQ2FwYWJpbGl0aWVzIHtcclxuXHRleHRlbnNpb25zOiB7XHJcblx0XHRvZmZpY2Vfdmlld2VyOiBib29sZWFuLFxyXG5cdFx0dmlkZW9fc2hvcnRjdXRzOiBib29sZWFuLFxyXG5cdH0sXHJcblx0YnJvd3NlcjogXCJjaHJvbWVcIiB8IFwiZmlyZWZveFwiIHwgXCJ1bmtub3duXCIsXHJcbn07XHJcblxyXG5pbnRlcmZhY2UgRDJMQXNzZXRNZXRhZGF0YSB7XHJcblx0dHlwZTogc3RyaW5nLFxyXG5cdGZpbGVuYW1lOiBzdHJpbmd8bnVsbCxcclxuXHRzaXplOiBudW1iZXIsXHJcbn1cclxuXHJcbnR5cGUgUGFnZVR5cGUgPSBQYWdlLlVua25vd24gfCBQYWdlLkNvbnRlbnQ7XHJcbm5hbWVzcGFjZSBQYWdlIHtcclxuXHRleHBvcnQgaW50ZXJmYWNlIFVua25vd24ge1xyXG5cdFx0dHlwZTogXCJ1bmtub3duXCI7XHJcblx0fVxyXG5cdGV4cG9ydCBpbnRlcmZhY2UgQ29udGVudCB7XHJcblx0XHR0eXBlOiBcImNvbnRlbnRcIjtcclxuXHRcdGNsYXNzOiBzdHJpbmc7XHJcblx0XHRhc3NldDogc3RyaW5nO1xyXG5cdH1cclxufVxyXG5cclxuIl19