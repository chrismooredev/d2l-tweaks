"use strict";
// ==UserScript==
// @name         D2L Tweaks
// @namespace    https://github.com/chrismooredev/d2l-tweaks
// @version      0.11
// @description  Add QoL changes to D2L's user interface
// @author       Chris Moore
// @include      https://*.edu/d2l/*
// @grant        none
// @updateUrl    https://raw.githubusercontent.com/chrismooredev/d2l-tweaks/master/d2l-tweaks.user.js
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
        let csstweaks = getCsstweaks();
        try {
            let ptype = getPageType();
            console.log("D2L Tweaks: found page of type ", ptype.type);
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
                // The TOC swaps chapters without navigation, so watch for new topic links.
                const TOPIC_LINK_SEL = '.d2l-page-main a.d2l-link:not(.d2l-hidden, [role])[title$=" - Page"]';
                const handled = new WeakSet();
                const scan = () => {
                    for (const a of Array.from(document.querySelectorAll(TOPIC_LINK_SEL))) {
                        if (handled.has(a))
                            continue;
                        handled.add(a);
                        injectVideoIntoTopicHeading(a);
                    }
                };
                scan();
                new MutationObserver(scan).observe(document.body, { childList: true, subtree: true });
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
async function injectVideoIntoTopicHeading(link) {
    try {
        const domp = new DOMParser();
        const viewerPage = await fetch(link.href).then(r => r.text());
        const viewerDoc = domp.parseFromString(viewerPage, 'text/html');
        const viewerFrame = viewerDoc.querySelector('.d2l-fileviewer iframe');
        if (!viewerFrame) {
            console.error('no fileviewer frame for', link.innerText, viewerDoc);
            return;
        }
        ;
        // fetch /content/enforced/$courseId-$courseName/$chapterName/$topicName.html?ou=$courseId&d2l_body_type=3
        const topicPage = await fetch(viewerFrame.src).then(r => r.text());
        const topicDoc = domp.parseFromString(topicPage, 'text/html');
        const topicFrame = topicDoc.querySelector('#container iframe');
        if (!topicFrame) {
            console.error('no container frame for', link.innerText, topicDoc);
            return;
        }
        ;
        // fetch /d2l/common/dialogs/quickLink/quickLink.d2l?ou=$courseId&type=lti&rcode=$resourceCode&srcou=1
        const embedPage = await fetch(topicFrame.src).then(r => r.text());
        const embedDoc = domp.parseFromString(embedPage, 'text/html');
        // console.log(embedDoc);
        const embedFrame = embedDoc.querySelector('d2l-lti-launch'); // iframe custom element
        if (!embedFrame) {
            console.error('no lti-launch frame for', link.innerText, embedDoc);
            return;
        }
        ;
        // the main content page doesn't seem to have the code for the d2l-lti-launch element
        // so lets just map the url into an iframe and see how it does
        let embedLink = embedFrame.attributes.getNamedItem('lti-launch-url')?.value;
        if (!embedLink) {
            console.error('no lti-launch url for', link.innerText, embedDoc);
            return;
        }
        ;
        const videoFrame = document.createElement('iframe');
        videoFrame.src = embedLink;
        videoFrame.setAttribute('allowfullscreen', 'true');
        videoFrame.style.setProperty('width', '100%');
        videoFrame.style.setProperty('height', 'auto');
        videoFrame.style.setProperty('aspect-ratio', '16 / 9');
        const itemContent = link.closest('.d2l-datalist-item-content');
        if (!itemContent) {
            console.error('no itemcontent element for', link.innerText);
            return;
        }
        ;
        itemContent.after(videoFrame);
    }
    catch (e) {
        console.error('error for ', link.innerText, e);
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
// this is just a function so it's declared before the main code is ran
// otherwise var/let/const would error on declaration order
function getCsstweaks() {
    return {
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
}
;
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
        // fails trivially on other browsers.
        // just return false on error: fail-safe
        // if (!e.message.includes("Invalid extension id")) { throw e; }
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
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZDJsLXR3ZWFrcy51c2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic3JjL2QybC10d2Vha3MudXNlci50cyIsInNyYy9jb250ZW50LXBhZ2UudHMiLCJzcmMvY29udGVudC10b2MudHMiLCJzcmMvY3NzLXR3ZWFrcy50cyIsInNyYy9oZWxwZXJzLnRzIiwic3JjL3R5cGVzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFBQSxpQkFBaUI7QUFDakIsMkJBQTJCO0FBQzNCLDREQUE0RDtBQUM1RCxxQkFBcUI7QUFDckIsd0RBQXdEO0FBQ3hELDRCQUE0QjtBQUM1QixvQ0FBb0M7QUFDcEMscUJBQXFCO0FBQ3JCLHFHQUFxRztBQUNyRyxrQkFBa0I7QUFFbEIsb0JBQW9CO0FBQ3BCLHlEQUF5RDtBQUV6RCwyQ0FBMkM7QUFFM0MsWUFBWTtBQUNaLE1BQU0sbUJBQW1CLEdBQUcsSUFBSSxDQUFDO0FBQ2pDLE1BQU0sMkNBQTJDLEdBQUcsS0FBSyxDQUFDO0FBRTFELFVBQVU7QUFDVixnREFBZ0Q7QUFDaEQsMEZBQTBGO0FBQzFGLElBQUksR0FBRyxHQUFHLE1BQU0sQ0FBQyxRQUFRLENBQUMsUUFBUSxDQUFDLENBQUM7QUFFcEMsSUFBSSxHQUFHLENBQUMsUUFBUSxJQUFJLFFBQVEsSUFBSSxDQUFDLEdBQUcsQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLFFBQVEsQ0FBQyxVQUFVLENBQUMsTUFBTSxDQUFDLEVBQUU7SUFDL0YsTUFBTSxJQUFJLEtBQUssQ0FBQyxxREFBcUQsR0FBRyxDQUFDLElBQUksR0FBRyxDQUFDLENBQUM7Q0FDbEY7QUFFRCxNQUFNLFlBQVksR0FBaUIsZ0JBQWdCLEVBQUUsQ0FBQztBQUV0RCxDQUFDLEtBQUs7SUFDTCxZQUFZLENBQUM7SUFFYixJQUFJLENBQUMsWUFBWSxFQUFFLEVBQUU7UUFDcEIsa0NBQWtDLEVBQUUsQ0FBQztRQUNyQyxvQkFBb0IsRUFBRSxD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QVUsQ0FBQyxNQUFNLElBQUksQ0FBQztXQUNuQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksS0FBSztXQUN0QixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksSUFBSTtXQUNyQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksU0FBUztRQUM3QixXQUFXO1dBQ1IsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLGFBQWE7UUFDakMsV0FBVztXQUNSLFVBQVUsQ0FBQyxDQUFDLENBQUMsSUFBSSxNQUFNLEVBQ3pCO1FBQ0QsT0FBTyxFQUFFLElBQUksRUFBRSxTQUFTLEVBQUUsS0FBSyxFQUFFLFVBQVUsQ0FBQyxDQUFDLENBQUMsRUFBRSxLQUFLLEVBQUUsVUFBVSxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUM7S0FDeEU7U0FBTSxJQUNOLFVBQVUsQ0FBQyxNQUFNLElBQUksQ0FBQztXQUNuQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksS0FBSztXQUN0QixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksSUFBSTtXQUNyQixVQUFVLENBQUMsQ0FBQyxDQUFDLElBQUksU0FBUztRQUM3QixXQUFXO1dBQ1IsVUFBVSxDQUFDLENBQUMsQ0FBQyxJQUFJLE1BQU0sRUFDekI7UUFDRCxPQUFPLEVBQUUsSUFBSSxFQUFFLGFBQWEsRUFBRSxLQUFLLEVBQUUsVUFBVSxDQUFDLENBQUMsQ0FBQyxFQUFFLENBQUM7S0FDckQ7SUFFRCxPQUFPLEVBQUUsSUFBSSxFQUFFLFNBQVMsRUFBRSxDQUFDO0FBQzVCLENBQUM7QUN6SEEsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8vID09VXNlclNjcmlwdD09XHJcbi8vIEBuYW1lICAgICAgICAgRDJMIFR3ZWFrc1xyXG4vLyBAbmFtZXNwYWNlICAgIGh0dHBzOi8vZ2l0aHViLmNvbS9jaHJpc21vb3JlZGV2L2QybC10d2Vha3NcclxuLy8gQHZlcnNpb24gICAgICAwLjExXHJcbi8vIEBkZXNjcmlwdGlvbiAgQWRkIFFvTCBjaGFuZ2VzIHRvIEQyTCdzIHVzZXIgaW50ZXJmYWNlXHJcbi8vIEBhdXRob3IgICAgICAgQ2hyaXMgTW9vcmVcclxuLy8gQGluY2x1ZGUgICAgICBodHRwczovLyouZWR1L2QybC8qXHJcbi8vIEBncmFudCAgICAgICAgbm9uZVxyXG4vLyBAdXBkYXRlVXJsICAgIGh0dHBzOi8vcmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbS9jaHJpc21vb3JlZGV2L2QybC10d2Vha3MvbWFzdGVyL2QybC10d2Vha3MudXNlci5qc1xyXG4vLyA9PS9Vc2VyU2NyaXB0PT1cclxuXHJcbi8vIEQyTCByZXN0IGFwaSBkb2NzXHJcbi8vIGh0dHBzOi8vZG9jcy52YWxlbmNlLmRlc2lyZTJsZWFybi5jb20vcmVzL2NvbnRlbnQuaHRtbFxyXG5cclxuLy8vIDxyZWZlcmVuY2UgcGF0aD1cIi4vZDJsLWdsb2JhbHMuZC50c1wiIC8+XHJcblxyXG4vKiBDb25maWcgKi9cclxuY29uc3QgTUFLRV9OQVRJVkVfT05fTE9BRCA9IHRydWU7XHJcbmNvbnN0IE9GRklDRV9ET0NVTUVOVFNfRElSRUNUX1ZJRVdfVVNFU19FWFRFTlNJT04gPSBmYWxzZTtcclxuXHJcbi8qIENvZGUgKi9cclxuLy8gRW5zdXJlIHRoZSBwYWdlIHdlJ3JlIG9uIGlzIGEgdmFsaWQgRDJMIHBhZ2UuXHJcbi8vIElmIHlvdXIgaW5zdGl0dXRpb24gZG9lc24ndCBtYXRjaCwgcGxlYXNlIHN1Ym1pdCBhIFBSIGFkZGluZyBhIGdlbmVyaWMgY2hlY2sgZm9yIHlvdXJzLlxyXG5sZXQgdXJsID0gbmV3VVJMKGRvY3VtZW50LmxvY2F0aW9uKTtcclxuXHJcbmlmICh1cmwucHJvdG9jb2wgIT0gJ2h0dHBzOicgfHwgIXVybC5ob3N0LmVuZHNXaXRoKCcuZWR1JykgfHwgIXVybC5wYXRobmFtZS5zdGFydHNXaXRoKCcvZDJsJykpIHtcclxuXHR0aHJvdyBuZXcgRXJyb3IoYEJhZCBob3N0IGZvciBEMkwgU2NyaXB0IChleGl0aW5nIHZpYSBleGNlcHRpb24pOiAnJHt1cmwuaG9zdH0nYCk7XHJcbn1cclxuXHJcbmNvbnN0IENBUEFCSUxJVElFUzogQ2FwYWJpbGl0aWVzID0gaW5pdENhcGFiaWxpdGllcygpO1xyXG5cclxuKGFzeW5jIGZ1bmN0aW9uICgpIHtcclxuXHQndXNlIHN0cmljdCc7XHJcblxyXG5cdGlmICghd2l0aGluSWZyYW1lKCkpIHtcclxuXHRcdHN1Z2dlc3RIVE1MNVZpZGVvS2V5Ym9hcmRTaG9ydGN1dHMoKTtcclxuXHRcdHN1Z2dlc3RPZmZpY2VFZGl0aW5nKCk7XHJcblx0XHRsZXQgY3NzdHdlYWtzID0gZ2V0Q3NzdHdlYWtzKCk7XHJcblxyXG5cdFx0dHJ5IHtcclxuXHRcdFx0bGV0IHB0eXBlID0gZ2V0UGFnZVR5cGUoKTtcclxuXHRcdFx0Y29uc29sZS5sb2coXCJEMkwgVHdlYWtzOiBmb3VuZCBwYWdlIG9mIHR5cGUgXCIsIHB0eXBlLnR5cGUpO1xyXG5cdFx0XHRpZihwdHlwZS50eXBlID09IFwiY29udGVudFwiKSB7XHJcblx0XHRcdFx0Y3NzdHdlYWtzLmNvbnRlbnQoKTtcclxuXHJcblx0XHRcdFx0bGV0IGNvbnRlbnRQYWdlID0gbmV3IENvbnRlbnRQYWdlKHB0eXBlLmNsYXNzLCBwdHlwZS5hc3NldCk7XHJcblx0XHRcdFx0Y29uc29sZS5sb2coXCJDb250ZW50IHBhZ2U6IFwiLCBjb250ZW50UGFnZSk7XHJcblx0XHRcdFx0aWYoTUFLRV9OQVRJVkVfT05fTE9BRClcclxuXHRcdFx0XHRcdGNvbnRlbnRQYWdlLm5vcm1hbGl6ZUNvbnRlbnQoKTtcclxuXHRcdFx0XHRjb250ZW50UGFnZS5hZGRIZWFkZXJCdXR0b25zKCk7XHJcblxyXG5cdFx0XHR9IGVsc2UgaWYocHR5cGUudHlwZSA9PSBcImNvbnRlbnRfdG9jXCIpIHtcclxuXHRcdFx0XHRjc3N0d2Vha3MuY29udGVudF90b2MoKTtcclxuXHJcblx0XHRcdFx0Ly8gVGhlIFRPQyBzd2FwcyBjaGFwdGVycyB3aXRob3V0IG5hdmlnYXRpb24sIHNvIHdhdGNoIGZvciBuZXcgdG9waWMgbGlua3MuXHJcblx0XHRcdFx0Y29uc3QgVE9QSUNfTElOS19TRUwgPSAnLmQybC1wYWdlLW1haW4gYS5kMmwtbGluazpub3QoLmQybC1oaWRkZW4sIFtyb2xlXSlbdGl0bGUkPVwiIC0gUGFnZVwiXSc7XHJcblx0XHRcdFx0Y29uc3QgaGFuZGxlZCA9IG5ldyBXZWFrU2V0PEVsZW1lbnQ+KCk7XHJcblx0XHRcdFx0Y29uc3Qgc2NhbiA9ICgpID0+IHtcclxuXHRcdFx0XHRcdGZvciAoY29uc3QgYSBvZiBBcnJheS5mcm9tKGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGw8SFRNTEFuY2hvckVsZW1lbnQ+KFRPUElDX0xJTktfU0VMKSkpIHtcclxuXHRcdFx0XHRcdFx0aWYgKGhhbmRsZWQuaGFzKGEpKSBjb250aW51ZTtcclxuXHRcdFx0XHRcdFx0aGFuZGxlZC5hZGQoYSk7XHJcblx0XHRcdFx0XHRcdGluamVjdFZpZGVvSW50b1RvcGljSGVhZGluZyhhKTtcclxuXHRcdFx0XHRcdH1cclxuXHRcdFx0XHR9O1xyXG5cdFx0XHRcdHNjYW4oKTtcclxuXHRcdFx0XHRuZXcgTXV0YXRpb25PYnNlcnZlcihzY2FuKS5vYnNlcnZlKGRvY3VtZW50LmJvZHksIHsgY2hpbGRMaXN0OiB0cnVlLCBzdWJ0cmVlOiB0cnVlIH0pO1xyXG5cclxuXHRcdFx0fSBlbHNlIHtcclxuXHRcdFx0XHRjb25zb2xlLndhcm4oXCJbRDJMIFR3ZWFrc10gVW5rbm93biBwYWdlIHR5cGUuXCIpO1xyXG5cdFx0XHR9XHJcblx0XHR9IGNhdGNoIChlKSB7XHJcblx0XHRcdGNvbnNvbGUuZXJyb3IoXCJFcnJvciBvY2N1cmVkIGluIHVzZXJzY3JpcHRcIiwgZSk7XHJcblx0XHRcdC8vYWxlcnQoXCJFcnJvciBvY2N1cmVkIGluIEQyTCBiZXR0ZXJpbmcgdXNlcnNjcmlwdCwgZXJyb3IgaW4gY29uc29sZS5cIik7XHJcblx0XHR9XHJcblx0fVxyXG59KSgpO1xyXG4iLCJcclxuLy8gTWF5YmUgdGhlc2Ugd2lsbCBiZSB1c2VmdWwgZm9yIGEgbm9uLWNvbnRlbnQgcGFnZT8gS2VlcGluZyBhcm91bmQgZm9yIG5vd1xyXG5mdW5jdGlvbiB1cmxPZkQyTEFzc2V0KGNsczogc3RyaW5nLCBhc3NldDogc3RyaW5nKTogW1VSTCwgUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhPl17XHJcblx0aWYgKCFOdW1iZXIuaXNGaW5pdGUoTnVtYmVyLnBhcnNlSW50KGNscykpKSB7XHJcblx0XHR0aHJvdyBuZXcgRXJyb3IoYEQyTCBjbGFzcyBJRCBpc24ndCBwYXJzYWJsZSB0byBhIG51bWJlci9JRDogJyR7Y2xzfSdgKTtcclxuXHR9XHJcblx0aWYgKCFOdW1iZXIuaXNGaW5pdGUoTnVtYmVyLnBhcnNlSW50KGFzc2V0KSkpIHtcclxuXHRcdHRocm93IG5ldyBFcnJvcihgRDJMIGFzc2V0IElEIGlzbid0IHBhcnNhYmxlIHRvIGEgbnVtYmVyL0lEOiAnJHthc3NldH0nYCk7XHJcblx0fVxyXG5cclxuXHQvLyB0aGUgP3N0cmVhbT10cnVlIHRlbGxzIEQyTCB0byB1c2UgYSByZXNwb25zZSBoZWFkZXIgZm9yIGNvbnRlbnQgdG8gYmUgdmlld2VkIGluIHRoZSBicm93c2VyLCByYXRoZXIgdGhhbiBkb3dubG9hZGVkXHJcblx0bGV0IGFzc2V0VVJMID0gbmV3VVJMKGAvZDJsL2FwaS9sZS8xLjM0LyR7Y2xzfS9jb250ZW50L3RvcGljcy8ke2Fzc2V0fS9maWxlYCk7XHJcblx0cmV0dXJuIFtcclxuXHRcdGFzc2V0VVJMLFxyXG5cdFx0RDJMQXNzZXRNZXRhKGFzc2V0VVJMKSwgLy8gcmV0dXJucyBwcm9taXNlXHJcblx0XVxyXG59XHJcbmFzeW5jIGZ1bmN0aW9uIEQyTEFzc2V0TWV0YShsb2NhdGlvbjogVVJMKTogUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhPiB7XHJcblx0bGV0IGYgPSBhd2FpdCBmZXRjaChsb2NhdGlvbi50b1N0cmluZygpLCB7IG1ldGhvZDogJ0hFQUQnIH0pO1xyXG5cclxuXHQvLyBnZXQgbWltZSBhbmQgb3JpZyBmaWxlIG5hbWUsIGlmIGF2YWlsYWJsZVxyXG5cdGxldCB0eXBlID0gZi5oZWFkZXJzLmdldChcImNvbnRlbnQtdHlwZVwiKSE7XHJcblx0bGV0IGZpbGVuYW1lID0gZi5oZWFkZXJzLmdldChcImNvbnRlbnQtZGlzcG9zaXRpb25cIik/Lm1hdGNoKC9maWxlbmFtZT1cIiguKylcIi8pPy5bMV0gPz8gbnVsbDsgLy9udWxsIGlmIGhlYWRlciBtaXNzaW5nLCBubyBmaWxlbmFtZSwgZXRjXHJcblx0bGV0IHNpemUgPSBOdW1iZXIucGFyc2VJbnQoZi5oZWFkZXJzLmdldChcImNvbnRlbnQtbGVuZ3RoXCIpISk7XHJcblxyXG5cdHJldHVybiB7IHR5cGUsIGZpbGVuYW1lLCBzaXplIH07XHJcbn1cclxuXHJcbi8qKiBSZXByZXNlbnRzIGEgRDJMIENvbnRlbnQgcGFnZS4gRGlzdGluZ3Vpc2hlZCBieSBhIGAvZDJsL2xlL2NvbnRlbnQvJGNsYXNzL3ZpZXdDb250ZW50LyRhc3NldC9WaWV3YCB1cmwqL1xyXG5jbGFzcyBDb250ZW50UGFnZSB7XHJcblx0cHJvdGVjdGVkIGNvbnRlbnRWaWV3OiBIVE1MRWxlbWVudDtcclxuXHRwcm90ZWN0ZWQgaGRyQmFyOiBIVE1MRWxlbWVudDtcclxuXHRwcm90ZWN0ZWQgY2xzOiBzdHJpbmc7XHJcblx0cHJvdGVjdGVkIGFzc2V0OiBzdHJpbmc7XHJcblx0LyoqIE1heSBwb2ludCB0byBhIDQwNC4gU2hvdWxkIGJlIGF2b2lkZWQuICovXHJcblx0cHJvdGVjdGVkIG5haXZlQXNzZXRVUkw6IFVSTDtcclxuXHJcblx0cHJvdGVjdGVkIGludGVyYWN0aXZlVVJMOiBVUkwgfCBudWxsO1xyXG5cdHByb3RlY3RlZCBkb3dubG9hZGFibGVVUkw6IFVSTCB8IG51bGw7XHJcblx0cHJvdGVjdGVkIG9mZmljZVVSTDogVVJMIHwgbnVsbDtcclxuXHJcblx0cHJvdGVjdGVkIF9uYWl2ZUFzc2V0TWV0YT86IFByb21pc2U8RDJMQXNzZXRNZXRhZGF0YSB8IG51bGw+O1xyXG5cdHByb3RlY3RlZCByZXBsYWNlZENvbnRlbnQ6IGJvb2xlYW47XHJcblxyXG5cdHN0YXRpYyBpbml0aWFsaXplKCk6IENvbnRlbnRQYWdlIHwgbnVsbCB7XHJcblx0XHRsZXQgcHR5cGUgPSBnZXRQYWdlVHlwZSgpO1xyXG5cdFx0aWYocHR5cGUudHlwZSA9PSBcImNvbnRlbnRcIikge1xyXG5cdFx0XHRyZXR1cm4gbmV3IENvbnRlbnRQYWdlKHB0eXBlLmNsYXNzLCBwdHlwZS5hc3NldCk7XHJcblx0XHR9XHJcblx0XHRyZXR1cm4gbnVsbDtcclxuXHR9XHJcblx0Y29uc3RydWN0b3IoY2xzOiBzdHJpbmcsIGFzc2V0OiBzdHJpbmcpIHtcclxuXHRcdGxldCBjdiA9IGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoXCIjQ29udGVudFZpZXdcIikgYXMgSFRNTEVsZW1lbnQgfCBudWxsO1xyXG5cdFx0aWYoIWN2KSB7IHRocm93IG5ldyBFcnJvcihcIlBhZ2UgZG9lc24ndCBoYXZlIGEgI0NvbnRlbnRWaWV3ICFcIik7IH1cclxuXHJcblx0XHRsZXQgaGRyQmFyID0gZG9jdW1lbnQucXVlcnlTZWxlY3RvcihcIi5kMmwtcGFnZS10aXRsZS1jIC5kMmwtYm94LWhcIikgYXMgSFRNTEVsZW1lbnQgfCBudWxsO1xyXG5cdFx0aWYoIWhkckJhcikgdGhyb3cgbmV3IEVycm9yKFwiVW5hYmxlIHRvIGZpbmQgY29udGVudCBoZWFkZXIgYmFyIVwiKTtcclxuXHRcdFxyXG5cclxuXHRcdHRoaXMuY29udGVudFZpZXcgPSBjdjtcclxuXHRcdHRoaXMuaGRyQmFyID0gaGRyQmFyO1xyXG5cdFx0dGhpcy5jbHMgPSBjbHM7XHJcblx0XHR0aGlzLmFzc2V0ID0gYXNzZXQ7XHJcblx0XHR0aGlzLm5haXZlQXNzZXRVUkwgPSBuZXdVUkwoYC9kMmwvYXBpL2xlLzEuMzQvJHtjbHN9L2NvbnRlbnQvdG9waWNzLyR7YXNzZXR9L2ZpbGVgKTtcclxuXHJcblx0XHR0aGlzLm9mZmljZVVSTCA9IG51bGw7XHJcblx0XHR0aGlzLmludGVyYWN0aXZlVVJMID0gdGhpcy5faW50ZXJhY3RpdmVVUkwoKTtcclxuXHRcdHRoaXMuZG93bmxvYWRhYmxlVVJMID0gdGhpcy5fZG93bmxvYWRhYmxlVVJMKCk7XHJcblxyXG5cdFx0dGhpcy5fbmFpdmVBc3NldE1ldGEgPSB1bmRlZmluZWQ7XHJcblx0XHR0aGlzLnJlcGxhY2VkQ29udGVudCA9IGZhbHNlO1xyXG5cclxuXHRcdGNvbnNvbGUubG9nKCdIZWFkZXIgZHJvcGRvd24gYWN0aW9uczogJywgdGhpcy5oZWFkZXJEcm9wZG93bkFjdGlvbnMoKSk7XHJcblx0fVxyXG5cclxuXHRwcm90ZWN0ZWQgZ2V0IHRpdGxlKCk6IHN0cmluZyB7XHJcblx0XHRsZXQgdGl0bGUgPSB0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yKCcuZDJsLXBhZ2UtdGl0bGUnKSEgYXMgSFRNTEVsZW1lbnQ7XHJcblx0XHRyZXR1cm4gdGl0bGUuaW5uZXJUZXh0O1xyXG5cdH1cclxuXHJcblx0cHVibGljIG5vcm1hbGl6ZUNvbnRlbnQoKSB7XHJcblx0XHRpZih0aGlzLmludGVyYWN0aXZlVVJMICYmICF0aGlzLnJlcGxhY2VkQ29udGVudCkge1xyXG5cdFx0XHR0aGlzLnJlcGxhY2VDb250ZW50KHRoaXMuaW50ZXJhY3RpdmVVUkwpO1xyXG5cdFx0fVxyXG5cdH1cclxuXHRcclxuXHRwdWJsaWMgYWRkSGVhZGVyQnV0dG9ucygpIHtcclxuXHRcdC8vIHJlcGxhY2UgaW5uZXIgY29udGVudFxyXG5cdFx0Ly8gb3BlbiBpbiBuZXcgdGFiXHJcblx0XHQvLyBkb3dubG9hZFxyXG5cclxuXHRcdGNvbnN0IGJ0bkJhciA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ3NwYW4nKTtcclxuXHRcdGJ0bkJhci5zdHlsZS5tYXJnaW5MZWZ0ID0gXCIwLjNyZW1cIjsgLy8gYXJiaXRyYXJ5IC0gbG9va2VkIGFscmlnaHQgZm9yIGEgZmV3IGRpZmZlcmVudCB0aXRsZSBuYW1lc1xyXG5cdFx0Y29uc3QgYXBwZW5kID0gKGVsZTogSFRNTEVsZW1lbnQpID0+IGJ0bkJhci5hcHBlbmRDaGlsZChlbGUpO1xyXG5cclxuXHRcdGlmKHRoaXMuaW50ZXJhY3RpdmVVUkwpIHtcclxuXHRcdFx0Y29uc29sZS5sb2coXCJBZGRpbmcgaW50ZXJhY3RpdmUgYnV0dG9uc1wiKTtcclxuXHRcdFx0aWYoIXRoaXMucmVwbGFjZWRDb250ZW50KSB7XHJcblx0XHRcdFx0Ly8gb25seSBzaG93IGJ1dHRvbiBpZiB3ZSBoYXZlbid0IHJlcGxhY2VkIG91cnNlbHZlcywgYW5kIHdlIGhhdmUgYW4gaW50ZXJhY3RpdmUgVVJMIHRvIHNob3dcclxuXHRcdFx0XHRhcHBlbmQodGhpcy50aXRsZUJ0bl9yZXBsYWNlQ29udGVudChcIlVzZSBOYXRpdmUgVmlld2VyXCIsIHRoaXMuaW50ZXJhY3RpdmVVUkwpKTtcclxuXHRcdFx0fVxyXG5cdFx0XHRhcHBlbmQodGhpcy50aXRsZUxpbmsoXCJWaWV3IERpcmVjdGx5XCIsIHRoaXMuaW50ZXJhY3RpdmVVUkwpKTtcclxuXHJcblx0XHRcdGlmKHRoaXMub2ZmaWNlVVJMKSB7XHJcblx0XHRcdFx0YXBwZW5kKHRoaXMudGl0bGVMaW5rKFwiVmlldyBJbnRlcmFjdGl2ZVwiLCB0aGlzLm9mZmljZVVSTCkpO1xyXG5cdFx0XHR9XHJcblx0XHR9XHJcblxyXG5cdFx0aWYodGhpcy5kb3dubG9hZGFibGVVUkwpIHtcclxuXHRcdFx0Y29uc29sZS5sb2coXCJBZGRpbmcgZG93bmxvYWRhYmxlIGJ1dHRvbnNcIik7XHJcblx0XHRcdGFwcGVuZCh0aGlzLnRpdGxlTGluayhcIkRvd25sb2FkXCIsIHRoaXMuZG93bmxvYWRhYmxlVVJMLCBkMmxfaWNvbl9kb3dubG9hZCgpKSk7XHJcblxyXG5cdFx0XHRsZXQgaGRyQnRucyA9IHRoaXMuaGVhZGVyRHJvcGRvd25BY3Rpb25zKCk7XHJcblx0XHRcdGlmKGhkckJ0bnMgJiYgaGRyQnRucy5sZW5ndGggPT0gMSAmJiBoZHJCdG5zWzBdID09IFwiRG93bmxvYWRcIikge1xyXG5cdFx0XHRcdC8vIHJlbW92ZSBkcm9wZG93biBpZiB0aGVyZSBpcyBvbmx5ICdEb3dubG9hZCcgLSB3ZSBhcmUgc3VwcGx5aW5nIG91ciBvd24gYnV0dG9uIGFib3ZlXHJcblx0XHRcdFx0dGhpcy5oZHJCYXIucXVlcnlTZWxlY3RvcignZDJsLWRyb3Bkb3duW2RhdGEtY29udGV4dG1lbnVpZD1kMmxfcGFnZVRpdGxlQWN0aW9uc10nKT8ucmVtb3ZlKCk7XHJcblx0XHRcdH1cclxuXHRcdH1cclxuXHJcblx0XHR0aGlzLmhkckJhci5zdHlsZS5mbGV4V3JhcCA9ICd3cmFwJztcclxuXHRcdHRoaXMuaGRyQmFyLnN0eWxlLmZsZXhEaXJlY3Rpb24gPSAncm93JztcclxuXHRcdHRoaXMuaGRyQmFyLmFwcGVuZENoaWxkKGJ0bkJhcik7XHJcblx0fVxyXG5cdFxyXG5cclxuXHQvKiogUmV0dXJucyBhIFVSTCBhcHByb3ByaWF0ZSB0byBlbWJlZCBpbiBhbiBJRnJhbWUsIG9yIG9wZW4gaW4gYSBuZXcgYnJvd3NlciB0YWIuXHJcblx0ICogXHJcblx0ICogTW9zdCBpbXBvcnRhbnRseSwgaXQgZG9lc24ndCBzZXJ2ZSB0aGUgYENvbnRlbnQtRGlzcG9zaXRpb246IGF0dGFjaG1lbnRgIGhlYWRlci5cclxuXHQqL1xyXG5cdHByb3RlY3RlZCBfaW50ZXJhY3RpdmVVUkwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHQvLyBUT0RPOiBHZXQgbWltZSB0eXBlIGFuZCBjaGVjayBhZ2FpbnN0IG5hdmlnYXRvci5taW1lVHlwZXMgP1xyXG5cclxuXHRcdGxldCBhc1F1aXogPSB0aGlzLnF1aXpVcmwoKTtcclxuXHRcdGlmKGFzUXVpeikgcmV0dXJuIGFzUXVpejtcclxuXHJcblx0XHRsZXQgYXNNUDQgPSB0aGlzLm1wNFVybCgpO1xyXG5cdFx0aWYoYXNNUDQpIHJldHVybiBhc01QNDtcclxuXHJcblx0XHRsZXQgYXNQREYgPSB0aGlzLnBkZlVybCgpO1xyXG5cclxuXHRcdC8vIG9mZmljZSBzaG91bGQgb25seSBzaG93IHZpYSBidXR0b25cclxuXHRcdGlmKGFzUERGKSB7XHJcblx0XHRcdGlmKCFhc1BERlsxXSlcclxuXHRcdFx0XHR0aGlzLm9mZmljZVVSTCA9IHRoaXMub2ZmaWNlVXJsKCk7XHJcblx0XHRcdHJldHVybiBhc1BERlswXTtcclxuXHRcdH1cclxuXHJcblx0XHRsZXQgYXNFeHRQYWdlID0gdGhpcy5leHRQYWdlVXJsKCk7XHJcblx0XHRpZihhc0V4dFBhZ2UpIHJldHVybiBhc0V4dFBhZ2U7XHJcblxyXG5cdFx0Y29uc29sZS5sb2coXCJQYWdlIG5vdCByZWNvZ25pemVkIGFzIGludGVyYWN0aXZlXCIpO1xyXG5cclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHJcblx0LyoqIFJldHVybmVkIFVSTCBzaG91bGQgaWRlYWxseSBzZXJ2ZSB0aGUgYENvbnRlbnQtRGlzcG9zaXRpb246IGF0dGFjaG1lbnRgIGhlYWRlciAqL1xyXG5cdHByb3RlY3RlZCBfZG93bmxvYWRhYmxlVVJMKCk6IFVSTCB8IG51bGwge1xyXG5cdFx0bGV0IGFzRXh0UGFnZSA9IHRoaXMuZXh0UGFnZVVybCgpO1xyXG5cdFx0aWYoYXNFeHRQYWdlKSByZXR1cm4gbnVsbDtcclxuXHJcblx0XHQvL2lmKHRoaXMucXVpelVybCgpKSByZXR1cm4gbnVsbDtcclxuXHRcdC8vIElmIHRoaXMgcGFnZSB3cmFwcyBEMkwgYW5vdGhlciBEMkwgcGFnZT9cclxuXHRcdC8vIGVnKSBxdWl6emVzLCBhc3NpZ25tZW50cywgZXRjIHBsYWNlZCBpbiBDb250ZW50XHJcblx0XHRpZihkb2N1bWVudC5xdWVyeVNlbGVjdG9yKFwiI0NvbnRlbnRWaWV3ID4gLmQybC1wbGFjZWhvbGRlclwiKSkgcmV0dXJuIG51bGw7XHJcblx0XHRpZih0aGlzLmV4dFBhZ2VVcmwoKSkgcmV0dXJuIG51bGw7XHJcblxyXG5cdFx0cmV0dXJuIHRoaXMubmFpdmVGaWxlVVJMKGZhbHNlKTtcclxuXHR9XHJcblxyXG5cdC8qKiBVbm5lZWRlZD8gKi9cclxuXHRwcml2YXRlIF9hc3NldE1ldGEoKTogUHJvbWlzZTxEMkxBc3NldE1ldGFkYXRhIHwgbnVsbD4ge1xyXG5cdFx0aWYodGhpcy5fbmFpdmVBc3NldE1ldGEpIHJldHVybiB0aGlzLl9uYWl2ZUFzc2V0TWV0YTtcclxuXHJcblx0XHRyZXR1cm4gdGhpcy5fbmFpdmVBc3NldE1ldGEgPSBmZXRjaCh0aGlzLm5haXZlQXNzZXRVUkwudG9TdHJpbmcoKSwgeyBtZXRob2Q6ICdIRUFEJyB9KVxyXG5cdFx0XHQudGhlbihyZXMgPT4ge1xyXG5cdFx0XHRcdGlmKHJlcy5zdGF0dXMgPT0gNDA0KSB7XHJcblx0XHRcdFx0XHRyZXR1cm4gbnVsbDtcclxuXHRcdFx0XHR9IGVsc2Uge1xyXG5cdFx0XHRcdFx0Ly8gZ2V0IG1pbWUgYW5kIG9yaWcgZmlsZSBuYW1lLCBpZiBhdmFpbGFibGVcclxuXHRcdFx0XHRcdGxldCB0eXBlID0gcmVzLmhlYWRlcnMuZ2V0KFwiY29udGVudC10eXBlXCIpITtcclxuXHRcdFx0XHRcdGxldCBmaWxlbmFtZSA9IHJlcy5oZWFkZXJzLmdldChcImNvbnRlbnQtZGlzcG9zaXRpb25cIik/Lm1hdGNoKC9maWxlbmFtZT1cIiguKylcIi8pPy5bMV0gPz8gbnVsbDsgLy9udWxsIGlmIGhlYWRlciBtaXNzaW5nLCBubyBmaWxlbmFtZSwgZXRjXHJcblx0XHRcdFx0XHRsZXQgc2l6ZSA9IE51bWJlci5wYXJzZUludChyZXMuaGVhZGVycy5nZXQoXCJjb250ZW50LWxlbmd0aFwiKSEpO1xyXG5cdFx0XHRcdFxyXG5cdFx0XHRcdFx0cmV0dXJuIHsgdHlwZSwgZmlsZW5hbWUsIHNpemUgfSBhcyBEMkxBc3NldE1ldGFkYXRhO1xyXG5cdFx0XHRcdH1cclxuXHRcdFx0fSk7XHJcblx0fVxyXG5cclxuXHQvKiogUmV0dXJucyB0aGUgaHVtYW4tcmVhZGFibGUgbGFiZWxzIGZvciB0aGUgY29udGVudCdzIGRyb3Bkb3duIGFjdGlvbnMgKi9cclxuXHRwcm90ZWN0ZWQgaGVhZGVyRHJvcGRvd25BY3Rpb25zKCk6IHN0cmluZ1tdIHwgbnVsbCB7XHJcblx0XHQvLyBvbmx5IHdvcmtzIGlmIHRoZSBkcm9wZG93biBoYXMgYmVlbiBhY3RpdmF0ZWRcclxuXHRcdC8vQXJyYXkuZnJvbSh0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yQWxsKCdkMmwtZHJvcGRvd24gZDJsLWRyb3Bkb3duLW1lbnUgZDJsLW1lbnUgZDJsLW1lbnUtaXRlbScpKVxyXG5cdFx0Ly9cdC5tYXAoZSA9PiBlLmdldEF0dHJpYnV0ZSgndGV4dCcpKVxyXG5cclxuXHRcdC8vIEFjY2VzcyB1bnJlbmRlcmVkICh0ZW1wbGF0ZWQpIGRyb3Bkb3duIGFjdGlvbnMsIGFuZCBnZXQgdGhlaXIgdGV4dC5cclxuXHRcdGxldCB0ZW1wbDogSFRNTFRlbXBsYXRlRWxlbWVudCB8IG51bGwgPSB0aGlzLmhkckJhci5xdWVyeVNlbGVjdG9yKCd0ZW1wbGF0ZSNkMmxfcGFnZVRpdGxlQWN0aW9ucycpO1xyXG5cdFx0aWYoIXRlbXBsKSByZXR1cm4gbnVsbDtcclxuXHRcdHJldHVybiBBcnJheS5mcm9tKHRlbXBsLmNvbnRlbnQucXVlcnlTZWxlY3RvckFsbCgnZDJsLW1lbnUtaXRlbVt0ZXh0XScpKVxyXG5cdFx0XHQubWFwKGUgPT4gZS5nZXRBdHRyaWJ1dGUoJ3RleHQnKSEpO1xyXG5cclxuXHRcdC8qIGN1cnJlbnRseSBrbm93bjpcclxuXHRcdFx0ZmlsZSBjb250ZW50OiBcIkRvd25sb2FkXCJcclxuXHRcdFx0cXVpejogXCJWaWV3IFN1bW1hcnlcIlxyXG5cdFx0XHRxdWl6OiBcIlZpZXcgU3VibWlzc2lvbnNcIlxyXG5cdFx0XHRxdWl6OiBcIlZpZXcgUmVwb3J0c1wiXHJcblx0XHQqL1xyXG5cdH1cclxuXHJcblx0Ly8gS2VlcCB0aGVzZSBvcmdhbml6ZWQgcm91Z2hseSBpbiB0aGUgb3JkZXIgb2YgbGVhc3QtPm1vc3QgZXhwZW5zaXZlIHRvIHJ1blxyXG5cclxuXHRwcml2YXRlIHF1aXpVcmwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHRsZXQgcXVpel9pZnJhbWUgPSB0aGlzLmNvbnRlbnRWaWV3LnF1ZXJ5U2VsZWN0b3IoJyNRdWl6Q29udGVudFZpZXdQbGFjZUhvbGRlciA+IGlmcmFtZVtzcmNdJyk7XHJcblx0XHRpZihxdWl6X2lmcmFtZSkge1xyXG5cdFx0XHRsZXQgdXJsID0gbmV3VVJMKHF1aXpfaWZyYW1lLmdldEF0dHJpYnV0ZSgnc3JjJykhKTtcclxuXHRcdFx0Ly8gRW5zdXJlIGEgcmVndWxhciBEMkwgcGFnZSwgcmF0aGVyIHRoYW4gd2l0aG91dCBoZWFkZXJzL21hcmdpbnMvYm9yZGVycy9ldGNcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2NmcWwnKTtcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2RuYicpO1xyXG5cdFx0XHR1cmwuc2VhcmNoUGFyYW1zLmRlbGV0ZSgnY29udGVudFRvcGljSWQnKTtcclxuXHRcdFx0dXJsLnNlYXJjaFBhcmFtcy5kZWxldGUoJ2QybF9ib2R5X3R5cGUnKTtcclxuXHRcdFx0cmV0dXJuIHVybDtcclxuXHRcdH1cclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHJcblx0Ly8gSW50ZXJhY3RpdmUgb25seSAtIG5vIGNvbnRlbnQtZGlzcG9zaXRpb25cclxuXHRwcml2YXRlIG1wNFVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdGxldCBwbGF5ZXJzID0gdGhpcy5jb250ZW50Vmlldy5xdWVyeVNlbGVjdG9yQWxsKFwiZGl2LnZ1aS1tZWRpYXBsYXllcltkYXRhLW1lZGlhcGxheWVyLXNyY11cIik7XHJcblx0XHRpZihwbGF5ZXJzLmxlbmd0aCA9PSAwKSB7XHJcblx0XHRcdHJldHVybiBudWxsO1xyXG5cdFx0fSBlbHNlIGlmKHBsYXllcnMubGVuZ3RoID4gMSkge1xyXG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoJ01vcmUgdGhhbiAxIG1lZGlhIHBsYXllciBmb3VuZCBvbiB0aGUgcGFnZSEnKTtcclxuXHRcdH1cclxuXHRcdGxldCBwbGF5ZXIgPSBwbGF5ZXJzWzBdO1xyXG5cclxuXHRcdHJldHVybiBuZXdVUkwocGxheWVyLmdldEF0dHJpYnV0ZSgnZGF0YS1tZWRpYXBsYXllci1zcmMnKSEpO1xyXG5cdH1cclxuXHJcblx0LyoqIEludGVyYWN0aXZlLiBNYXkgYmUgaGFuZGxlZCBieSBhbiBleHRlbnNpb24gKi9cclxuXHRwcml2YXRlIG9mZmljZVVybCgpOiBVUkwgfCBudWxsIHtcclxuXHRcdC8qXHJcblx0XHQgKiBUaGVyZSdzIGN1cnJlbnRseSBhIGJ1ZyBpbiB0aGUgYE9mZmljZSBFZGl0aW5nIGZvciBEb2NzLCBTaGVldHMsICYgU2xpZGVzYCBleHRlbnNpb24gdGhhdCBwcmV2ZW50cyBpdCBmcm9tIHBhcnNpbmcgdGhlICdDb250ZW50LURpc3Bvc2l0aW9uJyBoZWFkZXIgY29ycmVjdGx5LlxyXG5cdFx0ICogV2hpbGUgaXQgZG9lc24ndCBhZmZlY3QgZnVuY3Rpb25hbGl0eSwgdGhlIGZpbGVuYW1lIGlzIG1hbmdsZWQgZnJvbSB3aGF0IGl0IGlzIHN1cHBvc2VkIHRvIGJlLlxyXG5cdFx0ICogXHJcblx0XHQgKiBXZSBjb3VsZCB0cnkgdG8gc2VydmUgdGhlIGZpbGUgb3ZlciBgL2NvbnRlbnQvZW5mb3JjZWQvJHtjbGFzc19uYW1lfS8ke2RpdltkYXRhLXRpdGxlXX1gIGJ1dCB0aGVyZSBpc24ndCBhbiBlYXN5KD8pIHdheSB0byBnZXQgJGNsYXNzX25hbWUgZnJvbSB0aGUgcGFnZSAoRDJMIEpTIGFwaT8pXHJcblx0XHQgKiBNUDQgZmlsZXMgc2VlbSB0byBiZSBzZXJ2ZWQgZnJvbSB0aGlzIGZvbGRlciwgaG93ZXZlci5cclxuXHRcdCovXHJcblx0XHRpZihDQVBBQklMSVRJRVMuZXh0ZW5zaW9ucy5vZmZpY2Vfdmlld2VyKSB7XHJcblx0XHRcdGxldCBhc1BERiA9IHRoaXMucGRmVXJsKCk7XHJcblx0XHRcdGlmKGFzUERGICYmICFhc1BERlsxXSkge1xyXG5cdFx0XHRcdC8vIGlmIHdlIGFyZW4ndCBhIG5hdGl2ZSBQREYgLSB3ZSBtaWdodCBiZSBhIGNvbnZlcnRlZCBvZmZpY2UgUERGP1xyXG5cdFxyXG5cdFx0XHRcdC8vIGNoZWNrIGluIHBkZlVybCBlbnN1cmVzIHRoZXJlIGlzIG9ubHkgMSBlbGVtZW50IHRoYXQgbWF0Y2hlcyB0aGlzXHJcblx0XHRcdFx0bGV0IHZpZXdlciA9IHRoaXMuY29udGVudFZpZXcucXVlcnlTZWxlY3RvckFsbChcImRpdltjbGFzc149ZDJsLWZpbGV2aWV3ZXItcGRmLV1bZGF0YS1sb2NhdGlvbl1cIilbMF07XHJcblx0XHRcdFx0XHJcblx0XHRcdFx0bGV0IGV4dCA9IHZpZXdlci5nZXRBdHRyaWJ1dGUoJ2RhdGEtdGl0bGUnKT8uc3BsaXQoJy4nKS5zbGljZSgtMSlbMF07XHJcblx0XHRcdFx0aWYoWydkb2MnLCAnZG90JywgJ3hscycsICd4bHQnLCAnY3N2JywgJ3BwdCcsICdwb3QnLCAncHBzJywgJ3NsZCddLnNvbWUocyA9PiBleHQ/LnN0YXJ0c1dpdGgocykpKSB7XHJcblx0XHRcdFx0XHRyZXR1cm4gdGhpcy5uYWl2ZUZpbGVVUkwodHJ1ZSk7XHJcblx0XHRcdFx0XHQvL3JldHVybiBuZXdVUkwoYC9jb250ZW50L2VuZm9yY2VkL2ApXHJcblxyXG5cdFx0XHRcdFx0bGV0IHRpdGxlID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgndGl0bGUnKTtcclxuXHRcdFx0XHRcdHRpdGxlLmlubmVyVGV4dCA9IHRoaXMudGl0bGU7XHJcblx0XHRcdFx0XHRcclxuXHRcdFx0XHRcdGxldCBib2R5ID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnYm9keScpO1xyXG5cdFx0XHRcdFx0Ym9keS5zdHlsZS5tYXJnaW4gPSAnMCc7XHJcblxyXG5cdFx0XHRcdFx0bGV0IGZyYW1lID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnaWZyYW1lJyk7XHJcblx0XHRcdFx0XHRmcmFtZS5zcmMgPSB0aGlzLm5haXZlRmlsZVVSTCh0cnVlKS50b1N0cmluZygpO1xyXG5cdFx0XHRcdFx0ZnJhbWUuc2V0QXR0cmlidXRlKCdhbGxvd2Z1bGxzY3JlZW4nLCAnMScpO1xyXG5cdFx0XHRcdFx0ZnJhbWUuc3R5bGUuYm9yZGVyID0gJzAnXHJcblx0XHRcdFx0XHRmcmFtZS5zdHlsZS53aWR0aCA9ICcxMDAlJ1xyXG5cdFx0XHRcdFx0ZnJhbWUuc3R5bGUuaGVpZ2h0ID0gJzEwMCUnXHJcblxyXG5cdFx0XHRcdFx0bGV0IGRvYyA9IGBcclxuXHRcdFx0XHRcdFx0PGh0bWw+XHJcblx0XHRcdFx0XHRcdFx0PGhlYWQ+JHt0aXRsZS5vdXRlckhUTUx9PC9oZWFkPlxyXG5cdFx0XHRcdFx0XHRcdDxib2R5PiR7ZnJhbWUub3V0ZXJIVE1MfTwvYm9keT5cclxuXHRcdFx0XHRcdFx0PC9odG1sPlxyXG5cdFx0XHRcdFx0YDtcclxuXHJcblx0XHRcdFx0XHRsZXQgYXNCYXNlNjQgPSBidG9hKGRvYyk7XHJcblx0XHRcdFx0XHRyZXR1cm4gbmV3IFVSTCgnZGF0YTp0ZXh0L2h0bWw7YmFzZTY0LCcgKyBhc0Jhc2U2NCk7XHJcblx0XHRcdFx0fVxyXG5cdFx0XHR9XHJcblx0XHR9XHJcblx0XHRcclxuXHRcdHJldHVybiBudWxsO1xyXG5cdH1cclxuXHJcblx0LyoqIEludGVyYWN0aXZlIG9ubHkuIC0gbm8gY29udGVudC1kaXNwb3NpdGlvbi5cclxuXHQgKiBcclxuXHQgKiBNYXkgYmUgZG93bmxvYWRhYmxlIGlmZiBgcGRmVXJsWzFdYFxyXG5cdCAqL1xyXG5cdHByaXZhdGUgcGRmVXJsKCk6IFtVUkwsIG5hdGl2ZVBERjogYm9vbGVhbl0gfCBudWxsIHtcclxuXHRcdC8vIG5hdGl2ZSBwZGY6IC5kMmwtZmlsZXZpZXdlci1wZGYtbmF0aXZlXHJcblx0XHQvLyBjb252ZXJ0ZWQgdG8gcGRmOiAuZDJsLWZpbGV2aWV3ZXItcGRmLXBkZmpzXHJcblxyXG5cdFx0Ly8gQ291bGQgd2UgZGV0ZXJtaW5lIGlmIG5hdGl2ZS9jb252ZXJ0ZWQgdmlhIHRoZSBkYXRhLWxvY2F0aW9uIFVSTD9cclxuXHRcdC8vIEZpbGVzIGNvbnZlcnRlZCB0byBQREYgdGVuZCB0byBiZSBzZXJ2ZWQgb3ZlciBBV1NcclxuXHJcblx0XHQvLyBHZXQgUERGIHZpZXdlcnMgb24gdGhlIHBhZ2UsIGFuZCBlbnN1cmUgdGhlcmUgaXMgb25seSBvbmUgZm9yIG91ciBjb250ZW50XHJcblx0XHRsZXQgcGRmVmlld2VycyA9IHRoaXMuY29udGVudFZpZXcucXVlcnlTZWxlY3RvckFsbChcImRpdltjbGFzc149ZDJsLWZpbGV2aWV3ZXItcGRmLV1bZGF0YS1sb2NhdGlvbl1cIilcclxuXHRcdGlmKHBkZlZpZXdlcnMubGVuZ3RoID09IDApIHtcclxuXHRcdFx0cmV0dXJuIG51bGw7XHJcblx0XHR9IGVsc2UgaWYocGRmVmlld2Vycy5sZW5ndGggPiAxKSB7XHJcblx0XHRcdHRocm93IG5ldyBFcnJvcignTW9yZSB0aGFuIDEgUERGIEZpbGV2aWV3ZXIgd2l0aCBhIGRhdGEgbG9jYXRpb24gZm91bmQgb24gdGhlIHBhZ2UhJyk7XHJcblx0XHR9XHJcblx0XHRsZXQgdmlld2VyID0gcGRmVmlld2Vyc1swXTtcclxuXHJcblx0XHQvLyBFeHRyYWN0IHRoZSB0eXBlIG9mIGNvbnRlbnRzIG91ciBQREYgdmlld2VyIGlzIHNob3dpbmdcclxuXHRcdGxldCB0eXBlID0gQXJyYXkuZnJvbSh2aWV3ZXIuY2xhc3NMaXN0KVxyXG5cdFx0XHQuZmlsdGVyKHMgPT4gcy5zdGFydHNXaXRoKCdkMmwtZmlsZXZpZXdlci1wZGYtJykpXHJcblx0XHRcdC5tYXAocyA9PiBzLnNwbGl0KCctJykuc2xpY2UoLTEpWzBdKTsgLy8gZ2V0IGxhc3QgY29tcG9uZW50XHJcblx0XHRpZih0eXBlLmxlbmd0aCAhPSAxKSB7IC8vID4xIGJlY2F1c2Ugd2UgZmlsdGVyIHRvIHRoaXMgY2xhc3MgcHJlZml4XHJcblx0XHRcdHRocm93IG5ldyBFcnJvcignTW9yZSB0aGFuIG9uZSBEMkwgUERGIEZpbGV2aWV3ZXJzIG9uIHRoZSBwYWdlJyk7XHJcblx0XHR9XHJcblxyXG5cdFx0Ly8gcmV0dXJuIGEgdHVwbGUgb2YgW3VybCwgaXNOYXRpdmVQREZdXHJcblx0XHRsZXQgbG9jYXRpb24gPSBuZXdVUkwodmlld2VyLmdldEF0dHJpYnV0ZSgnZGF0YS1sb2NhdGlvbicpISk7XHJcblx0XHRzd2l0Y2godHlwZVswXSkge1xyXG5cdFx0XHRjYXNlIFwibmF0aXZlXCI6IHJldHVybiBbbG9jYXRpb24sIHRydWVdO1xyXG5cdFx0XHRjYXNlIFwicGRmanNcIjogcmV0dXJuIFtsb2NhdGlvbiwgZmFsc2VdO1xyXG5cdFx0XHRkZWZhdWx0OiB0aHJvdyBuZXcgRXJyb3IoJ1Vua25vd24gUERGIHZpZXdlciB0eXBlOiAnICsgdHlwZVswXSk7XHJcblx0XHR9XHJcblxyXG5cdH1cclxuXHJcblx0LyoqIE9ubHkgaW50ZXJhY3RpdmUgKi9cclxuXHRwcml2YXRlIGV4dFBhZ2VVcmwoKTogVVJMIHwgbnVsbCB7XHJcblx0XHRsZXQgaXRleHQgPSB0aGlzLmNvbnRlbnRWaWV3LmlubmVyVGV4dDtcclxuXHRcdGlmICghaXRleHQuaW5jbHVkZXMoXCJFeHRlcm5hbCBSZXNvdXJjZVwiKSB8fCAhaXRleHQuaW5jbHVkZXMoXCJPcGVuIGluIE5ldyBXaW5kb3dcIikpIHtcclxuXHRcdFx0cmV0dXJuIG51bGw7XHJcblx0XHR9XHJcblx0XHQvLyByZWFzb25hYmx5IHN1cmUgdGhpcyBwYWdlJ3MgY29udGVudCBpcyBhbiBleHRlcm5hbCBwYWdlIC0gdGhyb3cgb24gYW55IGVycm9yc1xyXG5cclxuXHRcdGxldCB1cmxzID0gT2JqZWN0LnZhbHVlcyhEMkwuT1IuX19nMSlcclxuXHRcdFx0Lm1hcChzID0+IHsgdHJ5IHtcclxuXHRcdFx0XHRyZXR1cm4gSlNPTi5wYXJzZShzKSBhcyBEMkxfT1JfT2JqZWN0XHJcblx0XHRcdH0gY2F0Y2goZSkge1xyXG5cdFx0XHRcdHRocm93IG5ldyBFcnJvcihcIkQyTC5PUi5fX2cxIGNvbnRhaW5zIG1hbGZvcm1lZCBKU09OIVwiKVxyXG5cdFx0XHR9IH0pXHJcblx0XHRcdC5maWx0ZXIoKG8pOiBvIGlzIE9SX09iamVjdHMuRnVuYyA9PiB7XHJcblx0XHRcdFx0cmV0dXJuIG8uX3R5cGUgPT0gXCJmdW5jXCIgJiYgby5OID09IFwiRDJMLkxFLkNvbnRlbnQuRGVza3RvcC5Ub3BpYy5PcGVuSW5OZXdXaW5kb3dcIiAmJiBvLlAubGVuZ3RoID09IDFcclxuXHRcdFx0fSlcclxuXHRcdFx0Lm1hcChvID0+IG8uUFswXSk7XHJcblx0XHRcclxuXHRcdGlmKHVybHMubGVuZ3RoID09IDApIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKGBDb250ZW50IFZpZXcgc2hvd2luZyBcIkV4dGVybmFsIFJlc291cmNlXCIgd2l0aG91dCBhbnkgT3BlbkluTmV3V2luZG93IGZ1bmN0aW9ucyBpbiBEMkwuT1IuX19nMSFgKTtcclxuXHRcdH0gZWxzZSBpZih1cmxzLmxlbmd0aCA+IDEpIHtcclxuXHRcdFx0dGhyb3cgbmV3IEVycm9yKGBNdWx0aXBsZSBPcGVuSW5OZXdXaW5kb3cgY29tbWFuZHMgZm9yIFwiRXh0ZXJuYWwgUmVzb3VyY2VcIiBjb250ZW50LiBOb3QgcmV0dXJuaW5nIGFueS4gKCR7SlNPTi5zdHJpbmdpZnkodXJscyl9KWApO1xyXG5cdFx0fSBlbHNlIGlmKHR5cGVvZiB1cmxzWzBdICE9ICdzdHJpbmcnKSB7XHJcblx0XHRcdHRocm93IG5ldyBFcnJvcihgU2luZ2xlIE9wZW5Jbk5ld1dpbmRvdyBjb21tYW5kIHBhcmFtZXRlciBpc24ndCBhIHN0cmluZyEgKCR7SlNPTi5zdHJpbmdpZnkodXJsc1swXSl9KWApXHJcblx0XHR9XHJcblxyXG5cdFx0bGV0IHVybCA9IG5ld1VSTCh1cmxzWzBdKTtcclxuXHRcdHVybC5wcm90b2NvbCA9IFwiaHR0cHNcIjsgLy8gb3RoZXJ3aXNlIGlmcmFtZSB3b24ndCBsb2FkIGIvYyBEMkwgaXMgSFRUUFNcclxuXHRcdGlmKHVybC5ob3N0LmluY2x1ZGVzKFwieW91dHViZS5jb21cIikgJiYgdXJsLnBhdGhuYW1lID09IFwiL3dhdGNoXCIpIHtcclxuXHRcdFx0bGV0IHZpZGVvX2lkID0gdXJsLnNlYXJjaFBhcmFtcy5nZXQoXCJ2XCIpO1xyXG5cdFx0XHRpZih2aWRlb19pZCkge1xyXG5cdFx0XHRcdHVybC5zZWFyY2hQYXJhbXMuZGVsZXRlKFwidlwiKTtcclxuXHRcdFx0XHR1cmwucGF0aG5hbWUgPSBcIi9lbWJlZC9cIiArIHZpZGVvX2lkO1xyXG5cdFx0XHR9XHJcblx0XHR9XHJcblxyXG5cdFx0cmV0dXJuIHVybDtcclxuXHR9XHJcblxyXG5cdC8qKiBPbmx5IGRvd25sb2FkYWJsZVxyXG5cdCAqIFxyXG5cdCAqIERvZXNuJ3QgY2hlY2sgZm9yIDQwNCAtIHNob3VsZCBiZSB1c2VkIGFmdGVyIGRldGVjdGlvbiBvZiBleHRlcm5hbCBwYWdlcy9ldGNcclxuXHQqL1xyXG5cdHByaXZhdGUgbmFpdmVGaWxlVVJMKHN0cmVhbTogYm9vbGVhbik6IFVSTCB7XHJcblx0XHRsZXQgdXJsID0gbmV3VVJMKHRoaXMubmFpdmVBc3NldFVSTC50b1N0cmluZygpKSAvLyBjbG9uZSBVUkxcclxuXHRcdHVybC5zZWFyY2hQYXJhbXMuc2V0KFwic3RyZWFtXCIsIHN0cmVhbS50b1N0cmluZygpKTtcclxuXHRcdHJldHVybiB1cmxcclxuXHR9XHJcblxyXG5cdC8qKiBET00gbWFuaXB1bGF0aW9uIGhlbHBlcnMgKi9cclxuXHRwcm90ZWN0ZWQgcmVwbGFjZUNvbnRlbnQoaWZyYW1lX3NyYzogVVJMKSB7XHJcblx0XHRsZXQgbmV3Q29udGVudDogSFRNTEVsZW1lbnQ7XHJcblx0XHRsZXQgaWZyYW0gPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdpZnJhbWUnKVxyXG5cdFx0aWZyYW0uc3JjID0gaWZyYW1lX3NyYy50b1N0cmluZygpO1xyXG5cdFx0aWZyYW0uc3R5bGUud2lkdGggPSAnMTAwJSc7XHJcblx0XHRpZnJhbS5zdHlsZS5oZWlnaHQgPSAnOTB2aCc7XHJcblx0XHRpZnJhbS5zdHlsZS5yZXNpemUgPSAnYm90aCc7XHJcblx0XHRpZnJhbS5zZXRBdHRyaWJ1dGUoJ3ByZWxvYWQnLCAnYXV0bycpO1xyXG5cdFx0aWZyYW0uc2V0QXR0cmlidXRlKCdhbGxvd2Z1bGxzY3JlZW4nLCAndHJ1ZScpOyAvLyBmb3IgbWVkaWEgcGxheWVycyAoZWc6IGVtYmVkZGVkIHlvdXR1YmUpXHJcblx0XHRuZXdDb250ZW50ID0gaWZyYW07XHJcblxyXG5cdFx0bGV0IGN2ID0gdGhpcy5jb250ZW50VmlldztcclxuXHRcdHdoaWxlIChjdi5sYXN0Q2hpbGQpIHsgY3YucmVtb3ZlQ2hpbGQoY3YubGFzdENoaWxkKSB9IC8vIHJlbW92ZSBleGlzdGluZyBjb250ZW50XHJcblx0XHRjdi5hcHBlbmRDaGlsZChuZXdDb250ZW50KTsgLy8gYWRkIHNlbGYgdG8gZG9tXHJcblxyXG5cdFx0Ly8gQXR0ZW1wdCB0byBwcmVzZXJ2ZSBhc3BlY3QgcmF0aW9cclxuXHRcdC8vIE11c3QgYmUgZG9uZSBhZnRlciBhcHBlbmRpbmcgdG8gdGhlIERPTSAoLmFwcGVuZENoaWxkKSBzbyAub2Zmc2V0V2lkdGggd29ya3NcclxuXHRcdGlmKGlmcmFtZV9zcmMucGF0aG5hbWUuZW5kc1dpdGgoJy5tcDQnKSkge1xyXG5cdFx0XHRsZXQgd2lkdGggPSBpZnJhbS5vZmZzZXRXaWR0aDtcclxuXHRcdFx0Ly8gYXNzdW1lIHZpZGVvcyBhcmUgMTY6OSBhc3BlY3QgcmF0aW9cclxuXHRcdFx0aWZyYW0uc3R5bGUuaGVpZ2h0ID0gKHdpZHRoICogKDkvMTYpKSArICdweCc7XHJcblx0XHR9XHJcblxyXG5cdFx0dGhpcy5yZXBsYWNlZENvbnRlbnQgPSB0cnVlO1xyXG5cdFx0cmV0dXJuIGlmcmFtO1xyXG5cdH1cclxuXHRwcm90ZWN0ZWQgdGl0bGVCdG4odGV4dDogc3RyaW5nLCBvbmNsaWNrOiAodGhpczogSFRNTEJ1dHRvbkVsZW1lbnQsIGV2OiBNb3VzZUV2ZW50KSA9PiBhbnkpOiBIVE1MQnV0dG9uRWxlbWVudCB7XHJcblx0XHRsZXQgYnRuID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnYnV0dG9uJyk7XHJcblx0XHRidG4uaW5uZXJUZXh0ID0gdGV4dDtcclxuXHRcdGJ0bi5jbGFzc0xpc3QuYWRkKCdkMmwtYnV0dG9uJyk7XHJcblx0XHRidG4uc3R5bGUubWFyZ2luTGVmdCA9ICcwLjI1cmVtJzsgLy8gNXB4IG1hdGNoZXMgLmQybC1jb250ZXh0bWVudS1waFxyXG5cdFx0YnRuLnN0eWxlLm1hcmdpblJpZ2h0ID0gJzAuMjVyZW0nO1xyXG5cdFx0YnRuLnN0eWxlLndpZHRoID0gJ2F1dG8nO1xyXG5cdFx0YnRuLmFkZEV2ZW50TGlzdGVuZXIoJ2NsaWNrJywgb25jbGljayk7XHJcblx0XHRyZXR1cm4gYnRuO1xyXG5cdH1cclxuXHRwcm90ZWN0ZWQgdGl0bGVMaW5rKHRleHQ6IHN0cmluZywgaHJlZjogVVJMLCBwcmVwZW5kTm9kZT86IEhUTUxFbGVtZW50KTogSFRNTEFuY2hvckVsZW1lbnQge1xyXG5cdFx0bGV0IGxpbmsgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCdhJyk7XHJcblx0XHRsaW5rLmlubmVyVGV4dCA9IHRleHQ7XHJcblx0XHRpZihwcmVwZW5kTm9kZSkgbGluay5wcmVwZW5kKHByZXBlbmROb2RlKTtcclxuXHRcdGxpbmsuaHJlZiA9IGhyZWYudG9TdHJpbmcoKTtcclxuXHRcdGxpbmsuY2xhc3NMaXN0LmFkZCgnZDJsLWJ1dHRvbicpO1xyXG5cdFx0bGluay5zdHlsZS5tYXJnaW5MZWZ0ID0gJzAuMjVyZW0nO1xyXG5cdFx0bGluay5zdHlsZS5tYXJnaW5SaWdodCA9ICcwLjI1cmVtJztcclxuXHRcdGxpbmsuc3R5bGUud2lkdGggPSAnYXV0byc7XHJcblx0XHRyZXR1cm4gbGluaztcclxuXHR9XHJcblxyXG5cdC8qKiBBZGRzIGEgYnV0dG9uIHRvIHVzZSBhIG5hdGl2ZSB2aWV3ZXIgZm9yIHRoZSBwcm92aWRlZCBVUkwuIEF1dG9tYXRpY2FsbHkgcmVtb3ZlcyBpdHNlbGYgZnJvbSB0aGUgRE9NIHdoZW4gY2xpY2tlZC4gKi9cclxuXHRwcm90ZWN0ZWQgdGl0bGVCdG5fcmVwbGFjZUNvbnRlbnQobGFiZWw6IHN0cmluZywgc3JjOiBVUkwpOiBIVE1MQnV0dG9uRWxlbWVudCB7XHJcblx0XHRjb25zdCB0aGF0ID0gdGhpcztcclxuXHRcdGZ1bmN0aW9uIGJ0bm9uY2xpY2sodGhpczogSFRNTEJ1dHRvbkVsZW1lbnQpIHtcclxuXHRcdFx0Ly8gaW5zdGFsbGVkIGFzIG9uY2xpY2sgaGFuZGxlciA9PiB0aGlzID0gYDxidXR0b24+Li4uPC9idXR0b24+YFxyXG5cdFx0XHQvLyByZW1vdmUgb3Vyc2VsdmVzIChidXR0b24pIHNpbmNlIHdlIGhhdmUgc2VydmVkIG91ciBwdXJwb3NlXHJcblx0XHRcdHRoaXMucmVtb3ZlKCk7XHJcblx0XHRcdHRoYXQucmVwbGFjZUNvbnRlbnQoc3JjKTtcclxuXHRcdH07XHJcblxyXG5cdFx0Ly8gTm90ZTogcHV0IGJ1dHRvbiBpbiBpbW1lZGlhdGx5LCBwcm92aWRlIGxpbmsgYWZ0ZXIgUERGIHdhcyBkb3dubG9hZGVkXHJcblx0XHRyZXR1cm4gdGhpcy50aXRsZUJ0bihsYWJlbCwgYnRub25jbGljayk7XHJcblx0fVxyXG59XHJcbiIsIlxyXG5hc3luYyBmdW5jdGlvbiBpbmplY3RWaWRlb0ludG9Ub3BpY0hlYWRpbmcobGluazogSFRNTEFuY2hvckVsZW1lbnQpIHtcclxuICAgIHRyeSB7XHJcbiAgICAgICAgY29uc3QgZG9tcCA9IG5ldyBET01QYXJzZXIoKTtcclxuICAgICAgICBcclxuICAgICAgICBjb25zdCB2aWV3ZXJQYWdlICA9IGF3YWl0IGZldGNoKGxpbmsuaHJlZikudGhlbihyID0+IHIudGV4dCgpKTtcclxuICAgICAgICBjb25zdCB2aWV3ZXJEb2MgICA9IGRvbXAucGFyc2VGcm9tU3RyaW5nKHZpZXdlclBhZ2UsICd0ZXh0L2h0bWwnKTtcclxuICAgICAgICBjb25zdCB2aWV3ZXJGcmFtZSA9IHZpZXdlckRvYy5xdWVyeVNlbGVjdG9yKCcuZDJsLWZpbGV2aWV3ZXIgaWZyYW1lJykgYXMgSFRNTElGcmFtZUVsZW1lbnQ7XHJcbiAgICAgICAgaWYoIXZpZXdlckZyYW1lKSB7IGNvbnNvbGUuZXJyb3IoJ25vIGZpbGV2aWV3ZXIgZnJhbWUgZm9yJywgbGluay5pbm5lclRleHQsIHZpZXdlckRvYyk7IHJldHVybiB9O1xyXG5cclxuICAgICAgICAvLyBmZXRjaCAvY29udGVudC9lbmZvcmNlZC8kY291cnNlSWQtJGNvdXJzZU5hbWUvJGNoYXB0ZXJOYW1lLyR0b3BpY05hbWUuaHRtbD9vdT0kY291cnNlSWQmZDJsX2JvZHlfdHlwZT0zXHJcbiAgICAgICAgY29uc3QgdG9waWNQYWdlID0gYXdhaXQgZmV0Y2godmlld2VyRnJhbWUuc3JjKS50aGVuKHIgPT4gci50ZXh0KCkpO1xyXG4gICAgICAgIGNvbnN0IHRvcGljRG9jID0gZG9tcC5wYXJzZUZyb21TdHJpbmcodG9waWNQYWdlLCAndGV4dC9odG1sJyk7XHJcbiAgICAgICAgY29uc3QgdG9waWNGcmFtZSA9IHRvcGljRG9jLnF1ZXJ5U2VsZWN0b3IoJyNjb250YWluZXIgaWZyYW1lJykgYXMgSFRNTElGcmFtZUVsZW1lbnQ7XHJcbiAgICAgICAgaWYoIXRvcGljRnJhbWUpIHsgY29uc29sZS5lcnJvcignbm8gY29udGFpbmVyIGZyYW1lIGZvcicsIGxpbmsuaW5uZXJUZXh0LCB0b3BpY0RvYyk7IHJldHVybiB9O1xyXG5cclxuICAgICAgICAvLyBmZXRjaCAvZDJsL2NvbW1vbi9kaWFsb2dzL3F1aWNrTGluay9xdWlja0xpbmsuZDJsP291PSRjb3Vyc2VJZCZ0eXBlPWx0aSZyY29kZT0kcmVzb3VyY2VDb2RlJnNyY291PTFcclxuICAgICAgICBjb25zdCBlbWJlZFBhZ2UgPSBhd2FpdCBmZXRjaCh0b3BpY0ZyYW1lLnNyYykudGhlbihyID0+IHIudGV4dCgpKTtcclxuICAgICAgICBjb25zdCBlbWJlZERvYyA9IGRvbXAucGFyc2VGcm9tU3RyaW5nKGVtYmVkUGFnZSwgJ3RleHQvaHRtbCcpO1xyXG4gICAgICAgIC8vIGNvbnNvbGUubG9nKGVtYmVkRG9jKTtcclxuICAgICAgICBjb25zdCBlbWJlZEZyYW1lID0gZW1iZWREb2MucXVlcnlTZWxlY3RvcignZDJsLWx0aS1sYXVuY2gnKTsgLy8gaWZyYW1lIGN1c3RvbSBlbGVtZW50XHJcbiAgICAgICAgaWYoIWVtYmVkRnJhbWUpIHsgY29uc29sZS5lcnJvcignbm8gbHRpLWxhdW5jaCBmcmFtZSBmb3InLCBsaW5rLmlubmVyVGV4dCwgZW1iZWREb2MpOyByZXR1cm4gfTtcclxuXHJcbiAgICAgICAgLy8gdGhlIG1haW4gY29udGVudCBwYWdlIGRvZXNuJ3Qgc2VlbSB0byBoYXZlIHRoZSBjb2RlIGZvciB0aGUgZDJsLWx0aS1sYXVuY2ggZWxlbWVudFxyXG4gICAgICAgIC8vIHNvIGxldHMganVzdCBtYXAgdGhlIHVybCBpbnRvIGFuIGlmcmFtZSBhbmQgc2VlIGhvdyBpdCBkb2VzXHJcblxyXG4gICAgICAgIGxldCBlbWJlZExpbmsgPSBlbWJlZEZyYW1lLmF0dHJpYnV0ZXMuZ2V0TmFtZWRJdGVtKCdsdGktbGF1bmNoLXVybCcpPy52YWx1ZTtcclxuICAgICAgICBpZighZW1iZWRMaW5rKSB7IGNvbnNvbGUuZXJyb3IoJ25vIGx0aS1sYXVuY2ggdXJsIGZvcicsIGxpbmsuaW5uZXJUZXh0LCBlbWJlZERvYyk7IHJldHVybiB9O1xyXG5cclxuICAgICAgICBjb25zdCB2aWRlb0ZyYW1lID0gZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgnaWZyYW1lJyk7XHJcbiAgICAgICAgdmlkZW9GcmFtZS5zcmMgPSBlbWJlZExpbms7XHJcbiAgICAgICAgdmlkZW9GcmFtZS5zZXRBdHRyaWJ1dGUoJ2FsbG93ZnVsbHNjcmVlbicsICd0cnVlJyk7XHJcbiAgICAgICAgdmlkZW9GcmFtZS5zdHlsZS5zZXRQcm9wZXJ0eSgnd2lkdGgnLCAnMTAwJScpO1xyXG4gICAgICAgIHZpZGVvRnJhbWUuc3R5bGUuc2V0UHJvcGVydHkoJ2hlaWdodCcsICdhdXRvJyk7XHJcbiAgICAgICAgdmlkZW9GcmFtZS5zdHlsZS5zZXRQcm9wZXJ0eSgnYXNwZWN0LXJhdGlvJywgJzE2IC8gOScpO1xyXG5cclxuICAgICAgICBjb25zdCBpdGVtQ29udGVudCA9IGxpbmsuY2xvc2VzdCgnLmQybC1kYXRhbGlzdC1pdGVtLWNvbnRlbnQnKTtcclxuICAgICAgICBpZighaXRlbUNvbnRlbnQpIHsgY29uc29sZS5lcnJvcignbm8gaXRlbWNvbnRlbnQgZWxlbWVudCBmb3InLCBsaW5rLmlubmVyVGV4dCk7IHJldHVybiB9O1xyXG5cclxuICAgICAgICBpdGVtQ29udGVudC5hZnRlcih2aWRlb0ZyYW1lKTtcclxuICAgIH0gY2F0Y2goZSkge1xyXG4gICAgICAgIGNvbnNvbGUuZXJyb3IoJ2Vycm9yIGZvciAnLCBsaW5rLmlubmVyVGV4dCwgZSk7XHJcbiAgICB9XHJcbn0iLCJcclxuY29uc3QgZ2VuZXJhbF9jc3MgPSBgXHJcblx0LyogZWxpbWluYXRlIHNvbWUgd2hpdGVzcGFjZSAqL1xyXG5cdC8qIHJlZHVjZV90b2NfcGFkZGluZyAqL1xyXG5cdFx0aHRtbCAuZGF5bGlnaHQgI0QyTF9MRV9Db250ZW50X1RyZWVCcm93c2VyIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0sXHJcblx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudFBsdWdpblRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbSB7XHJcblx0XHRcdHBhZGRpbmc6IDVweCAwIDVweCAwO1xyXG5cdFx0fVxyXG5cclxuXHQvKiBzZWN0aW9uIGhlYWRpbmcgKi9cclxuXHRcdGh0bWwgLmRheWxpZ2h0IC5kMmwtaGVhZGVyLXRvcCAuZDJsLWJveC1oIHtcclxuXHRcdFx0bWFyZ2luLXRvcDogMXJlbTtcclxuXHRcdFx0bWFyZ2luLWJvdHRvbTogMXJlbTtcclxuXHRcdH1cclxuXHRcdGh0bWwgLmQybC1wYWdlLWhlYWRlciB7XHJcblx0XHRcdG1hcmdpbi1ib3R0b206IDAuNXJlbTtcclxuXHRcdH1cclxuXHRcdGh0bWwgLmQybC10eXBvZ3JhcGh5IC5kMmwtaHRtbGJsb2NrICBoMiB7XHJcblx0XHRcdG1hcmdpbjogMC41cmVtIDA7XHJcblx0XHR9XHJcblxyXG5cdC8qIHNlY3Rpb24gY29udGVudHMgKi9cclxuXHRcdGh0bWwgLmQybC1kYXRhbGlzdC1zdHlsZTEgPiAuZDJsLWRhdGFsaXN0IC5kMmwtZGF0YWxpc3QtaXRlbS1jb250ZW50IHtcclxuXHRcdFx0cGFkZGluZy10b3A6IDAuMjVyZW07XHJcblx0XHRcdHBhZGRpbmctYm90dG9tOiAwLjI1cmVtOyBcclxuXHRcdH1cclxuXHRcdGh0bWwgLmQybC10eXBvZ3JhcGh5IC5kMmwtZGF0YWxpc3QtaXRlbS1jb250ZW50IC5kMmwtaHRtbGJsb2NrIHAge1xyXG5cdFx0XHRtYXJnaW46IDAuNWVtIDA7XHJcblx0XHR9XHJcblxyXG5cdC8qIGRpc2N1c3Npb25zICovXHJcblx0XHRodG1sICNGb3J1bXNUb3BpY3NQbGFjZWhvbGRlciAuZDJsLXNlcCB7XHJcblx0XHRcdGRpc3BsYXk6IG5vbmU7XHJcblx0XHR9XHJcblx0XHRodG1sIC5kMmwtZm9ydW0tZGV0YWlscyB7XHJcblx0XHRcdHBhZGRpbmctdG9wOiAwO1xyXG5cdFx0fVxyXG5cdGh0bWwgLmQybC1wYWdlLWhlYWRlciB7XHJcblx0XHRtYXJnaW4tYm90dG9tOiAwO1xyXG5cdH1cclxuYDtcclxuXHJcbi8vIHRoaXMgaXMganVzdCBhIGZ1bmN0aW9uIHNvIGl0J3MgZGVjbGFyZWQgYmVmb3JlIHRoZSBtYWluIGNvZGUgaXMgcmFuXHJcbi8vIG90aGVyd2lzZSB2YXIvbGV0L2NvbnN0IHdvdWxkIGVycm9yIG9uIGRlY2xhcmF0aW9uIG9yZGVyXHJcbmZ1bmN0aW9uIGdldENzc3R3ZWFrcygpIHtcclxuXHRyZXR1cm4ge1xyXG5cdFx0Y29udGVudDogZnVuY3Rpb24oKSB7XHJcblxyXG5cdFx0fSxcclxuXHRcdGNvbnRlbnRfdG9jOiBmdW5jdGlvbigpIHtcclxuXHRcdFx0Ly8gaW5jbHVkZSBgaHRtbGAgdG8gYmVhdCB0aGUgc3BlY2lmaWNpdHkgb2YgdGhlIGJ1aWx0LWluIHJ1bGVzXHJcblx0XHRcdC8vIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0NTUy9TcGVjaWZpY2l0eVxyXG5cclxuXHRcdFx0Y29uc3Qgc3RyID0gYFxyXG5cdFx0XHRcdC8qIE1ha2UgdGhlIFRvQyBwb2ludGVyIG1vcmUgdmlzaWJpbGUsIHJlbW92ZSBhbm5veWluZ2x5IHN0eWxlZCBncmFkaWVudCAqL1xyXG5cdFx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOjpiZWZvcmUgLFxyXG5cdFx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOmxpbms6OmJlZm9yZSB7XHJcblx0XHRcdFx0XHRcdHZpc2liaWxpdHk6IGhpZGRlbjtcclxuXHRcdFx0XHRcdH1cclxuXHRcdFx0XHRcdGh0bWwgLmRheWxpZ2h0ICNDb250ZW50TW9kdWxlVHJlZSAuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1TZWxlY3RlZCA+IGEuZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLWFuY2hvcjo6YWZ0ZXIsXHJcblx0XHRcdFx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudE1vZHVsZVRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tU2VsZWN0ZWQgPiBhLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1hbmNob3I6bGluazo6YWZ0ZXIsXHJcblx0XHRcdFx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudE1vZHVsZVRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tU2VsZWN0ZWQgPiBhLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1hbmNob3I6dmlzaXRlZDo6YWZ0ZXIsXHJcblx0XHRcdFx0XHRodG1sIC5kYXlsaWdodCAjQ29udGVudE1vZHVsZVRyZWUgLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tU2VsZWN0ZWQgPiBhLmQybC1sZS1UcmVlQWNjb3JkaW9uSXRlbS1hbmNob3I6aG92ZXI6OmFmdGVyLFxyXG5cdFx0XHRcdFx0aHRtbCAuZGF5bGlnaHQgI0NvbnRlbnRNb2R1bGVUcmVlIC5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0uZDJsLWxlLVRyZWVBY2NvcmRpb25JdGVtLVNlbGVjdGVkID4gYS5kMmwtbGUtVHJlZUFjY29yZGlvbkl0ZW0tYW5jaG9yOmZvY3VzOjphZnRlciB7XHJcblx0XHRcdFx0XHRcdGZpbHRlcjogaW52ZXJ0KDUwJSk7IC8qIGJhc2ljYWxseSBtYWtlIGl0IGdyYXkgKi9cclxuXHRcdFx0XHRcdH1cclxuXHJcblx0XHRcclxuXHRcdFx0XHRcclxuXHRcdFx0YDtcclxuXHJcblx0XHRcdGxldCBzdHlsZSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ3N0eWxlJyk7XHJcblx0XHRcdHN0eWxlLnNldEF0dHJpYnV0ZSgndHlwZScsICd0ZXh0L2NzcycpO1xyXG5cdFx0XHRzdHlsZS5pbm5lckhUTUwgPSBzdHI7XHJcblx0XHRcdGRvY3VtZW50LmhlYWQuYXBwZW5kQ2hpbGQoc3R5bGUpO1xyXG5cdFx0XHRjb25zb2xlLmxvZyhcIkluc3RhbGxpbmcgY3NzIHR3ZWFrcyB0bzogXCIsIHN0eWxlKTtcclxuXHRcdH0sXHJcblx0fTtcclxufTtcclxuXHJcbi8qKiBUcmF2ZXJzZXMgdGhlIERPTSBmaW5kaW5nIGVhY2ggcGFyZW50IGVsZW1lbnQgd2l0aCBgZGlzcGxheTogbm9uZWAgYW5kIHRvZ2dsZXMgaXQgb2ZmIGZvciB0aGUgZHVyYXRpb24gb2YgdGhlIGNhbGxiYWNrLiAqL1xyXG5mdW5jdGlvbiBhc1Zpc2libGU8VD4odGFyZ2V0OiBIVE1MRWxlbWVudCwgZjogKCkgPT4gVCk6IFQge1xyXG5cdGxldCBpc1Zpc2libGUgPSAodGFyZ2V0OiBIVE1MRWxlbWVudCkgPT4gdGFyZ2V0Lm9mZnNldFBhcmVudCAhPT0gbnVsbDtcclxuXHJcblx0aWYoaXNWaXNpYmxlKHRhcmdldCkpIHtcclxuXHRcdC8vIElmIHdlJ3JlIHZpc2libGUsIHdlJ3JlIGRvbmUgLSBjYWxsIHRoZSBmdW5jdGlvblxyXG5cdFx0cmV0dXJuIGYoKTtcclxuXHR9IGVsc2Uge1xyXG5cdFx0aWYoZ2V0Q29tcHV0ZWRTdHlsZSh0YXJnZXQpLmdldFByb3BlcnR5VmFsdWUoJ2Rpc3BsYXknKSA9PT0gJ25vbmUnKSB7XHJcblx0XHRcdGxldCBbb2xkViwgb2xkUF0gPSBbdGFyZ2V0LnN0eWxlLmdldFByb3BlcnR5VmFsdWUoJ2Rpc3BsYXknKSwgdGFyZ2V0LnN0eWxlLmdldFByb3BlcnR5UHJpb3JpdHkoJ2Rpc3BsYXknKV07XHJcblx0XHRcdHRhcmdldC5zdHlsZS5zZXRQcm9wZXJ0eSgnZGlzcGxheScsICdpbmhlcml0JywgJyFpbXBvcnRhbnQnKTsgLy8gYmV0dGVyIHZhbHVlP1xyXG5cclxuXHRcdFx0Ly8gdHJ5IGFnYWluIC0gbWF5YmUgd2UncmUgdmlzaWJsZSBub3c/XHJcblx0XHRcdGxldCBydG4gPSBhc1Zpc2libGUodGFyZ2V0LCBmKTtcclxuXHJcblx0XHRcdHRhcmdldC5zdHlsZS5zZXRQcm9wZXJ0eSgnZGlzcGxheScsIG9sZFYsIG9sZFApO1xyXG5cclxuXHRcdFx0cmV0dXJuIHJ0bjtcclxuXHRcdH0gZWxzZSB7XHJcblx0XHRcdC8vIHdlIGFyZW4ndCByZXNwb25zaWJsZSBmb3Igb3VyIGludmlzaWJpbGl0eSAtIGlzIG91ciBwYXJlbnQ/XHJcblx0XHRcdHJldHVybiBhc1Zpc2libGUodGFyZ2V0LnBhcmVudEVsZW1lbnQhLCBmKTtcclxuXHRcdH1cclxuXHR9XHJcbn1cclxuIiwiXHJcbmZ1bmN0aW9uIGluaXRDYXBhYmlsaXRpZXMoKTogQ2FwYWJpbGl0aWVzIHtcclxuXHQvLyBJIHJlYWxpemUgSSBzaG91bGRuJ3QgYmUgdXNpbmcgdGhlIHVzZXJBZ2VudCB0byBkZXRlY3QgYnJvd3NlciBmbGF2b3JzLlxyXG5cdC8vIEl0J2xsIGJlIGdvb2QgZW5vdWdoIGZvciB0aGlzLlxyXG5cdGxldCB1c2VyYWdlbnQgPSBuYXZpZ2F0b3IudXNlckFnZW50LnRvTG93ZXJDYXNlKCk7XHJcblxyXG5cdC8vIEB0cy1pZ25vcmVcclxuXHRpZih1c2VyYWdlbnQuaW5jbHVkZXMoJ2Nocm9tZScpKSB7XHJcblx0XHRjb25zdCBtdCA9IChhcHA6IHN0cmluZyk6IGJvb2xlYW4gPT4gbmF2aWdhdG9yLm1pbWVUeXBlcy5uYW1lZEl0ZW0oJ2FwcGxpY2F0aW9uLycgKyBhcHApICE9PSBudWxsO1xyXG5cdFx0cmV0dXJuIHtcclxuXHRcdFx0ZXh0ZW5zaW9uczoge1xyXG5cdFx0XHRcdC8vIGRldGVjdCBpZiBjaHJvbWUgc3VwcG9ydHMgdmlld2luZyBNUyBvZmZpY2UgZG9jdW1lbnRzICh2aWEgZXh0ZW5zaW9uL2V0YylcclxuXHRcdFx0XHRvZmZpY2Vfdmlld2VyOiBtdCgnbXN3b3JkJykgJiYgbXQoJ21zZXhjZWwnKSAmJiBtdCgnbXNwb3dlcnBvaW50JykgJiYgbXQoJ21zd29yZC10ZW1wbGF0ZScpLFxyXG5cdFx0XHRcdC8vb2ZmaWNlX3ZpZXdlcjogY2hyb21lRXh0ZW5zaW9uSW5zdGFsbGVkKCdnbXBsamRsZ2Nka2xqbHBwYWVrY2lhY2RtZGxoZmVvbicpLFxyXG5cdFx0XHRcdHZpZGVvX3Nob3J0Y3V0czogY2hyb21lRXh0ZW5zaW9uSW5zdGFsbGVkKCdsbGhtYWNpZ2duaWJuYmRva2lkbWJpbGtsY2Vhb2JhZScpLFxyXG5cdFx0XHR9LFxyXG5cdFx0XHRicm93c2VyOiBcImNocm9tZVwiLFxyXG5cdFx0fVxyXG5cdH0gZWxzZSBpZih1c2VyYWdlbnQuaW5jbHVkZXMoJ2ZpcmVmb3gnKSkge1xyXG5cdFx0cmV0dXJuIHtcclxuXHRcdFx0Ly8gbm8gd2F5IHRvIGtub3dcclxuXHRcdFx0ZXh0ZW5zaW9uczoge1xyXG5cdFx0XHRcdG9mZmljZV92aWV3ZXI6IGZhbHNlLFxyXG5cdFx0XHRcdHZpZGVvX3Nob3J0Y3V0czogdHJ1ZSwgLy8gRkYgaGFzIGJ1aWx0IGluIHNwZWVkIGNvbnRyb2xzIC0gd2hpY2ggSSBjYXJlIGFib3V0XHJcblx0XHRcdH0sXHJcblx0XHRcdGJyb3dzZXI6IFwiZmlyZWZveFwiLFxyXG5cdFx0fVxyXG5cdH1cclxuXHRyZXR1cm4ge1xyXG5cdFx0ZXh0ZW5zaW9uczogeyBvZmZpY2Vfdmlld2VyOiBmYWxzZSwgdmlkZW9fc2hvcnRjdXRzOiBmYWxzZSwgfSxcclxuXHRcdGJyb3dzZXI6IFwidW5rbm93blwiLFxyXG5cdH1cclxufVxyXG5cclxuZnVuY3Rpb24gc3VnZ2VzdEhUTUw1VmlkZW9LZXlib2FyZFNob3J0Y3V0cygpIHtcclxuXHQvLyBPbmx5IHN1Z2dlc3Qgb24gY2hyb21lLCBiL2MgZmlyZWZveCB1c2VycyBleGlzdFxyXG5cdC8vIChhbmQgZmlyZWZveCBhbHJlYWR5IGhhcyByaWdodC1jbGljaywgY2hhbmdlIHNwZWVkIGNvbnRyb2xzKVxyXG5cdGlmKCFjaHJvbWVFeHRlbnNpb25JbnN0YWxsZWQoJ2xsaG1hY2lnZ25pYm5iZG9raWRtYmlsa2xjZWFvYmFlJykpIHtcclxuXHRcdGNvbnNvbGUud2FybihgRDJMIHVzZXJzY3JpcHQgcmVjb21tZW5kcyBpbnN0YWxsaW5nIHRoZSBcIkhUTUw1IFZpZGVvIEtleWJvYXJkIFNob3J0Y3V0c1wiIGV4dGVuc2lvbiBmb3Igc3BlZWRpbmcgdXAgdmlkZW9zYCwgYGh0dHBzOi8vY2hyb21lLmdvb2dsZS5jb20vd2Vic3RvcmUvZGV0YWlsL2xsaG1hY2lnZ25pYm5iZG9raWRtYmlsa2xjZWFvYmFlYCk7XHJcblx0fVxyXG59XHJcbmZ1bmN0aW9uIHN1Z2dlc3RPZmZpY2VFZGl0aW5nKCkge1xyXG5cdC8vIE9ubHkgc3VnZ2VzdCBvbiBjaHJvbWUsIGIvYyBmaXJlZm94IHVzZXJzIGV4aXN0XHJcblx0aWYoIWNocm9tZUV4dGVuc2lvbkluc3RhbGxlZCgnZ2JrZWVnYmFpaWdtZW5mbWpmY2xjZGdkcGltYW1na2onKSkge1xyXG5cdFx0Y29uc29sZS53YXJuKGBEMkwgdXNlcnNjcmlwdCByZWNvbW1lbmRzIGluc3RhbGxpbmcgdGhlIFwiT2ZmaWNlIEVkaXRpbmcgZm9yIERvY3MsIFNoZWV0cyAmIFNsaWRlc1wiIGV4dGVuc2lvbiBmb3IgaW50ZXJhY3RpdmVseSB2aWV3aW5nIG9mZmljZSBkb2N1bWVudHNgLCBgaHR0cHM6Ly9jaHJvbWUuZ29vZ2xlLmNvbS93ZWJzdG9yZS9kZXRhaWwvZ2JrZWVnYmFpaWdtZW5mbWpmY2xjZGdkcGltYW1na2pgKTtcclxuXHR9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIGNocm9tZUV4dGVuc2lvbkluc3RhbGxlZChpZDogc3RyaW5nKSB7XHJcblx0dHJ5IHtcclxuXHRcdC8vIEB0cy1pZ25vcmVcclxuXHRcdGlmKHdpbmRvdy5jaHJvbWUpIHsgd2luZG93LmNocm9tZS5ydW50aW1lLnNlbmRNZXNzYWdlKGlkLCBudWxsKTsgcmV0dXJuIHRydWU7IH1cclxuXHRcdFxyXG5cdH0gY2F0Y2ggKGU6IGFueSkge1xyXG5cdFx0Ly8gZmFpbHMgdHJpdmlhbGx5IG9uIG90aGVyIGJyb3dzZXJzLlxyXG5cdFx0Ly8ganVzdCByZXR1cm4gZmFsc2Ugb24gZXJyb3I6IGZhaWwtc2FmZVxyXG5cdFx0Ly8gaWYgKCFlLm1lc3NhZ2UuaW5jbHVkZXMoXCJJbnZhbGlkIGV4dGVuc2lvbiBpZFwiKSkgeyB0aHJvdyBlOyB9XHJcblx0fVxyXG5cdHJldHVybiBmYWxzZTtcclxufVxyXG5cclxuLyoqIFVzZWZ1bCBmb3IgZWFybHkgZXhpdCBpZiB3ZSBhcmVuJ3QgdGhlIHRvcCBmcmFtZSAqL1xyXG5mdW5jdGlvbiB3aXRoaW5JZnJhbWUoKSB7XHJcblx0Ly8gaHR0cHM6Ly9zdGFja292ZXJmbG93LmNvbS9hLzMyNjA3Ni8xMTUzNjYxNFxyXG5cdHRyeSB7XHJcblx0XHRyZXR1cm4gd2luZG93LnNlbGYgIT09IHdpbmRvdy50b3A7XHJcblx0fSBjYXRjaCAoZSkge1xyXG5cdFx0cmV0dXJuIHRydWU7IC8vIGFjY2VzcyBkZW5pZWQgZXJyb3JcclxuXHR9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIGQybF9pY29uKGJhY2tncm91bmRJbWFnZTogc3RyaW5nKTogSFRNTFNwYW5FbGVtZW50IHtcclxuXHRsZXQgaSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ3NwYW4nKTtcclxuXHRpLmNsYXNzTGlzdC5hZGQoJ2QybC1pY29uLWN1c3RvbScpO1xyXG5cdGkuc3R5bGUuYmFja2dyb3VuZEltYWdlID0gYmFja2dyb3VuZEltYWdlO1xyXG5cdGkuc3R5bGUuYmFja2dyb3VuZFBvc2l0aW9uID0gXCIwIDBcIjtcclxuXHRpLnN0eWxlLmJhY2tncm91bmRSZXBlYXQgPSBcIm5vLXJlcGVhdFwiO1xyXG5cdGkuc3R5bGUud2lkdGggPSBcIjE4cHhcIjtcclxuXHRpLnN0eWxlLmhlaWdodCA9IFwiMThweFwiO1xyXG5cdGkuc3R5bGUuYmFja2dyb3VuZFNpemUgPSBcIjE4cHggMThweFwiO1xyXG5cdHJldHVybiBpO1xyXG59XHJcblxyXG5mdW5jdGlvbiBkMmxfaWNvbl9kb3dubG9hZCgpOiBIVE1MRWxlbWVudCB7XHJcblx0cmV0dXJuIGQybF9pY29uKFwidXJsKCdodHRwczovL3MuYnJpZ2h0c3BhY2UuY29tL2xpYi9ic2kvMjAuMjAuOC04NS9pbWFnZXMvdGllcjEvZG93bmxvYWQuc3ZnJylcIik7XHJcbn1cclxuXHJcbi8qKiBUaGUgVVJMIGludGVyZmFjZSByZXByZXNlbnRzIGFuIG9iamVjdCBwcm92aWRpbmcgc3RhdGljIG1ldGhvZHMgdXNlZCBmb3IgY3JlYXRpbmcgb2JqZWN0IFVSTHMuICovXHJcbmZ1bmN0aW9uIG5ld1VSTCh1cmw6IHN0cmluZyB8IFVSTCB8IExvY2F0aW9uKTogVVJMIHtcclxuXHRpZih0eXBlb2YgdXJsID09ICdzdHJpbmcnKVxyXG5cdFx0cmV0dXJuIG5ldyBVUkwodXJsLCBkb2N1bWVudC5sb2NhdGlvbi5ocmVmKTtcclxuXHRlbHNlIGlmKHVybCBpbnN0YW5jZW9mIFVSTCkge1xyXG5cdFx0cmV0dXJuIG5ldyBVUkwodXJsLmhyZWYpO1xyXG5cdH0gZWxzZSBpZih1cmwgaW5zdGFuY2VvZiBMb2NhdGlvbikge1xyXG5cdFx0cmV0dXJuIG5ldyBVUkwodXJsLmhyZWYpO1xyXG5cdH0gZWxzZSB7XHJcblx0XHR0aHJvdyBuZXcgRXJyb3IoJ3Vua25vd24gcGFyYW1ldGVyIHR5cGUnKTtcclxuXHR9XHJcbn1cclxuXHJcbi8qKiBSZXR1cm4gdGhlIHR5cGUgb2YgRDJMIHBhZ2Ugd2UgYXJlIG9uLCB3aXRoIHNvbWUgYXBwcm9wcmlhdGUgbWV0YWRhdGEgYWJvdXQgaXQgKi9cclxuZnVuY3Rpb24gZ2V0UGFnZVR5cGUoKTogUGFnZVR5cGUge1xyXG5cdGxldCBocmVmID0gbmV3VVJMKGRvY3VtZW50LmxvY2F0aW9uKTtcclxuXHQvLyBzbGljZSB0byByZW1vdmUgemVyby1sZW5ndGggY29tcG9uZW50IGF0IGJlZ2lubmluZyAoc2luY2UgcGF0aG5hbWVzIHN0YXJ0IHdpdGggJy8nKVxyXG5cdGxldCBjb21wb25lbnRzID0gaHJlZi5wYXRobmFtZS5zcGxpdCgnLycpLnNsaWNlKDEpO1xyXG5cdGlmKFxyXG5cdFx0Y29tcG9uZW50cy5sZW5ndGggPT0gN1xyXG5cdFx0JiYgY29tcG9uZW50c1swXSA9PSBcImQybFwiXHJcblx0XHQmJiBjb21wb25lbnRzWzFdID09IFwibGVcIlxyXG5cdFx0JiYgY29tcG9uZW50c1syXSA9PSBcImNvbnRlbnRcIlxyXG5cdFx0Ly8gY2xhc3MgSURcclxuXHRcdCYmIGNvbXBvbmVudHNbNF0gPT0gXCJ2aWV3Q29udGVudFwiXHJcblx0XHQvLyBhc3NldCBJRFxyXG5cdFx0JiYgY29tcG9uZW50c1s2XSA9PSBcIlZpZXdcIlxyXG5cdCkge1xyXG5cdFx0cmV0dXJuIHsgdHlwZTogXCJjb250ZW50XCIsIGNsYXNzOiBjb21wb25lbnRzWzNdLCBhc3NldDogY29tcG9uZW50c1s1XSwgfTtcclxuXHR9IGVsc2UgaWYoXHJcblx0XHRjb21wb25lbnRzLmxlbmd0aCA9PSA1XHJcblx0XHQmJiBjb21wb25lbnRzWzBdID09IFwiZDJsXCJcclxuXHRcdCYmIGNvbXBvbmVudHNbMV0gPT0gXCJsZVwiXHJcblx0XHQmJiBjb21wb25lbnRzWzJdID09IFwiY29udGVudFwiXHJcblx0XHQvLyBjbGFzcyBJRFxyXG5cdFx0JiYgY29tcG9uZW50c1s0XSA9PSBcIkhvbWVcIlxyXG5cdCkge1xyXG5cdFx0cmV0dXJuIHsgdHlwZTogXCJjb250ZW50X3RvY1wiLCBjbGFzczogY29tcG9uZW50c1szXSB9O1xyXG5cdH1cclxuXHJcblx0cmV0dXJuIHsgdHlwZTogXCJ1bmtub3duXCIgfTtcclxufVxyXG4iLCJcclxuaW50ZXJmYWNlIENhcGFiaWxpdGllcyB7XHJcblx0ZXh0ZW5zaW9uczoge1xyXG5cdFx0b2ZmaWNlX3ZpZXdlcjogYm9vbGVhbixcclxuXHRcdHZpZGVvX3Nob3J0Y3V0czogYm9vbGVhbixcclxuXHR9LFxyXG5cdGJyb3dzZXI6IFwiY2hyb21lXCIgfCBcImZpcmVmb3hcIiB8IFwidW5rbm93blwiLFxyXG59O1xyXG5cclxuaW50ZXJmYWNlIEQyTEFzc2V0TWV0YWRhdGEge1xyXG5cdHR5cGU6IHN0cmluZyxcclxuXHRmaWxlbmFtZTogc3RyaW5nfG51bGwsXHJcblx0c2l6ZTogbnVtYmVyLFxyXG59XHJcblxyXG50eXBlIFBhZ2VUeXBlID0gUGFnZS5Vbmtub3duIHwgUGFnZS5Db250ZW50IHwgUGFnZS5Db250ZW50VG9jO1xyXG5uYW1lc3BhY2UgUGFnZSB7XHJcblx0ZXhwb3J0IGludGVyZmFjZSBVbmtub3duIHtcclxuXHRcdHR5cGU6IFwidW5rbm93blwiO1xyXG5cdH1cclxuXHRleHBvcnQgaW50ZXJmYWNlIENvbnRlbnQge1xyXG5cdFx0dHlwZTogXCJjb250ZW50XCI7XHJcblx0XHRjbGFzczogc3RyaW5nO1xyXG5cdFx0YXNzZXQ6IHN0cmluZztcclxuXHR9XHJcblx0ZXhwb3J0IGludGVyZmFjZSBDb250ZW50VG9jIHtcclxuXHRcdHR5cGU6IFwiY29udGVudF90b2NcIjtcclxuXHRcdGNsYXNzOiBzdHJpbmc7XHJcblx0fVxyXG59XHJcblxyXG4iXX0=