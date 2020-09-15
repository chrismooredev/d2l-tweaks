"use strict";
// ==UserScript==
// @name         Make D2L a bit better
// @namespace    https://gist.github.com/csm123199/4bd7605a47ca89d65699bbbaef474c38
// @version      0.5
// @description  Add QoL changes to D2L's user interface
// @author       https://github.com/csm123199
// @include      https://d2l.*.edu/d2l/le/content/*/viewContent/*/View
// @grant        none
// @updateUrl    https://gist.github.com/csm123199/4bd7605a47ca89d65699bbbaef474c38/raw/d2l-improver.user.js
// ==/UserScript==
// D2L rest api docs
// https://docs.valence.desire2learn.com/res/content.html
/// <reference path="./d2l-globals.d.ts" />
//import 'd2l-globals';
{
    /* Config */
    const MAKE_NATIVE_ON_LOAD = true;
    /* Code */
    let host = null;
    let url = new URL(document.location.toString());
    if (url.protocol == 'https:' && url.host.match(/^d2l.[a-zA-Z0-9_-]+.edu/)) {
        host = url.host;
    }
    else {
        throw new Error(`Bad host for D2L Script (exiting): '${url.host}'`);
    }
    function create_dl_icon() {
        let i = document.createElement('span');
        i.classList.add('d2l-icon-custom');
        i.style.backgroundImage = "url('https://s.brightspace.com/lib/bsi/20.20.8-85/images/tier1/download.svg')";
        i.style.backgroundPosition = "0 0";
        i.style.backgroundRepeat = "no-repeat";
        i.style.width = "18px";
        i.style.height = "18px";
        i.style.backgroundSize = "18px 18px";
        return i;
    }
    let CONTENT_TYPES;
    (function (CONTENT_TYPES) {
        CONTENT_TYPES["PDF"] = "pdf";
        CONTENT_TYPES["MP4"] = "mp4";
        CONTENT_TYPES["Word"] = "msword";
        CONTENT_TYPES["Excel"] = "msexcel";
        CONTENT_TYPES["WebPage"] = "webpage";
        CONTENT_TYPES["ExternalPage"] = "extpage";
        CONTENT_TYPES["Panopto"] = "panopto";
        CONTENT_TYPES["UNKNOWN"] = "unknown";
    })(CONTENT_TYPES || (CONTENT_TYPES = {}));
    ;
    const PAGE_TITLE_MAP = {
        'PDF document': CONTENT_TYPES.PDF,
        'Word Document': CONTENT_TYPES.Word,
        'Excel Spreadsheet': CONTENT_TYPES.Excel,
    };
    // Types able to be directly downloaded
    const DIRECT_FILES = [
        CONTENT_TYPES.PDF,
        CONTENT_TYPES.MP4,
        CONTENT_TYPES.Word,
        CONTENT_TYPES.Excel,
    ];
    // Types that can be shown in the native iframe (even if we have to do some URL mangling to turn it into a PDF or smth)
    const NATIVE_VIEW_COMPAT = [
        CONTENT_TYPES.PDF,
        CONTENT_TYPES.MP4,
        CONTENT_TYPES.ExternalPage,
        CONTENT_TYPES.WebPage,
        CONTENT_TYPES.Word,
        CONTENT_TYPES.Excel,
    ];
    function getContentType() {
        function isMP4(child) {
            return child.attributes.getNamedItem("data-mediaplayer-src-original") != undefined;
        }
        function isExternalPage() {
            let dests = Object.values(D2L.OR.__g1)
                .map(s => JSON.parse(s))
                .filter((ent) => {
                return ent._type == "func" && ent.N == "D2L.LE.Content.Desktop.Topic.OpenInNewWindow" && ent.P.length == 1;
            })
                .map(ent => ent.P[0]); // get only the destination URLs
            if (dests.length > 1) {
                console.warn("Not recognizing as external page because multiple URLs showed up as valid:", dests);
            }
            if (dests.length == 1 && typeof dests[0] == "string") {
                return dests[0];
            }
            else {
                return null;
            }
        }
        // Used if types can be narrowed down (eg: WebPage -> Panopto)
        let intermediary = CONTENT_TYPES.UNKNOWN;
        let page_title_view = Array.from(document.getElementsByClassName("d2l-page-title-view"));
        if (page_title_view.length > 1) {
            console.warn("Page title views: ", page_title_view);
            throw new Error("More than one elements of class d2l-page-title-view, unable to determine page types.");
        }
        else if (page_title_view.length == 1) {
            // null safety: elements with .d2l-page-title-view won't be the root document/doctype, so textContent won't be null
            let ptv = page_title_view[0].textContent.trim();
            if (ptv == 'Web Page') {
                intermediary = CONTENT_TYPES.WebPage;
            }
            else if (ptv in PAGE_TITLE_MAP) {
                return PAGE_TITLE_MAP[ptv];
            }
            else {
                console.warn(`Unknown page title view value: ${ptv}`);
            }
        }
        let content_view = document.getElementById('ContentView');
        if (content_view) {
            console.log(content_view); // Easy reference to #ContentView
            let insideContent = Array.from(content_view.children);
            if (insideContent.length == 0) {
                console.log(`Unknown page contents: 0 elements inside #ContentView`);
            }
            else if (insideContent.length == 1) {
                let child = insideContent[0];
                if (isMP4(child))
                    return CONTENT_TYPES.MP4;
                if (isExternalPage())
                    return CONTENT_TYPES.ExternalPage;
            }
            else {
                console.log(`Unknown page contents: 2+ elements inside #ContentView`);
            }
        }
        else { // web page??
            console.trace("Web page?");
        }
        return CONTENT_TYPES.UNKNOWN;
    }
    function provideTypeFunctionality(type) {
        // add button to use native iframes for certain types
        // add link to direct download
        let [cls, asset] = new URL(document.URL).pathname.split('/').filter(c => Number.isFinite(Number.parseInt(c)));
        let [url, promMeta] = urlOfD2LAsset(cls, asset);
        let handled = false;
        function getUrl(type, apiUrl) {
            if (type == CONTENT_TYPES.MP4) {
                let vidplayer = document.querySelectorAll('#ContentView .vui-mediaplayer')[0];
                let vidurl = vidplayer.getAttribute('data-mediaplayer-src');
                return [vidurl, url + '?stream=false'];
            }
            else if (type == CONTENT_TYPES.ExternalPage) {
                let url = Object.values(D2L.OR.__g1).map(s => JSON.parse(s)).filter(o => o.N == 'D2L.LE.Content.Desktop.Topic.OpenInNewWindow')[0].P[0];
                url = new URL(url);
                url.protocol = "https"; // otherwise iframe won't load b/c D2L is HTTPS
                return [url, null];
            }
            else if ([CONTENT_TYPES.Word, CONTENT_TYPES.Excel].includes(type)) {
                // D2L converts office documents to PDF to preview them inside D2L - use the native PDF viewer instead for interactive viewing
                let awsPDF = document.querySelector('.d2l-fileviewer-pdf-pdfjs')?.getAttribute('data-location');
                return [awsPDF, url + '?stream=false'];
            }
            else {
                return [
                    NATIVE_VIEW_COMPAT.includes(type) ? apiUrl + '?stream=true' : null,
                    DIRECT_FILES.includes(type) ? apiUrl + '?stream=false' : null,
                ];
            }
        }
        // interactive is meant for in-browser viewing
        // downloadable is meant for files that can be downloaded by navigating to them
        let [interactive, downloadable] = getUrl(type, url);
        if (interactive) {
            handled = true;
            btn_useNativeIframe(interactive, MAKE_NATIVE_ON_LOAD);
            addTitleLink("Direct Link", interactive);
        }
        if (downloadable) {
            handled = true;
            addTitleLink("Download", downloadable, create_dl_icon());
        }
        if (!handled) {
            console.warn("Unhandled D2L content type from Userscript: ", type);
            console.log("Content type asset url: ", url);
            return promMeta.then(meta => { console.log("Asset meta for url:", meta); });
        }
    }
    function btn_useNativeIframe(src, now) {
        //returns promise for native iframe
        function insertIframe() {
            // inject own content iframe
            let ifram = document.createElement('iframe');
            ifram.src = src;
            ifram.style.width = '100%';
            ifram.style.height = '90vh';
            ifram.setAttribute('preload', 'auto');
            replaceContent(ifram);
            return ifram;
        }
        if (now) {
            return Promise.resolve(insertIframe());
        }
        else {
            // return promise that is fulfilled with iframe
            return new Promise((res, rej) => {
                function btnonclick() {
                    // installed as onclick handler => this = `<button>...</button>`
                    // remove ourselves (button) since we have served our purpose
                    this.remove();
                    res(insertIframe());
                }
                ;
                // Note: put button in immediatly, provide link after PDF was downloaded
                addTitleBtn("Use Native Viewer", btnonclick);
            });
        }
    }
    // make content more than 600px high
    function makeContentLong() {
        let docViewer = Array.from(document.getElementsByClassName("d2l-documentViewer"));
        if (docViewer.length > 1) {
            console.warn("There are multiple .d2l-documentViewer elements! Page may be extra long...", docViewer);
        }
        docViewer.forEach(e => { e.style.height = '90vh'; });
        console.log("Set doc viewers' height to 90vh");
    }
    // helper functions
    function withinIframe() {
        // https://stackoverflow.com/a/326076/11536614
        try {
            return window.self !== window.top;
        }
        catch (e) {
            return true; // access denied error
        }
    }
    function getPageHeader() {
        let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h");
        if (!hdrBar)
            throw new Error("Unable to find content header bar!");
        return hdrBar;
    }
    function addTitleBtn(text, onclick) {
        let btn = document.createElement('button');
        btn.innerText = text;
        btn.classList.add('d2l-button');
        btn.style.marginRight = '.75rem';
        btn.style.width = 'auto';
        btn.addEventListener('click', onclick);
        getPageHeader().appendChild(btn);
        return btn;
    }
    function addTitleLink(text, href, prependNode) {
        let link = document.createElement('a');
        link.innerText = text;
        if (prependNode)
            link.prepend(prependNode);
        link.href = href;
        link.classList.add('d2l-button');
        link.style.marginRight = '.75rem';
        link.style.width = 'auto';
        getPageHeader().appendChild(link);
        return link;
    }
    function replaceContent(ele) {
        // get content view
        let cv = document.getElementById("ContentView");
        if (cv == undefined) {
            throw new Error("Unable to retrieve #ContentView");
        }
        // remove existing content
        while (cv.lastChild) {
            cv.removeChild(cv.lastChild);
        }
        cv.appendChild(ele);
    }
    async function D2LAssetMeta(cls, asset) {
        let f = await fetch(`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`, { method: 'HEAD' });
        // get mime and orig file name, if available
        let type = f.headers.get("content-type");
        let filename = f.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
        let size = Number.parseInt(f.headers.get("content-length"));
        return { type, filename, size };
    }
    // Returns [url: string, Promise<[content-type: string, orig-filename: string, size: number]>]
    function urlOfD2LAsset(cls, asset) {
        if (!Number.isFinite(Number.parseInt(cls))) {
            throw new Error(`D2L class ID isn't parsable to a number/ID: '${cls}'`);
        }
        if (!Number.isFinite(Number.parseInt(asset))) {
            throw new Error(`D2L asset ID isn't parsable to a number/ID: '${asset}'`);
        }
        // the ?stream=true tells D2L to use a response header for content to be viewed in the browser, rather than downloaded
        return [
            `https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`,
            D2LAssetMeta(cls, asset),
        ];
    }
    (async function () {
        'use strict';
        if (!withinIframe()) {
            try {
                // @ts-ignore
                chrome.runtime.sendMessage("llhmaciggnibnbdokidmbilklceaobae", null);
            }
            catch (e) {
                if (e.message.includes("Invalid extension id")) {
                    console.warn(`D2L userscript includes installing the "HTML5 Video Keyboard Shortcuts" extension for speeding up videos`, `https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae`);
                }
                else {
                    throw e;
                }
            }
            try {
                makeContentLong();
                let type = getContentType();
                console.log(`Content Type: ${getContentType()}`);
                await provideTypeFunctionality(type); // wait for it, to catch errors
            }
            catch (e) {
                console.error("Error occured in userscript", e);
                //alert("Error occured in D2L bettering userscript, error in console.");
            }
        }
    })();
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZDJsLXR3ZWFrcy51c2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic3JjL2QybC10d2Vha3MudXNlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsaUJBQWlCO0FBQ2pCLHNDQUFzQztBQUN0QyxtRkFBbUY7QUFDbkYsb0JBQW9CO0FBQ3BCLHdEQUF3RDtBQUN4RCw2Q0FBNkM7QUFDN0Msc0VBQXNFO0FBQ3RFLHFCQUFxQjtBQUNyQiw0R0FBNEc7QUFDNUcsa0JBQWtCO0FBRWxCLG9CQUFvQjtBQUNwQix5REFBeUQ7QUFFekQsMkNBQTJDO0FBRTNDLHVCQUF1QjtBQUV2QjtJQUVDLFlBQVk7SUFDWixNQUFNLG1CQUFtQixHQUFHLElBQUksQ0FBQztJQUVqQyxVQUFVO0lBQ1YsSUFBSSxJQUFJLEdBQWdCLElBQUksQ0FBQztJQUM3QixJQUFJLEdBQUcsR0FBRyxJQUFJLEdBQUcsQ0FBQyxRQUFRLENBQUMsUUFBUSxDQUFDLFFBQVEsRUFBRSxDQUFDLENBQUM7SUFDaEQsSUFBSSxHQUFHLENBQUMsUUFBUSxJQUFJLFFBQVEsSUFBSSxHQUFHLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyx5QkFBeUIsQ0FBQyxFQUFFO1FBQzFFLElBQUksR0FBRyxHQUFHLENBQUMsSUFBSSxDQUFDO0tBQ2hCO1NBQU07UUFDTixNQUFNLElBQUksS0FBSyxDQUFDLHVDQUF1QyxHQUFHLENBQUMsSUFBSSxHQUFHLENBQUMsQ0FBQztLQUNwRTtJQVNELFNBQVMsY0FBYztRQUN0QixJQUFJLENBQUMsR0FBRyxRQUFRLENBQUMsYUFBYSxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQ3ZDLENBQUMsQ0FBQyxTQUFTLENBQUMsR0FBRyxDQUFDLGlCQUFpQixDQUFDLENBQUM7UUFDbkMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxlQUFlLEdBQUcsK0VBQStFLENBQUM7UUFDMUcsQ0FBQyxDQUFDLEtBQUssQ0FBQyxrQkFBa0IsR0FBRyxLQUFLLENBQUM7UUFDbkMsQ0FBQyxD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