"use strict";
// ==UserScript==
// @name         D2L Tweaks
// @namespace    https://github.com/csm123199/d2l-tweaks
// @version      0.7
// @description  Add QoL changes to D2L's user interface
// @author       Chris Moore
// @include      https://d2l.*.edu/d2l/le/content/*/viewContent/*/View
// @grant        none
// @updateUrl    https://raw.githubusercontent.com/csm123199/d2l-tweaks/master/d2l-tweaks.user.js
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
        function isExternalPage(child) {
            /*let dests = Object.values(D2L.OR.__g1)
                .map(s => JSON.parse(s))
                .filter((ent): ent is OR_Objects.Func => {
                    return ent._type == "func" && ent.N == "D2L.LE.Content.Desktop.Topic.OpenInNewWindow" && ent.P.length == 1
                })
                .map(ent => ent.P[0]); // get only the destination URLs
            
            if(dests.length > 1) {
                console.warn("Not recognizing as external page because multiple URLs showed up as valid:", dests);
            }
            if(dests.length == 1 && typeof dests[0] == "string") {
                return dests[0];
            } else {
                return null;
            }*/
            let itext = child.innerText;
            if (itext.includes("External Resource") && itext.includes("Open in New Window")) {
                return true;
            }
            return false;
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
                if (isExternalPage(child))
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
            {
                let url = new URL(src);
                if (url.host.includes("youtube.com") && url.pathname == "/watch") {
                    let video_id = url.searchParams.get("v");
                    if (video_id) {
                        url.searchParams.delete("v");
                        url.pathname = "/embed/" + video_id;
                        ifram.setAttribute("allowfullscreen", "true");
                    }
                    src = url.toString();
                }
            }
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
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZDJsLXR3ZWFrcy51c2VyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic3JjL2QybC10d2Vha3MudXNlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUEsaUJBQWlCO0FBQ2pCLDJCQUEyQjtBQUMzQix3REFBd0Q7QUFDeEQsb0JBQW9CO0FBQ3BCLHdEQUF3RDtBQUN4RCw0QkFBNEI7QUFDNUIsc0VBQXNFO0FBQ3RFLHFCQUFxQjtBQUNyQixpR0FBaUc7QUFDakcsa0JBQWtCO0FBRWxCLG9CQUFvQjtBQUNwQix5REFBeUQ7QUFFekQsMkNBQTJDO0FBRTNDLHVCQUF1QjtBQUV2QjtJQUVDLFlBQVk7SUFDWixNQUFNLG1CQUFtQixHQUFHLElBQUksQ0FBQztJQUVqQyxVQUFVO0lBQ1YsSUFBSSxJQUFJLEdBQWdCLElBQUksQ0FBQztJQUM3QixJQUFJLEdBQUcsR0FBRyxJQUFJLEdBQUcsQ0FBQyxRQUFRLENBQUMsUUFBUSxDQUFDLFFBQVEsRUFBRSxDQUFDLENBQUM7SUFDaEQsSUFBSSxHQUFHLENBQUMsUUFBUSxJQUFJLFFBQVEsSUFBSSxHQUFHLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyx5QkFBeUIsQ0FBQyxFQUFFO1FBQzFFLElBQUksR0FBRyxHQUFHLENBQUMsSUFBSSxDQUFDO0tBQ2hCO1NBQU07UUFDTixNQUFNLElBQUksS0FBSyxDQUFDLHVDQUF1QyxHQUFHLENBQUMsSUFBSSxHQUFHLENBQUMsQ0FBQztLQUNwRTtJQVNELFNBQVMsY0FBYztRQUN0QixJQUFJLENBQUMsR0FBRyxRQUFRLENBQUMsYUFBYSxDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQ3ZDLENBQUMsQ0FBQyxTQUFTLENBQUMsR0FBRyxDQUFDLGlCQUFpQixDQUFDLENBQUM7UUFDbkMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxlQUFlLEdBQUcsK0VBQStFLENBQUM7UUFDMUcsQ0FBQyxDQUFDLEtBQUssQ0FBQyxrQkFBa0IsR0FBRyxLQUFLLENBQUM7UUFDbkMsQ0FBQyxD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