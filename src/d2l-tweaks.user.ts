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
	let host: string|null = null;
	let url = new URL(document.location.toString());
	if (url.protocol == 'https:' && url.host.match(/^d2l.[a-zA-Z0-9_-]+.edu/)) {
		host = url.host;
	} else {
		throw new Error(`Bad host for D2L Script (exiting): '${url.host}'`);
	}

	
	interface D2LAssetMetadata {
		type: string,
		filename: string|null,
		size: number,
	}

	function create_dl_icon(): Element {
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
	enum CONTENT_TYPES {
		PDF = 'pdf',
		MP4 = 'mp4',
		Word = 'msword',
		Excel = 'msexcel',
		WebPage = 'webpage',
        ExternalPage = 'extpage', // not sure what the difference is in D2L but... *shrug*?
		Panopto = 'panopto',
		UNKNOWN = 'unknown',
	};
	const PAGE_TITLE_MAP = {
		'PDF document': CONTENT_TYPES.PDF,
		'Word Document': CONTENT_TYPES.Word,
		'Excel Spreadsheet': CONTENT_TYPES.Excel,
	}

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
		CONTENT_TYPES.MP4, // have to use link from page for URL seeking
		CONTENT_TYPES.ExternalPage,
		CONTENT_TYPES.WebPage, // maybe?

		CONTENT_TYPES.Word, // Should show PDF ( .d2l-fileviewer-pdf-pdfjs[data-location] )
		CONTENT_TYPES.Excel, // Should show PDF ( .d2l-fileviewer-pdf-pdfjs[data-location] )
		
	];

	function getContentType() {
        function isMP4(child: Element): boolean {
            return child.attributes.getNamedItem("data-mediaplayer-src-original") != undefined;
        }
        function isExternalPage(): string | null {
			let dests = Object.values(D2L.OR.__g1)
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
			}
		}
		
		// Used if types can be narrowed down (eg: WebPage -> Panopto)
		let intermediary = CONTENT_TYPES.UNKNOWN;

		let page_title_view = Array.from(document.getElementsByClassName("d2l-page-title-view"));
		if (page_title_view.length > 1) {
			console.warn("Page title views: ", page_title_view);
			throw new Error("More than one elements of class d2l-page-title-view, unable to determine page types.");
		} else if (page_title_view.length == 1) {
			// null safety: elements with .d2l-page-title-view won't be the root document/doctype, so textContent won't be null
			let ptv = page_title_view[0].textContent!.trim();
			if(ptv == 'Web Page') {
				intermediary = CONTENT_TYPES.WebPage;
			} else if(ptv in PAGE_TITLE_MAP) {
				return PAGE_TITLE_MAP[ptv as keyof typeof PAGE_TITLE_MAP];
			} else {
				console.warn(`Unknown page title view value: ${ptv}`)
			}
		}

		let content_view = document.getElementById('ContentView');
		if (content_view) {
			console.log(content_view) // Easy reference to #ContentView

			let insideContent = Array.from(content_view.children);
			if (insideContent.length == 0) {
				console.log(`Unknown page contents: 0 elements inside #ContentView`);
			} else if (insideContent.length == 1) {
                let child = insideContent[0];
                if(isMP4(child)) return CONTENT_TYPES.MP4;
                if(isExternalPage()) return CONTENT_TYPES.ExternalPage;
			} else {
				console.log(`Unknown page contents: 2+ elements inside #ContentView`);
			}
		} else { // web page??
			console.trace("Web page?");
		}
		return CONTENT_TYPES.UNKNOWN;
	}
	function provideTypeFunctionality(type: CONTENT_TYPES) {
		// add button to use native iframes for certain types
		// add link to direct download
		let [cls, asset] = new URL(document.URL).pathname.split('/').filter(c => Number.isFinite(Number.parseInt(c)));
		let [url, promMeta] = urlOfD2LAsset(cls, asset);
		let handled = false;

		function getUrl(type: CONTENT_TYPES, apiUrl: string) {
			if(type == CONTENT_TYPES.MP4) {
				let vidplayer = document.querySelectorAll('#ContentView .vui-mediaplayer')[0];
				let vidurl = vidplayer.getAttribute('data-mediaplayer-src');
				return [vidurl, url + '?stream=false' ];
			} else if(type == CONTENT_TYPES.ExternalPage) {
				let url = Object.values(D2L.OR.__g1).map(s => JSON.parse(s)).filter(o => o.N == 'D2L.LE.Content.Desktop.Topic.OpenInNewWindow')[0].P[0];
				url = new URL(url);
				url.protocol = "https"; // otherwise iframe won't load b/c D2L is HTTPS
				return [url, null];
			} else if([CONTENT_TYPES.Word, CONTENT_TYPES.Excel].includes(type)) {
				// D2L converts office documents to PDF to preview them inside D2L - use the native PDF viewer instead for interactive viewing
				let awsPDF = document.querySelector('.d2l-fileviewer-pdf-pdfjs')?.getAttribute('data-location');
				return [awsPDF, url + '?stream=false'];
			} else {
				return [
					NATIVE_VIEW_COMPAT.includes(type) ? apiUrl + '?stream=true' : null,
					DIRECT_FILES.includes(type) ? apiUrl + '?stream=false' : null,
				]
			}
		}

		// interactive is meant for in-browser viewing
		// downloadable is meant for files that can be downloaded by navigating to them
		let [interactive, downloadable] = getUrl(type, url);
		if(interactive) {
			handled = true;
			btn_useNativeIframe(interactive, MAKE_NATIVE_ON_LOAD);
			addTitleLink("Direct Link", interactive);
		}
		if(downloadable) {
			handled = true;
			addTitleLink("Download", downloadable, create_dl_icon());
		}

		if (!handled) {
			console.warn("Unhandled D2L content type from Userscript: ", type);
			console.log("Content type asset url: ", url);
			return promMeta.then(meta => { console.log("Asset meta for url:", meta); });
		}
	}

	function btn_useNativeIframe(src: string, now?: boolean) {
		//returns promise for native iframe

		function insertIframe() {
			// inject own content iframe
			let ifram = document.createElement('iframe')
			ifram.src = src;
			ifram.style.width = '100%'
			ifram.style.height = '90vh'
			ifram.setAttribute('preload', 'auto');

			replaceContent(ifram)
			return ifram;
		}

		if (now) {
			return Promise.resolve(insertIframe());
		} else {
			// return promise that is fulfilled with iframe
			return new Promise((res, rej) => {
				function btnonclick(this: HTMLButtonElement) {
					// installed as onclick handler => this = `<button>...</button>`
					// remove ourselves (button) since we have served our purpose
					this.remove();
					res(insertIframe());
				};
				// Note: put button in immediatly, provide link after PDF was downloaded
				addTitleBtn("Use Native Viewer", btnonclick);
			});
		}
	}

	// make content more than 600px high
	function makeContentLong() {
		let docViewer = Array.from(document.getElementsByClassName("d2l-documentViewer")) as HTMLElement[];
		if (docViewer.length > 1) {
			console.warn("There are multiple .d2l-documentViewer elements! Page may be extra long...", docViewer);
		}
		
		docViewer.forEach(e => { e.style.height = '90vh' });
		console.log("Set doc viewers' height to 90vh");
	}

	// helper functions
	function withinIframe() {
		// https://stackoverflow.com/a/326076/11536614
		try {
			return window.self !== window.top;
		} catch (e) {
			return true; // access denied error
		}
	}

	function getPageHeader(): Element {
		let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h");
		if(!hdrBar) throw new Error("Unable to find content header bar!");
		return hdrBar;
	}
	function addTitleBtn(text: string, onclick: (this: HTMLButtonElement, ev: MouseEvent) => any) {
		let btn = document.createElement('button');
		btn.innerText = text;
		btn.classList.add('d2l-button');
		btn.style.marginRight = '.75rem';
		btn.style.width = 'auto';
		btn.addEventListener('click', onclick);
		getPageHeader().appendChild(btn);
		return btn;
	}
	function addTitleLink(text: string, href: string, prependNode?: Element) {
		let link = document.createElement('a');
		link.innerText = text;
		if(prependNode) link.prepend(prependNode);
		link.href = href;
		link.classList.add('d2l-button');
		link.style.marginRight = '.75rem';
		link.style.width = 'auto';
		getPageHeader().appendChild(link);
		return link;
	}
	function replaceContent(ele: Element) {
		// get content view
		let cv = document.getElementById("ContentView");

		if(cv == undefined) {
			throw new Error("Unable to retrieve #ContentView");
		}

		// remove existing content
		while (cv.lastChild) { cv.removeChild(cv.lastChild) }

		cv.appendChild(ele);
	}
	async function D2LAssetMeta(cls: string, asset: string): Promise<D2LAssetMetadata> {
		let f = await fetch(`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`, { method: 'HEAD' });

		// get mime and orig file name, if available
		let type = f.headers.get("content-type")!;
		let filename = f.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
		let size = Number.parseInt(f.headers.get("content-length")!);

		return { type, filename, size };
	}
	// Returns [url: string, Promise<[content-type: string, orig-filename: string, size: number]>]
	function urlOfD2LAsset(cls: string, asset: string): [string, Promise<D2LAssetMetadata>]{
		if (!Number.isFinite(Number.parseInt(cls))) {
			throw new Error(`D2L class ID isn't parsable to a number/ID: '${cls}'`);
		}
		if (!Number.isFinite(Number.parseInt(asset))) {
			throw new Error(`D2L asset ID isn't parsable to a number/ID: '${asset}'`);
		}

		// the ?stream=true tells D2L to use a response header for content to be viewed in the browser, rather than downloaded
		return [
			`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`,
			D2LAssetMeta(cls, asset), // returns promise
		]
	}


	(async function () {
		'use strict';

		if (!withinIframe()) {
			try {
				// @ts-ignore
				chrome.runtime.sendMessage("llhmaciggnibnbdokidmbilklceaobae", null);
			} catch (e) {
				if (e.message.includes("Invalid extension id")) {
					console.warn(`D2L userscript includes installing the "HTML5 Video Keyboard Shortcuts" extension for speeding up videos`, `https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae`);
				} else {
					throw e;
				}
			}

			try {
				makeContentLong();

				let type = getContentType();
				console.log(`Content Type: ${getContentType()}`);
				await provideTypeFunctionality(type); // wait for it, to catch errors
			} catch (e) {
				console.error("Error occured in userscript", e);
				//alert("Error occured in D2L bettering userscript, error in console.");
			}

		}
	})();
}
