// ==UserScript==
// @name         Make D2L a bit better
// @namespace    http://tampermonkey.net/
// @version      0.2
// @description  Make D2L application views longer
// @author       https://github.com/csm123199
// @match        https://d2l.*.edu/d2l/le/content/*/viewContent/*/View
// @include      https://d2l.*.edu/*
// @grant        none
// @updateUrl    https://gist.github.com/csm123199/4bd7605a47ca89d65699bbbaef474c38/raw/d2l-improver.user.js
// ==/UserScript==

// D2L rest api docs
// https://docs.valence.desire2learn.com/res/content.html

let host = null;
let url = new URL(document.location);
if(url.protocol == 'https:' && url.host.match(/^d2l.[a-zA-Z0-9_-]+.edu/)) {
	host = url.host
} else {
	throw new Error('Bad host for D2L Script (exiting): ', url.host);
}

const CONTENT_TYPES = {
	PDF: 'pdf',
	WebPage: 'webpage',
	MP4: 'mp4',
	Panopto: 'panopto',
	UNKNOWN: 'unknown',
};

function getContentType() {
	// Used if types can be narrowed down (eg: WebPage -> Panopto)
	let intermediary = CONTENT_TYPES.UNKNOWN;

	let page_title_view = Array.from(document.getElementsByClassName("d2l-page-title-view"));
	if(page_title_view.length > 1) {
		console.warn("Page title views: ", page_title_view);
		throw new Error("More than one elements of class d2l-page-title-view, unable to determine page types.");
	} else if (page_title_view.length == 1) {
		let ptv = page_title_view[0].textContent.trim();
		switch(ptv) {
			case 'PDF document':
				return CONTENT_TYPES.PDF;
			case 'Web Page':
				intermediary = CONTENT_TYPES.WebPage;
				break;
			default:
				console.warn(`Unknown page title view value: ${ptv}`)
		}
	}

	let content_view = document.getElementById('ContentView');
	if(content_view) {
		console.log(content_view) // Easy reference to #ContentView

		let insideContent = Array.from(content_view.children);
		if(insideContent.length == 0) {
			console.log(`Unknown page contents: 0 elements inside #ContentView`);
		} else if(insideContent.length == 1) {
			let child = insideContent[0];
			let attr = null;
			if(attr = child.attributes.getNamedItem("data-mediaplayer-src-original")) {
				return CONTENT_TYPES.MP4;
			}
		} else {
			console.log(`Unknown page contents: 2+ elements inside #ContentView`);
		}
	} else { // web page??

	}
	return CONTENT_TYPES.UNKNOWN;
}
function provideTypeFunctionality(type) {
	let [cls, asset] = new URL(document.URL).pathname.split('/').filter(c => Number.isFinite(Number.parseInt(c)));
	let [url, promMeta] = urlOfD2LAsset(cls, asset)

	switch(type) {
		case CONTENT_TYPES.PDF:
		case CONTENT_TYPES.MP4:
			return nativeFrameBtnLink(url);
		/*case CONTENT_TYPES.PDF: {
			let promUrl = urlOfD2LAsset(cls, asset);

			return nativeFrameBtnLink(promUrl.then(([url, meta]) => url));
			break; }
		case CONTENT_TYPES.MP4: {
			let mediaplayer = document.querySelector("#ContentView .vui-mediaplayer");
			let link = Promise.resolved(mediaplayer.attributes.getNamedItem("data-mediaplayer-src"));

			return nativeFrameBtnLink(link);
			break; }*/9
		default:
			console.warn("Unhandled D2L content type from Userscript: ", type);
			console.log("Content type asset url: ", url);
			return promMeta.then(([typ, flname, size]) => {
				console.log("Asset meta for url:", {
					type: typ,
					filename: flname,
					size,
				});
			});
	}
}

async function nativeFrameBtnLink(destProm) {
	// Note: put button in immediatly, provide link after PDF was downloaded

	let g = null;
	async function btnonclick() {
		// inject own pdf frame
		let ifram = document.createElement('iframe')
		ifram.src = await destProm;
		ifram.style.width = '100%'
		ifram.style.height = '90vh'
		replaceContent(ifram);

		// remove ourselves since we have served our purpose
		document.querySelector(".d2l-page-title-c .d2l-box-h").removeChild(btn);
	};

	let btn = addTitleBtn("Use Native Viewer", async function() {
		// Ensure the function is only called once

		if(g === null) g = btnonclick();
		await g;
	});

	addTitleLink("Direct Link", await destProm);
}

// make content more than 600px high
function makeContentLong() {
	let docViewer = Array.from(document.getElementsByClassName("d2l-documentViewer"));
	if(docViewer.length > 1) {
		console.log("There are multiple .d2l-documentViewer elements! Page may be extra long...", docViewer);
	}

	docViewer.forEach(e => { e.style.height = '90vh' });
	console.log("Set doc viewers' height to 90vh");
}

// helper functions
function withinIframe() {
	// https://stackoverflow.com/a/326076/11536614
	try {
		return window.self !== window.top;
	} catch(e) {
		return true; // access denied error
	}
}
function addTitleBtn(text, onclick) {
	let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h");
	let btn = document.createElement('button');
	btn.innerText = text;
	btn.style.margin = '8px'; // because D2L removes margins/paddings/etc on every element >.>
	btn.onclick = onclick;
	hdrBar.appendChild(btn);
	return btn;
}
function addTitleLink(text, href) {
	let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h");
	let link = document.createElement('a');
	link.innerText = text;
	link.href = href;
	link.style.color = 'blue';
	link.style.margin = '8px'; // because D2L removes margins/paddings/etc on every element >.>
	hdrBar.appendChild(link);
	return link;
}
function replaceContent(ele) {
	// get content view
	let cv = document.getElementById("ContentView");

	// remove existing content
	while(cv.lastChild) {
		cv.removeChild(cv.lastChild)
	}

	cv.appendChild(ele);
}
async function D2LAssetMeta(cls, asset) {
	let f = await fetch(`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`, { method: 'HEAD' });

	// get mime and orig file name, if available
	let type = f.headers.get("content-type");
	let name;
	try {
		name = f.headers.get("content-disposition").match(/filename="(.+)"/)[1];
	} catch(e) {
		// bad match or not in content-disposition, most likely
		name = null;
	}
	let size = Number.parseInt(f.headers.get("content-length"));

	return [type, name, size];
}
// Returns [url: string, Promise<[content-type: string, orig-filename: string, size: number]>]
function urlOfD2LAsset(cls, asset) {
	if(!Number.isFinite(Number.parseInt(cls))) {
		throw new Error(`D2L class ID isn't parsable to a number/ID: '${cls}'`);
	}
	if(!Number.isFinite(Number.parseInt(asset))) {
		throw new Error(`D2L asset ID isn't parsable to a number/ID: '${asset}'`);
	}

	// the ?stream=true tells D2L to use a response header for content to be viewed in the browser, rather than downloaded
	return [
		`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file?stream=true`,
		D2LAssetMeta(cls, asset), // returns promise
	]

	//console.log(`https://${host}/d2l/api/le/1.34/${cls}/content/topics/${asset}/file`);
	//let f = await fetch(`https://${host}d2l/api/le/1.34/${cls}/content/topics/${asset}/file`);
	//console.log("Fetched asset object", f);

	//return [type, content_disposition, URL.createObjectURL(await f.blob())]
}

(async function() {
	'use strict';

	if(!withinIframe()) {
		try {
			makeContentLong();

			let type = getContentType();
			console.log(`Content Type: ${getContentType()}`);
			await provideTypeFunctionality(type);
		} catch(e) {
			console.error("Error occured in userscript", e);
			//alert("Error occured in D2L bettering userscript, error in console.");
		}
	}
})();
