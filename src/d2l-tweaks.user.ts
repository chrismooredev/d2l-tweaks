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

const CAPABILITIES: Capabilities = initCapabilities();

(async function () {
	'use strict';

	if (!withinIframe()) {
		suggestHTML5VideoKeyboardShortcuts();
		suggestOfficeEditing();

		try {
			let ptype = getPageType();
			if(ptype.type == "content") {
				let contentPage = new ContentPage(ptype.class, ptype.asset);
				console.log("Content page: ", contentPage);
				if(MAKE_NATIVE_ON_LOAD)
					contentPage.normalizeContent();
				contentPage.addHeaderButtons();
			}
		} catch (e) {
			console.error("Error occured in userscript", e);
			//alert("Error occured in D2L bettering userscript, error in console.");
		}

	}
})();
