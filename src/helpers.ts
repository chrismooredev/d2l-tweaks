
function initCapabilities(): Capabilities {
	// I realize I shouldn't be using the userAgent to detect browser flavors.
	// It'll be good enough for this.
	let useragent = navigator.userAgent.toLowerCase();

	// @ts-ignore
	if(useragent.includes('chrome')) {
		const mt = (app: string): boolean => navigator.mimeTypes.namedItem('application/' + app) !== null;
		return {
			extensions: {
				// detect if chrome supports viewing MS office documents (via extension/etc)
				office_viewer: mt('msword') && mt('msexcel') && mt('mspowerpoint') && mt('msword-template'),
				//office_viewer: chromeExtensionInstalled('gmpljdlgcdkljlppaekciacdmdlhfeon'),
				video_shortcuts: chromeExtensionInstalled('llhmaciggnibnbdokidmbilklceaobae'),
			},
			browser: "chrome",
		}
	} else if(useragent.includes('firefox')) {
		return {
			// no way to know
			extensions: {
				office_viewer: false,
				video_shortcuts: true, // FF has built in speed controls - which I care about
			},
			browser: "firefox",
		}
	}
	return {
		extensions: { office_viewer: false, video_shortcuts: false, },
		browser: "unknown",
	}
}

function suggestHTML5VideoKeyboardShortcuts() {
	// Only suggest on chrome, b/c firefox users exist
	// (and firefox already has right-click, change speed controls)
	if(!chromeExtensionInstalled('llhmaciggnibnbdokidmbilklceaobae')) {
		console.warn(`D2L userscript recommends installing the "HTML5 Video Keyboard Shortcuts" extension for speeding up videos`, `https://chrome.google.com/webstore/detail/llhmaciggnibnbdokidmbilklceaobae`);
	}
}
function suggestOfficeEditing() {
	// Only suggest on chrome, b/c firefox users exist
	if(!chromeExtensionInstalled('gbkeegbaiigmenfmjfclcdgdpimamgkj')) {
		console.warn(`D2L userscript recommends installing the "Office Editing for Docs, Sheets & Slides" extension for interactively viewing office documents`, `https://chrome.google.com/webstore/detail/gbkeegbaiigmenfmjfclcdgdpimamgkj`);
	}
}

function chromeExtensionInstalled(id: string) {
	try {
		// @ts-ignore
		if(window.chrome) { window.chrome.runtime.sendMessage(id, null); return true; }
		
	} catch (e) {
		if (!e.message.includes("Invalid extension id")) { throw e; }
	}
	return false;
}

/** Useful for early exit if we aren't the top frame */
function withinIframe() {
	// https://stackoverflow.com/a/326076/11536614
	try {
		return window.self !== window.top;
	} catch (e) {
		return true; // access denied error
	}
}

function d2l_icon(backgroundImage: string): HTMLSpanElement {
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

function d2l_icon_download(): HTMLElement {
	return d2l_icon("url('https://s.brightspace.com/lib/bsi/20.20.8-85/images/tier1/download.svg')");
}

/** The URL interface represents an object providing static methods used for creating object URLs. */
function newURL(url: string | URL | Location): URL {
	if(typeof url == 'string')
		return new URL(url, document.location.href);
	else if(url instanceof URL) {
		return new URL(url.href);
	} else if(url instanceof Location) {
		return new URL(url.href);
	} else {
		throw new Error('unknown parameter type');
	}
}

/** Return the type of D2L page we are on, with some appropriate metadata about it */
function getPageType(): PageType {
	let href = newURL(document.location);
	// slice to remove zero-length component at beginning (since pathnames start with '/')
	let components = href.pathname.split('/').slice(1);
	if(
		components.length == 7
		&& components[0] == "d2l"
		&& components[1] == "le"
		&& components[2] == "content"
		// class ID
		&& components[4] == "viewContent"
		// asset ID
		&& components[6] == "View"
	) {
		return { type: "content", class: components[3], asset: components[5], };
	} else if(
		components.length == 5
		&& components[0] == "d2l"
		&& components[1] == "le"
		&& components[2] == "content"
		// class ID
		&& components[4] == "Home"
	) {
		return { type: "content_toc", class: components[3] };
	}

	return { type: "unknown" };
}
