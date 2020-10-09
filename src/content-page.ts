
// Maybe these will be useful for a non-content page? Keeping around for now
function urlOfD2LAsset(cls: string, asset: string): [URL, Promise<D2LAssetMetadata>]{
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
		D2LAssetMeta(assetURL), // returns promise
	]
}
async function D2LAssetMeta(location: URL): Promise<D2LAssetMetadata> {
	let f = await fetch(location.toString(), { method: 'HEAD' });

	// get mime and orig file name, if available
	let type = f.headers.get("content-type")!;
	let filename = f.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
	let size = Number.parseInt(f.headers.get("content-length")!);

	return { type, filename, size };
}

/** Represents a D2L Content page. Distinguished by a `/d2l/le/content/$class/viewContent/$asset/View` url*/
class ContentPage {
	protected contentView: HTMLElement;
	protected hdrBar: HTMLElement;
	protected cls: string;
	protected asset: string;
	/** May point to a 404. Should be avoided. */
	protected naiveAssetURL: URL;

	protected interactiveURL: URL | null;
	protected downloadableURL: URL | null;
	protected officeURL: URL | null;

	protected _naiveAssetMeta?: Promise<D2LAssetMetadata | null>;
	protected replacedContent: boolean;

	static initialize(): ContentPage | null {
		let ptype = getPageType();
		if(ptype.type == "content") {
			return new ContentPage(ptype.class, ptype.asset);
		}
		return null;
	}
	constructor(cls: string, asset: string) {
		let cv = document.querySelector("#ContentView") as HTMLElement | null;
		if(!cv) { throw new Error("Page doesn't have a #ContentView !"); }

		let hdrBar = document.querySelector(".d2l-page-title-c .d2l-box-h") as HTMLElement | null;
		if(!hdrBar) throw new Error("Unable to find content header bar!");
		

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

	protected get title(): string {
		let title = this.hdrBar.querySelector('.d2l-page-title')! as HTMLElement;
		return title.innerText;
	}

	public normalizeContent() {
		if(this.interactiveURL && !this.replacedContent) {
			this.replaceContent(this.interactiveURL);
		}
	}
	
	public addHeaderButtons() {
		// replace inner content
		// open in new tab
		// download

		const btnBar = document.createElement('span');
		btnBar.style.marginLeft = "0.3rem"; // arbitrary - looked alright for a few different title names
		const append = (ele: HTMLElement) => btnBar.appendChild(ele);

		if(this.interactiveURL) {
			console.log("Adding interactive buttons");
			if(!this.replacedContent) {
				// only show button if we haven't replaced ourselves, and we have an interactive URL to show
				append(this.titleBtn_replaceContent("Use Native Viewer", this.interactiveURL));
			}
			append(this.titleLink("View Directly", this.interactiveURL));

			if(this.officeURL) {
				append(this.titleLink("View Interactive", this.officeURL));
			}
		}

		if(this.downloadableURL) {
			console.log("Adding downloadable buttons");
			append(this.titleLink("Download", this.downloadableURL, d2l_icon_download()));

			let hdrBtns = this.headerDropdownActions();
			if(hdrBtns && hdrBtns.length == 1 && hdrBtns[0] == "Download") {
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
	protected _interactiveURL(): URL | null {
		// TODO: Get mime type and check against navigator.mimeTypes ?

		let asQuiz = this.quizUrl();
		if(asQuiz) return asQuiz;

		let asMP4 = this.mp4Url();
		if(asMP4) return asMP4;

		let asPDF = this.pdfUrl();

		// office should only show via button
		if(asPDF) {
			if(!asPDF[1])
				this.officeURL = this.officeUrl();
			return asPDF[0];
		}

		let asExtPage = this.extPageUrl();
		if(asExtPage) return asExtPage;

		console.log("Page not recognized as interactive");

		return null;
	}

	/** Returned URL should ideally serve the `Content-Disposition: attachment` header */
	protected _downloadableURL(): URL | null {
		let asExtPage = this.extPageUrl();
		if(asExtPage) return null;

		//if(this.quizUrl()) return null;
		// If this page wraps D2L another D2L page?
		// eg) quizzes, assignments, etc placed in Content
		if(document.querySelector("#ContentView > .d2l-placeholder")) return null;
		if(this.extPageUrl()) return null;

		return this.naiveFileURL(false);
	}

	/** Unneeded? */
	private _assetMeta(): Promise<D2LAssetMetadata | null> {
		if(this._naiveAssetMeta) return this._naiveAssetMeta;

		return this._naiveAssetMeta = fetch(this.naiveAssetURL.toString(), { method: 'HEAD' })
			.then(res => {
				if(res.status == 404) {
					return null;
				} else {
					// get mime and orig file name, if available
					let type = res.headers.get("content-type")!;
					let filename = res.headers.get("content-disposition")?.match(/filename="(.+)"/)?.[1] ?? null; //null if header missing, no filename, etc
					let size = Number.parseInt(res.headers.get("content-length")!);
				
					return { type, filename, size } as D2LAssetMetadata;
				}
			});
	}

	/** Returns the human-readable labels for the content's dropdown actions */
	protected headerDropdownActions(): string[] | null {
		// only works if the dropdown has been activated
		//Array.from(this.hdrBar.querySelectorAll('d2l-dropdown d2l-dropdown-menu d2l-menu d2l-menu-item'))
		//	.map(e => e.getAttribute('text'))

		// Access unrendered (templated) dropdown actions, and get their text.
		let templ: HTMLTemplateElement | null = this.hdrBar.querySelector('template#d2l_pageTitleActions');
		if(!templ) return null;
		return Array.from(templ.content.querySelectorAll('d2l-menu-item[text]'))
			.map(e => e.getAttribute('text')!);

		/* currently known:
			file content: "Download"
			quiz: "View Summary"
			quiz: "View Submissions"
			quiz: "View Reports"
		*/
	}

	// Keep these organized roughly in the order of least->most expensive to run

	private quizUrl(): URL | null {
		let quiz_iframe = this.contentView.querySelector('#QuizContentViewPlaceHolder > iframe[src]');
		if(quiz_iframe) {
			let url = newURL(quiz_iframe.getAttribute('src')!);
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
	private mp4Url(): URL | null {
		let players = this.contentView.querySelectorAll("div.vui-mediaplayer[data-mediaplayer-src]");
		if(players.length == 0) {
			return null;
		} else if(players.length > 1) {
			throw new Error('More than 1 media player found on the page!');
		}
		let player = players[0];

		return newURL(player.getAttribute('data-mediaplayer-src')!);
	}

	/** Interactive. May be handled by an extension */
	private officeUrl(): URL | null {
		/*
		 * There's currently a bug in the `Office Editing for Docs, Sheets, & Slides` extension that prevents it from parsing the 'Content-Disposition' header correctly.
		 * While it doesn't affect functionality, the filename is mangled from what it is supposed to be.
		 * 
		 * We could try to serve the file over `/content/enforced/${class_name}/${div[data-title]}` but there isn't an easy(?) way to get $class_name from the page (D2L JS api?)
		 * MP4 files seem to be served from this folder, however.
		*/
		if(CAPABILITIES.extensions.office_viewer) {
			let asPDF = this.pdfUrl();
			if(asPDF && !asPDF[1]) {
				// if we aren't a native PDF - we might be a converted office PDF?
	
				// check in pdfUrl ensures there is only 1 element that matches this
				let viewer = this.contentView.querySelectorAll("div[class^=d2l-fileviewer-pdf-][data-location]")[0];
				
				let ext = viewer.getAttribute('data-title')?.split('.').slice(-1)[0];
				if(['doc', 'dot', 'xls', 'xlt', 'csv', 'ppt', 'pot', 'pps', 'sld'].some(s => ext?.startsWith(s))) {
					return this.naiveFileURL(true);
					//return newURL(`/content/enforced/`)

					let title = document.createElement('title');
					title.innerText = this.title;
					
					let body = document.createElement('body');
					body.style.margin = '0';

					let frame = document.createElement('iframe');
					frame.src = this.naiveFileURL(true).toString();
					frame.setAttribute('allowfullscreen', '1');
					frame.style.border = '0'
					frame.style.width = '100%'
					frame.style.height = '100%'

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
	private pdfUrl(): [URL, nativePDF: boolean] | null {
		// native pdf: .d2l-fileviewer-pdf-native
		// converted to pdf: .d2l-fileviewer-pdf-pdfjs

		// Could we determine if native/converted via the data-location URL?
		// Files converted to PDF tend to be served over AWS

		// Get PDF viewers on the page, and ensure there is only one for our content
		let pdfViewers = this.contentView.querySelectorAll("div[class^=d2l-fileviewer-pdf-][data-location]")
		if(pdfViewers.length == 0) {
			return null;
		} else if(pdfViewers.length > 1) {
			throw new Error('More than 1 PDF Fileviewer with a data location found on the page!');
		}
		let viewer = pdfViewers[0];

		// Extract the type of contents our PDF viewer is showing
		let type = Array.from(viewer.classList)
			.filter(s => s.startsWith('d2l-fileviewer-pdf-'))
			.map(s => s.split('-').slice(-1)[0]); // get last component
		if(type.length != 1) { // >1 because we filter to this class prefix
			throw new Error('More than one D2L PDF Fileviewers on the page');
		}

		// return a tuple of [url, isNativePDF]
		let location = newURL(viewer.getAttribute('data-location')!);
		switch(type[0]) {
			case "native": return [location, true];
			case "pdfjs": return [location, false];
			default: throw new Error('Unknown PDF viewer type: ' + type[0]);
		}

	}

	/** Only interactive */
	private extPageUrl(): URL | null {
		let itext = this.contentView.innerText;
		if (!itext.includes("External Resource") || !itext.includes("Open in New Window")) {
			return null;
		}
		// reasonably sure this page's content is an external page - throw on any errors

		let urls = Object.values(D2L.OR.__g1)
			.map(s => { try {
				return JSON.parse(s) as D2L_OR_Object
			} catch(e) {
				throw new Error("D2L.OR.__g1 contains malformed JSON!")
			} })
			.filter((o): o is OR_Objects.Func => {
				return o._type == "func" && o.N == "D2L.LE.Content.Desktop.Topic.OpenInNewWindow" && o.P.length == 1
			})
			.map(o => o.P[0]);
		
		if(urls.length == 0) {
			throw new Error(`Content View showing "External Resource" without any OpenInNewWindow functions in D2L.OR.__g1!`);
		} else if(urls.length > 1) {
			throw new Error(`Multiple OpenInNewWindow commands for "External Resource" content. Not returning any. (${JSON.stringify(urls)})`);
		} else if(typeof urls[0] != 'string') {
			throw new Error(`Single OpenInNewWindow command parameter isn't a string! (${JSON.stringify(urls[0])})`)
		}

		let url = newURL(urls[0]);
		url.protocol = "https"; // otherwise iframe won't load b/c D2L is HTTPS
		if(url.host.includes("youtube.com") && url.pathname == "/watch") {
			let video_id = url.searchParams.get("v");
			if(video_id) {
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
	private naiveFileURL(stream: boolean): URL {
		let url = newURL(this.naiveAssetURL.toString()) // clone URL
		url.searchParams.set("stream", stream.toString());
		return url
	}

	/** DOM manipulation helpers */
	protected replaceContent(iframe_src: URL) {
		let newContent: HTMLElement;
		let ifram = document.createElement('iframe')
		ifram.src = iframe_src.toString();
		ifram.style.width = '100%';
		ifram.style.height = '90vh';
		ifram.style.resize = 'both';
		ifram.setAttribute('preload', 'auto');
		ifram.setAttribute('allowfullscreen', 'true'); // for media players (eg: embedded youtube)
		newContent = ifram;

		let cv = this.contentView;
		while (cv.lastChild) { cv.removeChild(cv.lastChild) } // remove existing content
		cv.appendChild(newContent); // add self to dom

		// Attempt to preserve aspect ratio
		// Must be done after appending to the DOM (.appendChild) so .offsetWidth works
		if(iframe_src.pathname.endsWith('.mp4')) {
			let width = ifram.offsetWidth;
			// assume videos are 16:9 aspect ratio
			ifram.style.height = (width * (9/16)) + 'px';
		}

		this.replacedContent = true;
		return ifram;
	}
	protected titleBtn(text: string, onclick: (this: HTMLButtonElement, ev: MouseEvent) => any): HTMLButtonElement {
		let btn = document.createElement('button');
		btn.innerText = text;
		btn.classList.add('d2l-button');
		btn.style.marginLeft = '0.25rem'; // 5px matches .d2l-contextmenu-ph
		btn.style.marginRight = '0.25rem';
		btn.style.width = 'auto';
		btn.addEventListener('click', onclick);
		return btn;
	}
	protected titleLink(text: string, href: URL, prependNode?: HTMLElement): HTMLAnchorElement {
		let link = document.createElement('a');
		link.innerText = text;
		if(prependNode) link.prepend(prependNode);
		link.href = href.toString();
		link.classList.add('d2l-button');
		link.style.marginLeft = '0.25rem';
		link.style.marginRight = '0.25rem';
		link.style.width = 'auto';
		return link;
	}

	/** Adds a button to use a native viewer for the provided URL. Automatically removes itself from the DOM when clicked. */
	protected titleBtn_replaceContent(label: string, src: URL): HTMLButtonElement {
		const that = this;
		function btnonclick(this: HTMLButtonElement) {
			// installed as onclick handler => this = `<button>...</button>`
			// remove ourselves (button) since we have served our purpose
			this.remove();
			that.replaceContent(src);
		};

		// Note: put button in immediatly, provide link after PDF was downloaded
		return this.titleBtn(label, btnonclick);
	}
}
