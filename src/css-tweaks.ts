
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
	content: function() {

	},
	content_toc: function() {
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
function asVisible<T>(target: HTMLElement, f: () => T): T {
	let isVisible = (target: HTMLElement) => target.offsetParent !== null;

	if(isVisible(target)) {
		// If we're visible, we're done - call the function
		return f();
	} else {
		if(getComputedStyle(target).getPropertyValue('display') === 'none') {
			let [oldV, oldP] = [target.style.getPropertyValue('display'), target.style.getPropertyPriority('display')];
			target.style.setProperty('display', 'inherit', '!important'); // better value?

			// try again - maybe we're visible now?
			let rtn = asVisible(target, f);

			target.style.setProperty('display', oldV, oldP);

			return rtn;
		} else {
			// we aren't responsible for our invisibility - is our parent?
			return asVisible(target.parentElement!, f);
		}
	}
}
