
async function injectVideoIntoTopicHeading(link: HTMLAnchorElement) {
    try {
        const domp = new DOMParser();
        
        const viewerPage  = await fetch(link.href).then(r => r.text());
        const viewerDoc   = domp.parseFromString(viewerPage, 'text/html');
        const viewerFrame = viewerDoc.querySelector('.d2l-fileviewer iframe') as HTMLIFrameElement;
        if(!viewerFrame) { console.error('no fileviewer frame for', link.innerText, viewerDoc); return };

        // fetch /content/enforced/$courseId-$courseName/$chapterName/$topicName.html?ou=$courseId&d2l_body_type=3
        const topicPage = await fetch(viewerFrame.src).then(r => r.text());
        const topicDoc = domp.parseFromString(topicPage, 'text/html');
        const topicFrame = topicDoc.querySelector('#container iframe') as HTMLIFrameElement;
        if(!topicFrame) { console.error('no container frame for', link.innerText, topicDoc); return };

        // fetch /d2l/common/dialogs/quickLink/quickLink.d2l?ou=$courseId&type=lti&rcode=$resourceCode&srcou=1
        const embedPage = await fetch(topicFrame.src).then(r => r.text());
        const embedDoc = domp.parseFromString(embedPage, 'text/html');
        // console.log(embedDoc);
        const embedFrame = embedDoc.querySelector('d2l-lti-launch'); // iframe custom element
        if(!embedFrame) { console.error('no lti-launch frame for', link.innerText, embedDoc); return };

        // the main content page doesn't seem to have the code for the d2l-lti-launch element
        // so map the source into a iframe and it seems to work well enough

        let embedLink = embedFrame.attributes.getNamedItem('lti-launch-url')?.value;
        if(!embedLink) { console.error('no lti-launch-url for', link.innerText, embedDoc); return };

        const videoFrame = document.createElement('iframe');
        videoFrame.src = embedLink;
        videoFrame.setAttribute('allowfullscreen', 'true'); // enable the fullscreen button :)

        // try to size it to match the parent table of contents
        videoFrame.style.setProperty('width', '100%');
        videoFrame.style.setProperty('height', 'auto');
        videoFrame.style.setProperty('aspect-ratio', '16 / 9');

        // just stick it inline to the topic headings
        const itemContent = link.closest('.d2l-datalist-item-content');
        if(!itemContent) { console.error('no itemcontent element for', link.innerText); return };

        itemContent.after(videoFrame);
    } catch(e) {
        console.error('error for ', link.innerText, e);
    }
}