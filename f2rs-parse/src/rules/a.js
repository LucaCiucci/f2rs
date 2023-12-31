const videoElements = document.querySelectorAll('video');
if (videoElements.length == 1) {
    const video = videoElements[0];
    const src = video.src;
    const videoName = src.split('/').pop();
    const link = document.createElement('a');
    link.href = src;
    link.download = videoName;
    link.click();
}