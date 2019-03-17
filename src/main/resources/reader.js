
function saveCex(filename, cexString) {
	var blob = new Blob([cexString], {type: "text/plain;charset=utf-8"});
	saveAs(blob, filename);
}
