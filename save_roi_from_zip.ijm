inputdir = "C:\\Users\\sofa\\Dropbox\\Wing Evo Project\\Analysis\\2014 Probook-Berkeley-MyWork-copy\\nick\\Done (wings & ROI)\\ROI\\"
outputdir = "C:\\Users\\sofa\\Dropbox\\Wing Evo Project\\Analysis\\2014 Probook-Berkeley-MyWork-copy\\nick\\Done (wings & ROI)\\xydata\\"


function roi_to_xytxt(inputdir, outputdir, filename) {

roiManager("Open", inputdir + filename);

roiManager("Select", 0);
saveAs("XY Coordinates", outputdir + "scale_" + filename + ".txt");
roiManager("Select", 1);
saveAs("XY Coordinates", outputdir + "wLfr_" + filename + ".txt");
roiManager("Select", 2);
saveAs("XY Coordinates", outputdir + "wLnf_" + filename + ".txt");
roiManager("Select", 3);
saveAs("XY Coordinates", outputdir + "wAfr_" + filename + ".txt");
roiManager("Select", 4);
saveAs("XY Coordinates", outputdir + "wAnf_" + filename + ".txt");
roiManager("Select", newArray(0,1,2,3,4));
roiManager("Delete");

}

setBatchMode(true);
list = getFileList(inputdir);
for (i = 0; i < list.length; i++)
       roi_to_xytxt(inputdir, outputdir, list[i]);
setBatchMode(false);
