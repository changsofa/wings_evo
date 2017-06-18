
inputdir = "C:\\Users\\sofa\\Dropbox\\Wing Evo Project\\Analysis\\2014 Probook-Berkeley-MyWork-copy\\nick\\Done (wings & ROI)\\ROI\\"
outputdir = "C:\\Users\\sofa\\Dropbox\\Wing Evo Project\\Analysis\\2014 Probook-Berkeley-MyWork-copy\\nick\\Done (wings & ROI)\\xydata\\"


function roi_to_xytxt(inputdir, outputdir, filename) {

roiManager("Open", inputdir + filename);

roiManager("Select", 0);
saveAs("XY Coordinates", outputdir + filename + "_scale.txt");
roiManager("Select", 1);
saveAs("XY Coordinates", outputdir + filename + "_wl_fr.txt");
roiManager("Select", 3);
saveAs("XY Coordinates", outputdir + filename + "_wa_fr.txt");

roiManager("Select", newArray(0,1,2,3,4));
roiManager("Delete");

}

setBatchMode(true);
list = getFileList(inputdir);
for (i = 0; i < list.length; i++)
        action(inputdir, outputdir, list[i]);
setBatchMode(false);
