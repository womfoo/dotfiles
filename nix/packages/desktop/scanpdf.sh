set -e

: "${SCAN_DEVICE:=fujitsu:ScanSnap S1500:4491}"
: "${SCAN_SOURCE:=ADF Front}"
: "${SCAN_MODE:=COLOR}"
: "${SCAN_PAGES:=1}"
: "${SCAN_RESOLUTION:=300}"
: "${SCAN_X:=210}"
: "${SCAN_Y:=297}"
: "${SCAN_DIR:=${PWD}}"
: "${SCAN_CROP:=yes}"

tmp_dir=$(mktemp -d)
# echo "$tmp_dir is here"

cd "$tmp_dir"

scanimage -d "${SCAN_DEVICE}" \
  --batch \
  --batch=output%0${SCAN_PAGES}d.tif \
  --format tiff \
  --source "${SCAN_SOURCE}" \
  --resolution "${SCAN_RESOLUTION}" \
  --batch-start=1 \
  --batch-count="${SCAN_PAGES}" \
  --swcrop="${SCAN_CROP}" \
  -x "${SCAN_X}" \
  -y "${SCAN_Y}" \
  --page-width "${SCAN_X}" \
  --page-height "${SCAN_Y}" \
  --mode "${SCAN_MODE}"

  # --swdeskew=yes

tiffcp output*.tif merge.tif

tesseract merge.tif merge pdf

evince merge.pdf &

# read -p "Do you want to rename the Tesseract output file? (y/n): " rename_choice
# rename_choice="n"
rename_choice="y"

if [ "$rename_choice" == "y" ]; then
  read -p "Enter the new name: " new_name
  mv merge.pdf "${SCAN_DIR}/$new_name".pdf
else
  new_name="scan_$(date +"%Y%m%d%H%M%S").pdf"
  mv merge.pdf "${SCAN_DIR}/$new_name"
fi

# back to the orig dir
cd -

rm -rf "$tmp_dir"
