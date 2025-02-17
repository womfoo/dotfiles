{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "mpvdash";
  runtimeInputs = with pkgs; [
    mpv
  ];
  text = ''
    BASEDIR=$(pwd)/
    MTIME="''${MTIME:--2}"

    if [ "$BASEDIR" = "/home/kranium/" ]; then
      BASEDIR=/dashcam/corolla/manual_rec/
    fi

    videos=""

    # VIDTRANS="[vid1]scale=1920:1080[vid1_scaled]; [vid2]scale=1920:1080[vid2_scaled]; [vid1_scaled][vid2_scaled]hstack[vo]"
    # VIDTRANS="[vid1]format=yuva420p,setsar=1,pad=3840:2160:0:0:black[vid1p];[vid2]scale=1280:720[vid2s];[vid1p][vid2s]overlay=W-w:H-h[vo]" # top left bottom right
    VIDTRANS="[vid1]format=yuva420p,setsar=1,pad=3840:2160:640:0:black[vid1p];[vid2]scale=1280:720[vid2s];[vid1p][vid2s]overlay=W-w-1280:H-h[vo]" # stacked

    for front in $(find "$BASEDIR" -mtime "$MTIME" -name '*F.MP4' | sort); do
      rear=''${front//F\.MP4/R\.MP4}
      videos+=" --{ $front --external-file=$rear --lavfi-complex=$VIDTRANS --}"
    done

    if [ "$videos" = "" ]; then
      echo no videos selected, try changing \$MTIME
    else
      # shellcheck disable=SC2086
      mpv \
        --video-unscaled \
        --no-sub \
        $videos \
        "$@"
    fi
  '';
}
