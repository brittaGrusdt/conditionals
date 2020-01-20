for f in ./group1/*.jpg; do
    # do some stuff here with "$f"
    convert -resize 75% "$f" "$f"
done

for f in ./group2/*.jpg; do
    # do some stuff here with "$f"
    convert -resize 75% "$f" "$f"
done
