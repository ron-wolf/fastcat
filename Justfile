version:
    @grep -P -o '\d+\.\d+\.\d+' src/ac.dats

next:
    @export VERSION=$(cat src/ac.dats | grep -P -o '\d+\.\d+\.\d+' src/ac.dats | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "360s/[0-9]\+\.[0-9]\+\.[0-9]\+\+/$VERSION/" src/ac.dats
    @git commit -am "next"

release:
    git tag "$(grep -P -o '\d+\.\d+\.\d+' src/ac.dats)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+' src/ac.dats)"
    git push origin master
