Scripts and other resources to help maintain the [Zen Reddit][rzen].

## Install


    git clone https://github.com/kowey/zennit
    cd zennit
    cabal sandbox init # (optional) may require more recent cabal install
    cabal install
    cd ..

    git clone https://github.com/kowey/prawtools
    cd prawtools
    git checkout unicode
    mkdir -p $HOME/.virtualenvs/zennit
    source $HOME/.virtualenvs/zennit/bin/activate
    pip install .
    cd ..

## Run

    cd zennit
    export TODAY=$(date +%Y-%m-%d)
    subreddit_stats zen -d 90 -o userstats-${TODAY}-90d.csv -u YourUserName
    userstats userstats-${TODAY}-90d.csv

[rzen]: http://reddit.com/r/zen
