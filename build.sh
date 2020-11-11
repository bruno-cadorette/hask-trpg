cd app/frontend
elm make src/Main.elm --output ../static/main.js
cd ../..
stack run