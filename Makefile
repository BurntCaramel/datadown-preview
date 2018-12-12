dev:
	yarn dev

format: ./tests ./src
	./node_modules/.bin/elm-format ./src ./tests
