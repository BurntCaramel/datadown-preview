dev:
	yarn dev

format: ./tests ./src
	elm-format-0.18 ./src ./tests
