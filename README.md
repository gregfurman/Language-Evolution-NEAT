# EvoLang

*EvoLang* is a language evolution simulator that aims to test the affects of differing environmental (i.e environment size, obstacles) and sociological (i.e population size, number of resources) features on the evolution of languages and dialects.

Controllers for agents were evolved via NEAT.

## Preparation

Make sure to have Java 8 and Maven installed. Run the following script in order to generate the necessary .jar file required for running the simulator:

```bash
bash prepare_simulator.bash
```

## Usage
To change the simulator's parameters, edit the *config* file (as is seen in the repo). Any in these parameters errors will be reported via command line.

```bash
java -jar EvoLang.jar
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.


## License
[MIT](https://choosealicense.com/licenses/mit/)
