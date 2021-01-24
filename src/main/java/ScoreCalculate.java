import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;
import org.encog.neural.neat.NEATNetwork;

public class ScoreCalculate implements CalculateScore {


    Environment environment;
    StatsRecorder wordlist,fitnessStats;

    private int TRIALS, GENERATIONS;
    final static int MAX_ITERATIONS = 20000;

    Config config;


    public ScoreCalculate(Environment environment, Config config){

        this.environment = environment;
        this.wordlist = config.getWordList();;
        this.fitnessStats = config.getFitnessStats();
        this.config = config;
        this.GENERATIONS = config.getGenerations();
        this.TRIALS = config.getTrials();

    }



    @Override
    public double calculateScore(MLMethod mlMethod) {

        NEATNetwork network = (NEATNetwork) mlMethod;


        SimulationDriver[] drivers = new SimulationDriver[TRIALS];
        StatsCalculator trialScores = new StatsCalculator(false);


        double score;

        for (int trial = 0; trial < TRIALS; trial++) {


            Environment env = new Environment(environment,network);

            drivers[trial] = new SimulationDriver(env, MAX_ITERATIONS);
            drivers[trial].begin();

            trialScores.add(drivers[trial].getFitness());

            if (env.generation.get() % (config.getGenerations()/10) == 0)
            recordWords(drivers[trial].wordlist(false,trial));
            drivers[trial] = null;

        }

        score = trialScores.average(false);


        config.getTrialCalculator().add(trialScores.SSG(score) / (config.getPopulation_size() * (config.getTrials() - 1)));
        config.getCalculator().add(score);


        return score;


    }


    public double calculateControlScore() {

       return calculateScore(null);

    }


    void recordWords(String[] wordlist){
        boolean success=true;
        for(String words : wordlist ){
            success = success && this.wordlist.write(words);
            if (!success)
                break;
        }

    }



    @Override
    public boolean shouldMinimize() {
        return false;
    }

    @Override
    public boolean requireSingleThreaded() {
        return false;
    }



}