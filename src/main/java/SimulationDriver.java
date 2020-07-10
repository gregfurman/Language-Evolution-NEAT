
public class SimulationDriver implements Runnable{


    Environment environment;
    Environment[] environments;
    int iterations;


    public SimulationDriver(Environment environment, int iterations){

        this.environment = environment;
        this.iterations = iterations;

    }


    public SimulationDriver(Environment environment, int iterations, int trials){

        environments = new Environment[trials];

        for (int index=0; index<trials;index++){

            environments[index] = new Environment(environment);

        }

        this.iterations = iterations;

    }

    public double getFitness(){
        return environment.getFitness(false);
    }

    public String getSummaryStats(){
        return environment.getSummaryStatistics();

    }

    public String[] wordlist(boolean status,int trial){
        return environment.getWordList(status,trial);
    }

    public void begin(){

//        environment.loadGrid(resources, agents);

        if (iterations > 0)
            environment.begin(iterations);
        else
            environment.begin();

    }

    public void run(){

        begin();

    }



}
