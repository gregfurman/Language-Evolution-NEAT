import com.google.gson.Gson;
import java.io.FileReader;
import java.io.IOException;

public class Config {

    int population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations;


    public Config(int population_size, int dim_x, int dim_y, int initial_fitness, int agent_no, int resource_no, int trials) {
        this.population_size = population_size;
        this.dim_x = dim_x;
        this.dim_y = dim_y;
        this.initial_fitness = initial_fitness;
        this.agent_no = agent_no;
        this.resource_no = resource_no;
        this.trials = trials;
    }

    public Config(int[] config){

        this.population_size = config[0];
        this.dim_x = config[1];
        this.dim_y = config[2];
        this.initial_fitness = config[3];
        this.agent_no = config[4];
        this.resource_no = config[5];
        this.trials = config[6];
        this.generations = config[7];

    }

    private Config(Config config){

        this.population_size = config.getPopulation_size();
        this.dim_x = config.getDim_x();
        this.dim_y = config.getDim_y();
        this.initial_fitness = config.getInitial_fitness();
        this.agent_no = config.getAgent_no();
        this.resource_no = config.getResource_no();
        this.trials = config.getTrials();
        this.generations = config.getGenerations();



    }

    Config(){

    }

    public Config load(String filename){

        Gson gson = new Gson();

        try (FileReader reader = new FileReader(filename)){

            return gson.fromJson(reader, Config.class);

        } catch (IOException e){
            e.printStackTrace();
        }

        return null;

    }

    public int getPopulation_size() {
        return population_size;
    }

    public int getDim_x() {
        return dim_x;
    }

    public int getDim_y() {
        return dim_y;
    }

    public int getInitial_fitness() {
        return initial_fitness;
    }

    public int getAgent_no() {
        return agent_no;
    }

    public int getResource_no() {
        return resource_no;
    }

    public int getGenerations(){ return generations;}

    public int getTrials() {
        return trials;
    }


    public String toString(){

        return String.format("population_size=%d,dim_x=%d, dim_y=%d, initial_fitness=%d, agent_no=%d, resource_no=%d, trials=%d, generations=%d",population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations);

    }

}
