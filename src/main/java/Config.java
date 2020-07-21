import com.google.gson.Gson;
import java.io.FileReader;
import java.io.IOException;

public class Config {

    int population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations,id;
    boolean loadPopulation;

    StatsRecorder wordList, fitnessStats;

    StatsCalculator calculator, trialCalculator;

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

    public Config(Config config){

        this.population_size = config.getPopulation_size();
        this.dim_x = config.getDim_x();
        this.dim_y = config.getDim_y();
        this.initial_fitness = config.getInitial_fitness();
        this.agent_no = config.getAgent_no();
        this.resource_no = config.getResource_no();
        this.trials = config.getTrials();
        this.generations = config.getGenerations();
        this.fitnessStats = config.getFitnessStats();
        this.wordList = config.getWordList();
        this.id = getId();
        this.calculator = new StatsCalculator(true);
        this.trialCalculator = new StatsCalculator(true);
        this.loadPopulation = config.isLoadPopulation();

    }


    public Config(int agent_no){

        this.id = agent_no;
        this.agent_no = agent_no;

    }
    Config(){

    }

    public Config load(String filename){

        Gson gson = new Gson();

        Config config;

        try (FileReader reader = new FileReader(filename)){

            config = gson.fromJson(reader, Config.class);
            return config;


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

    public int getId() {
        return id;
    }

    public boolean isLoadPopulation() {
        return loadPopulation;
    }

    public void loadPopulation(boolean load){
        loadPopulation = load;
    }

    public void setAgent_no(int agent_no) {
        this.agent_no = agent_no;
    }


    public void setResource_no(double resource_no) {
        this.resource_no = (int)Math.ceil(getDim_x()*getDim_y()*resource_no);
    }


    public void setId(int id) {
        this.id = id;
    }

    public void loadStatsRecorders(){

        fitnessStats = new StatsRecorder("fitness_"+getId()+".csv","Generation,average,MSB,MSW,best,resources,agents",256);
        wordList =new StatsRecorder("wordList_"+getId()+".json");


    }

    public void closeStatsRecorders(){
        wordList.close();
        fitnessStats.close();
    }


    public StatsRecorder getFitnessStats() {
        return fitnessStats;
    }


    public void recordFitness(int generation, double best){

        getFitnessStats().write((generation+1)+"," + getCalculator().summaryStatistics() + "," + getTrialCalculator().sum(true)+","+ best + ","+ getResource_no() + ","+ getAgent_no());

    }

    public StatsRecorder getWordList() {
        return wordList;
    }

    public StatsCalculator getCalculator() {
        return calculator;
    }

    public StatsCalculator getTrialCalculator(){
        return trialCalculator;
    }

    public String toString(){

        return String.format("id=%d,population_size=%d,dim_x=%d, dim_y=%d, initial_fitness=%d, agent_no=%d, resource_no=%d, trials=%d, generations=%d",id,population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations);

    }


    public String experimentDetails(){


        return "Agents: " + getAgent_no() + "\nResources: " + getResource_no() + "\nNetworks: " + getPopulation_size();

    }
}
