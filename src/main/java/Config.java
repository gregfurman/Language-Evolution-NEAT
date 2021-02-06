import com.google.gson.Gson;
import java.io.FileReader;
import java.io.IOException;

public class Config {

    int population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations,id, trial_id, language_number;
    boolean loadPopulation,train,control;
    double split;

    StatsRecorder wordList, fitnessStats;

    StatsCalculator calculator, trialCalculator;

    public Config(int population_size, int dim_x, int dim_y, int initial_fitness, int agent_no, int resource_no, int trials,double split) {
        this.population_size = population_size;
        this.dim_x = dim_x;
        this.dim_y = dim_y;
        this.initial_fitness = initial_fitness;
        this.agent_no = agent_no;
        this.resource_no = resource_no;
        this.trials = trials;
        this.split = split;

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

        this.control = config.isControl();
        this.train = config.isTrain();
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
        this.language_number = config.getLanguageNumber();
        this.split = config.getSplit();



    }


    protected boolean check(){

        String log = "Error's in config file:\n";
        StringBuilder sb = new StringBuilder(log);

        if (train) {

            if (population_size < 2){
                sb.append("\n- Must be a population size of at least 2");
            }

            if (generations < 1){
                sb.append("\n- There must be at least 1 generation for evolution to take place.");
            }

        }

        if (getTrials() < 1)
            sb.append("\n- There must be at least 2 trials.");

        if (language_number < 1)
            sb.append(String.format("\n- %d resource types not allowed. Must be greater than 0."));

        if (agent_no<2)
            sb.append("\n- There must be at least 2 agents within the simulation.");

        if (resource_no<1){
            sb.append("\n- There must be at least 2 resources within the simulation.");
        }

        if (dim_y*dim_x <= agent_no)
            sb.append("\n- Talking agent population cannot exceed the size of the environment.");

        if (dim_x*dim_y < 25)
            sb.append("\n- Area of environment must be greater than or equal to 5 x 5 cells.");


        if (sb.toString().equals(log))
            return false;

        sb.append("\n\n**********************");


        System.out.println(sb.toString());
        return true;



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

    double getSplit(){return split;}

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

    public int getLanguageNumber(){
        return language_number;
    }

    public boolean isTrain() {
        return train;
    }

    public boolean isControl(){
        return control;
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


    void setDims(int x,int y){ this.dim_x=x; this.dim_y=y;}

    void setSplit(double split){this.split = split > 1 || split <= 0 ? 1: split;}

    public void setResources(int resources){
        this.resource_no = resources;
    }
    public void setId(int id) {
        this.id = id;
    }

    public void setTrial_id(int id){
        this.trial_id = id;
    }

    public void loadStatsRecorders(){

        if (check())
            System.exit(1);

        if (isTrain())
            fitnessStats = new StatsRecorder("fitness_"+getAgent_no()+".csv","Generation,average,MSB,MSW,best,resources,agents",256);

        wordList =new StatsRecorder("wordList_"+getAgent_no()+".json");


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

        return String.format("id=%d,population_size=%d,dim_x=%d, dim_y=%d, initial_fitness=%d, agent_no=%d, resource_no=%d, trials=%d, generations=%d, loadpop=%b",id,population_size,dim_x, dim_y,  initial_fitness, agent_no, resource_no, trials, generations,loadPopulation);

    }


    public String summaryParameters(){

        return String.format("%d,%d,%d,%d",agent_no,resource_no,id,trial_id);

    }

    public String experimentDetails(){


        return "Agents: " + getAgent_no() + "\nResources: " + getResource_no() + "\nNetworks: " + getPopulation_size();

    }
}
