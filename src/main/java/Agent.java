
import com.google.gson.Gson;
import com.google.gson.JsonElement;

import org.encog.ml.MLRegression;
import org.encog.ml.data.MLData;
import org.encog.ml.data.basic.BasicMLData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

public class Agent extends Cell implements Runnable{


    int id;
    private final static int AGENT_FITNESS = 10;
    private final static int TERM_SIZE_MAX = 9;
    private final static int TERM_SIZE_MIN = 3;


    ArrayList<Agent> neighbours;
    private HashMap<Character, String> wordMap = new HashMap();
    double fitness;

    double[] inputs;

    Resource resource;

    double bid;

    private MLRegression model;


    public Agent(int id){

        this.id = id;
        fitness = AGENT_FITNESS;

    }


    public Agent(int id, MLRegression model){

        this.id = id;
        this.model = model;
        fitness = AGENT_FITNESS;

    }

    public Agent(int id, MLRegression model, int fitness){

        this.id = id;
        this.model = model;
        this.fitness= fitness;

    }
    public Agent(Agent agent){
        this.id = agent.id;
        fitness = agent.fitness;
        wordMap = agent.wordMap;
    }


    public Agent(MLRegression model){
        this.model = model;
        fitness = AGENT_FITNESS;

    }


    void Greet(Agent agent){

        System.out.println(getId() + ": hello Agent " + agent.getId());

    }

    public void GreetNeighbours(Cell resource){


        if (neighbours.size() > 0 && resource.getClass() == Resource.class) {

            for (Agent agent : neighbours) {
                this.Greet(agent);
            }


            System.out.println("Consuming resource " + ((Resource) resource).type);
            ((Resource) resource).consume();
        }

    }

    public String generateTerm(){

        Random random = new Random();

        char[] letters = "abcdefghijklmnopqrstuvxyz".toCharArray();
        String term = "";



        for (int i = 0; i < random.nextInt(TERM_SIZE_MAX-TERM_SIZE_MIN)+TERM_SIZE_MIN; i++){
            term += letters[random.nextInt(letters.length)];
        }

        return term;

    }

    public double convertTerm(char type){


        String term = wordMap.get(type);


        String convertedTerm= "0.";

        for (char letter :  term.toCharArray()){

            convertedTerm += (int)letter + "";
        }



        return Double.valueOf(convertedTerm);

    }



    public void configureWordMap(){

        char[] types = {'A','B','C'};

        for (char type: types)
        wordMap.put(type, generateTerm());

    }


    public void setInputs(Resource resource){


        inputs = new double[9];

//        for (int i = 0; i <  neighbours.size(); i++){
//
//            inputs[i] =
//
//        }


    }

    public void NamingGame(){


        inputs = new double[9];

        for (int i = 0; i < neighbours.size(); i++){

            inputs[i] = neighbours.get(i).convertTerm(resource.type);
        }

        inputs[7] = resource.reward;


        try {

            if (fitness <= 0) {
                fitness = 0;
            }

            String fitness = String.valueOf(this.fitness).replace(".", "");
            inputs[8] = Double.valueOf("0." + fitness);



        } catch (NumberFormatException e){
            System.out.println("Cannot parse fitness: " + fitness + " " + this.fitness);
        }


        MLData modelInput = new BasicMLData(inputs);
        MLData predict = model.compute(modelInput);
        setBid(predict.getData(0));
    }

    public double getScore(double[] inputs){

        //Have NN input here


        convertTerm(resource.type);



        Random random = new Random();
        return random.nextFloat();

    }

    public void setResource(Resource resource){

        this.resource = resource;

    }

    public int getId(){
        return id;
    }


    public void run(){

       NamingGame();

    }



    void setBid(double bid){


        if (bid < 0 || fitness < bid)
            this.bid = 0;
        else
            this.bid = bid*fitness;


    }

    double getBid(){
        return bid;
    }


    void setTerm(char type, String term){

        wordMap.put(type,term);

    }

    String getTerm(char type){
        return wordMap.get(type);
    }


    void consume(double reward){
        fitness += reward;

    }

    void consume(double reward, boolean winner){

        if (Double.isNaN(reward) || Double.isNaN(getBid())){
            System.out.println(reward + " " +getBid());
        }

        if (winner)
            fitness += (reward-getBid());
        else
            fitness += reward;


    }

    public void setNetwork(MLRegression model){

        this.model = model;

    }

    public void addNeighbour(Agent agent){
        neighbours.add(agent);
    }

    public double getFitness() {
        return fitness;
    }

    public void setFitness(int fitness){
        this.fitness = fitness;
    }

    public void decrementFitness(double dec){
        if (fitness >= dec)
            fitness -= dec;
    }

    public void decrementFitness(){
        fitness--;
    }

    public String toString(){
        return "["+getId() + "]";
    }

    public JsonElement getWordMap() {

        Gson gson = new Gson();
        JsonElement element = gson.toJsonTree(wordMap);
        element.getAsJsonObject().addProperty("id",getId());
        element.getAsJsonObject().addProperty("fitness",getFitness());
        element.getAsJsonObject().addProperty("hashcode",model.hashCode());
        return element;

    }
}
