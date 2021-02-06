import java.util.*;

public class StatsCalculator {

    private List<Double> scores = new ArrayList<>();

    public StatsCalculator(boolean sync){


        if (sync)
        scores = Collections.synchronizedList(new ArrayList<Double>(scores));


    }



    private void resetScores(){

        scores = Collections.synchronizedList(new ArrayList<Double>());

    }

    public void add(double score){

        scores.add(score);

    }

    public String summaryStatistics(){


        double currentAverage = average(true);
        double variance = variance(currentAverage);

        resetScores();

        System.out.println("Average: " + currentAverage + "\nBetween groups Variance: " + variance);

        return currentAverage + "," + variance;


    }

    protected double average(boolean sync){

        if (sync)
        synchronized (scores){

            return sum(false)/scores.size();
        }

        else
           return  scores.stream().mapToDouble(Double::shortValue).average().getAsDouble();
    }

    protected double variance(double average){

        synchronized (scores) {
            double sumDiffsSquared = 0.0;
            for (double value : scores) {
                double diff = value - average;
                diff *= diff;
                sumDiffsSquared += diff;
            }
            return sumDiffsSquared / (scores.size() - 1);
        }

    }


    protected double SSG(double groupMean){


        double SSG = scores.stream()
                .map(i -> i - groupMean)
                .map(i -> i*i)
                .mapToDouble(i -> i).sum();

        return SSG;
    }


    protected double sum(boolean reset){

     double sum=0;

        synchronized(scores) {
            Iterator i = scores.iterator();
            while (i.hasNext())
                sum += ((Double) i.next());
        }

     if (reset) {
         resetScores();
         System.out.println("Within groups variance: " + sum);
     }
     return sum;
    }


    protected double max(){
        return Collections.max(scores);
    }


}
