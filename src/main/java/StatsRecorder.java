import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class StatsRecorder {

    BufferedWriter bufferedWriter;


    public StatsRecorder(String filename){


        File file = new File(filename);

        try {

            if (!file.exists()) {
                file.createNewFile();
            }

            bufferedWriter = new BufferedWriter(new FileWriter(filename));
        } catch (IOException e){
            System.out.println("File creation failed.");
        }

    }

    public StatsRecorder(String filename, String header){


        File file = new File(filename);

        try {

            boolean newfile=file.exists();

            if (!newfile) {
                file.createNewFile();
            }

            bufferedWriter = new BufferedWriter(new FileWriter(filename,newfile));

            if (newfile)
                bufferedWriter.write(header + "\n");

        } catch (IOException e){
            System.out.println("File creation failed.");
        }

    }


    public boolean write(String line){

        try {
            bufferedWriter.write(line + "\n");
        } catch (IOException e){
            System.out.println("Failed to write line");
            return false;
        }

        return true;

    }


    public boolean close(){

        try {
            bufferedWriter.close();
        } catch (IOException e){
            return false;
        }

        return true;
    }

    public boolean flush(){

        try {
            bufferedWriter.flush();
        } catch (IOException e){
            return false;
        }

        return true;
    }



}
