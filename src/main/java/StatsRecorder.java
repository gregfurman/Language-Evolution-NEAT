import java.io.*;

public class StatsRecorder {

    Object bufferedObject;
    private static int default_buffer_size = 1024;


    public StatsRecorder(String filename){


        File file = new File(filename);

        try {

            if (!file.exists()) {
                file.createNewFile();
            }

            bufferedObject = new BufferedWriter(new FileWriter(filename,true));
        } catch (IOException e){
            System.out.println("File creation failed.");
        }

    }


    public StatsRecorder(String filename, String header){
        this(filename,header,default_buffer_size);
    }

    public StatsRecorder(String filename, String header, int bufferSize){

        File file = new File(filename);

        try {

            boolean newfile=file.exists();

            if (!newfile) {
                file.createNewFile();
            }


//            FileOutputStream fos = new FileOutputStream(file);
//            OutputStreamWriter osr = new OutputStreamWriter(fos, "UTF-8");
//            bufferedWriter = new BufferedWriter(osr,bufferSize);

//            bufferedWriter = new BufferedWriter(new FileWriter(filename,newfile),bufferSize);
            bufferedObject = new BufferedOutputStream(
                    new FileOutputStream(file,true),bufferSize);
//            bufferedOutputStream = new BufferedOutputStream(
//                    new FileOutputStream(file,true),bufferSize);

            if (!newfile)
                write(header);

        } catch (IOException e){
            System.out.println("File creation failed.");
        }

    }


    public boolean write(String line){

        try {

            if (bufferedObject.getClass() == BufferedOutputStream.class)
                ((BufferedOutputStream) bufferedObject).write((line + "\n").getBytes());
            else
                ((BufferedWriter) bufferedObject).write(line + "\n");

        } catch (IOException e){
            System.out.println("Failed to write line");
            return false;
        }

        return true;

    }


    public boolean close(){

        Class objClass = bufferedObject.getClass();

        try {

            if (objClass == BufferedWriter.class)
                ((BufferedWriter) bufferedObject).close();
            else if (objClass == BufferedOutputStream.class)
                ((BufferedOutputStream) bufferedObject).close();

        } catch (IOException e){
            return false;
        }

        return true;
    }

    public boolean flush(){

        Class objClass = bufferedObject.getClass();

        try {

            if (objClass == BufferedWriter.class)
                ((BufferedWriter) bufferedObject).flush();
            else if (objClass == BufferedOutputStream.class)
                ((BufferedOutputStream) bufferedObject).flush();

        } catch (IOException e){
            return false;
        }

        return true;
    }



    public long getBuffer(){

        return 0;


    }

}
