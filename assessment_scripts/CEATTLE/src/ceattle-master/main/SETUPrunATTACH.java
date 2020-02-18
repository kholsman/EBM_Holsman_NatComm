import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
 
public class SETUPrunATTACH {
 
    public static void main(String a[]) {
        RConnection connection = null;

        try {
            /* Create a connection to Rserve instance running on default port
             * 6311
             */
            connection = new RConnection();
            /* Note four slashes (\\\\) in the path */
            //connection.eval("source('../Scripts/R_code/abc2c.R')");
            String path = System.getProperty("user.dir");
        
            //System.out.println("Working Directory = " + path);
            connection.eval("setwd(\"" +path+ "\")").asString();

            

        } catch (RserveException e) {
            e.printStackTrace();
        } catch (REXPMismatchException e) {
            e.printStackTrace();
        }
    }
}