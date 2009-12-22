package rtfspec.ant;

import org.apache.tools.ant.taskdefs.Java;
import java.io.File;

public class RTFSpecTask extends Java {
    private File specsDir;

    public void	setDir(File directoryWhereSpecsAre){
	this.specsDir = directoryWhereSpecsAre;
    }

    public void	execute(){
	this.setClassname("run_rtfspec");
	this.createArg().setValue(specsDir.getAbsolutePath());
	super.execute();
    }
}