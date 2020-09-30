package name.heavycarbon.jpl.callprolog;

import static org.junit.Assert.*;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jpl7.JPL;

public class ConnectTest {

    @Test
    public void gettingSettingDefaultInitArgs() {
       Logger logger = LoggerFactory.getLogger("gettingSettingDefaultInitArgs");
       // String[] initArgs = JPL.getActualInitArgs(); // returns null if not yet initialized
       String[] initArgs = JPL.getDefaultInitArgs(); // returns null if not yet initialized
       for (String str : initArgs) {
          logger.info("Found {}",str);
       }
    }

}
