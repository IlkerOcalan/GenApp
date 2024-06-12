          EXEC SQL
               DECLARE COMMERCIAL TABLE
                       ( PolicyNumber   INTEGER    NOT NULL,
                         RequestDate    TimeStamp  NOT NULL,
                         StartDate      date       NOT NULL,
                         RenewalDate    date       NOT NULL,
                         Address        CHAR(255)  NOT NULL,
                         Zipcode        Char(8)    NOT NULL,
                         Latitude	    CHAR(011)  NOT NULL,
                         Longitude	    CHAR(011)  NOT NULL,
                         Customer	    CHAR(255)  NOT NULL,
                         PropertyType   CHAR(255)  NOT NULL,
                         FirePeril	    SMALLINT   NOT NULL,
                         FirePremium	INTEGER    NOT NULL,
                         CrimePeril	    SMALLINT   NOT NULL,
                         CrimePremium	INTEGER    NOT NULL,
                         FloodPeril	    SMALLINT   NOT NULL,
                         FloodPremium	INTEGER    NOT NULL,
                         WeatherPeril	SMALLINT   NOT NULL,
                         WeatherPremium	INTEGER    NOT NULL,
                         Status	        SMALLINT   NOT NULL,
                         RejectReason	CHAR(255)  NOT NULL
                       )
          END-EXEC.