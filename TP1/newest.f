      PROGRAM DATAS
      IMPLICIT NONE
      REAL*8 HH, JD, MJD,GPSS
      INTEGER YY, MM, DD, OPTION, OPTION2, GPSW, INDIAB, DOY0
      CHARACTER*10 DSEMANA,ANOB
      WRITE(6,*)'Welcome to the date conversion program'
      WRITE(6,*)'What would you like to calculate?'
      WRITE(6,*)'1.JD - Hoffman-Wellenhof'
      WRITE(6,*)'2.YY MM DD HH'
      WRITE(6,*)'3.MJD'
      WRITE(6,*)'4.GPSW and GPSS'
      WRITE(6,*)'5.DOY'
      WRITE(6,*)'6.JD - Langley'
      WRITE(6,*)'7.JD - Leick'
      WRITE(6,*)'8.I want to see all the results'
      READ(5,*) OPTION
      IF(OPTION.EQ.1) THEN
      	WRITE(6,*)'Please insert'
      	WRITE(6,*)'Year,Month,Day,Hour'
      	READ(5,*)YY,MM,DD,HH
     	CALL CAL2JD (YY,MM,DD,HH,JD)
      ELSE IF(OPTION.EQ.2) THEN
      	WRITE(6,*)'Please insert'
      	WRITE(6,*)'JD'
      	READ(5,*)JD
     	CALL JD2CAL (JD,YY,MM,DD,HH,DSEMANA)
      ELSE IF(OPTION.EQ.3) THEN
      	WRITE(6,*)'Please choose'
      	WRITE(6,*)'1.Year,Month,Day,Hour'
      	WRITE(6,*)'2.JD'
      	READ(5,*)OPTION2
		IF(OPTION2.EQ.1) THEN
      			WRITE(6,*)'Please insert'
			WRITE(6,*)'Year,Month,Day,Hour'
			READ(5,*)YY,MM,DD,HH
     			CALL CAL2JD (YY,MM,DD,HH,JD)
     			CALL JD2MJD(JD,MJD)
		ELSE IF(OPTION2.EQ.2) THEN
      			WRITE(6,*)'Please insert'
      			WRITE(6,*)'JD'
      			READ(5,*)JD
     			CALL JD2MJD(JD,MJD)
		ELSE
      			WRITE(6,*)'Error'
		END IF
      ELSE IF(OPTION.EQ.4) THEN
      	WRITE(6,*)'Please choose'
      	WRITE(6,*)'1.Year,Month,Day'
      	WRITE(6,*)'2.JD'
      	READ(5,*)OPTION2
		IF(OPTION2.EQ.1) THEN
      		WRITE(6,*)'Please insert'
			WRITE(6,*)'Year,Month,Day,Hour'
			READ(5,*)YY,MM,DD,HH
                CALL CAL2JD(YY,MM,DD,HH,JD)
      			CALL JD2GPSW(JD,GPSW,GPSS)
		ELSE IF(OPTION2.EQ.2) THEN
      			WRITE(6,*)'Please insert'
      			WRITE(6,*)'JD'
      			READ(5,*)JD
      			CALL JD2GPSW(JD,GPSW,GPSS)
		ELSE
      			WRITE(6,*)'Error'
		END IF	
      ELSE IF(OPTION.EQ.5) THEN
      	WRITE(6,*)'Please choose'
      	WRITE(6,*)'1.Year,Month,Day'
      	WRITE(6,*)'2.JD'
      	READ(5,*)OPTION2
		IF(OPTION2.EQ.1) THEN
      			WRITE(6,*)'Please insert'
			WRITE(6,*)'Year,Month,Day'
			READ(5,*)YY,MM,DD
    			CALL DOYD(YY,MM,DD,DOY0,INDIAB,ANOB)
		ELSE IF(OPTION2.EQ.2) THEN
      			WRITE(6,*)'Please insert'
      			WRITE(6,*)'JD'
      			READ(5,*)JD
                CALL JD2CAL(JD,YY,MM,DD,HH,DSEMANA)
     			CALL DOYD(YY,MM,DD,DOY0,INDIAB,ANOB)
		ELSE
      			WRITE(6,*)'Error'
		END IF
      ELSE IF(OPTION.EQ.6) THEN
      	WRITE(6,*)'Please insert'
      	WRITE(6,*)'Year,Month,Day'
      	READ(5,*)YY,MM,DD
     	CALL CAL2JD1991 (YY,MM,DD,JD)
      ELSE IF(OPTION.EQ.7) THEN
      	WRITE(6,*)'Please insert'
      	WRITE(6,*)'Year,Month,Day'
      	READ(5,*)YY,MM,DD
     	CALL CAL2JD1990 (YY,MM,DD,JD)
	  ELSE IF(OPTION.EQ.8) THEN
	    WRITE(6,*)'Please choose'
      	WRITE(6,*)'1.Year,Month,Day,Hour'
      	WRITE(6,*)'2.JD'
		WRITE(6,*)'3.DOY'
      	READ(5,*)OPTION2
		IF(OPTION2.EQ.1) THEN
				WRITE(6,*)'Please insert'
				WRITE(6,*)'Year,Month,Day,Hour'
				READ(5,*)YY,MM,DD,HH
				CALL CAL2JD(YY,MM,DD,HH,JD)
				CALL JD2MJD(JD,MJD)
				CALL DOYD(YY,MM,DD,DOY0,INDIAB,ANOB)
				CALL JD2GPSW(JD,GPSW,GPSS)
		ELSE IF(OPTION2.EQ.2) THEN
      			WRITE(6,*)'Please insert'
      			WRITE(6,*)'JD'
      			READ(5,*)JD
				CALL JD2CAL(JD,YY,MM,DD,HH,DSEMANA)
				CALL JD2MJD(JD,MJD)
				CALL DOYD(YY,MM,DD,DOY0,INDIAB,ANOB)
				CALL JD2GPSW(JD,GPSW,GPSS)				
		ELSE IF(OPTION2.EQ.3) THEN
      			WRITE(6,*)'Please insert'
      			WRITE(6,*)'DOY,YY,HH'
      			READ(5,*)DOY0,YY,HH
				CALL DOY2CAL(DOY0,YY,MM,DD)
				CALL CAL2JD(YY,MM,DD,HH,JD)
		        CALL JD2MJD(JD,MJD)
				CALL JD2GPSW(JD,GPSW,GPSS)	
		ELSE
		     	WRITE(6,*)'Wrong Option Selected'
        END IF				
      ELSE
     	WRITE(6,*)'Wrong Option Selected'
      END IF
      END 	 


      SUBROUTINE CAL2JD(YY,MM,DD,HH,JD)
      IMPLICIT NONE
      REAL*8 HH,JD
      INTEGER YY,MM,DD,M,Y
      IF (MM.LE.2) THEN
      	M=MM+12
      	Y=YY-1
      ELSE
      	M=MM
      	Y=YY
      END IF
      JD = INT(365.25*Y)+INT(30.6001*(M+1))+DD+HH/24+1720981.5
      WRITE(6,*)'The Julian Date is'
      WRITE(6,'(F14.2)')JD
      END

      SUBROUTINE CAL2JD1991(YY,MM,DD,JD)
      IMPLICIT NONE
      REAL*8 JD
      INTEGER YY,MM,DD
      JD = 367*YY-7*(YY+(MM+9)/12)/4-3*((YY+(MM-9)/7)/100+1)/4+
     1 275*MM/9+DD+1721029
      WRITE(6,*)'The Julian Date calculated by the Langley method is'
      WRITE(6,*)JD
      END

      SUBROUTINE CAL2JD1990(YY,MM,DD,JD)
      IMPLICIT NONE
      REAL*8 JD
      INTEGER YY,MM,DD
      JD = 367*YY-7*(YY+(MM+9)/12)/4+275*MM/9+DD+1721014
      WRITE(6,*)'The Julian Date calculated by the Leick method is'
      WRITE(6,*)JD
      END

      SUBROUTINE JD2CAL(JD,YY,MM,DD,HH,DSEMANA)
      IMPLICIT NONE
      REAL*8 HH,JD
      INTEGER YY,MM,A,B,C,D,E,DD,N
      CHARACTER*10 DSEMANA
      A=INT(JD+0.5)
      B=A+1537
      C=INT((B-122.1)/365.25)
      D=INT(365.25*C)
      E=INT((B-D)/30.6001)
      DD=B-D-INT(30.6001*E)
      MM=E-1-12*INT(E/14)
      YY=C-4715-INT((7+MM)/10)
      HH=(JD+0.5-A)*24
      N=MOD(INT(JD+0.5),7)
      IF(N.EQ.0) THEN
      	DSEMANA = 'Monday'
      ELSE IF(N.EQ.1) THEN
      	DSEMANA = 'Tuesday'
      ELSE IF(N.EQ.2) THEN
      	DSEMANA = 'Wednesday'
      ELSE IF(N.EQ.3) THEN
      	DSEMANA = 'Thursday'
      ELSE IF(N.EQ.4) THEN
      	DSEMANA = 'Friday'
      ELSE IF(N.EQ.5) THEN
      	DSEMANA = 'Saturday'
      ELSE
      	DSEMANA = 'Sunday'
      END IF
	  WRITE(6,*) 'The Gregorian date is ', YY, MM, DD
      WRITE(6,*) 'The weekday is ',DSEMANA
      END

      SUBROUTINE JD2MJD(JD,MJD)
      IMPLICIT NONE
      REAL*8 JD,MJD
      MJD=JD-2400000.5
      WRITE(6,*)'The Modified Julian Date is'
	WRITE(6,*)MJD
      END

      SUBROUTINE DOYD(YY,MM,DD,DOY0,INDIAB,ANOB)
      IMPLICIT NONE
      REAL*8 IAB
      INTEGER YY,MM,DD,INDIAB, DOY0
      CHARACTER*10 ANOB
      IAB = (MOD(YY,4)+2)/3-(MOD(YY,100)+99)/100+(MOD(YY,400)+399)/400
      INDIAB = INT(IAB)
      IF(INDIAB.EQ.0) THEN
      	ANOB = 'Leap'
      ELSE
      	ANOB = 'Common'
      END IF
      DOY0 = (275 * MM/9) - (((MM+9)/12)*(1+INDIAB))+DD-30
      WRITE(6,*) 'This is day ',DOY0, ' of the year', YY
      WRITE(6,*) 'This is a ', ANOB, 'year'
      END

      SUBROUTINE JD2GPSW(JD,GPSW,GPSS)
      IMPLICIT NONE
      INTEGER GPSW
      REAL*8 JD, MJD, GPSS
      MJD=JD-2400000.5
	WRITE(6,*)MJD
      GPSW=INT((JD-2444244.5)/7)
      GPSS=(INT(MJD)-((GPSW+349178)*7-2400002)+(MJD-INT(MJD)))*86400
      WRITE(6,*) 'This is GPS Week ',GPSW,',   GPS Seconds', GPSS
	END
	  
	SUBROUTINE DOY2CAL(DOY0,YY,MM,DD)
	IMPLICIT NONE
	INTEGER INDIAB,YY,MM,DD,DOY0
      INDIAB = ((MOD(YY,4)+2)/3)-((MOD(YY,100)+99)/100)+
     1((MOD(YY,400)+399)/400)	
	IF (DOY0.GE.32) THEN
		MM=INT(9*((1+INDIAB)+DOY0)/275.0+0.98)
	ELSE
		MM=1
	END IF
	DD=DOY0-275*MM/9+(1+INDIAB)*((MM+9)/12)+30
      WRITE(6,*) 'The Gregorian calendar date is'
      WRITE(6,*) YY,MM,DD
	END
	  