      SUBROUTINE FCESND ( NOMTE, IND, XI1, XI2, XI3, CHAR, VF )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/01/97   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8 NOMTE
      CHARACTER*2 CHAR
      REAL*8  XI1,XI2,XI3,VF(*)
      REAL*8  A,B
C
      IF ( ( NOMTE(1:8) .EQ. 'MEC3QU9H' .OR.
     +       NOMTE(1:8) .EQ. 'MEGRC3Q9' ) .AND. (CHAR.EQ.'LI') ) THEN
         A=0.577350269189626D0
         B=1.732050807568888D0
         IF (IND.EQ.1) THEN
            VF(1)= 0.75D0*(A-XI1)*(A-XI2)*0.5D0*(1.D0-B*XI3)
            VF(2)= 0.75D0*(A+XI1)*(A-XI2)*0.5D0*(1.D0-B*XI3)
            VF(3)= 0.75D0*(A+XI1)*(A+XI2)*0.5D0*(1.D0-B*XI3)
            VF(4)= 0.75D0*(A-XI1)*(A+XI2)*0.5D0*(1.D0-B*XI3)
            VF(5)= 0.75D0*(A-XI1)*(A-XI2)*0.5D0*(1.D0+B*XI3)
            VF(6)= 0.75D0*(A+XI1)*(A-XI2)*0.5D0*(1.D0+B*XI3)
            VF(7)= 0.75D0*(A+XI1)*(A+XI2)*0.5D0*(1.D0+B*XI3)
            VF(8)= 0.75D0*(A-XI1)*(A+XI2)*0.5D0*(1.D0+B*XI3)
         ELSE IF (IND.EQ.0) THEN
            VF(1)= 0.75D0*(A-XI1)*(A-XI2)
            VF(2)= 0.75D0*(A+XI1)*(A-XI2)
            VF(3)= 0.75D0*(A+XI1)*(A+XI2)
            VF(4)= 0.75D0*(A-XI1)*(A+XI2)
         ENDIF
C
      ELSEIF ( ( NOMTE(1:8) .EQ. 'MEC3TR7H' .OR.
     +           NOMTE(1:8) .EQ. 'MEGRC3T7' ) .AND.(CHAR.EQ.'LI')) THEN
         A=0.166666666666667D0
         B=0.666666666666667D0
         C=1.732050807568888D0
         IF (IND.EQ.1) THEN
            VF(1)= 2.D0*(-(XI1-B)-(XI2-A))*0.5D0*(1.D0-C*XI3)
            VF(2)= 2.D0*(XI1-A)*0.5D0*(1.D0-C*XI3)
            VF(3)= 2.D0*(XI2-A)*0.5D0*(1.D0-C*XI3)
            VF(4)= 2.D0*(-(XI1-B)-(XI2-A))*0.5D0*(1.D0+C*XI3)
            VF(5)= 2.D0*(XI1-A)*0.5D0*(1.D0+C*XI3)
            VF(6)= 2.D0*(XI2-A)*0.5D0*(1.D0+C*XI3)
         ELSE IF (IND.EQ.0) THEN
            VF(1)= 2.D0*(-(XI1-B)-(XI2-A))
            VF(2)= 2.D0*(XI1-A)
            VF(3)= 2.D0*(XI2-A)
         ENDIF
C
      ENDIF
C
      IF (CHAR.EQ.'NL') THEN
            ATILD1= 1.14623256965195D0
            BTILD1=-1.03869285337708D0
            CTILD1=-0.33234921420688D0
            DTILD1= 0.30116815972783D0
            ETILD1= 0.D0
C
            ATILD2=-3.24623256965195D0
            BTILD2= 1.74799661222309D0
            CTILD2= 2.66568254754022D0
            DTILD2=-1.43538824233474D0
            ETILD2= 0.D0
C
            ATILD3= 4.2D0
            BTILD3= 0.D0
            CTILD3=-4.66666666666667D0
            DTILD3= 0.D0
            ETILD3= 1.D0
C
            ATILD4=-3.24623256965195D0
            BTILD4=-1.74799661222309D0
            CTILD4= 2.66568254754022D0
            DTILD4= 1.43538824233474D0
            ETILD4= 0.D0
C
            ATILD5= 1.14623256965195D0
            BTILD5= 1.03869285337708D0
            CTILD5=-0.33234921420688D0
            DTILD5=-0.30116815972783D0
            ETILD5= 0.D0
C
            XI12=XI1**2
C
            XI22=XI2**2
C
            XI32=XI3**2
            XI33=XI3**3
            XI34=XI3**4
C
            FTILD1=ATILD1*XI34+BTILD1*XI33+CTILD1*XI32+DTILD1*XI3+ETILD1
            FTILD2=ATILD2*XI34+BTILD2*XI33+CTILD2*XI32+DTILD2*XI3+ETILD2
            FTILD3=ATILD3*XI34+BTILD3*XI33+CTILD3*XI32+DTILD3*XI3+ETILD3
            FTILD4=ATILD4*XI34+BTILD4*XI33+CTILD4*XI32+DTILD4*XI3+ETILD4
            FTILD5=ATILD5*XI34+BTILD5*XI33+CTILD5*XI32+DTILD5*XI3+ETILD5
C
        IF ( NOMTE(1:8) .EQ. 'MEC3QU9H' .OR.
     +       NOMTE(1:8) .EQ. 'MEGRC3Q9' ) THEN
           A1= 0.833333333333333D0
           B1=-0.645497224367903D0
           C1= 0.D0
C
           A2=-1.666666666666667D0
           B2= 0.D0
           C2= 1.D0
C
           A3= 0.833333333333333D0
           B3= 0.645497224367903D0
           C3= 0.D0
C
          IF (IND.EQ.1) THEN
C
            VF(1)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)*FTILD1
            VF(2)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)*FTILD1
            VF(3)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)*FTILD1
            VF(4)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)*FTILD1
            VF(5)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)*FTILD1
            VF(6)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)*FTILD1
            VF(7)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)*FTILD1
            VF(8)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)*FTILD1
            VF(9)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)*FTILD1
C
           VF(10)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)*FTILD2
           VF(11)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)*FTILD2
           VF(12)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)*FTILD2
           VF(13)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)*FTILD2
           VF(14)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)*FTILD2
           VF(15)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)*FTILD2
           VF(16)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)*FTILD2
           VF(17)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)*FTILD2
           VF(18)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)*FTILD2
C
           VF(19)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)*FTILD3
           VF(20)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)*FTILD3
           VF(21)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)*FTILD3
           VF(22)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)*FTILD3
           VF(23)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)*FTILD3
           VF(24)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)*FTILD3
           VF(25)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)*FTILD3
           VF(26)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)*FTILD3
           VF(27)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)*FTILD3
C
           VF(28)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)*FTILD4
           VF(29)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)*FTILD4
           VF(30)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)*FTILD4
           VF(31)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)*FTILD4
           VF(32)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)*FTILD4
           VF(33)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)*FTILD4
           VF(34)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)*FTILD4
           VF(35)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)*FTILD4
           VF(36)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)*FTILD4
C
           VF(37)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)*FTILD5
           VF(38)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)*FTILD5
           VF(39)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)*FTILD5
           VF(40)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)*FTILD5
           VF(41)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)*FTILD5
           VF(42)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)*FTILD5
           VF(43)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)*FTILD5
           VF(44)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)*FTILD5
           VF(45)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)*FTILD5
C
          ELSE IF (IND.EQ.0) THEN
            VF(1)= (A1*XI12+B1*XI1+C1)*(A1*XI22+B1*XI2+C1)
            VF(2)= (A2*XI12+B2*XI1+C2)*(A1*XI22+B1*XI2+C1)
            VF(3)= (A3*XI12+B3*XI1+C3)*(A1*XI22+B1*XI2+C1)
            VF(4)= (A3*XI12+B3*XI1+C3)*(A2*XI22+B2*XI2+C2)
            VF(5)= (A3*XI12+B3*XI1+C3)*(A3*XI22+B3*XI2+C3)
            VF(6)= (A2*XI12+B2*XI1+C2)*(A3*XI22+B3*XI2+C3)
            VF(7)= (A1*XI12+B1*XI1+C1)*(A3*XI22+B3*XI2+C3)
            VF(8)= (A1*XI12+B1*XI1+C1)*(A2*XI22+B2*XI2+C2)
            VF(9)= (A2*XI12+B2*XI1+C2)*(A2*XI22+B2*XI2+C2)
          ENDIF
C
         ELSEIF ( NOMTE(1:8) .EQ. 'MEC3TR7H' .OR.
     +            NOMTE(1:8) .EQ. 'MEGRC3T7' ) THEN
             A=0.470142064105115D0
             B=0.101286507323456D0
C
             C=((0.333333333333333D0-B)**2)*((0.333333333333333D0-A)**2)
C
             F1=(XI1-B)*(XI2-B)*(XI1-A)*(XI2-A)/C
C
             ALF2=-9.734528142543403D-02
             BET2= 0.292035844276301D0
             GAM2= 0.292035844276301D0
             DEL2= 5.644455828827410D0
             EX22=-0.961088342345189D0
             EY22=-0.961088342345189D0
C
             ALF3=-0.766397779494321D0
             BET3= 1.630140840414080D0
             GAM3= 7.566632513517790D0
             DEL3=-7.566632513517790D0
             EX23=-0.961088342345190D0
             EY23=-7.566632513517790D0
C
             ALF4=-0.766397779494321D0
             BET4= 7.566632513517790D0
             GAM4= 1.630140840414080D0
             DEL4=-7.566632513517790D0
             EX24=-7.566632513517790D0
             EY24=-0.961088342345189D0
C
             ALF5= 2.097345281425430D0
             BET5=-6.292035844276300D0
             GAM5=-6.292035844276300D0
             DEL5= 8.355544171172590D0
             EX25= 4.461088342345190D0
             EY25= 4.461088342345190D0
C
             ALF6= 0.266397779494322D0
             BET6=-2.630140840414080D0
             GAM6=-0.566632513517786D0
             DEL6= 0.566632513517786D0
             EX26= 4.461088342345190D0
             EY26= 0.566632513517786D0
C
             ALF7= 0.266397779494322D0
             BET7=-0.566632513517786D0
             GAM7=-2.630140840414080D0
             DEL7= 0.566632513517786D0
             EX27= 0.566632513517786D0
             EY27= 4.461088342345190D0
C
          IF (IND.EQ.1) THEN
         VF(1)=F1*FTILD1
         VF(2)=(ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22)
     &                                                           *FTILD1
         VF(3)=(ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22)
     &                                                           *FTILD1
         VF(4)=(ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22)
     &                                                           *FTILD1
         VF(5)=(ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22)
     &                                                           *FTILD1
         VF(6)=(ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22)
     &                                                           *FTILD1
         VF(7)=(ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22)
     &                                                           *FTILD1
C
         VF(8)=F1*FTILD2
         VF(9)=(ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22)
     &                                                           *FTILD2
        VF(10)=(ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22)
     &                                                           *FTILD2
        VF(11)=(ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22)
     &                                                           *FTILD2
        VF(12)=(ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22)
     &                                                           *FTILD2
        VF(13)=(ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22)
     &                                                           *FTILD2
        VF(14)=(ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22)
     &                                                           *FTILD2
C
        VF(15)=F1*FTILD3
        VF(16)=(ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22)
     &                                                           *FTILD3
        VF(17)=(ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22)
     &                                                           *FTILD3
        VF(18)=(ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22)
     &                                                           *FTILD3
        VF(19)=(ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22)
     &                                                           *FTILD3
        VF(20)=(ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22)
     &                                                           *FTILD3
        VF(21)=(ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22)
     &                                                           *FTILD3
C
        VF(22)=F1*FTILD4
        VF(23)=(ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22)
     &                                                           *FTILD4
        VF(24)=(ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22)
     &                                                           *FTILD4
        VF(25)=(ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22)
     &                                                           *FTILD4
        VF(26)=(ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22)
     &                                                           *FTILD4
        VF(27)=(ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22)
     &                                                           *FTILD4
        VF(28)=(ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22)
     &                                                           *FTILD4
C
        VF(29)=F1*FTILD5
        VF(30)=(ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22)
     &                                                           *FTILD5
        VF(31)=(ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22)
     &                                                           *FTILD5
        VF(32)=(ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22)
     &                                                           *FTILD5
        VF(33)=(ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22)
     &                                                           *FTILD5
        VF(34)=(ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22)
     &                                                           *FTILD5
        VF(35)=(ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22)
     &                                                           *FTILD5
C
          ELSE IF (IND.EQ.0) THEN
         VF(1)=F1
         VF(2)=ALF2+BET2*XI1+GAM2*XI2+DEL2*XI1*XI2+EX22*XI12+EY22*XI22
         VF(3)=ALF3+BET3*XI1+GAM3*XI2+DEL3*XI1*XI2+EX23*XI12+EY23*XI22
         VF(4)=ALF4+BET4*XI1+GAM4*XI2+DEL4*XI1*XI2+EX24*XI12+EY24*XI22
         VF(5)=ALF5+BET5*XI1+GAM5*XI2+DEL5*XI1*XI2+EX25*XI12+EY25*XI22
         VF(6)=ALF6+BET6*XI1+GAM6*XI2+DEL6*XI1*XI2+EX26*XI12+EY26*XI22
         VF(7)=ALF7+BET7*XI1+GAM7*XI2+DEL7*XI1*XI2+EX27*XI12+EY27*XI22
C
          ENDIF
C
        ENDIF
      ENDIF
      END
