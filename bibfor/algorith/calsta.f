      SUBROUTINE CALSTA(PROJ,GAMMA,DH,DEF,NNO,KPG,SIG,TMP,KK,
     &                  KKD,MATUU,DSIDEP,JAC)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/04/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C-----------------------------------------------------------------------
C     CALCUL DES TERMES DE STABILISATION POUR LE QUAD4 SOUS INTEGRE
C     STABILISE PAR LA METHODE ASSUMED STRAIN => QUAS4
C-----------------------------------------------------------------------

      IMPLICIT NONE
    
      INTEGER KPG,KK,KKD,N,I,M,J,KL,NNO,J1,IFILTR,PROJ
      REAL*8 DSIDEP(6,6),F(3,3)
      REAL*8 TMP,SIG(6)
      REAL*8 GAMMA(4),DH(8)
      REAL*8 KRON(3,3),KRON2(2,3),KRON3(2,3)
      REAL*8 FILTR1(2,3),FILTR2(2,3),JAC
      REAL*8 MATUU(*),DEFST1(4,4,2)
      REAL*8 DEFST2(4,4,2),DEF(4,4,2)
      REAL*8 RAC2

      DATA KRON/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/
      DATA KRON2/1.D0,0.D0,1.D0,0.D0,0.D0,1.D0/
      DATA KRON3/1.D0,0.D0,0.D0,1.D0,1.D0,0.D0/


C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 OPTIMAL BENDING
C           2 INCOMPRESSIBLE

      RAC2 = SQRT(2.D0)

      DO 20 I = 1,3
        DO 10 J = 1,3
          F(I,J) = KRON(I,J)
   10   CONTINUE
   20 CONTINUE



      DO 40 I = 1,2
        DO 30 J = 1,3
          FILTR1(I,J) = KRON2(I,J)
          FILTR2(I,J) = KRON3(I,J)
   30   CONTINUE
   40 CONTINUE

      DO 120 IFILTR = 1,3
        DO 60 N = 1,NNO
          DO 50 I = 1,2

C         QUAS4 SANS PROJECTION
C         ---------------------
            IF (PROJ.EQ.0) THEN

              DEFST1(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*
     &                        FILTR1(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR1(2,IFILTR)

              DEFST1(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*
     &                        FILTR1(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR1(2,IFILTR)

              DEFST1(3,N,I) = 0.D0

              DEFST1(4,N,I) = (FILTR1(1,IFILTR)*
     &                        (F(I,1)*GAMMA(N)*DH(2*KPG)+F(I,
     &                        2)*GAMMA(N)*DH(2*KPG-1))/RAC2) +
     &                        DEF(4,N,I)*FILTR1(2,IFILTR)



              DEFST2(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*
     &                        FILTR2(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR2(2,IFILTR)

              DEFST2(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*
     &                        FILTR2(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR2(2,IFILTR)

              DEFST2(3,N,I) = 0.D0


              DEFST2(4,N,I) = (FILTR2(1,IFILTR)*
     &                        (F(I,1)*GAMMA(N)*DH(2*KPG)+F(I,
     &                        2)*GAMMA(N)*DH(2*KPG-1))/RAC2) +
     &                        DEF(4,N,I)*FILTR2(2,IFILTR)

C         OPTIMAL BENDING
C         ---------------
            ELSE IF (PROJ.EQ.1) THEN

              DEFST1(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*
     &                        FILTR1(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR1(2,IFILTR)

              DEFST1(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*
     &                        FILTR1(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR1(2,IFILTR)

              DEFST1(3,N,I) = 0.D0

              DEFST1(4,N,I) = DEF(4,N,I)*FILTR1(2,IFILTR)



              DEFST2(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*
     &                        FILTR2(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR2(2,IFILTR)

              DEFST2(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*
     &                        FILTR2(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR2(2,IFILTR)

              DEFST2(3,N,I) = 0.D0


              DEFST2(4,N,I) = DEF(4,N,I)*FILTR2(2,IFILTR)


C         INCOMPRESSIBLE
C         -------------- 
            ELSE IF (PROJ.EQ.2) THEN

              DEFST1(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*0.5D0*
     &                        FILTR1(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR1(2,IFILTR) + F(I,2)* (-0.5D0)*
     &                        DH(2*KPG)*GAMMA(N)

              DEFST1(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*0.5D0*
     &                        FILTR1(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR1(2,IFILTR) + F(I,1)* (-0.5D0)*
     &                        DH(2*KPG-1)*GAMMA(N)

              DEFST1(3,N,I) = 0.D0

              DEFST1(4,N,I) = DEF(4,N,I)*FILTR1(2,IFILTR)



              DEFST2(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPG-1)*0.5D0*
     &                        FILTR2(1,IFILTR) + DEF(1,N,I)*
     &                        FILTR2(2,IFILTR) + F(I,2)*GAMMA(N)*
     &                        DH(2*KPG)* (-0.5D0)

              DEFST2(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPG)*0.5D0*
     &                        FILTR2(1,IFILTR) + DEF(2,N,I)*
     &                        FILTR2(2,IFILTR) + F(I,1)*GAMMA(N)*
     &                        DH(2*KPG-1)* (-0.5D0)

              DEFST2(3,N,I) = 0.D0

              DEFST2(4,N,I) = DEF(4,N,I)*FILTR2(2,IFILTR)

            END IF
   50     CONTINUE
   60   CONTINUE


C - CALCUL DE LA MATRICE DE RIGIDITE


        DO 110 N = 1,NNO
          DO 100 I = 1,2
            DO 70,KL = 1,4
              SIG(KL) = 0.D0
              SIG(KL) = SIG(KL) + DEFST1(1,N,I)*DSIDEP(1,KL)
              SIG(KL) = SIG(KL) + DEFST1(2,N,I)*DSIDEP(2,KL)
              SIG(KL) = SIG(KL) + DEFST1(3,N,I)*DSIDEP(3,KL)
              SIG(KL) = SIG(KL) + DEFST1(4,N,I)*DSIDEP(4,KL)
   70       CONTINUE

            DO 90 J = 1,2
              DO 80 M = 1,N
                IF (M.EQ.N) THEN
                  J1 = I
                ELSE
                  J1 = 2
                END IF

C                 RIGIDITE ELASTIQUE
                TMP = 0.D0
                TMP = TMP + SIG(1)*DEFST2(1,M,J)
                TMP = TMP + SIG(2)*DEFST2(2,M,J)
                TMP = TMP + SIG(3)*DEFST2(3,M,J)
                TMP = TMP + SIG(4)*DEFST2(4,M,J)

C                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                IF (J.LE.J1) THEN
                  KKD = (2* (N-1)+I-1)* (2* (N-1)+I)/2
                  KK = KKD + 2* (M-1) + J
                  MATUU(KK) = MATUU(KK) + TMP*JAC
                END IF

   80         CONTINUE
   90       CONTINUE
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE

      END
