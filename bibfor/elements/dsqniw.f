      SUBROUTINE DSQNIW ( INT , R , DCI , BCM, BCB , BCA , AN , AM, 
     +                    WSQ, WMESQ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/12/2000   AUTEUR CIBHHGB G.BERTRAND 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C  
C  DSQNIW -- CALCUL DES FONCTIONS DE FORME CUBIQUES RELATIVES A 
C            LA FLECHE W DANS LE CADRE DU CALCUL DE LA MATRICE
C            DE MASSE POUR LES ELEMENTS DSQ .
C            POUR LA RIGIDITE CES FONCTIONS SONT LINEAIRES,
C            POUR LA MASSE ON CHOISIT DES FONCTIONS CUBIQUES
C            EN CONSIDERANT QUE L'ELEMENT EST DE TYPE HERMITE
C            (ICI CUBIQUE INCOMPLET) , SOIT :
C            W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA)
C            ON UTILISE LE FAIT QUE :
C                (D W)/(D X) = GAMMA_X - BETA_X
C            ET  (D W)/(D Y) = GAMMA_Y - BETA_Y
C
C            D'AUTRE PART , ON SAIT QUE :
C            | GAMMA_X |           | TX |
C            | GAMMA_Y | = [DCI] * | TY | = [DCI] * (T)
C
C            ET ENFIN (T) = [BCA] * (ALPHA) + [BCB]
C            AVEC (ALPHA) = [AN]*(UN)   DANS LE CAS NON EXCENTRE
C              ET (ALPHA) = [AN]*(UN) + [AM]*(UM) DANS LE CAS EXCENTRE
C
C            ON ABOUTIT A L'EXPRESSION :
C            W = WST*UN + WMESQ*UM
C             
C            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  
C            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
C                                     
C   ARGUMENT        E/S  TYPE         ROLE
C    INT            IN    I       INDICE DU POINT D'INTEGRATION
C    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
C                                 GEOMETRIQUES DE L'ELEMENT :
C                                 COS ET SIN DES ANGLES, LONGUEUR
C                                 DES COTES ,...
C    DCI(2,2)       IN    R       INVERSE DE LA MATRICE DE 
C                                 CISAILLEMENT
C    BCM(2,8)       IN    R       MATRICE RESULTANT DE LA DERIVATION
C                                 SECONDE DES FONCTIONS DE FORME
C                                 RELATIVES AUX DEPLACEMENTS EN 
C                                 MEMBRANE ET APPARAISSANT DANS
C                                 L'EXPRESSION DES EFFORTS TRANCHANTS 
C                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
C                                     + [BCM]*(UM)
C    BCB(2,12)      IN    R       MATRICE RESULTANT DE LA DERIVATION
C                                 SECONDE DES FONCTIONS DE FORME
C                                 DES ROTATIONS ET APPARAISSANT DANS
C                                 L'EXPRESSION DES EFFORTS TRANCHANTS 
C                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
C                                     + [BCM]*(UM)
C    BCA(2,4)       IN    R       MATRICE RELIANT LES EFFORTS
C                                 TRANCHANTS AUX ROTATIONS ALPHA 
C                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
C                                     + [BCM]*(UM)
C    AN(4,12)       IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(4,8)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C    WSQ(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 W = WSQ*UN (+ WMESQ*UM)
C    WMESQ(8)       OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 W = WMESQ*UM (+ WSQ*UN)
C
C -----  ARGUMENTS
      INTEGER   INT
      REAL*8    R(*)
      REAL*8    DCI(2,2), BCA(2,4), BCB(2,12), AN(4,12), AM(4,8)
      REAL*8    BCM(2,8)
      REAL*8    WSQ(12),   WMESQ(8)
C -----  VARIABLES LOCALES
      REAL*8  BN(2,12) , DBA(2,12), DB(2,4), DBAM(2,8), DCM(2,8)
      REAL*8  QSI,ETA , PQSI,MQSI , PETA,META , QSIC,ETAC
      REAL*8  X5,X6,X7,X8 , Y5,Y6,Y7,Y8
      REAL*8  N(12)
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
C     ------------------------------------------------------------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      HUIT   = 8.0D0
C
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
      X5  = R(LXYC)
      X6  = R(LXYC+1)
      X7  = R(LXYC+2)
      X8  = R(LXYC+3)
      Y5  = R(LXYC+4)
      Y6  = R(LXYC+5)
      Y7  = R(LXYC+6)
      Y8  = R(LXYC+7)
C
      DO 10 I = 1, 8
        WMESQ(I) = ZERO
  10  CONTINUE
C
      PETA = UN + ETA
      META = UN - ETA
      PQSI = UN + QSI
      MQSI = UN - QSI
      ETAC = UN - ETA * ETA
      QSIC = UN - QSI * QSI
C
C --- FONCTIONS DE FORME RELATIVES A LA FLECHE CORRESPONDANTES
C --- A L'INTERPOLATION DE TYPE HERMITE :
C ---     W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA) :
C     -----------------------------------------------------------
      N(1)  =   MQSI * META / HUIT * (QSIC + ETAC - QSI - ETA)
      N(2)  =   MQSI * META / HUIT * QSIC
      N(3)  =   MQSI * META / HUIT * ETAC
      N(4)  =   PQSI * META / HUIT * (QSIC + ETAC + QSI - ETA)
      N(5)  = - PQSI * META / HUIT * QSIC
      N(6)  =   PQSI * META / HUIT * ETAC
      N(7)  =   PQSI * PETA / HUIT * (QSIC + ETAC + QSI + ETA)
      N(8)  = - PQSI * PETA / HUIT * QSIC
      N(9)  = - PQSI * PETA / HUIT * ETAC
      N(10) =   MQSI * PETA / HUIT * (QSIC + ETAC - QSI + ETA)
      N(11) =   MQSI * PETA / HUIT * QSIC
      N(12) = - MQSI * PETA / HUIT * ETAC
C
C --- CALCUL DE (GAMMA) = [DCI]*(T)
C --- SOIT      (GAMMA) = [DCI]*([BCA]*[AN]*(UN) + [BCB])
C ---                       S'IL N'Y A PAS D'EXCENTREMENT
C ---                                           
C ---           (GAMMA) = [DCI]*([BCA]*([AN]*(UN) +[AM]*(UM)) + [BCB])
C ---                            SI LA PLAQUE EST EXCENTREE
C ---   EN FAIT ON CALCULE [DCI]*([BCA]*[AN] + [BCB])  
C ---                   ET [DCI]*[BCA]*[AM] :
C       ==================================
C
C ---   CALCUL DE  [DCI]*([BCA]*[AN] + [BCB]) :
C       -------------------------------------
      DO 20 I = 1, 2
         DO 20 J = 1, 12
            BN(I,J) = ZERO
  20  CONTINUE
      DO 30 J = 1, 12
         DO 40 K = 1, 4
            BN(1,J) = BN(1,J) + BCA(1,K)*AN(K,J)
            BN(2,J) = BN(2,J) + BCA(2,K)*AN(K,J)
 40   CONTINUE
            BN(1,J) = BN(1,J) + BCB(1,J)
            BN(2,J) = BN(2,J) + BCB(2,J)
 30   CONTINUE
      DO 50 J = 1, 12
         DBA(1,J) = DCI(1,1)*BN(1,J) + DCI(1,2)*BN(2,J)
         DBA(2,J) = DCI(2,1)*BN(2,J) + DCI(2,2)*BN(2,J)
 50   CONTINUE
C
C ---   FONCTIONS D'INTERPOLATION WST RELATIVES AUX DDLS DE FLEXION
C ---   W, BETA_X ET BETA_Y :
C       -------------------
      DO 60 J = 1, 12
        WSQ(J) = (- DBA(1,J)*X5 + DBA(2,J)*X8) * N(2)
     &         + (- DBA(1,J)*Y5 + DBA(2,J)*Y8) * N(2)
     &         + (- DBA(1,J)*X5 - DBA(2,J)*X6) * N(5)
     &         + (- DBA(1,J)*Y5 - DBA(2,J)*Y6) * N(5)
     &         + (  DBA(1,J)*X7 - DBA(2,J)*X6) * N(8)
     &         + (  DBA(1,J)*Y7 - DBA(2,J)*Y6) * N(8)
     &         + (  DBA(1,J)*X7 + DBA(2,J)*X8) * N(11)
     &         + (  DBA(1,J)*Y7 + DBA(2,J)*Y8) * N(11)
 60   CONTINUE
C
      WSQ(1)  = WSQ(1)  + N(1)
      WSQ(2)  = WSQ(2)  + (- X5*N(2)  + X8*N(3) ) * UNDEMI
      WSQ(3)  = WSQ(3)  + (- Y5*N(2)  + Y8*N(3) ) * UNDEMI
      WSQ(4)  = WSQ(4)  + N(4)
      WSQ(5)  = WSQ(5)  + (- X5*N(5)  - X6*N(6) ) * UNDEMI
      WSQ(6)  = WSQ(6)  + (- Y5*N(5)  - Y6*N(6) ) * UNDEMI
      WSQ(7)  = WSQ(7)  + N(7)
      WSQ(8)  = WSQ(8)  + (  X7*N(8)  - X6*N(9) ) * UNDEMI
      WSQ(9)  = WSQ(9)  + (  Y7*N(8)  - Y6*N(9) ) * UNDEMI
      WSQ(10) = WSQ(10) + N(10)
      WSQ(11) = WSQ(11) + (  X7*N(11) + X8*N(12)) * UNDEMI
      WSQ(12) = WSQ(12) + (  Y7*N(11) + Y8*N(12)) * UNDEMI
C
C ---   CALCUL DE  [DCI]*([BCA]*[AM]+[BCM]) :
C       -----------------------------------
      DO 70 J = 1, 4
         DB(1,J) = DCI(1,1)*BCA(1,J) + DCI(1,2)*BCA(2,J)
         DB(2,J) = DCI(2,1)*BCA(1,J) + DCI(2,2)*BCA(2,J)
 70   CONTINUE
      DO 80 J = 1, 8
         DBAM(1,J) = DB(1,1)*AM(1,J) + DB(1,2)*AM(2,J) + 
     +               DB(1,3)*AM(3,J) + DB(1,4)*AM(4,J)
         DBAM(2,J) = DB(2,1)*AM(1,J) + DB(2,2)*AM(2,J) + 
     +               DB(2,3)*AM(3,J) + DB(2,4)*AM(4,J)
 80   CONTINUE
      DO 90 J = 1, 8
         DCM(1,J) = DCI(1,1)*BCM(1,J) + DCI(1,2)*BCM(2,J)
         DCM(2,J) = DCI(2,1)*BCM(1,J) + DCI(2,2)*BCM(2,J)
 90   CONTINUE
      DO 100 J = 1, 8
         DBAM(1,J) = DBAM(1,J) + DCM(1,J)
         DBAM(2,J) = DBAM(2,J) + DCM(2,J)
 100  CONTINUE
C
C ---   FONCTIONS D'INTERPOLATION WMESQ RELATIVES AUX DDLS DE 
C ---   MEMBRANE U ET V :
C       ---------------
      DO 110 J = 1, 8
        WMESQ(J) = (- DBAM(1,J)*X5 + DBAM(2,J)*X8) * N(2)
     &           + (- DBAM(1,J)*Y5 + DBAM(2,J)*Y8) * N(2)
     &           + (- DBAM(1,J)*X5 - DBAM(2,J)*X6) * N(5)
     &           + (- DBAM(1,J)*Y5 - DBAM(2,J)*Y6) * N(5)
     &           + (  DBAM(1,J)*X7 - DBAM(2,J)*X6) * N(8)
     &           + (  DBAM(1,J)*Y7 - DBAM(2,J)*Y6) * N(8)
     &           + (  DBAM(1,J)*X7 + DBAM(2,J)*X8) * N(11)
     &           + (  DBAM(1,J)*Y7 + DBAM(2,J)*Y8) * N(11)
 110  CONTINUE
C
      END
