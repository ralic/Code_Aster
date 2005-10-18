      SUBROUTINE DSTNIW (QSI, ETA, CARAT3, DCI, BCA, AN, AM, WST, WMEST)
      IMPLICIT  NONE
      REAL*8    QSI, ETA, CARAT3(*), DCI(2,2), BCA(2,3), AN(3,9)
      REAL*8    AM(3,6), WST(9), WMEST(6)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C  
C  DSTNIW -- CALCUL DES FONCTIONS DE FORME CUBIQUES RELATIVES A 
C            LA FLECHE W DANS LE CADRE DU CALCUL DE LA MATRICE
C            DE MASSE POUR LES ELEMENTS DST .
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
C            ET ENFIN (T) = [BCA] * (ALPHA)
C            AVEC (ALPHA) = [AN]*(UN)   DANS LE CAS NON EXCENTRE
C              ET (ALPHA) = [AN]*(UN) + [AM]*(UM) DANS LE CAS EXCENTRE
C
C            ON ABOUTIT A L'EXPRESSION :
C            W = WST*UN + WMEST*UM
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
C    BCA(2,3)       IN    R       MATRICE RELIANT LES EFFORTS
C                                 TRANCHANTS AUX ROTATIONS ALPHA 
C                                 (T) = [BCA]*(ALPHA)
C    AN(3,9)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE FLEXION UN 
C    AM(3,6)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
C                                 AUX INCONNUES DE MEMBRANE UM
C    WST(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 W = WST*UN (+ WMEST*UM)
C    WMEST(6)       OUT   R       FONCTIONS DE FORME TELLES QUE
C                                 W = WMEST*UM (+ WST*UN)
C     ------------------------------------------------------------------
      INTEGER  I , J
      REAL*8   DB(2,3) , DBA(2,9) , DBAM(2,6) , N(9)
      REAL*8   LBD , ZERO,  X4,X6 , Y4,Y6
C     ------------------------------------------------------------------
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      ZERO = 0.0D0
C
      X4  = CARAT3(1)
      X6  = CARAT3(3)
      Y4  = CARAT3(4)
      Y6  = CARAT3(6)
C
      DO 10 I = 1, 6
        WMEST(I) = ZERO
  10  CONTINUE
C
      LBD = 1.D0 - QSI - ETA
C
C --- FONCTIONS DE FORME RELATIVES A LA FLECHE CORRESPONDANTES
C --- A L'INTERPOLATION DE TYPE HERMITE :
C ---     W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA) :
C     -----------------------------------------------------------
      N(1) = LBD*LBD * (3.D0 - 2.D0*LBD) + QSI*ETA*LBD * 2.D0
      N(2) = LBD*LBD *       QSI         + QSI*ETA*LBD / 2.D0
      N(3) = LBD*LBD *       ETA         + QSI*ETA*LBD / 2.D0
      N(4) = QSI*QSI * (3.D0 - 2.D0*QSI) + QSI*ETA*LBD * 2.D0
      N(5) = QSI*QSI *   (-1.D0 + QSI)   - QSI*ETA*LBD
      N(6) = QSI*QSI *       ETA         + QSI*ETA*LBD / 2.D0
      N(7) = ETA*ETA * (3.D0 - 2.D0*ETA) + QSI*ETA*LBD * 2.D0
      N(8) = ETA*ETA *       QSI         + QSI*ETA*LBD / 2.D0
      N(9) = ETA*ETA *   (-1.D0 + ETA)   - QSI*ETA*LBD
C
C --- CALCUL DE (GAMMA) = [DCI]*(T)
C --- SOIT      (GAMMA) = [DCI]*[BCA]*[AN]*(UN) S'IL N'Y A PAS
C ---                                           D'EXCENTREMENT
C ---           (GAMMA) = [DCI]*[BCA]*([AN]*(UN) +[AM]*(UM))
C ---                            SI LA PLAQUE EST EXCENTREE
C ---   EN FAIT ON CALCULE [DCI]*[BCA]*[AN] ET [DCI]*[BCA]*[AM] :
C       =======================================================
C
C ---   CALCUL DE  [DCI]*[BCA]*[AN] :
C       ---------------------------
      DO 20 J = 1, 3
         DB(1,J) = DCI(1,1)*BCA(1,J) + DCI(1,2)*BCA(2,J)
         DB(2,J) = DCI(2,1)*BCA(1,J) + DCI(2,2)*BCA(2,J)
 20   CONTINUE
      DO 30 J = 1, 9
         DBA(1,J) = DB(1,1)*AN(1,J) + DB(1,2)*AN(2,J) + DB(1,3)*AN(3,J)
         DBA(2,J) = DB(2,1)*AN(1,J) + DB(2,2)*AN(2,J) + DB(2,3)*AN(3,J)
 30   CONTINUE
C
C ---   CALCUL DE  [DCI]*[BCA]*[AM] :
C       ---------------------------
      DO 40 J = 1, 6
         DBAM(1,J) = DB(1,1)*AM(1,J) + DB(1,2)*AM(2,J) + DB(1,3)*AM(3,J)
         DBAM(2,J) = DB(2,1)*AM(1,J) + DB(2,2)*AM(2,J) + DB(2,3)*AM(3,J)
 40   CONTINUE
C
C ---   FONCTIONS D'INTERPOLATION WST RELATIVES AUX DDLS DE FLEXION
C ---   W, BETA_X ET BETA_Y :
C       -------------------
      DO 50 J = 1, 9
        WST(J) = (DBA(1,J)*X4 + DBA(2,J)*Y4) * (N(2) + N(5) + N(8))
     +         - (DBA(1,J)*X6 + DBA(2,J)*Y6) * (N(3) + N(6) + N(9))
 50   CONTINUE
C
      WST(1) = WST(1) + N(1)
      WST(2) = WST(2) - N(2)*X4 + N(3)*X6
      WST(3) = WST(3) - N(2)*Y4 + N(3)*Y6
      WST(4) = WST(4) + N(4)
      WST(5) = WST(5) - N(5)*X4 + N(6)*X6
      WST(6) = WST(6) - N(5)*Y4 + N(6)*Y6
      WST(7) = WST(7) + N(7)
      WST(8) = WST(8) - N(8)*X4 + N(9)*X6
      WST(9) = WST(9) - N(8)*Y4 + N(9)*Y6
C
C ---   FONCTIONS D'INTERPOLATION WMEST RELATIVES AUX DDLS DE 
C ---   MEMBRANE U ET V :
C       ---------------
      DO 60 J = 1, 6
        WMEST(J) = (DBAM(1,J)*X4 + DBAM(2,J)*Y4) * (N(2) + N(5) + N(8))
     +           - (DBAM(1,J)*X6 + DBAM(2,J)*Y6) * (N(3) + N(6) + N(9))
 60   CONTINUE
C
      END
