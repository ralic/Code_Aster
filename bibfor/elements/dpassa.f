      SUBROUTINE  DPASSA(XYZGAU, REPERE, IREP, PASSAG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/01/96   AUTEUR CIBHHGB G.BERTRAND 
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
C      DPASSA  -- CALCUL DE LA MATRICE DE PASSAGE DU REPERE
C                 D'ORTHOTROPIE AU REPERE GLOBAL POUR LE 
C                 TENSEUR D'ELASTICITE
C                 CETTE MATRICE EST CONSTRUITE EN PARTANT
C                 DE LA CONSIDERATION QUE L'ENERGIE DE DEFORMATION
C                 EXPRIMEE DANS LE REPERE GLOBAL EST EGALE A
C                 L'ENERGIE DE DEFORMATION EXPRIMEE DANS LE REPERE
C                 D'ORTHOTROPIE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
C                                   COURANT
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    IREP           OUT    I        = 0 
C                                     SI LE CHANGEMENT DE REPERE EST
C                                     TRIVIAL (I.E. PASSAG = IDENTITE)
C                                   = 1 SINON
C    PASSAG(6,6)    OUT    R        MATRICE DE PASSAGE DU REPERE
C                                   D'ORTHOTROPIE AU REPERE GLOBAL
C                                   POUR LE TENSEUR D'ELASTICITE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8       REPERE(7), XYZGAU(3), PASSAG(6,6)
C -----  VARIABLES LOCALES
           REAL*8 ANGL(3), P(3,3), DIRE(3), ORIG(3)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      DEUX   = 2.0D0
      IREP   = 0
C
      DO 10 I = 1, 3
      DO 10 J = 1, 3
         P(I,J)     = ZERO
 10   CONTINUE
C
C ---- CAS OU LE REPERE D'ORTHOTROPIE EST DEFINI PAR 3 ANGLES NAUTIQUES
C      ----------------------------------------------------------------
      IF (REPERE(1).GT.ZERO) THEN
C
          ANGL(1) = REPERE(2)
          ANGL(2) = REPERE(3)
          ANGL(3) = REPERE(4)
C
          IF (ANGL(1).EQ.ZERO.AND.ANGL(2).EQ.ZERO.AND.ANGL(3).EQ.ZERO)
     +     THEN
              IREP = 0
          ELSE
C
C ----     CONSTRUCTION DE LA MATRICE DE PASSAGE (POUR DES VECTEURS)
C ----     DU REPERE D'ORTHOTROPIE AU REPERE GLOBAL
C          ----------------------------------------
              CALL MATROT(ANGL, P)
              IREP = 1
          ENDIF
C
C ---- CAS OU LE REPERE D'ORTHOTROPIE EST DEFINI COMME SUIT :
C ----    LA DIRECTION D'ORTHOTROPIE EST DEFINIE PAR 2 ANGLES
C ----    CETTE DIRECTION EST EN OUTRE CELLE D'UN AXE AUTOUR DUQUEL
C ----    LA PARTIE DE LA STRUCTURE CONSIDEREE EST AXISYMETRIQUE,
C ----    CET AXE EST DEFINI PAR LA DONNEE SUPPLEMENTAIRE D'UN POINT
C ----    QUI LUI APPARTIENT
C      ----------------------------------------------------------------
      ELSE
C
          DIRE(1) = REPERE(2)
          DIRE(2) = REPERE(3)
          DIRE(3) = REPERE(4) 
C
          ORIG(1) = REPERE(5)     
          ORIG(2) = REPERE(6)     
          ORIG(3) = REPERE(7) 
C
C ---- CONSTRUCTION DE LA MATRICE DE PASSAGE (POUR DES VECTEURS)
C ---- DU REPERE D'ORTHOTROPIE AU REPERE GLOBAL
C      ----------------------------------------
           CALL UTRCYL(XYZGAU,DIRE,ORIG,P)
           IREP = 1    
      ENDIF
C
C ---- CONSTRUCTION DE LA MATRICE DE PASSAGE  POUR LE TENSEUR
C ---- D'ELASTICITE (QUI EST DU QUATRIEME ORDRE) DU REPERE
C ---- D'ORTHOTROPIE AU REPERE GLOBAL. 
C ---- CETTE MATRICE EST CONSTRUITE EN PARTANT DE LA CONSIDERATION QUE
C ----  (SIGMA_GLOB):(EPSILON_GLOB) = (SIGMA_ORTH):(EPSILON_ORTH)
C       ---------------------------------------------------------
      IF (IREP.EQ.1) THEN
C
          PASSAG(1,1) = P(1,1)*P(1,1)
          PASSAG(1,2) = P(1,2)*P(1,2)
          PASSAG(1,3) = P(1,3)*P(1,3)
          PASSAG(1,4) = P(1,1)*P(1,2)
          PASSAG(1,5) = P(1,1)*P(1,3)
          PASSAG(1,6) = P(1,2)*P(1,3)
C
          PASSAG(2,1) = P(2,1)*P(2,1)
          PASSAG(2,2) = P(2,2)*P(2,2)
          PASSAG(2,3) = P(2,3)*P(2,3)
          PASSAG(2,4) = P(2,1)*P(2,2)
          PASSAG(2,5) = P(2,1)*P(2,3)
          PASSAG(2,6) = P(2,2)*P(2,3)
C
          PASSAG(3,1) = P(3,1)*P(3,1)
          PASSAG(3,2) = P(3,2)*P(3,2)
          PASSAG(3,3) = P(3,3)*P(3,3)
          PASSAG(3,4) = P(3,1)*P(3,2)
          PASSAG(3,5) = P(3,1)*P(3,3)
          PASSAG(3,6) = P(3,2)*P(3,3)
C
          PASSAG(4,1) = DEUX*P(1,1)*P(2,1)
          PASSAG(4,2) = DEUX*P(1,2)*P(2,2)
          PASSAG(4,3) = DEUX*P(1,3)*P(2,3)
          PASSAG(4,4) = (P(1,1)*P(2,2) + P(1,2)*P(2,1))
          PASSAG(4,5) = (P(1,1)*P(2,3) + P(1,3)*P(2,1))
          PASSAG(4,6) = (P(1,2)*P(2,3) + P(1,3)*P(2,2))
C
          PASSAG(5,1) = DEUX*P(1,1)*P(3,1)
          PASSAG(5,2) = DEUX*P(1,2)*P(3,2)
          PASSAG(5,3) = DEUX*P(1,3)*P(3,3)
          PASSAG(5,4) = P(1,1)*P(3,2) + P(1,2)*P(3,1)
          PASSAG(5,5) = P(1,1)*P(3,3) + P(1,3)*P(3,1)
          PASSAG(5,6) = P(1,2)*P(3,3) + P(1,3)*P(3,2)
C
          PASSAG(6,1) = DEUX*P(2,1)*P(3,1)
          PASSAG(6,2) = DEUX*P(2,2)*P(3,2)
          PASSAG(6,3) = DEUX*P(2,3)*P(3,3)
          PASSAG(6,4) = P(2,1)*P(3,2) + P(2,2)*P(3,1)
          PASSAG(6,5) = P(2,1)*P(3,3) + P(2,3)*P(3,1)
          PASSAG(6,6) = P(2,2)*P(3,3) + P(3,2)*P(2,3)
C
      ENDIF
C.============================ FIN DE LA ROUTINE ======================
      END
