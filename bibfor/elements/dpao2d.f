      SUBROUTINE  DPAO2D(REPERE, IREP, PASSAG)
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
C      DPAO2D  -- CALCUL DE LA MATRICE DE PASSAGE DU REPERE
C                 D'ORTHOTROPIE AU REPERE GLOBAL POUR LE 
C                 TENSEUR D'ELASTICITE EN 2D
C
C   ARGUMENT        E/S  TYPE         ROLE
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
           REAL*8       REPERE(7), PASSAG(4,4)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      IREP   = 0
C
      ANGL = REPERE(2)
C
      IF (ANGL.EQ.ZERO) THEN
C
          IREP = 0
      ELSE
C
          COSA = COS(ANGL)
          SINA = SIN(ANGL)
          IREP = 1
C
C ---- CONSTRUCTION DE LA MATRICE DE PASSAGE  POUR LE TENSEUR
C ---- D'ELASTICITE (QUI EST DU QUATRIEME ORDRE) DU REPERE
C ---- D'ORTHOTROPIE AU REPERE GLOBAL. 
C       ------------------------------
C
          PASSAG(1,1) = COSA*COSA
          PASSAG(2,1) = SINA*SINA
          PASSAG(3,1) = ZERO
          PASSAG(4,1) =-DEUX*COSA*SINA
C
          PASSAG(1,2) = SINA*SINA
          PASSAG(2,2) = COSA*COSA
          PASSAG(3,2) = ZERO
          PASSAG(4,2) = DEUX*SINA*COSA
C
          PASSAG(1,3) = ZERO
          PASSAG(2,3) = ZERO
          PASSAG(3,3) = UN
          PASSAG(4,3) = ZERO
C
          PASSAG(1,4) = SINA*COSA
          PASSAG(2,4) =-SINA*COSA
          PASSAG(3,4) = ZERO
          PASSAG(4,4) = COSA*COSA - SINA*SINA
C
      ENDIF
C.============================ FIN DE LA ROUTINE ======================
      END
