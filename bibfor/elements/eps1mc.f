      SUBROUTINE EPS1MC ( NNO, NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +                    XYZ, DEPL, NHARM, EPS1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C
C      EPS1MC   -- CALCUL DES  DEFORMATIONS AUX POINTS D'INTEGRATION
C                  POUR LES ELEMENTS ISOPARAMETRIQUES 
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    IVF            IN     I        POINTEUR FONCTIONS DE FORME
C    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
C    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR 
C                                   L'ELEMENT 
C    NHARM          IN     R        NUMERO D'HARMONIQUE
C    EPS1(1)        OUT    R        DEFORMATIONS DU PREMIER ORDRE
C                                   AUX POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8       XYZ(1), DEPL(1), EPS1(1)
           REAL*8       NHARM
C -----  VARIABLES LOCALES
           REAL*8       B(486), JACGAU
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
C-----------------------------------------------------------------------
      INTEGER I ,IDFDE ,IGAU ,IPOIDS ,IVF ,J ,NBINCO 
      INTEGER NBSIG ,NDIM ,NNO ,NPG 
      REAL*8 S ,UNDEMI ,ZERO 
C-----------------------------------------------------------------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      NBINCO = NDIM*NNO
C
      DO 10 I = 1, NBSIG*NPG
         EPS1(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES DEFORMATIONS AUX POINTS D'INTEGRATION
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
      DO 20 IGAU = 1, NPG 
C
C  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU 
C  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION 
C  --      COURANT : (EPS_1) = (B)*(UN)
C          ----------------------------
          CALL BMATMC(IGAU, NBSIG, XYZ, IPOIDS,IVF,IDFDE,
     +                NNO, NHARM, JACGAU, B)
C
C ---      CALCUL DU VECTEUR DES COMPOSANTES DU TENSEUR DES
C ---      DEFORMATIONS AU POINT D'INTEGRATION COURANT
C          -------------------------------------------
          DO 30 I = 1, NBSIG 
C
             S = ZERO
C
             DO 40 J = 1, NBINCO
               S = S + DEPL(J)*B((J-1)*NBSIG+I)
  40         CONTINUE
C
             EPS1(NBSIG*(IGAU-1)+I) = S
  30      CONTINUE
C
          DO 50 I = 4, NBSIG 
             EPS1(NBSIG*(IGAU-1)+I) = UNDEMI*EPS1(NBSIG*(IGAU-1)+I)
  50      CONTINUE
C
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
