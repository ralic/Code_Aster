      SUBROUTINE ETHDST (MODELI,NNO,NDIM,NBSIG,NPG,NI,DNIDX,DNIDY,DNIDZ,
     +                   POIDS,XYZ,DEPL,TEMPE,TREF,INSTAN,REPERE,MATER,
     +                   OPTION,ENTHTH)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2002   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C      ETHDST   -- CALCUL DU TERME EPSTHT*D*EPSTH RENTRANT
C                  DANS LE CALCUL DE L'ENERGIE POTENTIELLE
C                  (I.E.  1/2*UT*K*U - UT*FTH + 1/2*EPSTHT*D*EPSTH)
C                  POUR LES ELEMENTS ISOPARAMETRIQUES                  
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI, FOURIER,...)
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    NI(1)          IN     R        FONCTIONS DE FORME
C    DNIDX(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / X SUR L'ELEMENT DE REFERENCE
C    DNIDY(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / Y SUR L'ELEMENT DE REFERENCE
C    DNIDZ(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / Z SUR L'ELEMENT DE REFERENCE
C    POIDS(1)       IN     R        POIDS D'INTEGRATION
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
C                                   L'ELEMENT
C    TEMPE(1)       IN     R        TEMPERATURES AUX NOEUDS DE
C                                   L'ELEMENT
C    TREF           IN     R        TEMPERATURE DE REFERENCE
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    MATER          IN     I        MATERIAU
C    OPTION         IN     K16      OPTION DE CALCUL
C    ENTHTH         OUT    R        SOMME(EPSTH_T*D*EPSTH)
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           CHARACTER*16 OPTION
           REAL*8       NI(*),DNIDX(*), DNIDY(*), DNIDZ(*), POIDS(*)
           REAL*8       XYZ(*), DEPL(*), TEMPE(*), REPERE(7)
           REAL*8       INSTAN, ENTHTH
C -----  VARIABLES LOCALES
           INTEGER      I, MATER, NBSIG, NDIM, NNO, NPG,K,IGAU,L
           CHARACTER*16 K16BID
           REAL*8       SIGTH(162),HYDR(27),SECH(27),TREF,ZERO,RAYON
           REAL*8       EPSITH(162),ENTHPG,DFDX(27),DFDY(27),DFDZ(27)
           REAL*8       POIDI
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      K16BID = ' '
      ENTHTH = ZERO
C
C --- PAS DE PRISE EN COMPTE DES VARIABLES D'HYDRATATION OU SECHAGE
C
      DO 10 I = 1, 27
         HYDR(I) = ZERO
         SECH(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES CONTRAINTES MECANIQUES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
      CALL EPTHMC(MODELI,NNO,NDIM,NBSIG,NPG,NI,TEMPE,TREF,HYDR,SECH,
     +           INSTAN,MATER,OPTION,EPSITH)
C
C --- CALCUL DES CONTRAINTES THERMIQUES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
      CALL SIGTMC(MODELI,NNO,NDIM,NBSIG,NPG,NI,XYZ,TEMPE,TREF,HYDR,
     +            SECH,INSTAN,MATER,REPERE,K16BID,SIGTH)
C
C --- CALCUL DES CONTRAINTES TOTALES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
       DO 20 IGAU = 1, NPG
         ENTHPG=0.D0
C ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 3D
         IF (MODELI(1:2).EQ.'CA'.OR.MODELI(1:2).EQ.'TA') THEN
             L = (IGAU-1)*NNO
             K = 3*L + 1     
            CALL DFDM3D ( NNO,POIDS(IGAU),DNIDX(K),DNIDY(K),DNIDZ(K),
     &                 XYZ,DFDX,DFDY,DFDZ,POIDI)
C ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 2D
         ELSE
            K=(IGAU-1)*NNO+1
            CALL DFDM2D (NNO,POIDS(IGAU),DNIDX(K),DNIDY(K),
     &               XYZ,DFDX,DFDY,POIDI)
            IF (MODELI(1:2).EQ.'AX') THEN
               RAYON = 0.D0
               DO 41 I = 1, NNO
                  RAYON = RAYON + NI(K-1+I)*XYZ(2*(I-1)+1)
  41           CONTINUE
               POIDI=POIDI*RAYON
            ENDIF
         ENDIF
         DO 30 I = 1, NBSIG
          ENTHPG=ENTHPG+EPSITH(I+NBSIG*(IGAU-1))*SIGTH(I+NBSIG*(IGAU-1))
 30      CONTINUE
         ENTHTH=ENTHTH+(ENTHPG*POIDI)
 20   CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
