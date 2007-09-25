      SUBROUTINE ETHDST (FAMI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,
     +                   IDFDE,XYZ,DEPL,INSTAN,REPERE,
     +                   MATER,OPTION,ENTHTH)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/09/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    IPOIDS         IN     I        POIDS D'INTEGRATION
C    IVF            IN     I        FONCTIONS DE FORME
C    IDFDE          IN     I        DERIVEES DES FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
C                                   L'ELEMENT
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    MATER          IN     I        MATERIAU
C    OPTION         IN     K16      OPTION DE CALCUL
C    ENTHTH         OUT    R        SOMME(EPSTH_T*D*EPSTH)
C
C.========================= DEBUT DES DECLARATIONS ====================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C -----  ARGUMENTS
           INTEGER      IPOIDS,IVF,IDFDE
           CHARACTER*16 OPTION
           CHARACTER*(*) FAMI
           REAL*8       XYZ(*), DEPL(*), REPERE(7)
           REAL*8       INSTAN, ENTHTH
C -----  VARIABLES LOCALES
           INTEGER      I, MATER, NBSIG, NDIM, NNO, NPG,K,IGAU,IRET
           CHARACTER*16 K16BID
           REAL*8       SIGTH(162),ZERO
           REAL*8       RAYON
           REAL*8       EPSITH(162),ENTHPG,DFDX(27),DFDY(27),DFDZ(27)
           REAL*8       POIDI
           LOGICAL      LTEATT
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      K16BID = ' '
      ENTHTH = ZERO
C
C --- CALCUL DES CONTRAINTES MECANIQUES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
      CALL EPTHMC(FAMI,NNO,NDIM,NBSIG,NPG,ZR(IVF),XYZ,REPERE,
     +          INSTAN,MATER,OPTION,EPSITH)
C
C --- CALCUL DES CONTRAINTES THERMIQUES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
      CALL SIGTMC(FAMI,NNO,NDIM,NBSIG,NPG,ZR(IVF),XYZ,
     +            INSTAN,MATER,REPERE,K16BID,SIGTH)
C
C --- CALCUL DES CONTRAINTES TOTALES AUX POINTS D'INTEGRATION
C      ---------------------------------------------------------
      DO 20 IGAU = 1, NPG
         ENTHPG=0.D0
C ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 3D

         IF (LTEATT(' ','DIM_TOPO_MAILLE','3')) THEN
            CALL DFDM3D ( NNO, IGAU, IPOIDS, IDFDE,
     &                    XYZ, DFDX, DFDY, DFDZ, POIDI )
C ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 2D
         ELSE
            K=(IGAU-1)*NNO
            CALL DFDM2D( NNO,IGAU, IPOIDS,IDFDE,XYZ,DFDX,DFDY,POIDI)
            IF (LTEATT(' ','AXIS','OUI')) THEN
               RAYON = 0.D0
               DO 41 I = 1, NNO
                  RAYON = RAYON + ZR(IVF+K-1+I)*XYZ(2*(I-1)+1)
  41           CONTINUE
               POIDI=POIDI*RAYON
            ENDIF
         ENDIF
         DO 30 I = 1, NBSIG
           ENTHPG = ENTHPG+EPSITH(I+NBSIG*(IGAU-1))*
     &                            SIGTH(I+NBSIG*(IGAU-1))
 30      CONTINUE
         ENTHTH = ENTHTH+(ENTHPG*POIDI)
 20   CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
