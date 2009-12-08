      SUBROUTINE  DORTVP(NDIM,NOMRC,D,MODELI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/09/2001   AUTEUR DURAND C.DURAND 
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
C      DORTVP --   CALCUL DES VALEURS PROPRES DE LA MATRICE
C                  HOOKE DORTH POUR S'ASSURER QUE CELLE-CI EST BIEN
C                  DEFINIE POSITIVE DANS LE CAS DE L'ORTHOTROPIE
C                  OU DE L'ISOTROPIE TRANSVERSE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NDIM           IN    I       DIMENSION DU MODELE
C    NOMRC          IN   K16      NOM DE LA RELATION DE COMPORTEMENT
C    D(6,6)         IN    R       MATRICE DE HOOKE
C    MODELI         IN    K2      INDICATEUR DE LA MODELISATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           INTEGER       NDIM
           REAL*8        D(6,6)
           CHARACTER*2   MODELI
           CHARACTER*16  NOMRC
C -----  VARIABLES LOCALES
           INTEGER      TYPE, IORDRE
C
           REAL*8 TR(21), TU(21), JACAUX(6)
           REAL*8 VECP2(4,4), VECP3(6,6), VALP(6)
C
           DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      ZERO   = 0.0D0
      UN     = 1.0D0
C
      TYPE   = 0
      IORDRE = 0
C
      DO 10 I = 1, 6
        VALP(I) = ZERO
  10  CONTINUE
C
C --- CONSTRUCTION DU VECTEUR TR QUI CONTIENT LES VECTEURS COLONNES
C --- DE LA DEMI-MATRICE INFERIEURE DE LA MATRICE DONT ON RECHERCHE  
C --- LES VALEURS PROPRES ET DU VECTEUR TU QUI CONTIENT LES VECTEURS 
C --- COLONNES DE LA DEMI-MATRICE INFERIEURE DE LA MATRICE UNITE :
C     ==========================================================
C
C --- CAS 3D :
C     ------
      IF (NDIM.EQ.3) THEN
C
        NBVEC = 6
C
C ---   TABLEAU TR :
C       ----------
        TR(1)  = D(1,1)
        TR(2)  = D(2,1)
        TR(3)  = D(3,1)
        TR(4)  = D(4,1)
        TR(5)  = D(5,1)
        TR(6)  = D(6,1)
C
        TR(7)  = D(2,2)
        TR(8)  = D(3,2)
        TR(9)  = D(4,2)
        TR(10) = D(5,2)
        TR(11) = D(6,2)
C
        TR(12) = D(3,3)
        TR(13) = D(4,3)
        TR(14) = D(5,3)
        TR(15) = D(6,3)
C
        TR(16) = D(4,4)
        TR(17) = D(5,4)
        TR(18) = D(6,4)
C
        TR(19) = D(5,5)
        TR(20) = D(6,5)
C
        TR(21) = D(6,6)
C
C ---   TABLEAU TU :
C       ----------
        DO 20 I = 1, 21
          TU(I) = ZERO
  20    CONTINUE
C
        K = 1
        DO 30 I = 1, 6
          TU(K) = UN
          K     = K+7-I
  30    CONTINUE
C
C --- CAS 2D :
C     ------
      ELSEIF (NDIM.EQ.2) THEN
C
        NBVEC = 4
C
C ---   TABLEAU TR :
C       ----------
        TR(1)  = D(1,1)
        TR(2)  = D(2,1)
        TR(3)  = D(3,1)
        TR(4)  = D(4,1)
C
        TR(5)  = D(2,2)
        TR(6)  = D(3,2)
        TR(7)  = D(4,2)
C
        TR(8)  = D(3,3)
        TR(9)  = D(4,3)
C
        TR(10) = D(4,4)
C
C ---   TABLEAU TU :
C       ----------
        DO 40 I = 1, 10
          TU(I) = ZERO
  40    CONTINUE
C
        K = 1
        DO 50 I = 1, 4
          TU(K) = UN
          K     = K+5-I
  50    CONTINUE
C
      ENDIF
C
C --- RECHERCHE DES VALEURS PROPRES DE D :
C     ==================================
      IF (NDIM.EQ.3) THEN
        CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,TR,TU,VECP3,VALP,JACAUX,
     +              NITJAC,TYPE,IORDRE)
      ELSEIF (NDIM.EQ.2) THEN
        CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,TR,TU,VECP2,VALP,JACAUX,
     +              NITJAC,TYPE,IORDRE)
      ENDIF
C
C --- RECUPERATION DU NIVEAU D'IMPRESSION :
C     -----------------------------------
      CALL INFNIV(IFM,NIV)
C
      INEG = 0
      DO 60 I = 1, NBVEC
        IF (VALP(I).LT.ZERO) THEN
           INEG = INEG + 1
        ENDIF
  60  CONTINUE
C
      IF (INEG.GT.1) THEN
C
         IF (MODELI.EQ.'CP') THEN
           WRITE(IFM,1080)
           WRITE(IFM,1100)
         ELSEIF (MODELI.EQ.'DP') THEN
           WRITE(IFM,1090)
           WRITE(IFM,1110)
         ENDIF
C
         WRITE(IFM,1010) 
         WRITE(IFM,1020) NOMRC
      ELSEIF (INEG.EQ.1) THEN
C
         IF (MODELI.EQ.'CP') THEN
           WRITE(IFM,1080)
           WRITE(IFM,1100)
         ELSEIF (MODELI.EQ.'DP') THEN
           WRITE(IFM,1090)
           WRITE(IFM,1110)
         ENDIF
C
         WRITE(IFM,1030) 
         WRITE(IFM,1020) NOMRC
      ENDIF
C
      IF (INEG.GT.0) THEN
        WRITE(IFM,1050)
        WRITE(IFM,1060)
C
        DO 70 I = 1, NBVEC
           WRITE(IFM,1070) I,VALP(I)
  70    CONTINUE
C
        WRITE(IFM,1060)
        WRITE(IFM,1040) 
C
      ENDIF
C
C 1000 FORMAT(7X,'VALEUR PROPRE NUMERO ',I1,' DE LA MATRICE DE HOOKE ',
C     +        E12.5)
 1010 FORMAT(7X,'LA MATRICE DE HOOKE A DES VALEURS PROPRES NEGATIVES ')
 1020 FORMAT(7X,'POUR LA RELATION DE COMPORTEMENT : ',A16,/)
 1030 FORMAT(7X,'LA MATRICE DE HOOKE A UNE VALEUR PROPRE NEGATIVE ')
 1040 FORMAT(/)
 1050 FORMAT(7X,'VALEURS PROPRES DE LA MATRICE DE HOOKE : ',/)
 1060 FORMAT(7X,'************************')
 1070 FORMAT(7X,'!  ',I1,'  !  ',E12.5,'  !')
 1080 FORMAT(7X,'TRAITEMENT DU CAS DES CONTRAINTES PLANES :')
 1100 FORMAT(7X,'---------------------------------------- ')
 1090 FORMAT(7X,'TRAITEMENT DES CAS DEFORMATIONS PLANES ET AXI :')
 1110 FORMAT(7X,'--------------------------------------------- ')
C
C.============================ FIN DE LA ROUTINE ======================
      END
