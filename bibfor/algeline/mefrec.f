      SUBROUTINE MEFREC(NDIM,NBCYL,NBGRP,NUMGRP,XINT,YINT,RINT,SGN,ORIG,
     &                  BETA,PPXX,PPXY,PPYX,PPYY,VNXX,VNXY,VNYX,VNYY,
     &                  TMP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       NBCYL,NDIM(14),NUMGRP(*),SGN(*),ORIG(*)
      REAL*8        RINT(*),XINT(*),YINT(*),BETA(*)
      REAL*8        PPXX(NBCYL,NBGRP),PPXY(NBCYL,NBGRP)
      REAL*8        PPYX(NBCYL,NBGRP),PPYY(NBCYL,NBGRP)
      REAL*8        VNXX(NBCYL,NBGRP),VNXY(NBCYL,NBGRP)
      REAL*8        VNYX(NBCYL,NBGRP),VNYY(NBCYL,NBGRP)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/10/1999   AUTEUR KXBADNG A.ADOBES 
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
C ----------------------------------------------------------------------
C     ASSEMBLAGE ET CALCUL DES COEFFICIENTS INTERVENANT DANS
C     L EXPRESSION DES FORCES DE PRESSION PERTURBEE, ET ET DES FORCES
C     NORMALES DE FROTTEMENTS SUR CHAQUE CYLINDRE DANS LE CAS D UNE
C     ENCEINTE RECTANGULAIRE
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : NBCYL  : NOMBRE DE CYLINDRES
C IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : RINT   : RAYONS DES CYLINDRES
C IN  : SGN    : -1 OU +1, COEFFICIENT INTERVENANT DANS LA DECOMPOSITION
C                EN SERIE DE LAURENT, SELON LE NIVEAU D IMAGE
C IN  : ORIG   : NUMERO DU CYLINDRE D ORIGINE DES CYLINDRES REELS OU
C                IMAGES
C IN  : BETA   : ANGLE CUMULE INTERVENANT DANS LA DECOMPOSITION EN
C                SERIE DE LAURENT, POUR LES CYLINDRES IMAGES
C OUT : PPXX   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
C                EFFORTS DE PRESSION PERTURBES SUIVANT XX
C OUT : PPXY   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
C                EFFORTS DE PRESSION PERTURBES SUIVANT XY
C OUT : PPYX   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
C                EFFORTS DE PRESSION PERTURBES SUIVANT YX
C OUT : PPYY   : COEFFICIENT DE MASSES AJOUTEES INTERVENANT DANS LES
C                EFFORTS DE PRESSION PERTURBES SUIVANT YY
C OUT : VNXX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
C                VISQUEUX NORMAUX SUIVANT XX
C OUT : VNXY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
C                VISQUEUX NORMAUX SUIVANT XY
C OUT : VNYX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
C                VISQUEUX NORMAUX SUIVANT YX
C OUT : VNYY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
C                VISQUEUX NORMAUX SUIVANT YY
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C ----------------------------------------------------------------------
      INTEGER      I,J,K
      INTEGER NCYL
      REAL*8  TMP(4,*),RAYOI,RAYOJ
C ----------------------------------------------------------------------
C
C --- LECTURE DES DIMENSIONS
      NBCYL  = NDIM(3)
      NBGRP  = NDIM(4)
      NBTRON = NDIM(5)
C
      CALL JEMARQ()
C
C
C --- TABLEAUX DE TRAVAIL, ALLOCATION MEMOIRE
      NV = 2 * NBTRON * NBCYL
      CALL WKVECT('&&MEFREC.TMP.AB','V V R',NV*(3+2*NBGRP+NV),IA)
      IB     = IA  + NV * NV
      IXX    = IB  + NV
      IX     = IXX + NV * 2 * NBGRP
      ITRAV  = IX  + NV
C
C --- INITIALISATIONS
C
      NMAX = 4 * NBTRON * NBCYL * NBGRP
      DO 1 I = 1,NMAX
          ZR(IXX+I-1) = 0.D0
   1  CONTINUE
C
      NMAX = 2 * NBTRON * NBCYL
      DO 2 IGRP = 1,NBGRP
         DO 21 IDIR = 1,2
            DO 211 I = 1,NMAX*NMAX
                ZR(IA+I-1) = 0.D0
 211        CONTINUE
            DO 212 I = 1,NMAX
                ZR(IB+I-1) = 0.D0
                ZR(IX+I-1) = 0.D0
 212        CONTINUE
C
C ---       ASSEMBLAGE
            CALL MEFASR(NDIM,NBCYL,NBGRP,NBTRON,NUMGRP,IDIR,IGRP,
     &                  XINT,YINT,RINT,SGN,ORIG,BETA,ZR(IA),ZR(IB))
C
C ---       RESOLUTION DU SYSTEME A.X = B PAR LA METHODE DE CROUT
            IER = 1
            CALL MTCROG(ZR(IA),ZR(IB),NMAX,NMAX,1,ZR(IX),ZR(ITRAV),
     &                  IER)
            IF(IER.EQ.1) THEN
               CALL UTMESS('F','MEFREC','RESOLUTION IMPOSSIBLE '//
     &              'MATRICE SINGULIERE, PEUT ETRE A CAUSE DES '//
     &              'ERREURS D ARRONDIS' )
            ENDIF
C
            DO 213 I = 1,NMAX
                ZR(IXX+I-1+NMAX*(2*IGRP-IDIR)) = ZR(IX+I-1)
 213        CONTINUE
  21     CONTINUE
   2  CONTINUE
C
C
C --- CALCUL DES COEFFICIENTS PPXX, PPXY, PPYX, PPYY,
C --- ET VNXX, VNXY, VNYX, VNYY
      DO 3 I = 1,NBGRP
         DO 31 J = 1,NBCYL
            PPXX(J,I) = 2.D0*ZR(IXX-1+2*NBTRON*(NBCYL*(2*I-1)+J-1)+1)
            PPXY(J,I) = 2.D0*ZR(IXX-1+2*NBTRON*(NBCYL*(2*I-2)+J-1)+1)
            PPYX(J,I) = 2.D0*ZR(IXX-1+2*NBTRON*(NBCYL*(2*I-1)+J-1)+2)
            PPYY(J,I) = 2.D0*ZR(IXX-1+2*NBTRON*(NBCYL*(2*I-2)+J-1)+2)
C
            IF(I.EQ.NUMGRP(J)) THEN
               PPXX(J,I) = PPXX(J,I) + 1.D0
               PPYY(J,I) = PPYY(J,I) + 1.D0
            ENDIF
  31     CONTINUE
   3  CONTINUE
C
      DO 4 I = 1,NBGRP
         DO 41 J = 1,NBGRP
            TMP(1,J) = 0.D0
            TMP(2,J) = 0.D0
            TMP(3,J) = 0.D0
            TMP(4,J) = 0.D0
  41     CONTINUE
         DO 42 J = 1,NBCYL
            TMP(1,NUMGRP(J)) = TMP(1,NUMGRP(J)) + PPXX(J,I)
            TMP(2,NUMGRP(J)) = TMP(2,NUMGRP(J)) + PPXY(J,I)
            TMP(3,NUMGRP(J)) = TMP(3,NUMGRP(J)) + PPYX(J,I)
            TMP(4,NUMGRP(J)) = TMP(4,NUMGRP(J)) + PPYY(J,I)
  42     CONTINUE
         DO 43 J = 1,NBGRP
            PPXX(J,I) = TMP(1,J)
            PPXY(J,I) = TMP(2,J)
            PPYX(J,I) = TMP(3,J)
            PPYY(J,I) = TMP(4,J)
  43     CONTINUE
   4  CONTINUE
C
C --- ON FORCE LA SYMETRIE DES COEFFICIENTS
C
      DO 5 I = 1,NBGRP
         DO 50 K = 1,NBCYL
            IF (NUMGRP(K).EQ.I) THEN
               RAYOI = RINT(K)
            ENDIF
  50     CONTINUE
         PPXY(I,I) = PPXY(I,I) + PPYX(I,I)
         PPXY(I,I) = PPXY(I,I) / 2.D0
         PPYX(I,I) = PPXY(I,I)
         DO 51 J = 1,I-1
            DO 511 K = 1,NBCYL
               IF (NUMGRP(K).EQ.J) THEN
                  RAYOJ = RINT(K)
               ENDIF
  511       CONTINUE
            PPXX(I,J) = RAYOI * RAYOI * PPXX(I,J) +
     &                  RAYOJ * RAYOJ * PPXX(J,I)
            PPXX(I,J) = PPXX(I,J) / 2.D0
            PPXX(J,I) = PPXX(I,J)
            PPXX(I,J) = PPXX(I,J) / RAYOI / RAYOI
            PPXX(J,I) = PPXX(J,I) / RAYOJ / RAYOJ
C
            PPXY(I,J) = RAYOI * RAYOI * PPXY(I,J) +
     &                  RAYOJ * RAYOJ * PPYX(J,I)
            PPXY(I,J) = PPXY(I,J) / 2.D0
            PPYX(J,I) = PPXY(I,J)
            PPXY(I,J) = PPXY(I,J) / RAYOI / RAYOI
            PPYX(J,I) = PPYX(J,I) / RAYOJ / RAYOJ
C
            PPYX(I,J) = RAYOI * RAYOI * PPYX(I,J) +
     &                  RAYOJ * RAYOJ * PPXY(J,I)
            PPYX(I,J) = PPYX(I,J) / 2.D0
            PPXY(J,I) = PPYX(I,J)
            PPYX(I,J) = PPYX(I,J) / RAYOI / RAYOI
            PPXY(J,I) = PPXY(J,I) / RAYOJ / RAYOJ
C
            PPYY(I,J) = RAYOI * RAYOI * PPYY(I,J) +
     &                  RAYOJ * RAYOJ * PPYY(J,I)
            PPYY(I,J) = PPYY(I,J) / 2.D0
            PPYY(J,I) = PPYY(I,J)
            PPYY(I,J) = PPYY(I,J) / RAYOI / RAYOI
            PPYY(J,I) = PPYY(J,I) / RAYOJ / RAYOJ
  51     CONTINUE
   5  CONTINUE
C
      DO 6 I = 1,NBGRP
         DO 61 J = 1,NBGRP
            VNXX(J,I) = 0.5D0 * PPXX(J,I)
            VNXY(J,I) = 0.5D0 * PPXY(J,I)
            VNYX(J,I) = 0.5D0 * PPYX(J,I)
            VNYY(J,I) = 0.5D0 * PPYY(J,I)
            IF (J.EQ.I) THEN
               NCYL = 0
               DO 611 K = 1,NBCYL
                  IF (NUMGRP(K).EQ.I) NCYL = NCYL + 1
  611          CONTINUE
               VNXX(J,I) = VNXX(J,I) + 0.5D0 * NCYL
               VNYY(J,I) = VNYY(J,I) + 0.5D0 * NCYL
            ENDIF
  61     CONTINUE
   6  CONTINUE
C
      CALL JEDETC('V','&&MEFREC',1)
      CALL JEDEMA()
      END
