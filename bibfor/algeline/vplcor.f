      SUBROUTINE VPLCOR(LDYNAM,NEQ,NBVECT,NBORTO,PRORTO,SIGNES,VECT,
     &                  IVECP,PKX,PLX)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER  LDYNAM,NEQ,NBORTO,NBVECT,IVECP
      REAL*8   PRORTO
      REAL*8   SIGNES(NBVECT),VECT(NEQ,NBVECT),PKX(NEQ,NBVECT),PLX(NEQ)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ORTHOGONALISATION D'UN VECTEUR PAR RAPPORT A UNE FAMILLE DE
C     VECTEURS DEJA ORTHOGONAUX
C     LES VECTEURS OBTENUS SONT K-ORTHOGONAUX .
C     ------------------------------------------------------------------
C IN  LDYNAM : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
C IN  NEQ    : IS : DIMENSION DES VECTEURS
C IN  NBVECT : IS : NOMBRE TOTAL DE VECTEURS (SERT A DIMENSIONER)
C IN  PRORTO : R8 : PRECISON DEMANDEE POUR L'ORTHOGONALISATION
C IN  NBORTO : IS : NOMBRE MAXIMUM D'ORTHOGONALISATION PERMISE.
C IN  PLX    : R8 : VECTEUR DE TRAVAIL
C     SIGNES : R8 : (+/- 1)  SIGNE DU PSEUDO PRODUIT SCALAIRE ENTRE LE
C                   I EME VECTEUR ET LE I-1 EME VECTEUR
C     VECT   : R8 : VECT(1..NEQ,1..NBVECT) PRODUIT DE LA MATRICE DE
C                   RAIDEUR PAR LES VECTEURS DE LA BASE
C IN  IVECP  : IS : NUMERO DU VECTEUR A ORTHOGONALISER AVEC LES IVECP-1
C                   AUTRES PREMIERS VECTEURS
C     ------------------------------------------------------------------
C
C
C     -----------------------------------------------------------------
      INTEGER     IEQ, ITO
      INTEGER VALI(2)
      REAL*8      COEF, XIKXI, XJKXI, XJKXIS
C     -----------------------------------------------------------------
C
C         --- K-REORTHOGONALISATION COMPLETE DU VECTEUR IVECP
C
          IOR = 0
C
          DO 10 JVEC = 1, IVECP-1
C
             XJKXI = 0.D0
             DO 20 IEQ = 1, NEQ
                XJKXI = XJKXI + PKX(IEQ,JVEC) * VECT(IEQ,IVECP)
  20         CONTINUE
C
             IORTHO = 0
             IF (ABS(XJKXI).GT.PRORTO) THEN
                IOR = 1
                DO 30 ITO = 1, NBORTO
                   IORTHO = ITO
C
                   DO 35 IEQ = 1,NEQ
                      PLX(IEQ) = VECT(IEQ,IVECP)
     &                         - XJKXI*SIGNES(JVEC)*VECT(IEQ,JVEC)
  35               CONTINUE
C
                   XJKXIS = 0.D0
                   DO 40 IEQ = 1, NEQ
                      XJKXIS = XJKXIS + PKX(IEQ,JVEC) * PLX(IEQ)
  40               CONTINUE
C
                   IF (ABS(XJKXIS).LT.PRORTO) THEN
                      DO 50 IEQ = 1, NEQ
                         VECT(IEQ,IVECP) = PLX(IEQ)
  50                  CONTINUE
                      XJKXI = XJKXIS
                      GOTO 100
                   ELSEIF (ABS(XJKXIS).LT.ABS(XJKXI)) THEN
                      DO 60 IEQ = 1, NEQ
                         VECT(IEQ,IVECP) = PLX(IEQ)
  60                  CONTINUE
                      XJKXI = XJKXIS
                   ELSE
                      VALI (1) = IORTHO
                      VALI (2) = IORTHO
      CALL U2MESG('A', 'ALGELINE4_76',0,' ',2,VALI,0,0.D0)
                      GOTO 100
                   ENDIF
C
  30            CONTINUE
                IORTHO = - NBORTO
C
  100           CONTINUE
                CALL MRMULT('ZERO',LDYNAM,VECT(1,IVECP),
     &                                                   PKX(1,IVECP),1,
     &.FALSE.)
             ENDIF
C
  10      CONTINUE
C
C         --- SI LE VECTEUR IVECP A ETE MODIFIE (IOR=1) ALORS ---
C         ---               ON LE RENORMALISE                 ---
C
          IF (IOR.EQ.1) THEN
              XIKXI = 0.D0
              DO 200 IEQ = 1, NEQ
                 XIKXI = XIKXI + VECT(IEQ,IVECP) * PKX(IEQ,IVECP)
 200          CONTINUE
              SIGNES(IVECP) = SIGN(1.D0,XIKXI)
              COEF = 1.D0 / SQRT(ABS(XIKXI))
              DO 210 IEQ = 1, NEQ
                 VECT(IEQ,IVECP) = COEF * VECT(IEQ,IVECP)
                 PKX(IEQ,IVECP)  = COEF * PKX(IEQ,IVECP)
 210          CONTINUE
          ENDIF
C
          END
