      SUBROUTINE PROJSE(NUTYP,NDIM,NBNO,PROJ,COORDA,COORDB,COORDC,
     &                  COORDP,NORM,COORDM,COEF,OLDJEU,JEU,TANG,JEUFX,
     &                  PRONOR,TANGDF,VLISSA)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/07/2002   AUTEUR ADBHHPM P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

      IMPLICIT NONE

      INTEGER NUTYP,NBNO,PROJ,PRONOR,TANGDF,NDIM
      REAL*8 COORDA(3),COORDB(3),COORDC(3),COORDP(3),NORM(3),VECSEG(3)
      REAL*8 COORDM(3),COEF(3),OLDJEU,JEU,TANG(6),JEUFX,JEUFY
      REAL*8 VLISSA(9)

C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJEC
C ----------------------------------------------------------------------

C "PROJECTION" D'UN NOEUD ESCLAVE SUR UN SEG2 OU UN SEG3.
C ON UTILISE LA NORMALE AU SEGMENT.

C IN  NUTYP  : TYPE DE LA MAILLE ( SEG2 , SEG3 )
C IN  NDIM   : DIMENSION DU PB
C IN  NBNO   : NOMBRE DE NOEUDS (2 OU 3)
C IN  PROJ   : PROJECTION LINEAIRE (1) OU QUADRATIQUE (2) SUR LA MAILLE
C              OU PAS DE NOUVELLE PROJECTION (0)
C IN  COORDA : COORDONNEES DU SOMMET A DU SEGMENT
C IN  COORDB : COORDONNEES DU SOMMET B DU SEGMENT
C IN  COORDC : COORDONNEES DU MILIEU C DU SEGMENT
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE P
C OUT NORM   : NORMALE ENTRANTE A LA MAILLE MAITRE
C OUT COORDM : COORDONNEES DE LA "PROJECTION" M
C OUT COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
C OUT OLDJEU : JEU PM DANS LA DIRECTION PM
C OUT JEU    : JEU DANS LA DIRECTION DE LA NORMALE CHOISIE (PM.NORM)

C ----------------------------------------------------------------------

      INTEGER NTSEG2,NTSEG3
      REAL*8 LAMBDA,NORME,DENOM,XNORM(3),INORM(3)
      CHARACTER*32 JEXNOM

C ----------------------------------------------------------------------

      IF (PROJ.EQ.0) THEN
        CALL UTMESS('F','PROJSE_00','IL FAUT REACTUALISER LA PROJECTION'
     &              )
      END IF
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),NTSEG2)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG3'),NTSEG3)

C ======================================================================
C         PROJECTION SUR LE SEG2 OU LE SEG3 SUPPOSE RECTILIGNE
C ======================================================================


C --- SI LISSAGE DES NORMALES

        IF (PRONOR.EQ.1 .OR. PRONOR.EQ.3) THEN
          DENOM = (COORDB(1)-COORDA(1))**2 + (COORDB(2)-COORDA(2))**2
          IF (NDIM.EQ.3) THEN
            DENOM = DENOM + (COORDB(3)-COORDA(3))**2
          END IF

          IF (DENOM.EQ.0) THEN
            CALL UTMESS('F','PROJLI_01','UNE MAILLE MAITRE EST DE '//
     &                  'LONGUEUR NULLE')
          END IF

C --- ON STOCKE DANS INORM LA NORMALE AU NOEUD ESCLAVE

          IF (NDIM.EQ.3) THEN
            INORM(1) = NORM(1)
            INORM(2) = NORM(2)
            INORM(3) = NORM(3)
          ELSE
            INORM(1) = NORM(1)
            INORM(2) = NORM(2)
            INORM(3) = 0.0D0
          END IF

C --- ON STOCKE DANS XNORM LA NORMALE A LA MAILLE MAITRE

          IF (NDIM.EQ.3) THEN
            VECSEG(1) = (COORDB(1)-COORDA(1))/SQRT(DENOM)
            VECSEG(2) = (COORDB(2)-COORDA(2))/SQRT(DENOM)
            VECSEG(3) = (COORDB(3)-COORDA(3))/SQRT(DENOM)
            XNORM(1) = VECSEG(2)*TANG(3) - VECSEG(3)*TANG(2)
            XNORM(2) = VECSEG(3)*TANG(1) - VECSEG(1)*TANG(3)
            XNORM(3) = VECSEG(1)*TANG(2) - VECSEG(2)*TANG(1)
          ELSE
            XNORM(1) = (COORDB(2)-COORDA(2))/SQRT(DENOM)
            XNORM(2) = - (COORDB(1)-COORDA(1))/SQRT(DENOM)
            XNORM(3) = 0.0D0
          END IF

C --- ON FAIT UNE MOYENNE ENTRE LES DEUX NORMALES POUR LA PROJECTION

          IF (NDIM.EQ.3) THEN
            NORM(1) = INORM(1) - XNORM(1)
            NORM(2) = INORM(2) - XNORM(2)
            NORM(3) = INORM(3) - XNORM(3)
          ELSE
            NORM(1) = INORM(1) - XNORM(1)
            NORM(2) = INORM(2) - XNORM(2)
            NORM(3) = 0.0D0
          END IF
          CALL NORMEV(NORM,NORME)
        END IF
        IF ((NUTYP.EQ.NTSEG2) .OR. (PROJ.EQ.1)) THEN
         CALL PROJLI(COORDA,COORDB,COORDP,NORM,COORDM,LAMBDA,OLDJEU,JEU,
     &              TANG,JEUFX,PRONOR,TANGDF,NDIM)
        ELSE IF ((NUTYP.EQ.NTSEG3) .AND. (PROJ.EQ.2)) THEN
         CALL PROJSQ(COORDA,COORDB,COORDC,COORDP,NORM,COORDM,LAMBDA,
     &              OLDJEU,JEU,TANG,JEUFX,PRONOR,TANGDF,NDIM)
        END IF       
        COEF(1) = - (1.D0-LAMBDA)
        COEF(2) = -LAMBDA
        COEF(3) = 0.0D0
        IF (NUTYP.EQ.NTSEG3) THEN
          COEF(1) = -2* (1.D0-LAMBDA)* (0.5D0-LAMBDA)
          COEF(2) = -2*LAMBDA* (LAMBDA-0.5D0)
          COEF(3) = -4*LAMBDA* (1.D0-LAMBDA)
        END IF

C ======================================================================
C                APPLICATION DU LISSAGE DES NORMALES
C ======================================================================

        IF (PRONOR.GE.2) THEN
          IF (NUTYP.EQ.NTSEG2) THEN
            NORM(1) = COEF(1)*VLISSA(1) + COEF(2)*VLISSA(4)
            NORM(2) = COEF(1)*VLISSA(2) + COEF(2)*VLISSA(5)
            NORM(3) = COEF(1)*VLISSA(3) + COEF(2)*VLISSA(6)
          ELSE IF (NUTYP.EQ.NTSEG3) THEN
            NORM(1) = COEF(1)*VLISSA(1) + COEF(2)*VLISSA(4) +
     &                COEF(3)*VLISSA(7)
            NORM(2) = COEF(1)*VLISSA(2) + COEF(2)*VLISSA(5) +
     &                COEF(3)*VLISSA(8)
            NORM(3) = COEF(1)*VLISSA(3) + COEF(2)*VLISSA(6) +
     &                COEF(3)*VLISSA(9)
          END IF
          IF (PRONOR.EQ.3) THEN
            CALL NORMEV(NORM,NORME)
            NORM(1) = INORM(1) + NORM(1)
            NORM(2) = INORM(2) + NORM(2)
            NORM(3) = INORM(3) + NORM(3)
          END IF
          CALL NORMEV(NORM,NORME)
          JEU = (COORDM(1)-COORDP(1))*NORM(1) +
     &          (COORDM(2)-COORDP(2))*NORM(2)
          IF (NDIM.EQ.3) JEU = JEU + (COORDM(3)-COORDP(3))*NORM(3)
          CALL CATANG(2,NORM,TANG,TANGDF)
          JEUFX = (COORDM(1)-COORDP(1))*TANG(1) +
     &            (COORDM(2)-COORDP(2))*TANG(2)
          IF (NDIM.EQ.3) JEUFX = JEUFX + (COORDM(3)-COORDP(3))*TANG(3)
        END IF


C ======================================================================
C         PROJECTION SUR LE SEG3 QUADRATIQUE GAUCHE (PROJ = 2)
C ======================================================================

C ----------------------------------------------------------------------

      END
