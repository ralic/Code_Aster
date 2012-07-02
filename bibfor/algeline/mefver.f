      SUBROUTINE MEFVER(NDIM,SOM,XINT,YINT,RINT)
      IMPLICIT NONE
C
      INTEGER       NDIM(14)
      REAL*8        SOM(9),XINT(*),YINT(*),RINT(*)
C TOLE CRP_6
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     VERIFICATION DE L'ORDRE ET DE LA BONNE DISPOSITION DES SOMMETS DE
C     L ENCEINTE RECTANGULAIRE
C     VERIFICATION DE L INCLUSION DES FAISCEAUX DANS L ENCEINTE
C     CIRCULAIRE
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
C                OU XEXT,YEXT,REXT
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : RINT   : RAYONS DES CYLINDRES
C ----------------------------------------------------------------------
      INTEGER      IND(3)
      REAL*8       XSOM(4),YSOM(4),UX(4),UY(4),NORM,A1,A(4)
      REAL*8       VECT(4),LONG(4)
      CHARACTER*3  NOTE
C     ------------------------------------------------------------------
C
C --- LECTURE DES DIMENSIONS
C-----------------------------------------------------------------------
      INTEGER I ,IENCEI ,J ,NBCYL 
      REAL*8 DIFF ,EPSIT ,PI ,PIS2 ,PROJ ,R8PI ,REXT 
      REAL*8 XEXT ,YEXT 
C-----------------------------------------------------------------------
      NBCYL  = NDIM(3)
      IENCEI = NDIM(6)
C
C
      PI = R8PI()
      PIS2 = PI / 2.D0
      EPSIT = 1.D-5
C
      IF(IENCEI.EQ.2) THEN
         DO 10 I = 1,4
             XSOM(I) = SOM(2*I-1)
             YSOM(I) = SOM(2*I)
  10     CONTINUE
C
C
C ---    MISE EN ORDRE DES SOMMETS DE L ENCEINTE
C
         UX(1) = XSOM(2) - XSOM(1)
         UY(1) = YSOM(2) - YSOM(1)
         UX(2) = XSOM(3) - XSOM(1)
         UY(2) = YSOM(3) - YSOM(1)
         UX(3) = XSOM(4) - XSOM(1)
         UY(3) = YSOM(4) - YSOM(1)
C
         DO 20 I = 2,3
            NORM = (UX(I)*UX(I)+UY(I)*UY(I)) * (UX(1)*UX(1)+UY(1)*UY(1))
            NORM = SQRT(NORM)
            IF(NORM.EQ.0.D0) THEN
               CALL U2MESS('F','ALGELINE_88')
            ENDIF
            A(I-1) = ACOS((UX(I)*UX(1)+UY(I)*UY(1)) / NORM)
            A1     = ASIN((UX(1)*UY(I)-UY(1)*UX(I)) / NORM)
            IF(A1.LT.0.D0) A(I-1) = 2*PI - A(I-1)
  20     CONTINUE
C
         IF(A(1).LT.A(2).AND.A(2).LT.PI) THEN
            IND(1) = 2
            IND(2) = 3
            IND(3) = 4
         ELSE IF(A(1).GT.A(2) .AND.A(1).LT.PI) THEN
            IND(1) = 2
            IND(2) = 4
            IND(3) = 3
         ELSE IF(A(1).LT.PIS2.AND.A(2).GT.PI) THEN
            IND(1) = 4
            IND(2) = 2
            IND(3) = 3
         ELSE IF(A(2).LT.PIS2.AND.A(1).GT.PI) THEN
            IND(1) = 3
            IND(2) = 2
            IND(3) = 4
         ELSE IF(A(1).LT.A(2).AND.A(1).GT.PI) THEN
            IND(1) = 3
            IND(2) = 4
            IND(3) = 2
         ELSE IF(A(1).GT.A(2).AND.A(2).GT.PI) THEN
            IND(1) = 4
            IND(2) = 3
            IND(3) = 2
         ELSE
            CALL U2MESS('F','ALGELINE_89')
         ENDIF
C
         DO 30 I = 1,3
             SOM(2*(I+1)-1) = XSOM(IND(I))
             SOM(2*(I+1))   = YSOM(IND(I))
  30     CONTINUE
C
C ---    ON VERIFIE QUE LES QUATRES SOMMETS FORMENT BIEN UN RECTANGLE
         DO 40 I = 1,4
             XSOM(I) = SOM(2*I-1)
             YSOM(I) = SOM(2*I)
  40     CONTINUE
C
         UX(1) = XSOM(2) - XSOM(1)
         UY(1) = YSOM(2) - YSOM(1)
         UX(2) = XSOM(4) - XSOM(1)
         UY(2) = YSOM(4) - YSOM(1)
         UX(3) = XSOM(4) - XSOM(3)
         UY(3) = YSOM(4) - YSOM(3)
         UX(4) = XSOM(3) - XSOM(2)
         UY(4) = YSOM(3) - YSOM(2)
C
         DO 60 I = 1,2
             VECT(I) = UX(I)*UY(I+2)-UY(I)*UX(I+2)
  60     CONTINUE
C
         NORM = (UX(2)*UX(2)+UY(2)*UY(2)) * (UX(1)*UX(1)+UY(1)*UY(1))
         NORM = SQRT(NORM)
         IF(NORM.EQ.0.D0) THEN
            CALL U2MESS('F','ALGELINE_88')
         ENDIF
         A(1) = ACOS((UX(2)*UX(1)+UY(2)*UY(1)) / NORM)
         IF((ABS(A(1)-PIS2)+ABS(VECT(1))+ABS(VECT(2))).GT.EPSIT)
     &   CALL U2MESS('F','ALGELINE_89')
C
C
C ---    VERIFICATION DE L INCLUSION DES CYLINDRES DANS L ENCEINTE
C ---    RECTANGULAIRE
C
C ---    NORMALISATION DES VECTEURS U(1) ET U(2)
         DO 70 I = 1,2
            LONG(I) = SQRT(UX(I)*UX(I)+UY(I)*UY(I))
            UX(I) = UX(I) / LONG(I)
            UY(I) = UY(I) / LONG(I)
  70     CONTINUE
C
         DO 90 I = 1,NBCYL
            DO 80 J = 1,2
               PROJ = UX(J)*(XINT(I)-XSOM(1)) + UY(J)*(YINT(I)-YSOM(1))
               IF((PROJ-RINT(I)).LT.0.D0.OR.(PROJ+RINT(I)).GT.LONG(J))
     &                  THEN
                   WRITE(NOTE(1:3),'(I3.3)') I
                   CALL U2MESK('F','ALGELINE_90',1,NOTE)

               ENDIF
  80        CONTINUE
  90     CONTINUE
C
C ---    VERIFICATION DE L INCLUSION DES CYLINDRES DANS L ENCEINTE
C ---    CIRCULAIRE
C
      ELSE IF(IENCEI.EQ.1) THEN
         XEXT = SOM(1)
         YEXT = SOM(2)
         REXT = SOM(3)
         DO 100 I = 1,NBCYL
            DIFF = SQRT((XEXT-XINT(I))**2 + (YEXT-YINT(I))**2)
            IF((DIFF+RINT(I)).GT.REXT) THEN
               WRITE(NOTE(1:3),'(I3.3)') I
               CALL U2MESK('F','ALGELINE_81',1,NOTE)
            ENDIF
 100     CONTINUE
      ENDIF
C
C
      END
