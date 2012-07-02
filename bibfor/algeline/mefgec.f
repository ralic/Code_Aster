      SUBROUTINE MEFGEC(NDIM,NBCYL,SOM,XINT,YINT,RINT,DCENT,FICENT,D,
     &                  FI)
      IMPLICIT NONE
C
      INTEGER       NDIM(14),NBCYL
      REAL*8        SOM(9),XINT(*),YINT(*),RINT(*),DCENT(*),FICENT(*)
      REAL*8        D(NBCYL,NBCYL),FI(NBCYL,NBCYL)
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
C     CALCUL DES COORDONNEES POLAIRES ABSOLUES ET RELATIVES DES CENTRES
C     DES CYLINDRES
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NDIM   : TABLEAU DES DIMENSIONS
C IN  : NBCYL  : NOMBRE DE CYLINDRES
C IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
C                OU XEXT,YEXT,REXT
C IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
C                LE REPERE AXIAL
C IN  : RINT   : RAYONS DES CYLINDRES
C OUT : DCENT  : DISTANCE DU CENTRE DES CYLINDRES AU CENTRE DE
C                L ENCEINTE
C OUT : FICENT : ANGLE POLAIRE PAR RAPPORT AU CENTRE DE L ENCEINTE
C OUT : D      : DISTANCE RELATIVE ENTRE LES CENTRES DES CYLINDRES
C OUT : FI     : ANGLE POLAIRE RELATIF PAR RAPPORT AU CENTRE DE CHAQUE
C                CYLINDRE
C ----------------------------------------------------------------------
      INTEGER      I,J
      CHARACTER*3  NOTE,NOT2
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
C
C --- LECTURE DES DIMENSIONS
C-----------------------------------------------------------------------
      REAL*8 DELTA ,PI ,R8PI ,REXT ,XEXT ,YEXT 
C-----------------------------------------------------------------------
      NBCYL  = NDIM(3)
C
C
      PI = R8PI()
      XEXT = SOM(1)
      YEXT = SOM(2)
      REXT = SOM(3)
C
C --- (DCENT,FICENT) : COORDONNEES POLAIRES DES CENTRES
C ---                  DES CYLINDRES INTERIEURS
C
      DO 10 I = 1,NBCYL
         DCENT(I) = SQRT((XINT(I)-XEXT)*(XINT(I)-XEXT)
     &            +      (YINT(I)-YEXT)*(YINT(I)-YEXT))
         IF (DCENT(I).NE.0.D0) THEN
            FICENT(I) = ACOS((XINT(I)-XEXT)/DCENT(I))
            IF ((YINT(I)-YEXT).LT.0.D0) THEN
               FICENT(I) = 2.D0*PI-FICENT(I)
            ENDIF
          ELSE
            FICENT(I) = 0.D0
         ENDIF
  10  CONTINUE
C
C --- (D,FI) : COORDONNEES POLAIRES RELATIVES DES CENTRES
C ---          DES CYLINDRES LES UNS PAR RAPPORT AUX AUTRES
C
      DO 30 I = 1,NBCYL
         DO 20 J = 1,NBCYL
            D(J,I) = SQRT((XINT(I)-XINT(J))*(XINT(I)-XINT(J))+
     &                   (YINT(I)-YINT(J))*(YINT(I)-YINT(J)))
            IF (I.NE.J) THEN
               IF ((RINT(J)+RINT(I)).GE.D(J,I)) THEN
                  WRITE(NOTE(1:3),'(I3.3)') I
                  WRITE(NOT2(1:3),'(I3.3)') J
                   VALK(1) = NOTE
                   VALK(2) = NOT2
                   CALL U2MESK('F','ALGELINE_80', 2 ,VALK)
               ENDIF
            ENDIF

            IF (D(J,I).NE.0.D0) THEN
               FI(J,I) = ACOS((XINT(I)-XINT(J))/D(J,I))
               IF ((YINT(I)-YINT(J)).LT.0.D0) THEN
                  FI(J,I) = 2.D0*PI-FI(J,I)
               ENDIF
            ELSE
               FI(J,I) = 0.D0
            ENDIF

  20     CONTINUE
  30  CONTINUE
C
C --- VERIFICATION DE L INCLUSION DE TOUS LES CYLINDRES DANS
C --- L ENCEINTE CIRCULAIRE
C
C
      DO 40 I  = 1,NBCYL
          DELTA = SQRT((XINT(I)-XEXT)**2+(YINT(I)*YEXT)**2)
          IF(DELTA.GE.(REXT-RINT(I))) THEN
                WRITE(NOTE(1:3),'(I3.3)') I
                CALL U2MESK('F','ALGELINE_81',1,NOTE)
         ENDIF
  40  CONTINUE
C
C
      END
