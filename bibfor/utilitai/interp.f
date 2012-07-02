      SUBROUTINE INTERP(TABX,TABY,NECR,X,Y,ISEG)
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ----------------------------------------------------------------
C     AUTEUR : C. DUVAL DEPT AMV
C     ----------------------------------------------------------------
C
C     BUT: FAIT L'INTERPOLATION LINEAIRE ENTRE 2 POINTS D'UNE FONCTION
C
C     ----------------------------------------------------------------
C
C     TABX,TABY /IN/:PARAMETRES ET VALEURS DE LA FONCTION
C     NECR         /IN/:NOMBRE DE POINTS DE LA FONCTION
C     X            /IN/:VALEUR A INTERPOLER
C
C     Y            /OUT/:VALEUR DE LA FONCTION EN X
C     ISEG         /OUT/:RANG DU SEGMENT DE LA FONCTION CONTENANT X
C
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
      REAL*8 TABX(*),TABY(*)
      INTEGER IPT ,ISEG ,NECR
      REAL*8 X ,X1 ,X2 ,Y ,Y1 ,Y2
C-----------------------------------------------------------------------
      DO 1 ,IPT=2,NECR
       X1=TABX(IPT-1)
       X2=TABX(IPT)
       IF(((X-X1)*(X-X2)).LE.0.D0)THEN
         ISEG=IPT
         Y1=TABY(IPT-1)
         Y2=TABY(IPT)
         IF(X1.EQ.X2)THEN
           Y=Y1
         ELSE
           Y=Y1+(X-X1)*(Y1-Y2)/(X1-X2)
         ENDIF
         GOTO 9999
       ENDIF
 1    CONTINUE
 9999 CONTINUE
      END
