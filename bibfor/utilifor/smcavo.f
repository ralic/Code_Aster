      SUBROUTINE SMCAVO(X,IND,NBHIST,TRC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C......................................................................C
C RECHERCHE DES PLUS PROCHES VOISINS DE X PARMI LES POINTS CONSTRUITS  C
C......................................................................C
      IMPLICIT NONE
      REAL*8  SDX,TRC((3*NBHIST),5),X(5),D(6),DX(5)
      INTEGER IND(6),NBHIST,INVOIS,I,J,N
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DO 10 I=1,6
        IND(I)=0
        D(I)=10.0D25
 10   CONTINUE
      DO 70 N=1,(3*NBHIST)
        DO 20 I=1,3
          DX(I)=(X(I)-TRC(N,I))**2
 20     CONTINUE
        DO 30 I=4,5
          DX(I)=((X(I)-TRC(N,I))/X(I))**2
 30     CONTINUE
        SDX=0.D0
        DO 40 I=1,5
          SDX=SDX+DX(I)
 40     CONTINUE
        IF(SDX.GE.D(6))THEN
          GOTO 70
        ELSE
          DO 50 I=6,1,-1
            IF(SDX.LT.D(I))THEN
              INVOIS=I
            ENDIF
 50       CONTINUE
          IF(INVOIS.EQ.6)THEN
            D(6)=SDX
            IND(6)=N
          ELSE
            DO 60 J=6,INVOIS+1,-1
              D(J)=D(J-1)
              IND(J)=IND(J-1)
 60         CONTINUE
            D(INVOIS)=SDX
            IND(INVOIS)=N
          ENDIF
        ENDIF
 70   CONTINUE
      END
