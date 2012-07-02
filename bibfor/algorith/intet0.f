      SUBROUTINE INTET0(ANGLE,TET0,IAX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C    P. RICHARD     DATE 11/03/91
C-----------------------------------------------------------------------
C  BUT:  CALCULER LA MATRICE 6X6 DE CHANGEMENT DE REPERE PERMETTANT
C    D'EFFECTUER UNE ROTATION D'AXE OZ SUR LES DDL SUIVANTS:
      IMPLICIT NONE
C
C                 DX  DY  DZ  DRX  DRY  DRZ  ? ? PRES PHI
C
C-----------------------------------------------------------------------
C
C ANGLE    /I/: NOMBRE DE SECTEURS
C TET0     /O/: MATRICE DE CHANGEMENT DE REPERE
C IAX      /O/: NUMERO AXE ROTATION (1-X,2-Y,3-Z)
C
C-----------------------------------------------------------------------
C
      REAL*8 TET0(10,10)
      INTEGER I ,IAX ,J ,JJ
      REAL*8 A ,ANGLE ,B
C-----------------------------------------------------------------------
C
      DO 10 I=1,10
        DO 20 J=1,10
          TET0(I,J)=0.D0
 20     CONTINUE
 10   CONTINUE
C
      A=COS(ANGLE)
      B=SIN(ANGLE)
C
      IF(IAX.EQ.3) THEN
        DO 30 I=1,2
          JJ=3*(I-1)
          TET0(JJ+1,JJ+1)=A
          TET0(JJ+2,JJ+2)=A
          TET0(JJ+1,JJ+2)=-B
          TET0(JJ+2,JJ+1)=B
          TET0(JJ+3,JJ+3)=1.D0
 30     CONTINUE
        TET0(7,7)=1.D0
        TET0(8,8)=1.D0
        TET0(9,9)=1.D0
        TET0(10,10)=1.D0
      ELSEIF(IAX.EQ.2)THEN
        DO 40 I=1,2
          JJ=3*(I-1)
          TET0(JJ+1,JJ+1)=A
          TET0(JJ+3,JJ+3)=A
          TET0(JJ+1,JJ+3)=B
          TET0(JJ+3,JJ+1)=-B
          TET0(JJ+2,JJ+2)=1.D0
 40     CONTINUE
        TET0(7,7)=1.D0
        TET0(8,8)=1.D0
        TET0(9,9)=1.D0
        TET0(10,10)=1.D0
      ELSEIF(IAX.EQ.1) THEN
        DO 50 I=1,2
          JJ=3*(I-1)
          TET0(JJ+2,JJ+2)=A
          TET0(JJ+3,JJ+3)=A
          TET0(JJ+2,JJ+3)=-B
          TET0(JJ+3,JJ+2)=B
          TET0(JJ+1,JJ+1)=1.D0
 50     CONTINUE
        TET0(7,7)=1.D0
        TET0(8,8)=1.D0
        TET0(9,9)=1.D0
        TET0(10,10)=1.D0
      ELSE
        CALL U2MESG('F', 'ALGORITH13_28',0,' ',0,0,0,0.D0)
      ENDIF
C
C
      END
