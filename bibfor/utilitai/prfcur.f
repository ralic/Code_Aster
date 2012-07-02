      SUBROUTINE PRFCUR(VEC1,NBN,VEC2,NBP,INTERP,PROLGD)
      IMPLICIT NONE
C     ------------------------------------------------------------------
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
C     PROLONGEMENT DE LA FONCTION SUIVANT OPTION CHOISIE
C     ------------------------------------------------------------------
C     IN  : VEC1   : POINTEUR DE NOEUDS DANS LA LISTE DE NOEUDS
C     IN  : NBN    : DIMENSION DU VECTEUR VEC1
C     I/O : VEC2   : VALEURS DE LA FONCTION
C     IN  : NBP    : DIMENSION DU VECTEUR VEC2
C     IN  : INTERP : TYPE INTERPOLATION DE LA FONCTION
C     IN  : PROLGD : TYPE DE PROLONGEMENT DE LA FONCTION
      INCLUDE 'jeveux.h'
      INTEGER      VEC1(NBN)
      REAL*8       VEC2(NBP)
      CHARACTER*2  PROLGD
      CHARACTER*8  INTERP
      INTEGER I ,ID ,IDE ,IF ,IFI ,IP ,J
      INTEGER NBN ,NBP ,NBP2
      REAL*8 RESU ,X ,X1 ,X2 ,Y1 ,Y2, XLINE, XLOG
C-----------------------------------------------------------------------
      XLINE(X,X1,Y1,X2,Y2) = Y1+(X-X1)*(Y2-Y1)/(X2-X1)
      XLOG(X,X1,X2,Y1,Y2)=EXP(LOG(Y1)+(LOG(X)-LOG(X1))*(LOG(Y2)-LOG(Y1))
     +                                /(LOG(X2)-LOG(X1)))
C     ------------------------------------------------------------------
C
C     --- INTERPOLATION DE LA FONCTION ---
C
      DO 10 I = 1,NBN-1
        IDE = VEC1(I)
        IFI = VEC1(I+1)
        IF (IDE+1 .NE. IFI) THEN
          IP = (IFI-IDE)-1
          DO 20 J =1,IP
            IF (INTERP(1:3) .EQ. 'LIN') THEN
              VEC2(2*IDE+2*J) = XLINE(VEC2(2*IDE+2*J-1),VEC2(2*IDE-1),
     +                          VEC2(2*IDE),VEC2(2*IFI-1),VEC2(2*IFI))
            ELSEIF (INTERP(1:3) .EQ. 'LOG') THEN
              VEC2(2*IDE+2*J) = XLOG(VEC2(2*IDE+2*J-1),VEC2(2*IDE-1),
     +                          VEC2(2*IDE),VEC2(2*IFI-1),VEC2(2*IFI))
            ENDIF
  20      CONTINUE
        ENDIF
  10  CONTINUE
C
C     --- PROLONGEMENT A GAUCHE ---
C
      ID = VEC1(1)
      IF (PROLGD(1:1) .EQ. 'C') THEN
        DO 30  I=1,ID-1
          VEC2(2*I) = VEC2(2*ID-1)
   30   CONTINUE
      ELSE IF (PROLGD(1:1) .EQ . 'L') THEN
        DO 40 I=1,ID-1
          RESU = XLINE(VEC2(2*I-1),VEC2(2*ID-1),VEC2(2*ID),
     +          VEC2(2*(ID+1)-1),VEC2(2*(ID+1)))
          IF (RESU .LT. 0.D0) THEN
            RESU = 0.D0
          ENDIF
          VEC2(2*I) = RESU
  40    CONTINUE
      ENDIF
C
C     --- PROLONGEMENT A DROITE ---
C
      NBP2 = NBP/2
      IF = VEC1(NBN)
      IF (PROLGD(2:2) .EQ. 'C') THEN
        DO 50 I=IF+1,NBP2
          VEC2(2*I) = VEC2(2*IF)
  50    CONTINUE
      ELSE IF (PROLGD(2:2) .EQ. 'L') THEN
        DO 60 I=IF+1,NBP2
          RESU = XLINE(VEC2(2*I-1),VEC2(2*IF-1),VEC2(2*IF),
     +                VEC2(2*(IF-1)-1),VEC2(2*(IF-1)))
          IF (RESU .LT. 0.D0) THEN
            RESU = 0.D0
          ENDIF
          VEC2(I) = RESU
  60    CONTINUE
      ENDIF
      END
