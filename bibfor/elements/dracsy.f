      SUBROUTINE DRACSY(A0,B0,C0,D0,E0,F0,NBROOT,X,Y)

      IMPLICIT NONE

C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
      REAL*8     A0(2),B0(2),C0(2),D0(2),E0(2),F0(2)
      INTEGER    NBROOT 
      REAL*8     X(8),Y(8)

      REAL*8  A(2),B(2),C(2),D(2),E(2),F(2)
      REAL*8 FF(2),AUX
      REAL*8 POLY(5),XX(4),YY(2),AA,BB,CC,COEF(2)
      INTEGER DEG,NBXX,NBYY,I,J,NBROOX
       REAL*8   ZERO
       INTEGER     II,JJ

       DATA     ZERO  /1.D-8/ 
       
      NBROOT=0
       CALL R8INIR(8,0.0D0,X,1)
       CALL R8INIR(8,0.0D0,Y,1)

      COEF(1)=MAX(ABS(A0(1)),ABS(B0(1)),ABS(C0(1)),
     &          ABS(D0(1)),ABS(E0(1)),ABS(F0(1)))
      COEF(2)=MAX(ABS(A0(2)),ABS(B0(2)),ABS(C0(2)),
     &          ABS(D0(2)),ABS(E0(2)),ABS(F0(2)))
      IF((COEF(1) .EQ. 0.D0).OR.(COEF(2) .EQ. 0.D0)) GOTO 50
      IF((COEF(1) .LT. ZERO*COEF(2))
     &    .OR.(COEF(2) .LT. ZERO*COEF(1))) GOTO 50
      DO 10, I=1,2
         A(I)=A0(I)/COEF(I)
         B(I)=B0(I)/COEF(I)
         C(I)=C0(I)/COEF(I)
         D(I)=D0(I)/COEF(I)
         E(I)=E0(I)/COEF(I)
         F(I)=F0(I)/COEF(I)
10    CONTINUE

      POLY(5)=-2*F(2)*A(1)*F(1)*A(2)-C(2)*F(2)*C(1)*A(1)
     & +F(1)*C(2)**2*A(1)+F(1)**2*A(2)**2+A(2)*F(2)*C(1)**2
     & +F(2)**2*A(1)**2-C(2)*C(1)*F(1)*A(2)

      POLY(4)=2*F(2)**2*A(1)*B(1)+B(2)*F(2)*C(1)**2
     & -2*F(2)*A(1)*F(1)*B(2)
     & -2*F(2)*B(1)*F(1)*A(2)+2*F(1)**2*B(2)*A(2)-C(2)*F(2)*C(1)*B(1)
     & -C(2)*C(1)*F(1)*B(2)-C(2)*F(2)*D(1)*A(1)-C(2)*D(1)*F(1)*A(2)
     & +F(1)*C(2)**2*B(1)+2*C(2)*F(1)*D(2)*A(1)-D(2)*F(2)*C(1)*A(1)
     & -D(2)*C(1)*F(1)*A(2)+2*A(2)*F(2)*C(1)*D(1)

      POLY(3)=2*F(2)**2*A(1)*E(1)+F(1)**2*B(2)**2+E(2)*F(2)*C(1)**2
     & +A(2)*F(2)*D(1)**2+F(2)**2*B(1)**2-2*F(2)*A(1)*F(1)*E(2)
     & -2*F(2)*B(1)*F(1)*B(2)-2*F(2)*E(1)*F(1)*A(2)+2*F(1)**2*A(2)*E(2)
     & -C(2)*F(2)*C(1)*E(1)-C(2)*C(1)*F(1)*E(2)-C(2)*F(2)*D(1)*B(1)
     & -C(2)*D(1)*F(1)*B(2)+F(1)*C(2)**2*E(1)+2*F(1)*C(2)*D(2)*B(1)
     & -D(2)*F(2)*C(1)*B(1)-D(2)*C(1)*F(1)*B(2)-D(2)*F(2)*D(1)*A(1)
     & -D(2)*D(1)*F(1)*A(2)+D(2)**2*F(1)*A(1)+2*B(2)*F(2)*C(1)*D(1)

      POLY(2)=2*F(2)**2*B(1)*E(1)+B(2)*F(2)*D(1)**2
     & -2*F(2)*B(1)*F(1)*E(2)
     & -2*F(2)*F(1)*B(2)*E(1)+2*F(1)**2*B(2)*E(2)-C(2)*F(2)*D(1)*E(1)
     & -C(2)*D(1)*F(1)*E(2)+2*F(1)*C(2)*D(2)*E(1)-D(2)*F(2)*C(1)*E(1)
     & -D(2)*C(1)*F(1)*E(2)-D(2)*F(2)*D(1)*B(1)-D(2)*D(1)*F(1)*B(2)
     & +D(2)**2*F(1)*B(1)+2*E(2)*F(2)*C(1)*D(1)

      POLY(1)=F(1)**2*E(2)**2+E(2)*F(2)*D(1)**2+F(2)**2*E(1)**2
     & -2*F(2)*E(1)*F(1)*E(2)-D(2)*F(2)*D(1)*E(1)-D(2)*D(1)*F(1)*E(2)
     & +D(2)**2*F(1)*E(1)

      DEG=4

      CALL DRAACN(DEG,POLY,NBXX,XX)

      NBROOT=0
       CALL R8INIR(8,0.0D0,X,1)
       CALL R8INIR(8,0.0D0,Y,1)

      IF (NBXX .GT.0) THEN
         DO 40, I=1,NBXX
            NBROOX=0
            AA = F(1)
            BB = C(1) + D(1)*XX(I)
            CC = A(1) + B(1)*XX(I) + E(1)*XX(I)**2 
            CALL DRAAC2(AA,BB,CC,YY(1),YY(2),NBYY)
            IF (NBYY .GT.0) THEN
             DO 20, J=1,NBYY
                 FF(J)= A(2)+B(2)*XX(I)+C(2)*YY(J)+D(2)*XX(I)*YY(J)
     &                 +E(2)*XX(I)**2 + F(2)*YY(J)**2
                 IF (ABS(FF(J)) .LT. 1.D-7) THEN
                    NBROOT=NBROOT+1
                    X(NBROOT)=XX(I)
                    Y(NBROOT)=YY(J)
                    NBROOX=NBROOX+1
                 ENDIF
 20          CONTINUE
             IF (NBROOX .EQ. 0) THEN
              AA = F(2)
              BB = C(2) + D(2)*XX(I)
              CC = A(2) + B(2)*XX(I) + E(2)*XX(I)**2 
              CALL DRAAC2(AA,BB,CC,YY(1),YY(2),NBYY)
              IF (NBYY .GT.0) THEN
                  DO 30, J=1,NBYY
                      FF(J)= A(1)+B(1)*XX(I)+C(1)*YY(J)+D(1)*XX(I)*YY(J)
     &                      +E(1)*XX(I)**2 + F(1)*YY(J)**2
                      IF (ABS(FF(J)) .LT. 1.D-7) THEN
                         NBROOT=NBROOT+1
                         X(NBROOT)=XX(I)
                         Y(NBROOT)=YY(J)
                         NBROOX=NBROOX+1
                      ENDIF
 30               CONTINUE
                  IF ((NBROOX .EQ. 0).AND.(NBYY .EQ. 2)) THEN
                      AUX=ABS(FF(1)/FF(2))
                      IF ((AUX .LT. 1.D-5)
     &                   .AND.(FF(1)) .LT. 1.D-4)THEN
                         NBROOT=NBROOT+1
                         X(NBROOT)=XX(I)
                         Y(NBROOT)=YY(1)
                      ELSEIF ((AUX .GT. 1.D5)
     &                       .AND.(FF(2)) .LT. 1.D-4)THEN
                         NBROOT=NBROOT+1
                         X(NBROOT)=XX(I)
                         Y(NBROOT)=YY(2)
                      ELSEIF ((ABS(FF(1)) .LT. 1.D-4)
     &                   .AND.(ABS(FF(2)) .LT. 1.D-4))THEN
                         NBROOT=NBROOT+1
                         X(NBROOT)=XX(I)
                         Y(NBROOT)=YY(1)
                         NBROOT=NBROOT+1
                         X(NBROOT)=XX(I)
                         Y(NBROOT)=YY(2)
                      ENDIF
                  ENDIF
              ENDIF
             ENDIF
            ENDIF
 40      CONTINUE
      ENDIF 

 50    CONTINUE
      END 
