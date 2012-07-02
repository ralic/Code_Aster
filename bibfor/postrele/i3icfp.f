      SUBROUTINE I3ICFP(EPSI,FGLO,SGT,FLOC1,FLOC2,FLOC3,TYPF,NBPT,IRET)
      IMPLICIT NONE
C
      INTEGER NBPT,TYPF,IRET
      REAL*8 EPSI,FGLO(3,*),SGT(*),FLOC1(*),FLOC2(*),FLOC3(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ------------------------------------------------------------------
C     INTERSECTION SGT COUPR DE FACE GAUCHE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  FGLO   : R : TABLE(1..3,1..3) : COORDO DE LA COUPE DANS (S1,EI)
C IN  SGT    : R : TABLE(1..6)      : COORDO DU SEGMENT  DANS (S1,EI)
C VAR FLOC1  : R : TABLE(1..3)
C            :   :   IN  : COORDO DEBUT COUPE DANS REPERE COUPE
C            :   :   OUT : REPERAGE POINT 1 CALCULE (PARAB,SGT)
C VAR FLOC2  : R : TABLE(1..3)
C            :   :   IN  : COORDO FIN COUPE DANS REPERE COUPE
C            :   :   OUT : REPERAGE POINT 2 CALCULE (PARAB,SGT)
C IN  FLOC3  : R : TABLE(1..3) : COORDO MILIEU COUPE DANS REPERE COUPE
C OUT TYPF   : I : CODE DE CONVEXITE DE LA FACE PAR RAPPORT A LA COUPE
C OUT NBPT   : I : NBR DE POINT CALCULES (-2 = INFINI)
C OUT IRET   : I : CODE RETOUR -1 = DEGENERESCE
C     ------------------------------------------------------------------
C     PARAMETRAGE COUPE = (-1,1)
C     PARAMETRAGE SGT   = ( 0,1)
C     ------------------------------------------------------------------
C
      REAL*8  ZERO,UNSUR2,UN,DEUX
      REAL*8  X1,X2,Y1,Y2,X3,Y3,XA,YA,XB,YB,DX,DY,A,B,C,D,R1,R2,T1,T2
      LOGICAL OKT1,OKT2,OKR1,OKR2
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ZERO   = 0.0D0
      UNSUR2 = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      IRET   = 0
      NBPT   = 0
      TYPF   = 0
      XA     = SGT(1)
      YA     = SGT(2)
      XB     = SGT(4)
      YB     = SGT(5)
      X1     = FGLO(1,1)
      Y1     = FGLO(2,1)
      X2     = FGLO(1,2)
      Y2     = FGLO(2,2)
      X3     = FGLO(1,3)
      Y3     = FGLO(2,3)
      DX     = UN/(FLOC2(1)-FLOC1(1))
      XA     = ((XA-X1)*(X2-X1)+(YA-Y1)*(Y2-Y1))*DX
      XB     = ((XB-X1)*(X2-X1)+(YB-Y1)*(Y2-Y1))*DX
      YA     = SGT(3)
      YB     = SGT(6)
      X1     = FLOC1(1)
      Y1     = FLOC1(2)
      X2     = FLOC2(1)
      Y2     = FLOC2(2)
      X3     = FLOC3(1)
      Y3     = FLOC3(2)
      D      = (X3-X1)*(Y2-Y1) - (X2-X1)*(Y3-Y1)
      R1     = MAX(ABS(X1),ABS(X2),ABS(X3),ABS(Y1),ABS(Y2),ABS(Y3))
      R2     = MAX(ABS(XA),ABS(XB),ABS(YA),ABS(YB))
      IF ( ABS(D) .LT. EPSI*R1 ) THEN
         CALL I3ICFS(EPSI,FGLO,SGT,FLOC1,FLOC2,NBPT,IRET)
         TYPF = 0
      ELSE
         IF ( D .GE. ZERO ) THEN
            TYPF = -1
         ELSE
            TYPF =  1
         ENDIF
         DX =  XB - XA
         DY =  YB - YA
         A  = (UNSUR2*(X2+X1)-X3)*DY - (UNSUR2*(Y2+Y1)-Y3)*DX
         B  =  UNSUR2*(DY*(X2-X1)-DX*(Y2-Y1))
         C  =  DY*(X3-XA)-DX*(Y3-YA)
         D  = MAX(ABS(A),ABS(B),ABS(C))
         IF ( ABS(D) .LE. EPSI*R1*R2 ) THEN
            IRET = -1
            NBPT = 0
         ELSE
            D = UN/D
            A = A*D
            B = B*D
            C = C*D
            D = B*B - (DEUX+DEUX)*A*C
            IF ( ABS(A) .GT. EPSI) THEN
               IF ( D .GT. EPSI ) THEN
                  A    = UNSUR2/A
                  R1   = A*(-B-SQRT(D))
                  R2   = A*(-B+SQRT(D))
                  OKR1 = (((-UN-EPSI).LE.R1).AND.(R1.LE.(UN+EPSI)))
                  OKR2 = (((-UN-EPSI).LE.R2).AND.(R2.LE.(UN+EPSI)))
                  IF ( OKR1 .OR. OKR2 ) THEN
                     B  = (X2-X1)*UNSUR2
                     C  = (UNSUR2*(X1+X2)-X3)
                     X1 = X3 + R1*(B + R1*C)
                     X2 = X3 + R2*(B + R2*C)
                     B  = (Y2-Y1)*UNSUR2
                     C  = (UNSUR2*(Y1+Y2)-Y3)
                     Y1 = Y3 + R1*(B + R1*C)
                     Y2 = Y3 + R2*(B + R2*C)
                     D  = (XA-XB)*(XA-XB)+(YA-YB)*(YA-YB)
                     IF ( ABS(D) .GT. EPSI ) THEN
                        D    = UN/D
                        T1   = ((X1-XA)*DX+(Y1-YA)*DY)*D
                        T2   = ((X2-XA)*DX+(Y2-YA)*DY)*D
                        OKT1 = ((-EPSI.LE.T1).AND.(T1.LE.(UN+EPSI)))
                        OKT2 = ((-EPSI.LE.T2).AND.(T2.LE.(UN+EPSI)))
                        IF ( OKT1 .AND. OKR1 ) THEN
                           IF ( OKT2 .AND. OKR2 ) THEN
                              NBPT = 2
                              IF ( T1 .GT. T2 ) THEN
                                 D  = T1
                                 T1 = T2
                                 T2 = D
                                 D  = R1
                                 R1 = R2
                                 R2 = D
                              ENDIF
                           ELSE
                              NBPT = 1
                           ENDIF
                        ELSE IF ( OKT2 .AND. OKR2 ) THEN
                           NBPT = 1
                           T1   = T2
                           R1   = R2
                        ELSE
                           NBPT = 0
                        ENDIF
                     ELSE
                        IRET = -1
                        NBPT = 0
                     ENDIF
                  ELSE
                     NBPT = 0
                  ENDIF
               ELSE
                  NBPT = 0
               ENDIF
            ELSE
               IF ( ABS(B) .LE. EPSI*R1*R2 ) THEN
                  NBPT = 0
               ELSE
                  R1   = -C/B
                  OKR1 = ( (-EPSI .LE. R1) .AND. (R1 .LE. (UN+EPSI)) )
                  IF ( OKR1 ) THEN
                     D  = (XA-XB)*(XA-XB)+(YA-YB)*(YA-YB)
                     IF ( ABS(D) .GT. EPSI ) THEN
                        D    = UN/D
                        T1   = ((X1-XA)*DX+(Y1-YA)*DY)*D
                        T2   = ((X2-XA)*DX+(Y2-YA)*DY)*D
                        OKT1 = ((-EPSI.LE.T1).AND.(T1.LE.(UN+EPSI)))
                        IF ( OKT1 ) THEN
                           NBPT = 1
                        ELSE
                           NBPT = 0
                        ENDIF
                     ELSE
                        NBPT =  0
                        IRET = -1
                     ENDIF
                  ELSE
                     NBPT = 0
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF ( TYPF .NE. 0 ) THEN
         IF ( NBPT .GE. 1 ) THEN
            FLOC1(1) = R1
            FLOC1(2) = T1
         ENDIF
         IF ( NBPT .GE. 2 ) THEN
            FLOC2(1) = R2
            FLOC2(2) = T2
         ENDIF
      ENDIF
      END
