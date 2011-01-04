      SUBROUTINE DGMPLA(EB,NUB,EA,SYA,NUM,NUF,H,A,B1,B,NNAP,RX,RY,
     &                  MP,DRP,W)

      IMPLICIT   NONE

C - PARAMETRES ENTRANTS

      REAL*8  EB,NUB,EA(*),SYA(*),NUM,NUF
      REAL*8  H,A,B1,B,MP,RX(*),RY(*)
      INTEGER NNAP,ILIT1,NA

C - PARAMETRES SORTANTS
      REAL*8  DRP,D3,W

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/01/2011   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE SFAYOLLE S.FAYOLLE
C ----------------------------------------------------------------------
C
C BUT : CALCUL DES INFORMATIONS NECESSAIRES A LA DETERMINATION DES
C PENTES POST ELASTIQUE EN FLEXION DE GLRC_DM OPTION PLAS
C
C ----------------------------------------------------------------------

C - PARAMETRES INTERMEDIAIRES
      REAL*8  DELTA,X1,X2,M,F,P
      REAL*8  D1,D2,D4,D5,A1,Z,C

      M=EB/(1.D0-NUB**2)*(1.D0-NUB*NUM)
      F=EB/(1.D0-NUB**2)*(1.D0-NUB*NUF)
      A1=(F+M)*H**2
      Z=-(B*H+M*H**2/2.D0)
      C=-F*H**2/4.D0-H*B1

      DELTA=Z*Z-4.D0*A1*C
      X1=(Z-SQRT(DELTA))/(2.D0*A1)
      X2=(Z+SQRT(DELTA))/(2.D0*A1)

      IF ((X1 .GT. 0.D0) .AND. (X1 .LT. 1.D0)) THEN
        W=X1
      ELSEIF ((X2 .GT. 0.D0) .AND. (X2 .LT. 1.D0)) THEN
        W=X2
      ENDIF

      D1=EB/(1.D0-NUB**2)*(1.D0-NUB*NUM)*H/2.D0*(1.D0-2.D0*W)+B
      D2=-EB/(1.D0-NUB**2)*(1.D0-NUB*NUF)*H**2/8.D0*
     &   (1.D0-4.D0*W**2)-H*B1
      D3=-D2/D1
      P=(RX(1)+RY(1))/2.D0
      DRP=ABS(SYA(1)/(EA(1)*(D3+P*H)))

      DO 10, ILIT1 = 1,NNAP
        P=(RX(ILIT1)+RY(ILIT1))/2.D0
        IF (ABS(SYA(ILIT1)/(EA(ILIT1)*(D3-P*H))) .LT. DRP) THEN
          DRP=ABS(SYA(ILIT1)/(EA(ILIT1)*(D3-P*H)))
        ENDIF
 10   CONTINUE

      ILIT1 = NNAP
      D4=EB/(1.D0-NUB**2)*(-(1.D0-NUB*NUM)*D3*H**2/8.D0*
     &   (1.D0-4.D0*W**2)+(1.D0-NUB*NUF)*H**3/24.D0*(1.D0-8.D0*W**3))
      D5=H*(-B1*D3+A*H)
      C=D4+D5
      MP=C*DRP

      END
