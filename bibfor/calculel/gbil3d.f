      SUBROUTINE GBIL3D(DUDM,DVDM,DTDM,POIDS,C1,C2,C3,G)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT NONE
C
      REAL*8 DUDM(3,3),DVDM(3,3),DTDM(3,3)
      REAL*8 C1,C2,C3,POIDS,G
C
C ----------------------------------------------------------------------
C     CALCUL DU TAUX DE RESTITUTION D'ENERGIE G SOUS LA FORME
C     BILINEAIRE SYMETRIQUE G(U,V) EN ELASTICITE LINEAIRE EN 3D
C ----------------------------------------------------------------------
C
      REAL*8  VECT(33),S11,S12,S13,S21,S22,S23,S1,S2,SA21,SA22,SA23
      REAL*8  TCLA,DIVT,BIL(3,3,3,3)
      INTEGER I,J,K,P,L,M
C
      DIVT = DTDM(1,1)+DTDM(2,2)+DTDM(3,3)
C
C - TERME CLASSIQUE 
C
      VECT(1)= 0.5D0*(DVDM(1,1)*DUDM(2,2)+DUDM(1,1)*DVDM(2,2))
      VECT(2)= 0.5D0*(DVDM(1,1)*DUDM(1,2)+DUDM(1,1)*DVDM(1,2))
      VECT(3)= 0.5D0*(DVDM(1,1)*DUDM(2,1)+DUDM(1,1)*DVDM(2,1))
      VECT(4)= 0.5D0*(DVDM(2,2)*DUDM(1,2)+DUDM(2,2)*DVDM(1,2))
      VECT(5)= 0.5D0*(DVDM(2,2)*DUDM(2,1)+DUDM(2,2)*DVDM(2,1))
      VECT(6)= 0.5D0*(DVDM(1,2)*DUDM(2,1)+DUDM(1,2)*DVDM(2,1))
      VECT(7)= 0.5D0*(DVDM(3,3)*DUDM(3,1)+DUDM(3,3)*DVDM(3,1))
      VECT(8)= 0.5D0*(DVDM(1,1)*DUDM(1,3)+DUDM(1,1)*DVDM(1,3))
      VECT(9)= 0.5D0*(DVDM(3,3)*DUDM(3,2)+DUDM(3,3)*DVDM(3,2))
      VECT(10)= 0.5D0*(DVDM(2,2)*DUDM(2,3)+DUDM(2,2)*DVDM(2,3))
      VECT(11)= 0.5D0*(DVDM(1,1)*DUDM(3,3)+DUDM(1,1)*DVDM(3,3))
      VECT(12)= 0.5D0*(DVDM(2,2)*DUDM(3,3)+DUDM(2,2)*DVDM(3,3))
      VECT(13)= 0.5D0*(DVDM(3,3)*DUDM(1,2)+DUDM(3,3)*DVDM(1,2))
      VECT(14)= 0.5D0*(DVDM(3,3)*DUDM(2,1)+DUDM(3,3)*DVDM(2,1))
      VECT(15)= 0.5D0*(DVDM(1,1)*DUDM(3,1)+DUDM(1,1)*DVDM(3,1))
      VECT(16)= 0.5D0*(DVDM(2,2)*DUDM(3,1)+DUDM(2,2)*DVDM(3,1))
      VECT(17)= 0.5D0*(DVDM(1,1)*DUDM(3,2)+DUDM(1,1)*DVDM(3,2))
      VECT(18)= 0.5D0*(DVDM(2,2)*DUDM(3,2)+DUDM(2,2)*DVDM(3,2))
      VECT(19)= 0.5D0*(DVDM(1,1)*DUDM(2,3)+DUDM(1,1)*DVDM(2,3))
      VECT(20)= 0.5D0*(DVDM(3,3)*DUDM(2,3)+DUDM(3,3)*DVDM(2,3))
      VECT(21)= 0.5D0*(DVDM(2,2)*DUDM(1,3)+DUDM(2,2)*DVDM(1,3))
      VECT(22)= 0.5D0*(DVDM(3,3)*DUDM(1,3)+DUDM(3,3)*DVDM(1,3))
      VECT(23)= 0.5D0*(DVDM(1,3)*DUDM(3,1)+DUDM(1,3)*DVDM(3,1))
      VECT(24)= 0.5D0*(DVDM(2,3)*DUDM(3,2)+DUDM(2,3)*DVDM(3,2))
      VECT(25)= 0.5D0*(DVDM(2,3)*DUDM(3,1)+DUDM(2,3)*DVDM(3,1))
      VECT(26)= 0.5D0*(DVDM(3,2)*DUDM(3,1)+DUDM(3,2)*DVDM(3,1))
      VECT(27)= 0.5D0*(DVDM(1,3)*DUDM(3,2)+DUDM(1,3)*DVDM(3,2))
      VECT(28)= 0.5D0*(DVDM(1,2)*DUDM(1,3)+DUDM(1,2)*DVDM(1,3))
      VECT(29)= 0.5D0*(DVDM(2,1)*DUDM(1,3)+DUDM(2,1)*DVDM(1,3))
      VECT(30)= 0.5D0*(DVDM(1,2)*DUDM(3,1)+DUDM(1,2)*DVDM(3,1))
      VECT(31)= 0.5D0*(DVDM(2,1)*DUDM(2,3)+DUDM(2,1)*DVDM(2,3))
      VECT(32)= 0.5D0*(DVDM(2,1)*DUDM(3,2)+DUDM(2,1)*DVDM(3,2))
      VECT(33)= 0.5D0*(DVDM(2,3)*DUDM(1,2)+DUDM(2,3)*DVDM(1,2))
C

 
      S11 =  DUDM(1,1)*DVDM(1,1) + DUDM(2,2)*DVDM(2,2) 
     &     + DUDM(3,3)*DVDM(3,3)
      S12 =  DUDM(1,1)*DVDM(2,2) + DUDM(2,2)*DVDM(1,1)
     &     + DUDM(1,1)*DVDM(3,3) + DUDM(3,3)*DVDM(1,1)
     &     + DUDM(2,2)*DVDM(3,3) + DUDM(3,3)*DVDM(2,2)
      S13 = (DUDM(1,2)+DUDM(2,1))*(DVDM(1,2)+DVDM(2,1))
     &     + (DUDM(2,3)+DUDM(3,2))*(DVDM(2,3)+DVDM(3,2))
     &     + (DUDM(3,1)+DUDM(1,3))*(DVDM(3,1)+DVDM(1,3))
C
      S21 =  DUDM(1,1)*DVDM(1,1)*DTDM(1,1)
     &     + DUDM(2,2)*DVDM(2,2)*DTDM(2,2)
     &     + DUDM(3,3)*DVDM(3,3)*DTDM(3,3)
     &     + VECT(5)*DTDM(1,2)
     &     + VECT(2)*DTDM(2,1)
     &     + VECT(7)*DTDM(1,3)
     &     + VECT(8)*DTDM(3,1)
     &     + VECT(9)*DTDM(2,3)
     &     + VECT(10)*DTDM(3,2)
C     
      S22 = VECT(1)*(DTDM(1,1)+DTDM(2,2))
     &     +VECT(11)*(DTDM(1,1)+DTDM(3,3))
     &     +VECT(12)*(DTDM(2,2)+DTDM(3,3))
     &     +(VECT(3)+VECT(14))* DTDM(1,2)
     &     +(VECT(4)+VECT(13))* DTDM(2,1)
     &     +(VECT(15)+VECT(16))* DTDM(1,3)
     &     +(VECT(17)+VECT(18))* DTDM(2,3)
     &     +(VECT(19)+VECT(20))* DTDM(3,2)
     &     +(VECT(21)+VECT(22))* DTDM(3,1)
C
      S23 = (VECT(6)+DUDM(2,1)*DVDM(2,1))*DTDM(1,1)
     &     +(VECT(23)+DUDM(3,1)*DVDM(3,1))*DTDM(1,1)
     &     +(VECT(6)+DUDM(1,2)*DVDM(1,2))*DTDM(2,2)
     &     +(VECT(24)+DUDM(3,2)*DVDM(3,2))*DTDM(2,2)
     &     +(VECT(24)+DUDM(2,3)*DVDM(2,3))*DTDM(3,3)
     &     +(VECT(23)+DUDM(1,3)*DVDM(1,3))*DTDM(3,3)
     &     +(VECT(2)+VECT(3))*DTDM(1,2)
     &     +(VECT(25)+VECT(26))*DTDM(1,2)
     &     +(VECT(4)+VECT(5))*DTDM(2,1)
     &     +(VECT(26)+VECT(27))*DTDM(2,1)
     &     +(VECT(28)+VECT(29))*DTDM(3,2)
     &     +(VECT(9)+VECT(20))*DTDM(3,2)
     &     +(VECT(10)+VECT(18))*DTDM(2,3)
     &     +(VECT(28)+VECT(30))*DTDM(2,3)
     &     +(VECT(8)+VECT(15))*DTDM(1,3)
     &     +(VECT(31)+VECT(32))*DTDM(1,3)
     &     +(VECT(31)+VECT(33))*DTDM(3,1)
     &     +(VECT(22)+VECT(7))*DTDM(3,1)
C
       S1 = C1*S11 +C2*S12 +C3*S13
       S2 = C1*S21 +C2*S22 +C3*S23
C
      TCLA = (-DIVT/2.D0*S1+ S2)*POIDS
C
      G  = TCLA
C--------------------------AUTRE MANIÈRE DE CALCUL POUR S2----------
      DO 100 I=1,3
        DO 101 J=1,3
          DO 102 K=1,3
            DO 103 L=1,3
            BIL(I,J,K,L)=0.5D0*(DUDM(I,J)*DVDM(K,L)+DUDM(K,L)*DVDM(I,J))
 103        CONTINUE
 102      CONTINUE
 101    CONTINUE
 100  CONTINUE

      SA21=0.D0
      DO 10 K=1,3
        DO 20 P=1,3
          SA21=SA21+BIL(K,K,K,P)*DTDM(P,K)
 20     CONTINUE
 10   CONTINUE

      SA22=0.D0
      DO 300 K=1,3
        DO 301 L=1,3
          IF (L.NE.K) THEN
            DO 302 P=1,3
              SA22=SA22+BIL(L,L,K,P)*DTDM(P,K)
 302        CONTINUE
          ENDIF
 301    CONTINUE  
 300  CONTINUE

      SA23=0.D0
      DO 400 K=1,3
        DO 401 L=1,3
          IF (L.NE.K) THEN
            DO 402 M=1,3
              IF (M.NE.K.AND.M.NE.L) THEN
                DO 403 P=1,3
                  SA23=SA23+BIL(L,M,L,P)*DTDM(P,M)
                  SA23=SA23+BIL(L,M,M,P)*DTDM(P,L)
 403            CONTINUE
              ENDIF
 402        CONTINUE
          ENDIF
 401    CONTINUE
 400  CONTINUE

      IF (ABS(SA21-S21).GT.1.D-14) CALL UTMESS('A','GBIL3D','SA21')
      IF (ABS(SA22-S22).GT.1.D-14) CALL UTMESS('A','GBIL3D','SA22')
      IF (ABS(SA23-S23).GT.1.D-14) CALL UTMESS('A','GBIL3D','SA23')

      END
