      SUBROUTINE GBIL3D(DUDM,DVDM,DTDM,DFUDM,DFVDM,TGUDM,TGVDM,
     &                  TTRGU,TTRGV,POIDS,C1,C2,C3,K3A,RHO,PULS,G)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/10/2005   AUTEUR GALENNE E.GALENNE 
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
      REAL*8 DUDM(3,4),DVDM(3,4),DTDM(3,4)
      REAL*8 DFUDM(3,4),DFVDM(3,4),TGUDM(3),TGVDM(3)
      REAL*8 C1,C2,C3,POIDS,G,K3A,RHO,PULS,TTRGU,TTRGV,GCLA
      REAL*8 ENETHU(2),ENETHV(2)
C
C ----------------------------------------------------------------------
C     CALCUL DU TAUX DE RESTITUTION D'ENERGIE G SOUS LA FORME
C     BILINEAIRE SYMETRIQUE G(U,V) EN ELASTICITE LINEAIRE EN 3D
C ----------------------------------------------------------------------
C
      REAL*8 VECT(33),S11,S12,S13,S21,S22,S23,S1,S2,SA21,SA22,SA23
      REAL*8 TCLA,DIVT,BIL(3,3,3,3)
      REAL*8 DIVV,DIVU,TTT1U,TTT1V
      REAL*8 SOMM1U,SOMM2U,SOMM1V,SOMM2V,PROD
      REAL*8 S2TH1U,S2TH2U,S2TH3U,S2TH1V,S2TH2V,S2TH3V
      REAL*8 S1TH,S2TH,TTHER,TFOR,TDYN,R8BID
      INTEGER I,J,K,P,L,M
C
      DIVT = DTDM(1,1) + DTDM(2,2) + DTDM(3,3)
C
C - TERME CLASSIQUE S1 + S2
C
      VECT(1) = 0.5D0 * (DVDM(1,1)*DUDM(2,2)+DUDM(1,1)*DVDM(2,2))
      VECT(2) = 0.5D0 * (DVDM(1,1)*DUDM(1,2)+DUDM(1,1)*DVDM(1,2))
      VECT(3) = 0.5D0 * (DVDM(1,1)*DUDM(2,1)+DUDM(1,1)*DVDM(2,1))
      VECT(4) = 0.5D0 * (DVDM(2,2)*DUDM(1,2)+DUDM(2,2)*DVDM(1,2))
      VECT(5) = 0.5D0 * (DVDM(2,2)*DUDM(2,1)+DUDM(2,2)*DVDM(2,1))
      VECT(6) = 0.5D0 * (DVDM(1,2)*DUDM(2,1)+DUDM(1,2)*DVDM(2,1))
      VECT(7) = 0.5D0 * (DVDM(3,3)*DUDM(3,1)+DUDM(3,3)*DVDM(3,1))
      VECT(8) = 0.5D0 * (DVDM(1,1)*DUDM(1,3)+DUDM(1,1)*DVDM(1,3))
      VECT(9) = 0.5D0 * (DVDM(3,3)*DUDM(3,2)+DUDM(3,3)*DVDM(3,2))
      VECT(10) = 0.5D0 * (DVDM(2,2)*DUDM(2,3)+DUDM(2,2)*DVDM(2,3))
      VECT(11) = 0.5D0 * (DVDM(1,1)*DUDM(3,3)+DUDM(1,1)*DVDM(3,3))
      VECT(12) = 0.5D0 * (DVDM(2,2)*DUDM(3,3)+DUDM(2,2)*DVDM(3,3))
      VECT(13) = 0.5D0 * (DVDM(3,3)*DUDM(1,2)+DUDM(3,3)*DVDM(1,2))
      VECT(14) = 0.5D0 * (DVDM(3,3)*DUDM(2,1)+DUDM(3,3)*DVDM(2,1))
      VECT(15) = 0.5D0 * (DVDM(1,1)*DUDM(3,1)+DUDM(1,1)*DVDM(3,1))
      VECT(16) = 0.5D0 * (DVDM(2,2)*DUDM(3,1)+DUDM(2,2)*DVDM(3,1))
      VECT(17) = 0.5D0 * (DVDM(1,1)*DUDM(3,2)+DUDM(1,1)*DVDM(3,2))
      VECT(18) = 0.5D0 * (DVDM(2,2)*DUDM(3,2)+DUDM(2,2)*DVDM(3,2))
      VECT(19) = 0.5D0 * (DVDM(1,1)*DUDM(2,3)+DUDM(1,1)*DVDM(2,3))
      VECT(20) = 0.5D0 * (DVDM(3,3)*DUDM(2,3)+DUDM(3,3)*DVDM(2,3))
      VECT(21) = 0.5D0 * (DVDM(2,2)*DUDM(1,3)+DUDM(2,2)*DVDM(1,3))
      VECT(22) = 0.5D0 * (DVDM(3,3)*DUDM(1,3)+DUDM(3,3)*DVDM(1,3))
      VECT(23) = 0.5D0 * (DVDM(1,3)*DUDM(3,1)+DUDM(1,3)*DVDM(3,1))
      VECT(24) = 0.5D0 * (DVDM(2,3)*DUDM(3,2)+DUDM(2,3)*DVDM(3,2))
      VECT(25) = 0.5D0 * (DVDM(2,3)*DUDM(3,1)+DUDM(2,3)*DVDM(3,1))
      VECT(26) = 0.5D0 * (DVDM(3,2)*DUDM(3,1)+DUDM(3,2)*DVDM(3,1))
      VECT(27) = 0.5D0 * (DVDM(1,3)*DUDM(3,2)+DUDM(1,3)*DVDM(3,2))
      VECT(28) = 0.5D0 * (DVDM(1,2)*DUDM(1,3)+DUDM(1,2)*DVDM(1,3))
      VECT(29) = 0.5D0 * (DVDM(2,1)*DUDM(1,3)+DUDM(2,1)*DVDM(1,3))
      VECT(30) = 0.5D0 * (DVDM(1,2)*DUDM(3,1)+DUDM(1,2)*DVDM(3,1))
      VECT(31) = 0.5D0 * (DVDM(2,1)*DUDM(2,3)+DUDM(2,1)*DVDM(2,3))
      VECT(32) = 0.5D0 * (DVDM(2,1)*DUDM(3,2)+DUDM(2,1)*DVDM(3,2))
      VECT(33) = 0.5D0 * (DVDM(2,3)*DUDM(1,2)+DUDM(2,3)*DVDM(1,2))
C
C
      S11 = DUDM(1,1)*DVDM(1,1) + DUDM(2,2)*DVDM(2,2) +
     &      DUDM(3,3)*DVDM(3,3)
      S12 = DUDM(1,1)*DVDM(2,2) + DUDM(2,2)*DVDM(1,1) +
     &      DUDM(1,1)*DVDM(3,3) + DUDM(3,3)*DVDM(1,1) +
     &      DUDM(2,2)*DVDM(3,3) + DUDM(3,3)*DVDM(2,2)
      S13 = (DUDM(1,2)+DUDM(2,1))*(DVDM(1,2)+DVDM(2,1)) +
     &      (DUDM(2,3)+DUDM(3,2))*(DVDM(2,3)+DVDM(3,2)) +
     &      (DUDM(3,1)+DUDM(1,3))*(DVDM(3,1)+DVDM(1,3))
C
      S21 = DUDM(1,1)*DVDM(1,1)*DTDM(1,1) +
     &      DUDM(2,2)*DVDM(2,2)*DTDM(2,2) +
     &      DUDM(3,3)*DVDM(3,3)*DTDM(3,3) + VECT(5)*DTDM(1,2) +
     &      VECT(2)*DTDM(2,1) + VECT(7)*DTDM(1,3) + VECT(8)*DTDM(3,1) +
     &      VECT(9)*DTDM(2,3) + VECT(10)*DTDM(3,2)
C     
      S22 = VECT(1)*(DTDM(1,1)+DTDM(2,2)) +
     &      VECT(11)*(DTDM(1,1)+DTDM(3,3)) +
     &      VECT(12)*(DTDM(2,2)+DTDM(3,3)) +
     &      (VECT(3)+VECT(14))*DTDM(1,2) + (VECT(4)+VECT(13))*DTDM(2,1)
     &      + (VECT(15)+VECT(16))*DTDM(1,3) +
     &      (VECT(17)+VECT(18))*DTDM(2,3) +
     &      (VECT(19)+VECT(20))*DTDM(3,2) +
     &      (VECT(21)+VECT(22))*DTDM(3,1)
C
      S23 = (VECT(6)+DUDM(2,1)*DVDM(2,1))*DTDM(1,1) +
     &      (VECT(23)+DUDM(3,1)*DVDM(3,1))*DTDM(1,1) +
     &      (VECT(6)+DUDM(1,2)*DVDM(1,2))*DTDM(2,2) +
     &      (VECT(24)+DUDM(3,2)*DVDM(3,2))*DTDM(2,2) +
     &      (VECT(24)+DUDM(2,3)*DVDM(2,3))*DTDM(3,3) +
     &      (VECT(23)+DUDM(1,3)*DVDM(1,3))*DTDM(3,3) +
     &      (VECT(2)+VECT(3))*DTDM(1,2) + (VECT(25)+VECT(26))*DTDM(1,2)
     &      + (VECT(4)+VECT(5))*DTDM(2,1) +
     &      (VECT(26)+VECT(27))*DTDM(2,1) +
     &      (VECT(28)+VECT(29))*DTDM(3,2) + (VECT(9)+VECT(20))*DTDM(3,2)
     &      + (VECT(10)+VECT(18))*DTDM(2,3) +
     &      (VECT(28)+VECT(30))*DTDM(2,3) + (VECT(8)+VECT(15))*DTDM(1,3)
     &      + (VECT(31)+VECT(32))*DTDM(1,3) +
     &      (VECT(31)+VECT(33))*DTDM(3,1) + (VECT(22)+VECT(7))*DTDM(3,1)
C
      S1 = C1*S11 + C2*S12 + C3*S13
      S2 = C1*S21 + C2*S22 + C3*S23

C--------------------------AUTRE MANIERE DE CALCUL POUR S2----------
      DO 100 I = 1,3
        DO 101 J = 1,3
          DO 102 K = 1,3
            DO 103 L = 1,3
              BIL(I,J,K,L) =
     &          0.5D0 * (DUDM(I,J)*DVDM(K,L)+DUDM(K,L)*DVDM(I,J))
 103        CONTINUE
 102      CONTINUE
 101    CONTINUE
 100  CONTINUE
C
      SA21 = 0.D0
      DO 10 K = 1,3
        DO 20 P = 1,3
          SA21 = SA21 + BIL(K,K,K,P)*DTDM(P,K)
 20     CONTINUE
 10   CONTINUE
C
      SA22 = 0.D0
      DO 300 K = 1,3
        DO 301 L = 1,3
          IF (L .NE. K) THEN
            DO 302 P = 1,3
              SA22 = SA22 + BIL(L,L,K,P)*DTDM(P,K)
 302        CONTINUE
          END IF
 301    CONTINUE
 300  CONTINUE
C
      SA23 = 0.D0
      DO 400 K = 1,3
        DO 401 L = 1,3
          IF (L .NE. K) THEN
            DO 402 M = 1,3
              IF (M.NE.K .AND. M.NE.L) THEN
                DO 403 P = 1,3
                  SA23 = SA23 + BIL(L,M,L,P)*DTDM(P,M)
                  SA23 = SA23 + BIL(L,M,M,P)*DTDM(P,L)
 403            CONTINUE
              END IF
 402        CONTINUE
          END IF
 401    CONTINUE
 400  CONTINUE
C
      IF (ABS(SA21-S21) .GT. MAX(1.D-8,ABS(SA21)*1.D-8)) THEN
         CALL UTMESS('A','GBIL3D','SA21')
      ENDIF
      IF (ABS(SA22-S22) .GT. MAX(1.D-8,ABS(SA22)*1.D-8)) THEN
         CALL UTMESS('A','GBIL3D','SA22')
      ENDIF
      IF (ABS(SA23-S23) .GT. MAX(1.D-8,ABS(SA23)*1.D-8)) THEN
         CALL UTMESS('A','GBIL3D','SA23')
      ENDIF
C     
C     
C
C - TERME CLASSIQUE DU A LA THERMIQUE S1TH + S2TH
C  
      DIVV = DVDM(1,1) + DVDM(2,2) + DVDM(3,3)
      DIVU = DUDM(1,1) + DUDM(2,2) + DUDM(3,3)
      ENETHV(1) = K3A * TTRGU * DIVV
      ENETHU(1) = K3A * TTRGV * DIVU
C         
      S1TH = ENETHU(1) + ENETHV(1)
C
      S2TH1V = DVDM(1,1) * DTDM(1,1) +
     &        DVDM(1,2) * DTDM(2,1) +       
     &        DVDM(1,3) * DTDM(3,1)     
      S2TH2V = DVDM(2,1) * DTDM(1,2) +
     &        DVDM(2,2) * DTDM(2,2) +       
     &        DVDM(2,3) * DTDM(3,2)
      S2TH3V = DVDM(3,1) * DTDM(1,3) +
     &        DVDM(3,2) * DTDM(2,3) +       
     &        DVDM(3,3) * DTDM(3,3)
C
      S2TH1U = DUDM(1,1) * DTDM(1,1) +
     &        DUDM(1,2) * DTDM(2,1) +       
     &        DUDM(1,3) * DTDM(3,1)     
      S2TH2U = DUDM(2,1) * DTDM(1,2) +
     &        DUDM(2,2) * DTDM(2,2) +       
     &        DUDM(2,3) * DTDM(3,2)
      S2TH3U = DUDM(3,1) * DTDM(1,3) +
     &        DUDM(3,2) * DTDM(2,3) +       
     &        DUDM(3,3) * DTDM(3,3)
C
      S2TH = 0.5D0 * (K3A * TTRGU * (S2TH1V + S2TH2V + S2TH3V) +
     &                K3A * TTRGV * (S2TH1U + S2TH2U + S2TH3U)) 
C
      GCLA = (S2 - S2TH - 0.5D0 * (S1 - S1TH) * DIVT) * POIDS
C     
C - TERME FORCE VOLUMIQUE
C
      SOMM1U = 0.0D0
      DO 500 I = 1,3
        DO 550 J = 1,3
          SOMM1U = SOMM1U + 0.5D0 * DFUDM(I,J) * DTDM(J,4) * DVDM (I,4)
 550    CONTINUE
 500  CONTINUE
C
      SOMM1V = 0.0D0
      DO 1000 I = 1,3
        DO 1010 J = 1,3
          SOMM1V = SOMM1V + 0.5D0 * DFVDM(I,J) * DTDM(J,4) * DUDM (I,4)
 1010   CONTINUE
 1000 CONTINUE
C
      SOMM2U = 0.0D0
      DO 600 I = 1,3
        SOMM2U = SOMM2U + 0.5D0 * DFUDM(I,4) * DVDM(I,4) * DIVT
 600  CONTINUE
C
      SOMM2V = 0.0D0
      DO 1600 I = 1,3
        SOMM2V = SOMM2V + 0.5D0 * DFVDM(I,4) * DUDM(I,4) * DIVT
 1600 CONTINUE
C

      TFOR = (SOMM1U + SOMM1V + SOMM2U + SOMM2V) * POIDS
C
C  - TERME THERMIQUE
C
      TTT1U = 0.0D0
      DO 700 J = 1,3
        TTT1U = TTT1U + 0.5D0 * TGUDM(J) * DTDM(J,4)
 700  CONTINUE
C
      TTT1V = 0.0D0
      DO 1700 J = 1,3
        TTT1V = TTT1V + 0.5D0 * TGVDM(J) * DTDM(J,4)
 1700 CONTINUE
C
      ENETHV(2) = -1.D0 * K3A * DIVV
      ENETHU(2) = -1.D0 * K3A * DIVU
C
      TTHER = -1.D0*(TTT1U * ENETHV(2) + TTT1V * ENETHU(2))*POIDS
C
C  - TERME DYNAMIQUE
C
      PROD = 0.D0
      DO 800 I=1,3
        DO 810 J=1,3
          PROD = PROD + DUDM(I,J) * DTDM(J,4) * DVDM(I,4) +
     &                  DVDM(I,J) * DTDM(J,4) * DUDM(I,4)
 810    CONTINUE
 800  CONTINUE   
C
      TDYN = -0.5D0*RHO*PULS*PULS*PROD*POIDS
C     
      G = GCLA + TFOR + TTHER + TDYN
C       
      END
