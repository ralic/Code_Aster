       SUBROUTINE CEOBFB (BM,EPSM,LAMBDA,MU,ECROB,BDIM,FB,NOFBM,FBM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR IDOUX L.IDOUX 
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
C RESPONSABLE IDOUX L.IDOUX
      IMPLICIT NONE
      REAL*8             EPSM(6),BM(6),FB(6),FBM(6),NOFBM
      REAL*8             LAMBDA,MU,ECROB
      INTEGER            BDIM
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
C     ROUTINE DE CALCUL DE LA FORCE THERMODYNAMIQUE FB
C
C  IN BM     : TENSEUR D'ENDOMMAGEMENT DE TRACTION
C  IN EPSM   : TENSEUR DE DEFORMATION
C  IN LAMBDA : /
C  IN MU     : / COEFFICIENTS DE LAME
C  IN ECROB  : PARAMETRE DU MODELE
C  IN BDIM   : DIMENSION DE L ESPACE
C
C OUT FB     : FORCE THERMODYNAMIQUE
C OUT FBM    : PARTIE POSITIVE DE FB
C OUT NOFBM  : NORME DE FBM
C ----------------------------------------------------------------------

      INTEGER     I,J,K
      INTEGER     T(3,3),R(2,2)

      REAL*8      CC(6),CPE(6),CCP(6),EPS(6),B(6),FBS(3)
      REAL*8      DEUX,TREB,KRON(6),R8PREM
      REAL*8      VECC(3,3),VALCC(3),VECFB(3,3),VALFB(3)
      REAL*8      VECFBS(2,2),VALFBS(2)

      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

      DEUX=2.D0
      
      DO 100 I=1,6
        B(I)=BM(I)
        EPS(I)=EPSM(I)
 100  CONTINUE
C
C CALCUL DE FB
C
      CALL R8INIR(6,0.D0,CC,1)

      DO 9 I=1,3
        DO 10 J=I,3
          DO 11 K=1,3
            CC(T(I,J))=CC(T(I,J))+B(T(I,K))*EPS(T(K,J))+
     &                 B(T(J,K))*EPS(T(K,I))
 11       CONTINUE
 10     CONTINUE
 9    CONTINUE
      CALL DIAGO3(CC,VECC,VALCC)
      CALL R8INIR(6,0.D0,CCP,1)
      CALL R8INIR(6,0.D0,CPE,1)
      DO 12 I=1,3
        IF (VALCC(I).LT.0.D0) THEN
          VALCC(I)=0.D0
        ENDIF
 12   CONTINUE
      DO 13 I=1,3
        DO 14 J=I,3
          DO 15 K=1,3
          CCP(T(I,J))=CCP(T(I,J))+VECC(I,K)*VALCC(K)*VECC(J,K)
 15       CONTINUE
 14     CONTINUE
 13   CONTINUE
      DO 16 I=1,3
        DO 17 J=I,3
          DO 18 K=1,3
            CPE(T(I,J))=CPE(T(I,J))+ CCP(T(I,K))*EPS(T(K,J))+
     &                    CCP(T(J,K))*EPS(T(K,I))
  18      CONTINUE
  17    CONTINUE
  16  CONTINUE

      CALL R8INIR(6,0.D0,FB,1)
      TREB=0.D0
      DO 301 I=1,3
      TREB=TREB+CC(I)/DEUX
 301  CONTINUE
      IF (TREB.GT.0.D0) THEN
        DO 19 I=1,6
          FB(I)=-LAMBDA*TREB*EPS(I)
  19    CONTINUE
      ENDIF
      DO 20 I=1,6
        FB(I)=FB(I)-MU/DEUX*CPE(I)+ECROB*(KRON(I)-B(I))
  20  CONTINUE
C
C CALCUL DE LA PARTIE POSITIVE DE FBM ET DE SA NORME NOFB
C
      CALL R8INIR(6,0.D0,FBM,1)
      IF (BDIM.EQ.3) THEN
        CALL DIAGO3(FB,VECFB,VALFB)
        NOFBM=0.D0

        DO 129 I=1,3
          IF (VALFB(I).GT.0.D0) THEN
            VALFB(I)=0.D0
          ENDIF
          NOFBM=NOFBM+VALFB(I)*VALFB(I)
 129    CONTINUE

        DO 126 I=1,3
          DO 127 J=I,3
            DO 128 K=1,3
              FBM(T(I,J))=FBM(T(I,J))+VECFB(I,K)*VALFB(K)*VECFB(J,K)
 128        CONTINUE
 127      CONTINUE
 126    CONTINUE
 
      ELSE IF (BDIM.EQ.2) THEN
        R(1,1)=1
        R(2,2)=2
        R(1,2)=3
        R(2,1)=3
        FBS(1)=FB(1)
        FBS(2)=FB(2)
        FBS(3)=FB(4)

        CALL DIAGO2(FBS,VECFBS,VALFBS)

        NOFBM=0.D0
        DO 29 I=1,2
          IF (VALFBS(I).GT.0.D0) THEN
            VALFBS(I)=0.D0
          ENDIF
          NOFBM=NOFBM+VALFBS(I)*VALFBS(I)
  29    CONTINUE

        DO 26 I=1,2
          DO 27 J=I,2
            DO 28 K=1,2
            FBM(R(I,J))=FBM(R(I,J))+VECFBS(I,K)*VALFBS(K)*VECFBS(J,K)
  28        CONTINUE
  27      CONTINUE
  26    CONTINUE     

      ELSE IF (BDIM.EQ.1) THEN
        IF (FB(1).LT.0.D0) THEN
          FBM(1)=FB(1)
        ENDIF
        NOFBM=FBM(1)**2

      ENDIF
      
      IF (ABS(NOFBM).LT.R8PREM()) THEN
        NOFBM=0.D0
      ENDIF
         
      END
