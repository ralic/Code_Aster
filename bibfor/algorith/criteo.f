      SUBROUTINE CRITEO(EPSP,EPSD,ETA,BA,D,LAMBDA,MU,ALPHA,ECROB,
     &                   ECROD,SEUIL,CRIT,CRITP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2004   AUTEUR GODARD V.GODARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      REAL*8             EPSP(6), EPSD(6),ETA
      REAL*8             BA(6),D
      REAL*8             LAMBDA,MU,ALPHA,SEUIL,ECROB,ECROD
      REAL*8             CRIT,CRITP

C----------------------------------------------------------------
C----------------------------------------------------------------
C----------------------------------------------------------------
C ----------------------------------------------------------------------
C     CALCUL DU CRITERE DE ENDO_ORTH_BETON F(ETA) ET DE SA DERIVEE
C
C IN EPSP    : DEFORMATIONS DUES AUX CHARGEMENTS ANTERIEURS ET FIXES
C IN EPSD    : DEFORMATIONS PROPORTIONNELLES A ETA
C IN ETA     : INTENSITE DU PILOTAGE
C IN LAMBDA  : |
C IN DEUXMU  : | COEFFICIENTS DE LAME
C IN ALPHA   : /
C IN ECROB   : /
C IN ECROD   : /PARAMETRES DE LA LOI
C IN SEUIL   : SEUIL DU CRITERE
C OUT CRIT   : VALEUR DU CRITERE POUR ETA DONNEE EN ENTREE
C OUT CRITP  : VALEUR DE LA DERIVEE DU CRITERE POUR ETA DONNEE EN ENTREE
C ----------------------------------------------------------------------


      INTEGER     K,I,J,L,T(3,3)
      
      REAL*8      EPSA(6),EPS(6),EPSDR(6),B(6)
      REAL*8      FB(6),FBR(6),FBM(6),FD,REC(6)
      REAL*8      CC(6),VECC(3,3),VALCC(3),CCP(6),CPE(6),VALB(3)
      REAL*8      VECB(3,3)
      REAL*8      VALFB(3),VECFB(3,3)
      REAL*8      TDFBDE(6,6),TDFDDE(6),DFDE(6)
      REAL*8      RTEMP,TREB,TREPS,TREM,DCOEFD,ENE,COUPL
      REAL*8      TOLE,RAC2,KRON(6)

      REAL*8      R8DOT

      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C TOLE: TOLERANCE POUR ARRET EVOLUTION DE L ENDOMMAGEMENT
      TOLE=1.D-2

      RAC2=SQRT(2.D0)

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3




      EPSA(1)=EPSP(1)+ETA*EPSD(1)
      EPSA(2)=EPSP(2)+ETA*EPSD(2)
      EPSA(3)=EPSP(3)+ETA*EPSD(3)
      EPSA(4)=EPSP(4)+ETA*EPSD(4)
      EPSA(5)=EPSP(5)+ETA*EPSD(5)
      EPSA(6)=EPSP(6)+ETA*EPSD(6)



C--ON TRAVAILLE DANS ESPACE PROPRE ENDO

      CALL R8INIR(6,1.D0,REC,1)
      CALL R8INIR(6,0.D0,B,1)
      CALL R8INIR(6,0.D0,EPS,1)
      CALL R8INIR(6,0.D0,EPSDR,1)
      
      CALL DIAGO3(BA,VECB,VALB)
      DO 201 I=1,3
        B(I)=VALB(I)
 201  CONTINUE

      IF (ABS(VALB(1)).LT.TOLE) THEN
        REC(1)=0.D0
        REC(4)=0.D0
        REC(5)=0.D0
      ENDIF
      IF (ABS(VALB(2)).LT.TOLE) THEN
        REC(2)=0.D0
        REC(4)=0.D0
        REC(6)=0.D0
      ENDIF
      IF (ABS(VALB(3)).LT.TOLE) THEN
        REC(3)=0.D0
        REC(5)=0.D0
        REC(6)=0.D0
      ENDIF



        DO 202 I=1,3
          DO 203 J=I,3
            DO 204 K=1,3
              DO 205 L=1,3
            EPS(T(I,J))=EPS(T(I,J))+VECB(K,I)*EPSA(T(K,L))*VECB(L,J)
            EPSDR(T(I,J))=EPSDR(T(I,J))+VECB(K,I)*EPSD(T(K,L))*VECB(L,J)
 205        CONTINUE
 204        CONTINUE
 203      CONTINUE
 202    CONTINUE



C--------------------------------------------------------------------
C---CALCUL DU CRITERE------------------------------------------------
C--------------------------------------------------------------------

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
      TREB=TREB+CC(I)/2 
 301  CONTINUE      
      IF (TREB.GT.0.D0) THEN
        DO 19 I=1,6
          FB(I)=-LAMBDA*TREB*EPS(I)
  19    CONTINUE
      ENDIF
      DO 20 I=1,6
        FB(I)=FB(I)-MU/2.D0*CPE(I)+ECROB*(KRON(I)-B(I))
  20  CONTINUE


      DO 100 I=1,6
        FBR(I)=FB(I)*REC(I)
 100  CONTINUE


      CALL DIAGO3(FBR,VECFB,VALFB)
      RTEMP=0.D0
      DO 29 I=1,3
        IF (VALFB(I).GT.0.D0) THEN
          VALFB(I)=0.D0
        ENDIF
        RTEMP=RTEMP+VALFB(I)*VALFB(I)
  29  CONTINUE
  
      CALL R8INIR(6,0.D0,FBM,1)
      DO 26 I=1,3
        DO 27 J=I,3
          DO 28 K=1,3
            FBM(T(I,J))=FBM(T(I,J))+VECFB(I,K)*VALFB(K)*VECFB(J,K)
  28      CONTINUE
  27    CONTINUE
  26  CONTINUE   

     
      TREPS=EPS(1)+EPS(2)+EPS(3)

      
      
      CALL DIAGO3(EPS,VECC,VALCC)
      DO 22 I=1,3
        IF (VALCC(I).GT.0.D0) THEN
          VALCC(I)=0.D0
        ENDIF
 22   CONTINUE

      CALL R8INIR(6,0.D0,CCP,1)

      DO 23 I=1,3
        DO 24 J=I,3
          DO 25 K=1,3
          CCP(T(I,J))=CCP(T(I,J))+VECC(I,K)*VALCC(K)*VECC(J,K)
 25       CONTINUE
 24     CONTINUE
 23   CONTINUE

      TREM=VALCC(1)**2+VALCC(2)**2+VALCC(3)**2
      IF (TREPS.GT.0.D0) THEN
        TREPS=0.D0
      ENDIF
        DCOEFD=2.D0*(1.D0-D)
        ENE=LAMBDA/2*TREPS**2+MU*TREM
        FD=DCOEFD*ENE-2.D0*D*ECROD
        IF (FD.LT.0.D0) THEN
          FD=0.D0
        ENDIF

      COUPL=SQRT(ALPHA*RTEMP+(1-ALPHA)*FD**2)
      CRIT=COUPL-SEUIL 



C----------------------------------------------------------------
C----CALCUL DE LA DERIVEE DU CRITERE-----------------------------
C----------------------------------------------------------------

      FBM(4)=RAC2*FBM(4)
      FBM(5)=RAC2*FBM(5)
      FBM(6)=RAC2*FBM(6)

      CALL DFBDE(3,B,EPS,2.D0*MU,LAMBDA,TDFBDE)
      CALL DFDDE(EPS,D,3,LAMBDA,MU,TDFDDE)


      CALL R8INIR(6,0.D0,DFDE,1)
      
      IF (COUPL.GT.1.D-20) THEN
      DO 101 I=1,6
        DO 102 J=1,6
           DFDE(I)=DFDE(I)+ALPHA/COUPL*FBM(J)*TDFBDE(J,I)*REC(J)
 102    CONTINUE
           DFDE(I)=DFDE(I)+(1.D0-ALPHA)*FD/COUPL*TDFDDE(I)
 101  CONTINUE
      ENDIF
      
      DO 52 I=4,6
        EPSDR(I)=EPSDR(I)*RAC2
52    CONTINUE

      CRITP=R8DOT(6,DFDE,1,EPSDR,1)

      
      END
