      SUBROUTINE LCMMRO(TAMPON,OMP,NVI,VIND,VINF)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2011   AUTEUR PROIX J-M.PROIX 
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
C     Stockage variables internes rotation reseau
C     ----------------------------------------------------------------
      INTEGER  I,J,NVI,K
      REAL*8 OMP(3),DTHETA,IDEN(3,3),R8MIEM,NAX(3,3),Q(3,3)
      REAL*8 TAMPON(*),OMEGAP(3,3),OMEGAE(3,3),OMEGA(3,3),DQ(3,3)
      REAL*8 VIND(NVI),VINF(NVI),L(3,3),QM(3,3)
      DATA IDEN/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
      
C     LA MATRICE DE ROTATION QM EST STOCKEE DANS VIND (N-19 A N-9)
      DO 24 I = 1, 3
      DO 24 J=1,3
         QM(I,J)=VIND(NVI-19+3*(I-1)+J)+IDEN(I,J)
 24   CONTINUE
 
C     TAMPON CONTIENT L(3,3)
      DO 21 I = 1, 3
      DO 21 J=1,3
         L(I,J)=TAMPON(3*(I-1)+J)
 21   CONTINUE
      DO 22 I = 1, 3
      DO 22 J=1,3
         OMEGA(I,J)=0.5D0*(L(I,J)-L(J,I))
 22   CONTINUE
C     LE VECTEUR  ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 A N-7)
      CALL R8INIR(9,0.D0,OMEGAP,1)
      OMEGAP(2,3)=-OMP(1)
      OMEGAP(3,2)=+OMP(1)
      OMEGAP(1,3)=+OMP(2)
      OMEGAP(3,1)=-OMP(2)
      OMEGAP(1,2)=-OMP(3)
      OMEGAP(2,1)=+OMP(3)
      DO 23 I = 1, 3
      DO 23 J=1,3
         OMEGAE(I,J)=OMEGA(I,J)-OMEGAP(I,J)
 23   CONTINUE
C     ANGLE = NORME DU VECTEUR AXIAL
      DTHETA=SQRT(OMEGAE(1,2)**2+OMEGAE(1,3)**2+OMEGAE(2,3)**2)

      CALL DCOPY(9,IDEN,1,DQ,1)
      IF (DTHETA.GT.R8MIEM()) THEN
         DO 25 I = 1, 3
         DO 25 J=1,3
            NAX(I,J)=OMEGAE(I,J)/DTHETA
 25      CONTINUE
         DO 26 I = 1, 3
         DO 26 J=1,3
            DQ(I,J)=DQ(I,J)+SIN(DTHETA)*NAX(I,J)
 26      CONTINUE
         DO 27 I = 1, 3
         DO 27 J=1,3
         DO 27 K=1,3
            DQ(I,J)=DQ(I,J)+(1.D0-COS(DTHETA))*NAX(I,K)*NAX(K,J)
 27      CONTINUE
      ENDIF
      CALL R8INIR(9,0.D0,Q,1)
      DO 28 I=1,3
      DO 28 J=1,3
      DO 28 K=1,3
         Q(I,J)=Q(I,J)+DQ(I,K)*QM(K,J)
 28   CONTINUE
 
C LA MATRICE DE ROTATION EST STOCKEE DANS VINF (N-18 A N-10)
      DO 29 I = 1, 3
      DO 29 J=1,3
         VINF(NVI-19+3*(I-1)+J)=(Q(I,J)-IDEN(I,J))
 29   CONTINUE
 
C LE VECTEUR D-ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 A N-7)

      VINF(NVI-9) = OMP(1)                           
      VINF(NVI-8) = OMP(2)                           
      VINF(NVI-7) = OMP(3)   
      
C LE VECTEUR D-ROTATION ELASTIQUE EST STOCKE DANS VINF (N-6 A N-4)

      VINF(NVI-6) = OMEGAE(3,2)
      VINF(NVI-5) = OMEGAE(1,3)
      VINF(NVI-4) = OMEGAE(2,1)
      
      VINF(NVI-3) = DTHETA+VIND(NVI-3)
      
      END
