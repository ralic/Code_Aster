        SUBROUTINE LCDPEC(VIND,NBCOMM,NMAT,NDT,CPMONO,MATERF,ITER,NVI,
     &          ITMAX, TOLER, PGL, TOUTMS,HSR, DT, DY, YF, VINF,EPSEQ,
     &          TAMPON)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C     POUR LE MONOCRISTAL
C     IN  VIND   :  VARIABLES INTERNES A T
C     IN  VINF   :  VARIABLES INTERNES A T+DT
C     ----------------------------------------------------------------
      INTEGER  NMAT,NDT,I,NBCOMM(NMAT,3),NBSYS,IEC,IFA,IS,NBFSYS,ITMAX
      INTEGER  NUV1,NUVI,MONO1,ITER,NVI,IRET,IFL,IU,J,IR,K
      REAL*8   VIND(*),VINF(*),DVIN(6),DY(*),YF(*),MATERF(NMAT*2)
      REAL*8   LCNRTE, EPSEQ,PGL(3,3),D,MS(6),NG(3),DGAMMA,DP,DALPHA
      REAL*8   ALPHAM,DEVI(6),TOUTMS(5,24,6),TOLER,HSR(5,24,24)
      REAL*8   BSD,GCB,KDCS, RACR,SOM, AUX, TAUS,CISA2,L(3,3)
      REAL*8   CRIT, SGNS, DT,OMP(3),R8MIEM,Q(3,3),OMEGAE(3,3)
      REAL*8   SI(3,3),SING(3),SICL, P,LG(3),DTHETA,TAMPON(*)
      CHARACTER*16 CPMONO(5*NMAT+1),NOMFAM,NECRCI,NECOUL
      REAL*8 IDEN(3,3),OMEGA(3,3),OMEGAP(3,3),QM(3,3),NAX(3,3),DQ(3,3)
      DATA IDEN/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C
C     CAS MONO1 : ON RECALCULE LES VARIABLES INTERNES      
      CALL R8INIR(6, 0.D0, DEVI, 1)                        
      CALL R8INIR(3, 0.D0, OMP, 1)                        
      CALL R8INIR(9, 0.D0, QM, 1)                        
      NBFSYS=NBCOMM(NMAT,2)                                
      NUVI=6                                               
      NUV1=NDT                                             
      SICL = 0.D0
C ROTATION RESEAU DEBUT
      IF (NBCOMM(NMAT,1).GT.0) THEN
C        LA MATRICE DE ROTATION QM EST STOCKEE DANS VIND (N-19 a N-9)
         DO 24 I = 1, 3
         DO 24 J=1,3
            QM(I,J)=VIND(NVI-19+3*(I-1)+J)+IDEN(I,J)
 24      CONTINUE
         IR=1
      ELSE
         IR=0
      ENDIF

      DO 6 IFA=1,NBFSYS                                     
         NOMFAM=CPMONO(5*(IFA-1)+1)                        
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRCI=CPMONO(5*(IFA-1)+5)
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS,NG,LG,0,Q)                
         DO 7 IS=1,NBSYS                                   
            DO 101 I=1,6                                    
              MS(I)=TOUTMS(IFA,IS,I)                       
 101        CONTINUE                                       
            NUVI=NUVI+3                                           
            NUV1=NUV1+1                                           
            
            IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
               TAUS=0.D0
               DO 102 I=1,6
                  TAUS=TAUS+YF(I)*MS(I)
 102            CONTINUE
               IF (MATERF(NMAT).EQ.0) THEN
                  CISA2 = (MATERF(1)/2.D0/(1.D0+MATERF(2)))**2
               ELSE
                  CISA2 = (MATERF(36)/2.D0)**2
               ENDIF
               CALL LCMMKR(TAUS,MATERF(NMAT+1),CISA2,IFA,NMAT,NBCOMM,
     &           IS,NBSYS,HSR,VIND(7),DY(NUV1),DT,
     &           DALPHA,DGAMMA,DP,CRIT,SGNS,IRET)
            ELSE
               DGAMMA=DY(NUV1)                                       
               DP=ABS(DGAMMA)                                        
C              ECROUISSAGE CINEMATIQUE - CALCUL DE DALPHA  
               ALPHAM=VIND(NUVI-2)
               CALL LCMMFC( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,
     &                ITMAX, TOLER,ALPHAM,DGAMMA,DALPHA, IRET)
            ENDIF
            DO 19 I=1,6
               DEVI(I)=DEVI(I)+MS(I)*DGAMMA
 19         CONTINUE
            VINF(NUVI-2)=VIND(NUVI-2)+DALPHA
            VINF(NUVI-1)=VIND(NUVI-1)+DGAMMA
            VINF(NUVI ) =VIND(NUVI)+DP
            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS,NG,LG,IR,QM)
C           SIGMA (3,3)
C           calcul du max de Ns.(SIGMA.Ns)
            SI(1,1) = YF(1)
            SI(1,2) = YF(4)/SQRT(2.D0)
            SI(1,3) = YF(5)/SQRT(2.D0)
            SI(2,1) = SI(1,2)
            SI(2,2) = YF(2)
            SI(2,3) = YF(6)/SQRT(2.D0)
            SI(3,1) = SI(1,3)
            SI(3,2) = SI(2,3)
            SI(3,3) = YF(3)
            DO 9 I = 1,3
               SING(I) = 0.D0
  9         CONTINUE
            DO 11 I = 1 , 3
            DO 10 J = 1 , 3
               SING(I) = SING(I) + SI(I,J) * NG(J)
 10         CONTINUE
 11         CONTINUE
            P = 0.D0
            DO 1 I = 1 , 3
               P = P + SING(I)*NG(I)
 1          CONTINUE
            SICL = MAX(SICL, P)
            VINF(NVI-2) = SICL
            IF (IR.EQ.1) THEN
C ROTATION RESEAU DEBUT
C           stockage de OMEGAP pour la rotation de reseau            
               OMP(1)=OMP(1)+DGAMMA*0.5D0*(NG(2)*LG(3)-NG(3)*LG(2))
               OMP(2)=OMP(2)+DGAMMA*0.5D0*(NG(3)*LG(1)-NG(1)*LG(3))
               OMP(3)=OMP(3)+DGAMMA*0.5D0*(NG(1)*LG(2)-NG(2)*LG(1))
C ROTATION RESEAU FIN
            ENDIF
  7      CONTINUE                                             
  6   CONTINUE
                                               
C ROTATION RESEAU DEBUT
C     stockage de OMEGAP pour la rotation de reseau            
      IF (IR.EQ.1) THEN
C         TAMPON CONTIENT L(3,3)
          DO 21 I = 1, 3
          DO 21 J=1,3
             L(I,J)=TAMPON(3*(I-1)+J)
             print *,'I,J,L',I,J,L(I,J)
 21       CONTINUE
          DO 22 I = 1, 3
          DO 22 J=1,3
             OMEGA(I,J)=0.5D0*(L(I,J)-L(J,I))
 22       CONTINUE
C LE VECTEUR  ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 a N-7)
          CALL R8INIR(9,0.D0,OMEGAP,1)
          OMEGAP(2,3)=-OMP(1)
          OMEGAP(3,2)=+OMP(1)
          OMEGAP(1,3)=-OMP(2)
          OMEGAP(3,1)=+OMP(2)
          OMEGAP(1,2)=-OMP(3)
          OMEGAP(2,1)=+OMP(3)
          DO 23 I = 1, 3
          DO 23 J=1,3
             OMEGAE(I,J)=OMEGA(I,J)-OMEGAP(I,J)
 23       CONTINUE
C         ANGLE = NORME DU VECTEUR AXIAL
          DTHETA=SQRT(OMEGAE(1,2)**2+OMEGAE(1,3)**2+OMEGAE(2,3)**2)

          CALL DCOPY(9,IDEN,1,DQ,1)
          IF (DTHETA.GT.R8MIEM()) THEN
             DO 25 I = 1, 3
             DO 25 J=1,3
                NAX(I,J)=OMEGAE(I,J)/DTHETA
 25          CONTINUE
             DO 26 I = 1, 3
             DO 26 J=1,3
                DQ(I,J)=DQ(I,J)+SIN(DTHETA)*NAX(I,J)
 26          CONTINUE
             DO 27 I = 1, 3
             DO 27 J=1,3
             DO 27 K=1,3
                DQ(I,J)=DQ(I,J)+(1.D0-COS(DTHETA))*NAX(I,K)*NAX(K,J)
 27          CONTINUE
          ENDIF
          CALL R8INIR(9,0.D0,Q,1)
          DO 28 I=1,3
          DO 28 J=1,3
          DO 28 K=1,3
             Q(I,J)=Q(I,J)+DQ(I,K)*QM(K,J)
 28       CONTINUE
C LE VECTEUR D-ROTATION PLASTIQUE EST STOCKE DANS VINF (N-9 a N-7)
          VINF(NVI-9) = OMP(1)                           
          VINF(NVI-8) = OMP(2)                           
          VINF(NVI-7) = OMP(3)   
C LE VECTEUR D-ROTATION ELASTIQUE EST STOCKE DANS VINF (N-6 a N-4)
          VINF(NVI-6) = OMEGAE(2,3)
          VINF(NVI-5) = OMEGAE(3,1)
          VINF(NVI-4) = OMEGAE(1,2)
          VINF(NVI-3) = DTHETA+VIND(NVI-3)
C LA MATRICE DE ROTATION ESt STOCKEE DANS VINF (N-18 a N-10)
          DO 29 I = 1, 3
          DO 29 J=1,3
             VINF(NVI-19+3*(I-1)+J)=(Q(I,J)-IDEN(I,J))
 29       CONTINUE
          VINF(NVI-3)   =DTHETA+VIND(NVI-3)
      ENDIF
C ROTATION RESEAU FIN      
                              
      DO 8 I=1,6                                           
         VINF(I)=VIND(I)+DEVI(I)                           
8     CONTINUE                                             
      CALL LCDIVE(VINF,VIND,DVIN)
      EPSEQ = LCNRTE(DVIN)
      VINF (NVI-1) = VIND (NVI-1) + EPSEQ
      VINF (NVI) = ITER

      END
