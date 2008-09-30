      SUBROUTINE LCGLDM (EPSM,DEPS,VIM,OPTION,SIG,VIP,DSIDEP,
     &             T,LAMBDA,DEUXMU,LAMF,DEUMUF,GMT,GMC,GF,SEUIL,ALF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/09/2008   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16       OPTION
      REAL*8             EPSM(6), DEPS(6), VIM(*),GF,GF1,GF2
      REAL*8             TM, TP, TREF,SECHM,SECHP,SREF
      REAL*8             SIG(6), VIP(*), DSIDEP(6,6),LAMF,DEUMUF
C ----------------------------------------------------------------------
C
C      LOI GLOBALE POUR LES PLAQUES/COQUES DKT - GLRC_DM
C
C IN:
C       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
C       LAMF    : PARAMETRE D ELASTICITE - FLEXION
C       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION 
C       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C       GF      : PARAMETRE GAMMA POUR LA FLEXION
C       SEUIL   : INITIAL MEMBRANE
C       ALF     : PARAMETRE DE SEUIL FLEXION
C       VIM     : VARIABLES INTERNES EN T-
C       OPTION  : TOUTE
C OUT:
C       SIG     : CONTRAINTE
C       VIP     : VARIABLES INTERNES EN T+
C       DSIDEP  : MATRICE TANGENTE
C       D2      : ET DE L AUTRE 
C ----------------------------------------------------------------------
      LOGICAL     RIGI, RESI,ELAS,MTG, COUP,ELAS1,ELAS2,LSING1,LSING2
      INTEGER     NDTOT, K, L, I, J, M, N, P, T(2,2),IRET,KDMAX
      REAL*8      EPS(6),TREPS,SIGEL(3)
      REAL*8      FD1, FD2,DA1,DA2, ENER, TROISK, G
      REAL*8      TR(6), RTEMP2
      REAL*8      EMP(2),VMP(2,2),VFP(2,2),DSPDEP(6,6),VECP2(3,3)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8      E, NU, ALPHA, KDESS, BENDO, GMT, GMC
      REAL*8      SEUIL,TREPSM,LAMBDA,DEUXMU
      REAL*8      DDOT,K1,TREPS0,QME33,KSIM,GM
      REAL*8      TPS(6),HYDRM,HYDRP,MU,A11,A22,A12,A21,B1,B2
      
      REAL*8      TREPS2,TR2,EN0,TR2P,TR2M,ENP,ENM,QEN,COF1,COF2
      REAL*8      FDI1(3),FDI2(3),SD1(3),SD2(3),D1E(3),D2E(3),DR1D
      REAL*8      QDE(3),SIGHP(3),SIGHM(3),TRF,EFP(2),GI(2),DE33D
      REAL*8      RTP2F,RTP3F,RTP4F,TR2D,KSI2D,EPS33,RDI
      REAL*8      DKSI1,DKSI2,TOLD,DE33D1,DE33D2,Q2D,GTR2,RD1,RD2
      REAL*8      DR2D,DD1,DD2,DE33I,VVP(2,2),EEP(2),QFF(2),SIGF(2)
      REAL*8      LAMFD,DEMUDF(2),TROT,TROT2,DLMFD1,DLMFD2
      REAL*8      D1MUDF(2),D2MUDF(2),SFFT1,SFFT2,SFF1(2),SFF2(2)
      REAL*8      MD1(2),MD2(2),D1K(2),D2K(2),MUF,RTMF2,RTFM2
      REAL*8      RTMF4,RTFM4,ALF,QM1,QM2,DM1,DM2,DF1,DF2
C -- OPTION ET MODELISATION
      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      COUP  = (OPTION(6:9).EQ.'COUP')
      IF (COUP) RIGI=.TRUE.

C -- INITIALISATION
      MU  = DEUXMU*0.5D0
      MUF = DEUMUF*0.5D0
C-- ICI ON SUPPOSE QUE GF1=GF2, CE QUI N EST PAS NECESSAIRE
      GF1 = GF
      GF2 = GF
      IF (RESI) THEN
        DO 10 K = 1, 6
          EPS(K) = EPSM(K) + DEPS(K) 
 10     CONTINUE
      ELSE
        DO 40 K=1,6
          EPS(K) = EPSM(K) 
40      CONTINUE
      ENDIF
      EPS(3) = EPS(3)/2.0D0
      EPS(6) = EPS(6)/2.0D0

C -- DIAGONALISATION DES DEFORMATIONS
      TR(1) = EPS(1)
      TR(2) = EPS(3)
      TR(3) = EPS(2)
      CALL DIAGP2(TR,VMP,EMP)
      TR(1) = EPS(4)
      TR(2) = EPS(6)
      TR(3) = EPS(5)
      CALL DIAGP2(TR,VFP,EFP)

C -- CALCUL DES D1,D2,EPS33 INITIAUX 
      TR2D  = EPS(1)+EPS(2)
      TROT  = EFP(1)+EFP(2)
      TROT2 = TROT*TROT 
      
C   CALCULER LES CONSTANTES INDEPENDANT DE D1,D2,EPS33
      IF(TR2D .GT. 0.0D0) THEN
        GTR2 = 1.0D0 - GMT 
      ELSE
        GTR2 = 1.0D0 
      ENDIF
      QFF(1) = 0.0D0
      QFF(2) = 0.0D0
      IF(TROT .GT. 0.0D0) THEN
        QFF(2) = 0.5D0*LAMF*TROT2
      ELSE  
        QFF(1) = 0.5D0*LAMF*TROT2
      ENDIF
      DO 4510, K = 1,2
        IF(EFP(K) .GT. 0.0D0) THEN
          QFF(2) = QFF(2) + MUF*EFP(K)*EFP(K)
        ELSE  
          QFF(1) = QFF(1) + MUF*EFP(K)*EFP(K)
        ENDIF     
4510  CONTINUE   
      QFF(1) = ALF*QFF(1)*(1.0D0 - GF1)
      QFF(2) = ALF*QFF(2)*(1.0D0 - GF2)
      DO 50, K = 1,2
        IF(EMP(K) .GT. 0.0D0) THEN
          GI(K) = 1.0D0 - GMT 
        ELSE
          GI(K) = 1.0D0 
        ENDIF      
 50   CONTINUE
      COF1 = 0.5D0*LAMBDA*GTR2 + MU
      COF2 = 0.5D0*LAMBDA*TR2D * GTR2
      Q2D = 0.25D0*LAMBDA * TR2D*TR2D * GTR2 
     &    + 0.5D0*MU * (EMP(1)*EMP(1)*GI(1) + EMP(2)*EMP(2)*GI(2)) 
      DA1 = VIM(1)
      DA2 = VIM(2)
      CALL CEPS33 (LAMBDA,DEUXMU,TR2D,DA1,DA2,GMT,GMC
     &                   ,EPS33,DE33D1,DE33D2,KSI2D,DKSI1,DKSI2)
      TREPS  = TR2D + EPS33
      TREPS2 = TREPS*TREPS 
      
      QM1 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
      QM2 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
C--------CALCUL DE DA1,DA2,EPS33
       IF (RESI) THEN
C        CONSTRUCTION DU TOLD :
C        LE CRITERE DE CONV. EST BASE SUR LE PRODUIT 
C        D(RESIDU)*D(ENDOM.)<TOLD
C        DONC SI L'ORDRE DE EPS = 1.0D-3, L'ORDRE DE R = EPS*MU*EPS, 
C        L'ORDRE DE D1,2 = 1 ET LA PRECISION 1D-8*1D-8 ON A 
         TOLD  = MU*1.0D-06 * 1.0D-16 
         KDMAX = 30 
         CALL GLDLOC(LAMBDA,DEUXMU,SEUIL,ALF,GMT,GMC,COF1,COF2,VIM,
     &                      Q2D,QFF,TR2D,EPS33,DE33D1,DE33D2,
     &                      KSI2D,DA1,DA2,KDMAX,TOLD)
        IF (DA1.LT.VIM(1)) THEN  
          DA1 = VIM(1) 
        END IF
        IF (DA2.LT.VIM(2)) THEN 
          DA2 = VIM(2)  
        END IF  
        ELAS = (DA1.LE.VIM(1)) .AND. (DA2.LE.VIM(2))        
        ELAS1 = DA1.LE.VIM(1)
        ELAS2 = DA2.LE.VIM(2)        
        VIP(1) = DA1  
        VIP(2) = DA2  

        IF(ELAS1) THEN 
          VIP(3) = 0.0D0 
        ELSE
          VIP(3) = 1.0D0 
        ENDIF
        IF(ELAS2) THEN 
          VIP(4) = 0.0D0 
        ELSE
          VIP(4) = 1.0D0 
        ENDIF
      ELSE
C   IF NOT RESI ....
        DA1   = VIM(1) 
        DA2   = VIM(2) 
        ELAS1 = NINT(VIM(3)).EQ.0
        ELAS2 = NINT(VIM(4)).EQ.0
        ELAS=(ELAS1.AND.ELAS2)
      ENDIF
      TR2 = EMP(1)*EMP(1) + EMP(2)*EMP(2) + EPS33*EPS33
      EN0 = 0.5D0*LAMBDA*TREPS2 + MU*TR2
      TR2P = 0.0D0      
      TR2M = 0.0D0      
      DO 70, K=1,2
        IF(EMP(K) .GT. 0.0D0) THEN
          TR2P = TR2P + EMP(K)*EMP(K)
        ELSE
          TR2M = TR2M + EMP(K)*EMP(K)
        ENDIF
 70   CONTINUE
      TR2P = TR2P + EPS33*EPS33
      TR2M = TR2M + EPS33*EPS33
      ENP = MU*TR2P
      ENM = MU*TR2M
      
      IF(TR2D .GT. 0.0D0) THEN
        ENP = ENP + 0.5D0*LAMBDA*TREPS2
      ELSE
        ENM = ENM + 0.5D0*LAMBDA*TREPS2      
      ENDIF
      QEN = 0.5D0*(EN0 - GMT*ENP- GMC*ENM)
      IF(QEN .LT. 0.0D0) QEN = 0.0D0

      IF(TR2D .GT. 0.0D0) THEN
        FD1  = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)  
        FD2  = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)  
      ELSE
        FD1  = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)         
        FD2  = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)          
      ENDIF  
C -- CALCUL DES CONTRAINTES
      LAMBDD = LAMBDA *0.5D0 *(FD1 + FD2)
      DO 80, K=1,2
        IF (EMP(K).GT.0.D0) THEN
          FDI1(K)  = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)  
          FDI2(K)  = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)  
        ELSE
          FDI1(K)  = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)          
          FDI2(K)  = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)      
        ENDIF
        DEUMUD(K) = DEUXMU* 0.5D0*(FDI1(K) + FDI2(K))
 80   CONTINUE
      SIGP(1)=LAMBDD*TREPS + DEUMUD(1)*EMP(1)
      SIGP(2)=LAMBDD*TREPS + DEUMUD(2)*EMP(2)
      
      IF(TROT .GT. 0.0D0) THEN
          LAMFD  =  LAMF*(ALF + GF2*DA2)/(ALF + DA2)
          DLMFD2 = -LAMF*ALF*(1.0D0 - GF2   )/(ALF + DA2)**2
          DLMFD1 = 0.0D0
          SFFT2  =  LAMF*ALF*(1.0D0 - GF2   )
          SFFT1  =  0.0D0
      ELSE  
          LAMFD  =  LAMF*(ALF + GF1*DA1)/(ALF + DA1)
          DLMFD1 = -LAMF*ALF*(1.0D0 - GF1   )/(ALF + DA1)**2
          DLMFD2 = 0.0D0
          SFFT1  =  LAMF*ALF*(1.0D0 - GF1   )
          SFFT2  =  0.0D0
      ENDIF
      DO 90, K = 1,2
        IF(EFP(K) .GT. 0.0D0) THEN
           DEMUDF(K) =  DEUMUF*(ALF + GF2*DA2)/(ALF + DA2)
           D2MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF2   )/(ALF + DA2)**2
           D1MUDF(K) = 0.0D0
           SFF2(K)   =  DEUMUF*ALF*(1.0D0 - GF2   ) 
           SFF1(K)   =  0.0D0
        ELSE
           DEMUDF(K) =  DEUMUF*(ALF + GF1*DA1)/(ALF + DA1)
           D1MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF1   )/(ALF + DA1)**2
           D2MUDF(K) = 0.0D0
           SFF1(K)   =  DEUMUF*ALF*(1.0D0 - GF1   ) 
           SFF2(K)   =  0.0D0
        ENDIF
        SIGF(K) = LAMFD*TROT + DEMUDF(K)*EFP(K)      
 90   CONTINUE                  
 
      IF ((RESI).AND.(.NOT.COUP)) THEN
        CALL R8INIR(6,0.D0,SIG,1)
        DO 1010 I=1,2
          RTEMP=SIGP(I)
          SIG(1)=SIG(1)+VMP(1,I)*VMP(1,I)*RTEMP
          SIG(2)=SIG(2)+VMP(2,I)*VMP(2,I)*RTEMP
          SIG(3)=SIG(3)+VMP(1,I)*VMP(2,I)*RTEMP          

          RTEMP=SIGF(I)
          SIG(4)=SIG(4)+VFP(1,I)*VFP(1,I)*RTEMP
          SIG(5)=SIG(5)+VFP(2,I)*VFP(2,I)*RTEMP
          SIG(6)=SIG(6)+VFP(1,I)*VFP(2,I)*RTEMP          
1010    CONTINUE
      ENDIF
C -- CALCUL DE LA MATRICE TANGENTE
      IF (RIGI) THEN
        IF (OPTION(11:14).EQ.'ELAS') THEN         
          ELAS  =.TRUE.
          ELAS1 =.TRUE.
          ELAS2 =.TRUE.
        ENDIF
        CALL R8INIR(36, 0.D0, DSPDEP, 1)
        IF (COUP) THEN
          CALL R8INIR(72, 0.D0, DSIDEP, 1)
        ELSE
          CALL R8INIR(36,0.D0,DSIDEP,1)
        ENDIF
        IF(TROT .GT. 0.0D0) THEN
          LAMFD  =  LAMF*(ALF + GF2*DA2)/(ALF + DA2)
          DLMFD2 = -LAMF*ALF*(1.0D0 - GF2   )/(ALF + DA2)**2
          DLMFD1 = 0.0D0
          SFFT2  =  ALF*LAMF*(1.0D0 - GF2   )
          SFFT1  =  0.0D0
        ELSE  
          LAMFD  =  LAMF*(ALF + GF1*DA1)/(ALF + DA1)
          DLMFD1 = -LAMF*ALF*(1.0D0 - GF1   )/(ALF + DA1)**2
          DLMFD2 = 0.0D0
          SFFT1  =  ALF*LAMF*(1.0D0 - GF1   )
          SFFT2  =  0.0D0
        ENDIF
       
        DO 1020, K = 1,2
          IF(EFP(K) .GT. 0.0D0) THEN
             DEMUDF(K) =  DEUMUF*(ALF + GF2*DA2)/(ALF + DA2)
             D2MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF2   )/(ALF + DA2)**2
             D1MUDF(K) = 0.0D0
             SFF2(K)   =  ALF*DEUMUF*(1.0D0 - GF2   ) 
             SFF1(K)   =  0.0D0
          ELSE
             DEMUDF(K) =  DEUMUF*(ALF + GF1*DA1)/(ALF + DA1)
             D1MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF1   )/(ALF + DA1)**2
             D2MUDF(K) = 0.0D0
             SFF1(K)   =  ALF*DEUMUF*(1.0D0 - GF1   ) 
             SFF2(K)   =  0.0D0
          ENDIF
 1020   CONTINUE                  
        IF(TR2D .GT. 0.0D0) THEN
          FD1  = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)  
          FD2  = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)  
        ELSE
          FD1  = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)         
          FD2  = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)          
        ENDIF  
        LAMBDD = LAMBDA *0.5D0 *(FD1 + FD2)

        DO 1030, K=1,2
          IF (EMP(K).GT.0.D0) THEN
            FDI1(K)  = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)  
            FDI2(K)  = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)  
          ELSE
            FDI1(K)  = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)          
            FDI2(K)  = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)      
          ENDIF
          DEUMUD(K) = DEUXMU* 0.5D0*(FDI1(K) + FDI2(K))
 1030   CONTINUE
        DE33I = -LAMBDA*KSI2D/(DEUXMU  + LAMBDA*KSI2D)
        
        DO 100 K = 1,2
          DO 110 L = 1,2
            DSPDEP(K,L) = LAMBDD + LAMBDA*KSI2D*DE33I   
            DSPDEP(K+3,L+3) = LAMFD
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,2
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
          DSPDEP(K+3,K+3) = DSPDEP(K+3,K+3) + DEMUDF(K)
 120    CONTINUE
        IF (ABS(EMP(1) - EMP(2)) .LE. 1.D-15) THEN
          DSPDEP(3,3)=DEUMUD(1)
        ELSE
          DSPDEP(3,3)=(DEUMUD(1)*EMP(1)-DEUMUD(2)*EMP(2))
     &                                    /(EMP(1)-EMP(2))
        ENDIF
        
        IF (ABS(EFP(1) - EFP(2)) .LE. 1.D-15) THEN
          DSPDEP(6,6)=DEMUDF(1)
        ELSE
          DSPDEP(6,6)=(DEMUDF(1)*EFP(1)-DEMUDF(2)*EFP(2))
     &                                    /(EFP(1)-EFP(2))
        ENDIF
C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).AND.((EN0.GT.0.D0) .OR. 
     &     ((QFF(1) + QFF(2)).GT.0.0D0))) THEN
        
        DO 700, K=1,2
          SIGEL(K) = LAMBDA*TREPS + DEUXMU*EMP(K) 
 700    CONTINUE
        CALL R8INIR(3, 0.D0, SIGHP, 1)
        CALL R8INIR(3, 0.D0, SIGHM, 1)
 
        IF(TR2D .GT. 0.0D0) THEN
          FD1  = (1.0D0-GMT) / ((1.0D0+DA1)*(1.0D0+DA1)) *0.5D0*LAMBDA 
          FD2  = (1.0D0-GMT) / ((1.0D0+DA2)*(1.0D0+DA2)) *0.5D0*LAMBDA
          KSIM = 0.5D0*( (1.0D0+GMT*DA1)/(1.0D0+DA1) 
     &                 + (1.0D0+GMT*DA2)/(1.0D0+DA2)) 
          GM   = 1.0D0-GMT 
          
          DO 710, K=1,2
            SIGHP(K) = LAMBDA*TREPS
 710      CONTINUE
          
        ELSE
          KSIM = 0.5D0*( (1.0D0+GMC*DA1)/(1.0D0+DA1) 
     &                 + (1.0D0+GMC*DA2)/(1.0D0+DA2)) 
          GM   = 1.0D0 
          
          FD1  = (1.0D0-GMC) / ((1.0D0+DA1)*(1.0D0+DA1)) *0.5D0*LAMBDA 
          FD2  = (1.0D0-GMC) / ((1.0D0+DA2)*(1.0D0+DA2)) *0.5D0*LAMBDA 
          
          DO 720, K=1,2
            SIGHM(K) = LAMBDA*TREPS
 720      CONTINUE
 
        ENDIF  
        
        DO 800, K=1,2
          IF (EMP(K).GT.0.D0) THEN
            FDI1(K)  = (1.0D0-GMT) / ((1.0D0+DA1)*(1.0D0+DA1)) *MU 
            FDI2(K)  = (1.0D0-GMT) / ((1.0D0+DA2)*(1.0D0+DA2)) *MU 
            
              SIGHP(K) = SIGHP(K) + DEUXMU*EMP(K)
          ELSE
            FDI1(K)  = (1.0D0-GMC) / ((1.0D0+DA1)*(1.0D0+DA1)) *MU 
            FDI2(K)  = (1.0D0-GMC) / ((1.0D0+DA2)*(1.0D0+DA2)) *MU 
            
            SIGHM(K) = SIGHM(K) + DEUXMU*EMP(K)
          ENDIF
          TREPS  = TR2D + EPS33
          QM1 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
          QM2 = 0.5D0*COF1 * EPS33*EPS33 + COF2 * EPS33 + Q2D
          QME33 = 0.5D0*LAMBDA*TREPS*GM + MU*EPS33
          QDE(K) = 0.5D0 * (SIGEL(K) - GMT*SIGHP(K) - GMC*SIGHM(K))

          A11 = 2.0D0*(QM1/(1.0D0 + DA1)**3 + QFF(1)/(ALF + DA1)**3)
     &          - QME33*DE33D1/(1.0D0 + DA1)**2
          A12 = -QME33*DE33D2/(1.0D0 + DA1)**2
          
          A22 = 2.0D0*(QM2/(1.0D0 + DA2)**3 + QFF(2)/(ALF + DA2)**3)
     &          - QME33*DE33D2/(1.0D0 + DA2)**2
          A21 = -QME33*DE33D1/(1.0D0 + DA2)**2
          
          B1  = (QDE(K) - QME33* KSIM*LAMBDA/(DEUXMU + KSIM*LAMBDA)) 
     &          / (1.0D0 + DA1)**2
          B2  = (QDE(K) - QME33* KSIM*LAMBDA/(DEUXMU + KSIM*LAMBDA)) 
     &          / (1.0D0 + DA2)**2
          LSING1 = ABS(A11) .LT. MAX(1.0D-10*A22,1.0D-14)
          LSING2 = ABS(A22) .LT. MAX(1.0D-10*A11,1.0D-14)

          IF(LSING2 .AND. (.NOT. LSING1)) THEN  
            D1E(K) = B1/A11
            D2E(K) = 0.0D0
          ELSEIF(LSING1 .AND. (.NOT. LSING2)) THEN  
            D1E(K) = 0.0D0
            D2E(K) = B2/A22
          ELSEIF(LSING2 .AND. LSING1) THEN  
            D1E(K) = 0.0D0
            D2E(K) = 0.0D0
          ELSE
            D1E(K) = (B1 - A12*B2/A22)/(A11 - A12*A21/A22)
            D2E(K) = (B2 - A21*D1E(K))/A22
          ENDIF
          SD1(K) = -TREPS*FD1 - EMP(K)*FDI1(K)
          SD2(K) = -TREPS*FD2 - EMP(K)*FDI2(K)
          
          MD1(K) = DLMFD1*TROT + D1MUDF(K)*EFP(K)
          MD2(K) = DLMFD2*TROT + D2MUDF(K)*EFP(K)

          B1  = (SFFT1*TROT + SFF1(K)*EFP(K)) / (ALF + DA1)**2
          B2  = (SFFT2*TROT + SFF2(K)*EFP(K)) / (ALF + DA2)**2

          IF(LSING2 .AND. (.NOT. LSING1)) THEN  
            D1K(K) = B1/A11
            D2K(K) = 0.0D0
          ELSEIF(LSING1 .AND. (.NOT. LSING2)) THEN  
            D1K(K) = 0.0D0
            D2K(K) = B2/A22
          ELSEIF(LSING2 .AND. LSING1) THEN  
            D1K(K) = 0.0D0
            D2K(K) = 0.0D0
          ELSE
            D1K(K) = (B1 - A12*B2/A22)/(A11 - A12*A21/A22)
            D2K(K) = (B2 - A21*D1K(K))/A22
          ENDIF
 800    CONTINUE
        DO 910, K=1,2
          DO 900, L=1,2
C--------MEMBRANE-----------------          
           IF(.NOT. ELAS1) DSPDEP(L,K) = DSPDEP(L,K) + SD1(L)*D1E(K)
           IF(.NOT. ELAS2) DSPDEP(L,K) = DSPDEP(L,K) + SD2(L)*D2E(K)
           IF(.NOT. ELAS1)
     &        DSPDEP(L,K) = DSPDEP(L,K) + LAMBDA*KSI2D*DE33D1*D1E(K)
           IF(.NOT. ELAS2)
     &        DSPDEP(L,K) = DSPDEP(L,K) + LAMBDA*KSI2D*DE33D2*D2E(K)
C--------FLEXION-----------------          
           IF(.NOT. ELAS1)
     &       DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3) + MD1(L)*D1K(K)
           IF(.NOT. ELAS2)
     &       DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3) + MD2(L)*D2K(K)
     
C--------COUPLAGE M-F -----------------          
           IF(.NOT. ELAS1) DSPDEP(L,K+3)=DSPDEP(L,K+3) + SD1(L)*D1K(K) 
           IF(.NOT. ELAS2) DSPDEP(L,K+3)=DSPDEP(L,K+3) + SD2(L)*D2K(K) 
           IF(.NOT. ELAS1) DSPDEP(L+3,K)=DSPDEP(L+3,K) + MD1(L)*D1E(K) 
           IF(.NOT. ELAS2) DSPDEP(L+3,K)=DSPDEP(L+3,K) + MD2(L)*D2E(K) 
 900      CONTINUE
 910    CONTINUE
C   NOT ELAS
      END IF 
      CALL TANMGL(T,VMP,VFP,DSPDEP,DSIDEP)
      ENDIF 
      END
