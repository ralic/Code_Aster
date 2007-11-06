        SUBROUTINE HUJJID( MOD, MATER, INDI, DEPS,
     &                     YD, YF, VIND, R, SIGNE, DRDY, IRET )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C  --------------------------------------------------------------------
C  INTEGRATION PLASTIQUE (MECANISME DEVIATOIRE SEUL) DE LA LOI HUJEUX
C
C  RESOLUTION PAR METHODE DE NEWTON   DRDY(DY).DDY = - R(DY)
C
C  CALCUL DU SECOND MEMBRE : - R(DY)
C  CALCUL DU JACOBIEN      : DRDY(DY)
C  DY   =  ( SIG     , ESPVP     , R       , LAMBDA   )
C  R    = -( LE      , LEVP      , LR      , LF       )
C  DRDY =  ( DLEDS   , DLEDEVP   , DLEDR   , DLEDLA   )
C          ( DLEVPDS , DLEVPDEVP , DLEVPDR , DLEVPDLA )
C          ( DLRDS   , DLRDEVP   , DLRDR   , DLRDLA   )
C          ( DLFDS   , DLFDEVP   , DLFDR   , DLFDLA   )
C =====================================================================
C  IN   MOD   :  MODELISATION
C       MATER :  COEFFICIENTS MATERIAU
C       INDI  :  INDICE DES MECANISMES SUPPOSES ACTIFS
C       DEPS  :  INCREMENT DE DEFORMATION
C       YD    :  VARIABLES A T = (SIGD, VIND, DLAMBDAD)
C       YF    :  VARIABLES A T+DT = (SIGF, VINF, DLAMBDAF)
C       VIND  :  VARIABLES INTERNES A T
C  VAR  IND   :  TABLEAU DES NUMEROS DE MECANISMES ACTIFS
C  OUT  R     :  SECOND MEMBRE
C       SIGNE :  SIGNE DE SK:DEPSDP POUR LE MECANISME K
C       DRDY  :  JACOBIEN
C       IRET  :  CODE RETOUR
C                = 0 OK
C                = 1 NOOK : SI LA SUBDIVISION DU PAS DE TEMPS EST ACTIV
C                           DANS STAT_NON_LINE, IL Y A SUBDIVISION
C =====================================================================
        INTEGER       NDT, NDI, NMOD, I, II, J, K, KK, L
        INTEGER       INDI(4), NBMECA, IRET
        INTEGER       IFM, NIV
        PARAMETER     (NMOD = 15)
        REAL*8        DEPSP(6), DEPSPK(3), DEPSE(6)
        REAL*8        SIGD(3),SIGF(6),P(4),Q(3),SIGNE(4)
        REAL*8        YD(NMOD), YF(NMOD), DRDY(NMOD,NMOD)
        REAL*8        MATER(22,2), N, BETA, D, M, PCO, PREF, PC
        REAL*8        DEGR, PHI, ANGDIL, MDIL, B, DKSIDR(3)
        REAL*8        RC(4), DLAMBD(4), DEPSDS(6,6)
        REAL*8        HOOKNL(6,6), HOOK(6,6), DHOKDS(6,6)
        REAL*8        I1F, E, NU, AL, DEMU, COEF0, DCOEF0
        REAL*8        LE(6), LEVP, LR(4), LF(4), R(NMOD), DELTA(6)
        REAL*8        DLEDS(6,6), DLEDEV(6), DLEDR(6,4), DLEDLA(6,4)
        REAL*8        DLEVDS(6), DLEVDE, DLEVDR(4), DLEVDL(4)
        REAL*8        DLRDS(4,6), DLRDLE(4), DLRDR(4,4), DLRDLA(4,4)
        REAL*8        DLFDS(4,6), DLFDLE(4), DLFDR(4,4), DLFDLA(4,4)
        REAL*8        CDE(6), CTILD(6), CD2FDS(6,6)
        REAL*8        DLADR(6), DLEDR1(6), PSI(24), AD(3), KSI(4)
        REAL*8        DPSIDS(6,6), DFDS(6), DLEK(6), ID(6)
        REAL*8        TRACE, EPSVP, DEPS(6), QXK, TH(2), PROD
        REAL*8        LEPV, ACYC, AMON, CMON, CCYC, XH(2)
        REAL*8        ZERO, UN, D12, D13, DEUX, TRUC, LA, ALPHA
        REAL*8        TOLE, COEF, MUL, CCOND, VIND(*), SI, SIGDC(9)
        REAL*8        PRODC, PRODM, PS, SCXH, SXH, FAC
        CHARACTER*8   MOD
        LOGICAL       DEBUG
C =====================================================================
        PARAMETER     ( D12    = 0.5D0  )
        PARAMETER     ( D13    = 0.333333333334D0  )
        PARAMETER     ( UN     = 1.D0   )
        PARAMETER     ( ZERO   = 0.D0   )
        PARAMETER     ( DEUX   = 2.D0   )
        PARAMETER     ( TOLE   = 1.D-6 )
        PARAMETER     ( DEGR = 0.0174532925199D0 )
C =====================================================================
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C =====================================================================
C        CALL JEMARQ ()
        CALL INFNIV(IFM,NIV)
C =====================================================================
C --- PROPRIETES HUJEUX MATERIAU --------------------------------------
C =====================================================================
        N      = MATER(1,2)
        BETA   = MATER(2,2)
        D      = MATER(3,2)
        B      = MATER(4,2)
        PHI    = MATER(5,2)
        ANGDIL = MATER(6,2)
        PCO    = MATER(7,2)
        PREF   = MATER(8,2)
        ACYC   = MATER(9,2)
        AMON   = MATER(10,2)
        CCYC   = MATER(11,2)
        CMON   = MATER(12,2)
        M      = SIN(DEGR*PHI)
        MDIL   = SIN(DEGR*ANGDIL)
        COEF   = MATER(20,2)
        ALPHA  = COEF*D12 

C =====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
C =====================================================================
        I1F  = D13 * TRACE(NDI,YF)
        IF ( I1F .GT. ZERO ) I1F = 1.D-12*PREF
        
        DO 6 I = 1, NDT
          SIGF(I) = YF(I)
          PSI(I) = ZERO
          PSI(NDT+I)  = ZERO
          PSI(2*NDT+I) = ZERO
          PSI(3*NDT+I) = ZERO
 6       CONTINUE
C        WRITE(6,'(A,6(1X,E16.9))')'HUJJID : SIGF =',(SIGF(I),I=1,6)
        DO 3 I = 1, 9
              SIGDC(I)=ZERO
  3     CONTINUE
        
        NBMECA = 0
        DO 4 K = 1, 4
          IF (INDI(K) .GT. 0) NBMECA = NBMECA + 1
          DLAMBD(K) = ZERO
 4        CONTINUE
        DO 5 K = 1, NBMECA
          RC(K) = YF(NDT+1+K)
          CALL HUJDDD('PSI   ', INDI(K), MATER, INDI, YF, VIND,
     &                PSI((K-1)*NDT+1), DPSIDS, IRET)
          IF (IRET .EQ. 1) GOTO 1000           
          IF (INDI(K) .LT. 4) THEN
            CALL HUJPRJ (INDI(K), SIGF, SIGD, P(K), Q(K))
            IF ((P(K)/PREF) .LE. TOLE) GOTO 999
            CALL HUJKSI('DKSIDR', MATER, RC(K), DKSIDR(K), IRET)
            CALL HUJKSI('KSI   ', MATER, RC(K), KSI(K), IRET)
            IF(IRET.EQ.1)GOTO 1000
            AD(K)  = ACYC+KSI(K)*(AMON-ACYC)
          ELSEIF (INDI(K) .EQ. 4) THEN
            KSI(K) = UN

Caf 11/05/07 Debut      
    
          ELSEIF ((INDI(K) .LT. 8) .AND. (INDI(K) .GT. 4)) THEN 
            CALL HUJPRC (K, INDI(K)-4, SIGF, VIND, MATER, YF,
     &                      P(K), Q(K), SIGDC(3*K-2))
            IF ((P(K)/PREF) .LE. TOLE) GOTO 999
            CALL HUJKSI('DKSIDR', MATER, RC(K), DKSIDR(K), IRET)
            CALL HUJKSI('KSI   ', MATER, RC(K), KSI(K), IRET)
            IF(IRET.EQ.1) GOTO 1000
            AD(K)  = DEUX*(ACYC+KSI(K)*(AMON-ACYC))
C           WRITE(6,'(A,E12.5)')'QCYC =',Q(K)
          ELSEIF (INDI(K) .EQ. 8) THEN
            KSI(K) = UN    
            CALL HUJPIC(K, INDI(K),SIGF, VIND, MATER, YF, P(K))
C           WRITE(6,'(A,E12.5)')'HUJJID --- PCYC =',P(K)
Caf 11/05/07 Fin        
          ELSE
            CALL U2MESS('F', 'COMPOR1_8')
          ENDIF
          DLAMBD(K) = YF(NDT+1+NBMECA+K)
 5        CONTINUE
C       WRITE(6,'(A,24(1X,E12.5))'),'PSI =',(PSI(I),I=1,24)

        EPSVP = YF(NDT+1)
        PC    = PCO*EXP(-BETA*EPSVP)
        
        CMON = CMON * PC/PREF
        
Caf 15/05/07 Debut
        CCYC = CCYC * PC/PREF
Caf 15/05/07 Fin
        
C --- CONDITIONNEMENT DE LA MATRICE JACOBIENNE
        CCOND= MATER(1,1)
C =====================================================================
C --- OPERATEURS DE RIGIDITE ET DE SOUPLESSE (LINEAIRES OU NON LINEA.) 
C =====================================================================
C --- OPERATEURS LINEAIRES --------------------------------------------
C =====================================================================
        CALL LCINMA (ZERO, HOOK)
        E    = MATER(1,1)
        NU   = MATER(2,1)
        AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E     /(UN+NU)
        LA   = E*NU/(UN+NU)/(UN-DEUX*NU)
                
        
C =====================================================================
C --- 3D/DP/AX --------------------------------------------------------
C =====================================================================
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS')  THEN
     
           DO 30 I = 1, NDI
             DO 30 J = 1, NDI
               IF (I.EQ.J) HOOK(I,J) = AL
               IF (I.NE.J) HOOK(I,J) = LA
 30            CONTINUE
           DO 35 I = NDI+1, NDT
             HOOK(I,I) = DEMU
 35          CONTINUE
 
 
C =====================================================================
C --- CP/1D -----------------------------------------------------------
C =====================================================================
        ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &          MOD(1:2) .EQ. '1D')   THEN
     
          CALL U2MESS('F', 'COMPOR1_4')
     
        ENDIF
        
C =====================================================================
C --- OPERATEUR NON LINEAIRE ------------------------------------------
C =====================================================================
        COEF0 = (I1F/PREF) ** N
        DO 40 I = 1, NDT
          DO 40 J = 1, NDT
            HOOKNL(I,J) = COEF0*HOOK(I,J)
 40         CONTINUE
 
C =====================================================================
C --- DERIVEE PAR RAPPORT A DS DE L'OPERATEUR NON LINEAIRE: DHOOKDS ---
C =====================================================================
        DCOEF0 = D13*N/PREF * (I1F/PREF)**(N-1)
        DO 41 I = 1, NDT
          DO 41 J = 1, NDT
            DHOKDS(I,J) = DCOEF0*HOOK(I,J)
 41         CONTINUE

C =====================================================================
C --- I. CALCUL DE DLEDS (6X6) ----------------------------------------
C =====================================================================
C ---> I.1. CALCUL DE CTILD = DHOOKDS*(DEPS - DEPSP)
C ---> I.1.1. CALCUL DE DEPSP A T+DT
        DO 50 I = 1,NDT
          DEPSP(I) = ZERO
  50     CONTINUE

       DO 51 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 53 I = 1, NDT
           DEPSP(I) = DEPSP(I) + DLAMBD(K)*PSI(KK+I)
 53        CONTINUE           
C        WRITE(6,'(A,6(1X,E16.9))')'DEPSP =',(DEPSP(I),I=1,6)
C       WRITE(6,'(A,6(1X,E16.9))')'DEPS =',(DEPS(I),I=1,6)
C       WRITE(6,'(A,24(1X,E16.9))')'PSI =',(PSI(I),I=1,24)    
C ---- SIGNE DE S:DEPSDP POUR CHAQUE MECANISME
C          CALL HUJPRJ ( KK, DEPSP, DEPSPK, PK, QK)
C          CALL HUJPRJ ( KK, SIGF, SIGDK, PK, QK )
C          TRUC = DEPSPK(1)*SIGDK(1)+DEPSPK(2)*SIGDK(2)+DEPSPK(3)*SIGDK
C          IF(TRUC .GE. ZERO) THEN
C            SIGNE(K) = UN
C          ELSE
C            SIGNE(K) = -UN
C          ENDIF
        
 51      CONTINUE
 
C ------------ FIN I.1.1.
       DO 52 I = 1, NDT
         DEPSE(I) = DEPS(I) - DEPSP(I)
 52      CONTINUE
C       WRITE(6,'(A,6(1X,E16.9))')'DEPSE =',(DEPSE(I),I=1,6)
       CALL LCPRMV (DHOKDS, DEPSE, CTILD)
C ------------ FIN I.1.
C ---> I.2. CALCUL DE CD2FDS = HOOK * DEPSDS
C                     (6X6)    (6X6)  (6X6)
       CALL LCINMA (ZERO, DEPSDS)

       DO 60 K = 1, NBMECA
         KK = INDI(K)
         IF ((KK .EQ. 4).OR.(KK.EQ.8)) GOTO 610

         CALL HUJDDD('DPSIDS', KK, MATER, INDI, YF, VIND,
     &               DFDS, DPSIDS, IRET)
         IF (IRET.EQ.1) GOTO 1000
         DO 60 I = 1, NDT
           DO 60 J = 1, NDT
             DEPSDS(I,J) = DEPSDS(I,J) + DLAMBD(K)*DPSIDS(I,J)
 60          CONTINUE
 610   CONTINUE
C       IF(KK.EQ.7)THEN
C       WRITE(6,'(A)')'DPSIDS ='
C       DO 811 I=1, NDT
C         WRITE(6,'(6(1X,E16.9))')(DPSIDS(I,J),J=1,NDT)
C 811    CONTINUE
C       ENDIF
       
       CALL LCPRMM (HOOKNL, DEPSDS, CD2FDS)

C ------------ FIN I.2.
        CALL LCINMA (ZERO, DLEDS)
        DO 63 I = 1, NDT
          DLEDS(I,I) = UN /CCOND
 63       CONTINUE
 
       DO 61 I = 1, NDT
         DO 62 J = 1, NDI
           DLEDS(I,J) = DLEDS(I,J) - (CTILD(I) - CD2FDS(I,J)) /CCOND
 62        CONTINUE
          DO 61 J = NDI+1, NDT
            DLEDS(I,J) = DLEDS(I,J) + CD2FDS(I,J) /CCOND
  61        CONTINUE
C       WRITE(6,'(A)')'CD2FDS'
C       DO 812 I=1, NDT
C         WRITE(6,'(6(1X,E16.9))')(CD2FDS(I,J),J=1,NDT)
C 812    CONTINUE 
       
C       WRITE(6,'(A)')'DLEDS'
C       DO 811 I=1, NDT
C         WRITE(6,'(6(1X,E16.9))')(DLEDS(I,J),J=1,NDT)
C 811    CONTINUE 
C =====================================================================
C --- II. CALCUL DE DLEDR (6XNBMEC) -----------------------------------
C =====================================================================
      DO 70 I = 1, NDT
        DO 70 K = 1, NBMECA
           DLEDR(I,K) = ZERO
 70        CONTINUE

Caf 15/05/07 Debut

      DO 71 K = 1, NBMECA
         KK = INDI(K)
         IF ((KK .EQ. 4) .OR. (KK .EQ. 8)) GOTO 710
         
         IF (KK .LT. 4) THEN
         MUL = - DLAMBD(K)*ALPHA*(MDIL+Q(K)/P(K))*DKSIDR(K)
         
         DO 72 I = 1, NDI
           IF (I .NE. KK) THEN
             DELTA(I) = MUL
           ELSE
             DELTA(I) = ZERO
           ENDIF
 72        CONTINUE
 
         DO 73 I = NDI+1, NDT
           DELTA(I) = ZERO
 73        CONTINUE
         
C        WRITE(6,'(A,6(1X,E12.5))')'DELTA =',(DELTA(I),I=1,6)
         
         ELSEIF ((KK .LT. 8) .AND. (KK .GT. 4)) THEN
C ---> MECANISME CYCLIQUE DEVIATOIRE
                
           TH(1) = VIND(4*KK-9)
           TH(2) = VIND(4*KK-8) 
           CALL HUJPRJ (INDI(K)-4, SIGF, SIGD, PS, PROD)  
           PRODC  = 2*SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)
           PRODM  = 2*SIGD(1)*TH(1) + SIGD(3)*TH(2)
           PS     = 2*SIGDC(3*K-2)*SIGD(1)+SIGDC(3*K)*SIGD(3)
           
           SI = UN
           DO 74 I = 1, NDI
           IF (I .NE. (KK-4)) THEN
             IF((-Q(K)/PREF).GT.TOLE)THEN
               DELTA(I) = DLAMBD(K)*(M*P(K)*(UN-B*LOG(P(K)/PC))/
     &                    (2.D0*Q(K))*(TH(1)*SI-SIGDC(3*K-2)*SI*PRODC/
     &                    (2.D0*Q(K)**2))            
     &                                 -ALPHA*
     &                    (DKSIDR(K)*(MDIL+PS/(P(K)*2.D0*Q(K)))+
     &                    KSI(K)/2.D0*M*(UN-B*LOG(P(K)/PC))*(PRODM
     &                    -PS*PRODC/(2.D0*Q(K)**2))/Q(K)))
             ELSE
               DELTA(I) = DLAMBD(K)*(-ALPHA)*DKSIDR(K)*MDIL            
             ENDIF
             
               SI = - SI               
           ELSE
             DELTA(I) = ZERO
           ENDIF
 74        CONTINUE
 
         DO 75 I = NDI+1, NDT
           DELTA(I) = ZERO
 75      CONTINUE
        
         IF((-Q(K)/PREF).GT.TOLE)THEN         
           DELTA(NDT+5-KK)= DLAMBD(K)*(M*P(K)*(UN-B*LOG(P(K)/PC))/
     &                    (2.D0*Q(K))*(TH(2)-SIGDC(3*K)*PRODC/
     &                    (2.D0*Q(K)**2)))
         ELSE
           DELTA(NDT+5-KK)= ZERO         
         ENDIF
C        WRITE(6,'(A,6(1X,E12.5))')'DELTA =',(DELTA(I),I=1,6)
                 
         ENDIF
         
C        WRITE(6,'(A,6(1X,E12.5))')'DELTA =',(DELTA(I),I=1,6)
         
         CALL LCPRMV (HOOKNL, DELTA, DLEDR1)
         DO 71 I = 1, NDT
           DLEDR(I,K) = DLEDR1(I) /CCOND
 71        CONTINUE
Caf 15/05/07 Fin
 
 710  CONTINUE
C       WRITE(6,'(A)')'DLEDR'
C       DO 811 I=1, NDT
C         WRITE(6,'(6(1X,E16.9))')(DLEDR(I,J),J=1,NBMECA)
C 811    CONTINUE 
C =====================================================================
C --- III. CALCUL DE DLEDEVP (6X1) ------------------------------------
C =====================================================================
      DO 80 I = 1, NDT
        DLEDEV(I) = ZERO
C        IF(I.GT.NDI)THEN
C         ID(I) = ZERO 
C       ELSE
C         ID(I) = UN
C       ENDIF
 80     CONTINUE
C       CALL LCPRMV (HOOKNL, ID, DLEDEV)
        
C       DO 81 K = 1, NBMECA
C         KK = INDI(K)
C         IF((KK.GT.4).AND.(KK.LT.8))THEN
C           CALL HUJPRJ (INDI(K)-4, SIGF, SIGD, PS, PROD)  
            
C           PS = 2*SIGDC(3*K-2)*SIGD(1)+SIGDC(3*K)*SIGD(3)
C           XH(1) = VIND(4*KK-11)
C           XH(2) = VIND(4*KK-10)
C           TH(1) = VIND(4*KK-9)
C           TH(2) = VIND(4*KK-8)
            
C           SXH   = 2*SIGD(1)*(XH(1)-TH(1)*RC(K))+
C     &             SIGD(3)*(XH(2)-TH(2)*RC(K))
C           SCXH  = 2*SIGDC(3*K-2)*(XH(1)-TH(1)*RC(K))+
C     &             SIGDC(3*K)*(XH(2)-TH(2)*RC(K))
C           SI = UN
C           DO 82 I = 1, NDI
C             IF(I.NE.(KK-4))THEN
C               DLEDEV(I) = DLEDEV(I) + DLAMBD(K)*M*P(K)*B*BETA*D12/Q(K)
C     &                    *((XH(1)-TH(1)*RC(K))*SI-SIGDC(3*K-2)*
C     &                      SI*D12/Q(K)**2*SCXH-ALPHA*KSI(K)/P(K)*
C     &                     (SXH-PS*D12/Q(K)**2*SCXH))
C             SI = - SI
C             ENDIF               
C  82       CONTINUE 
C         DLEDEV(NDT+5-KK) = DLAMBD(K)*M*P(K)*B*BETA*D12/Q(K)
C     &             *((XH(2)-TH(2)*RC(K))-SIGDC(3*K)*D12/Q(K)**2*SCXH)
C         ENDIF
C  81   CONTINUE
  
C        CALL LCPRMV (HOOKNL, DLEDEV, DLEDEV)
        
C =====================================================================
C --- IV. CALCUL DE DLEDLA (6XNBMEC) ----------------------------------
C =====================================================================
      DO 91 K = 1, NBMECA
         KK = (K-1)*NDT+1
         CALL LCPRMV (HOOKNL, PSI(KK), DLEK)         
         DO 91 I = 1, NDT
           DLEDLA(I,K) = DLEK(I) /CCOND
 91        CONTINUE
       
C       WRITE(6,'(A)')'DLEDLA'
C       DO 811 I=1, NDT
C         WRITE(6,'(6(1X,E16.9))')(DLEDLA(I,J),J=1,NBMECA)
C 811    CONTINUE   
       
C =====================================================================
C --- V. CALCUL DE DLRDS (NBMECX6) ------------------------------------
C =====================================================================
      DO 100 K = 1, NBMECA
        DO 100 I = 1, NDT
          DLRDS(K,I) = ZERO
 100      CONTINUE


C =====================================================================
C --- VI. CALCUL DE DLRDR (NBMECXNBMEC) -------------------------------
C =====================================================================
      DO 110 K = 1, NBMECA
        DO 110 L = 1, NBMECA
          DLRDR(K,L) = ZERO
 110    CONTINUE
 
      DO 111 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          MUL        = (UN-RC(K))/AD(K)
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*MUL
     &  + DLAMBD(K)*DKSIDR(K)*(AMON-ACYC)*MUL**DEUX
        ELSEIF (KK.EQ.4) THEN
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(UN-RC(K))/CMON

Caf 15/05/07 Debut

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
          MUL        = (UN-RC(K))/AD(K)
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(MUL
     &  + DKSIDR(K)*(AMON-ACYC)*MUL**DEUX)
        
        ELSEIF (KK .EQ. 8) THEN
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(UN-RC(K))/CCYC
        
Caf 15/05/07 Fin
        ENDIF   
 111    CONTINUE
C       WRITE(6,'(A)')'DLRDR'
C       DO 811 I=1, NBMECA
C         WRITE(6,'(6(1X,E12.5))')(DLRDR(I,J),J=1,NBMECA)
C 811    CONTINUE   
C =====================================================================
C --- VII. CALCUL DE DLRDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
      DO 112 K = 1, NBMECA
        DO 112 L = 1, NBMECA
          DLRDLA(K,L) = ZERO
 112      CONTINUE
 
      DO 113 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /AD(K)
        ELSEIF (KK.EQ.4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /CMON
          
Caf 15/05/07 Debut

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /AD(K)
        
        ELSEIF (KK .EQ. 8) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /CCYC
          
Caf 15/05/07 Fin

        ENDIF
 113    CONTINUE
C       WRITE(6,'(A)')'DLRDLA'
C       DO 811 I=1, NBMECA
C         WRITE(6,'(6(1X,E12.5))')(DLRDLA(I,J),J=1,NBMECA)
C 811    CONTINUE   
 
C =====================================================================
C --- VIII. CALCUL DE DLRDEVP (NBMECX1) -------------------------------
C =====================================================================
      DO 120 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLE(K) = ZERO
        ELSEIF (KK .EQ. 4) THEN
          DLRDLE(K) = -DLAMBD(K)*BETA*( UN-RC(K) )**DEUX /CMON

Caf 15/05/07 Debut

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
          DLRDLE(K) = ZERO
        
        ELSEIF (KK. EQ. 8) THEN
          DLRDLE(K) = -DLAMBD(K)*BETA*( UN-RC(K) )**DEUX /CCYC
          
Caf 15/05/07 Fin

        ENDIF
 120    CONTINUE
C       WRITE(6,'(4(1X,E12.5))')(DLRDLE(J),J=1,NBMECA)
 
C =====================================================================
C --- IX. CALCUL DE DLEVPDS (1X6) -------------------------------------
C =====================================================================
       DO 130 I = 1, NDT
         DLEVDS(I) = ZERO
 130     CONTINUE
 
      DO 131 K = 1, NBMECA
        KK = INDI(K)
        IF ((KK .EQ. 4) .OR. (KK .EQ. 8)) GOTO 1310

Caf 15/05/07 Debut
        
        IF (KK .LT. 4) THEN
        CALL HUJPRJ (KK, SIGF, SIGD, COEF0, MUL)
        IF ((-Q(K)/PREF) .LE. TOLE) GOTO 131
        DLEVDS(NDT+1-KK) = DLEVDS(NDT+1-KK) +   
     &  DLAMBD(K) * KSI(K)*COEF*SIGD(3) /P(K)/Q(K)/2.D0
        
        SI = UN
        DO 132 I = 1, NDI
          IF (I .NE. KK) THEN
            DLEVDS(I) = DLEVDS(I) +
     &      DLAMBD(K)*KSI(K)*COEF*(SIGD(1)*SI /P(K)/Q(K)/2.D0  
     &      - D12*Q(K) /P(K)**DEUX)
          SI = -SI
          ENDIF
 132      CONTINUE
        
        ELSEIF ((KK. LT. 8) .AND. (KK. GT. 4)) THEN
          IF((-Q(K)/PREF) .LE. TOLE) GOTO 131
          
          
          CALL HUJPRJ(KK-4, SIGF, SIGD, COEF0, MUL)
          PS = 2*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)
          
          XH(1) = VIND(4*KK-11)
          XH(2) = VIND(4*KK-10)
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)
          
          SXH = 2*SIGD(1)*(XH(1)-TH(1)*RC(K))+
     &           SIGD(3)*(XH(2)-TH(2)*RC(K))
          SCXH  = 2*SIGDC(3*K-2)*(XH(1)-TH(1)*RC(K))+
     &           SIGDC(3*K)*(XH(2)-TH(2)*RC(K))
          
          FAC = D12*M*(UN-B*(UN+LOG(P(K)/PC)))
          
          IF((-Q(K)/PREF).GT.TOLE)THEN
            DLEVDS(NDT+5-KK) = DLEVDS(NDT+5-KK) +
     &      DLAMBD(K) * KSI(K)*COEF/(2.D0*P(K)*Q(K))*
     &      (SIGD(3)+SIGDC(3*K)*(UN-PS/Q(K)**2/2.D0))
          ENDIF
          
          SI = UN
          DO 133 I = 1, NDI
            IF (I .NE. (KK-4)) THEN
              IF((-Q(K)/PREF).GT.TOLE)THEN
                DLEVDS(I) = DLEVDS(I) +
     &          DLAMBD(K)*KSI(K)*COEF/(2.D0*P(K)*Q(K))*(
     &          SIGDC(3*K-2)*SI*(UN-PS/(2.D0*Q(K)**2))+SIGD(1)*SI 
     &          -FAC*(SXH-SCXH*PS*D12/Q(K)**2)-D12*PS /P(K))
                SI = -SI
              ENDIF
            ENDIF
 133      CONTINUE
        ENDIF
Caf 15/05/07 Fin
 
 131    CONTINUE
 1310   CONTINUE
C       WRITE(6,'(6(1X,E12.5))')(DLEVDS(J),J=1,6)
C =====================================================================
C --- X. CALCUL DE DLEVPDEVP (1X1) ------------------------------------
C =====================================================================
       DLEVDE = UN
       DO 140 K = 1, NBMECA
         KK = INDI(K)
         IF((KK.GT.4).AND.(KK.LT.8))THEN
         
           CALL HUJPRJ( KK-4, SIGF, SIGD, COEF0, MUL)
           IF(Q(K).GT.TOLE)THEN
             XH(1) = VIND(4*KK-11)
             XH(2) = VIND(4*KK-10)
             TH(1) = VIND(4*KK-9)
             TH(2) = VIND(4*KK-8)
             PRODC  = 2*SIGDC(3*K-2)*(XH(1)-RC(K)*TH(1)) + 
     &               (SIGDC(3*K)*(XH(2)-RC(K)*TH(2))) 
             PRODM  = 2.D0*SIGD(1)*(XH(1)-RC(K)*TH(1)) + 
     &               (SIGD(3)*(XH(2)-RC(K)*TH(2)))
             PS     = 2*SIGD(1)*SIGDC(3*K-2)+SIGD(3)+SIGDC(3*K)
             IF((-Q(K)/PREF).GT.TOLE)THEN
               DLEVDE = DLEVDE + DLAMBD(K)*COEF*KSI(K)/Q(K)/2.D0*
     &                         M*B*BETA*(PRODM - PS/2.D0/Q(K)**2*
     &                         PRODC)
             ENDIF
           ENDIF  
         ENDIF
 140   CONTINUE
       
C =====================================================================
C --- XI. CALCUL DE DLEVPDR (1XNBMEC) ---------------------------------
C =====================================================================
      DO 151 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLEVDR(K) =
     &    DLAMBD(K)*COEF*DKSIDR(K)*(MDIL+Q(K)/P(K))
        ELSEIF (KK .EQ. 4) THEN
          DLEVDR(K) = ZERO
          
Caf 15/05/07 Debut

        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
                
          CALL HUJPRJ(KK-4, SIGF, SIGD, COEF0, MUL)     
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)
             
          PRODC = 2*SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)
          PRODM = 2*SIGD(1)*TH(1) + SIGD(3)*TH(2)
          PS    = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)
          IF((-Q(K)/PREF).GT.TOLE)THEN
            DLEVDR(K) = DLAMBD(K)*COEF*(
     &                  DKSIDR(K)*(MDIL+PS/(2.D0*Q(K)*P(K)))
     &                  +KSI(K)*M*(UN-B*LOG(P(K)/PC))/(2.D0*Q(K))*
     &                  (PRODM-PS*PRODC/(2.D0*Q(K)**2)))
          ELSE
            DLEVDR(K) = DLAMBD(K)*COEF*DKSIDR(K)*MDIL
          ENDIF
         
        ELSEIF (KK .EQ. 8) THEN 
          DLEVDR(K) = ZERO
Caf 15/05/07 Fin
          
        ENDIF
 151    CONTINUE
C       WRITE(6,'(4(1X,E12.5))')(DLEVDR(J),J=1,NBMECA)
C       WRITE(6,'(A,4(1X,E12.5))')'DKSIDR =',(DKSIDR(K),K=1,NBMECA)
C =====================================================================
C --- XII. CALCUL DE DLEVPDLA (1XNBMEC) -------------------------------
C =====================================================================
      DO 161 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLEVDL(K) = KSI(K)*COEF*(MDIL+Q(K)/P(K))
        ELSEIF (KK .EQ. 4) THEN
          DLEVDL(K) = UN
          
Caf 15/05/07 Debut

        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
          
          CALL HUJPRJ(KK-4,SIGF,SIGD,COEF0,MUL)
          PS = 2*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3)
         
          IF((-Q(K)/PREF).GT.TOLE)THEN
            DLEVDL(K) = KSI(K)*COEF*(MDIL+PS/(2.D0*Q(K)*P(K)))
          ELSE
            DLEVDL(K) = KSI(K)*COEF*MDIL
          ENDIF
          
        ELSEIF (KK .EQ. 8) THEN
          IF(VIND(22).EQ.UN)THEN
            DLEVDL(K) = - UN
          ELSE  
            DLEVDL(K) = UN
          ENDIF
Caf 15/05/07 Fin
          
        ENDIF
 161    CONTINUE
C        WRITE(6,'(4(1X,E12.5))')(DLEVDL(J),J=1,NBMECA)
C =====================================================================
C --- XIII. CALCUL DE DLFDS (NBMECX6) ---------------------------------
C =====================================================================
      DO 171 K = 1, NBMECA
        KK = INDI(K)
        CALL HUJDDD('DFDS  ', KK, MATER, INDI, YF, VIND,
     &              DFDS, DPSIDS, IRET)
        IF (IRET.EQ.1) GOTO 1000
        DO 171 I = 1, NDT
          DLFDS(K,I) = DFDS(I) /CCOND
 171      CONTINUE
      
C       WRITE(6,'(A)')'DLFDS ='
C       DO 178 K = 1, NBMECA
C         WRITE(6,'(6(1X,E12.5))')(DLFDS(K,I),I=1,6)
C 178   CONTINUE
 
C =====================================================================
C --- XIV. CALCUL DE DLFDR (NBMECXNBMEC) ------------------------------
C =====================================================================
      DO 180 K = 1, NBMECA
        DO 180 L = 1, NBMECA
          DLFDR(K,L) = ZERO
 180      CONTINUE
 
      DO 181 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLFDR(K,K) = M*P(K)*( UN-B*LOG(P(K)/PC) ) /CCOND
        ELSEIF (KK .EQ. 4) THEN
          DLFDR(K,K) = D*PC /CCOND
        
Caf 15/05/07 Debut      
        
        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
          
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)
              
          PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)*D12
     
          DLFDR(K,K) = M*P(K)*( UN-B*LOG(P(K)/PC) ) /CCOND
     &                 *(UN+PROD/Q(K))          
        ELSEIF (KK .EQ. 8) THEN
          
          DLFDR(K,K) = D*PC /CCOND
          
Caf 15/05/07 Fin          
          
        ENDIF
181     CONTINUE
C       WRITE(6,'(A)')'DLFDR ='
C       DO 172 K = 1, NBMECA
C         WRITE(6,'(4(1X,E12.5))')(DLFDR(K,L),L=1,NBMECA)
C 172   CONTINUE
C =====================================================================
C --- XV. CALCUL DE DLFDEVP (NBMECX1) ---------------------------------
C =====================================================================
       DO 190 K = 1, NBMECA
         KK = INDI(K)
         IF (KK .LT. 4) THEN
           DLFDLE(K) = -M*B*P(K)*RC(K)*BETA /CCOND
         ELSEIF (KK .EQ. 4) THEN
           DLFDLE(K) = -RC(K)*D*PC*BETA /CCOND
           
Caf 15/05/07 Debut  
         ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
           
           XH(1) = VIND(4*KK-11)
           XH(2) = VIND(4*KK-10)
           TH(1) = VIND(4*KK-9)
           TH(2) = VIND(4*KK-8)
           PROD  = SIGDC(3*K-2)*(XH(1)-RC(K)*TH(1)) + 
     &             (SIGDC(3*K)*(XH(2)-RC(K)*TH(2)))*D12 
           
           DLFDLE(K) = M*B*P(K)*(PROD/Q(K)-RC(K))*BETA /CCOND
        
         ELSEIF (KK .EQ. 8) THEN
           IF (VIND(22).EQ.UN) THEN
             DLFDLE(K) = -D*PC*BETA*(RC(K)-VIND(21))/CCOND
           ELSE  
             DLFDLE(K) = -D*PC*BETA*(VIND(21)+RC(K)) /CCOND
           ENDIF
  
Caf 15/05/07 Fin           
           
         ENDIF
 190     CONTINUE
C        WRITE(6,'(4(1X,E12.5))')(DLFDLE(J),J=1,NBMECA)
 
C =====================================================================
C --- XVI. CALCUL DE DLFDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
       DO 200 K = 1, NBMECA
         DO 200 L = 1, NBMECA
           DLFDLA(K,L) = ZERO
 200       CONTINUE
 
C =====================================================================
C --- XVII. CALCUL DE LE (6x6) ---------------------------------------
C =====================================================================
C ---- XVII.1. CALCUL DE CDE = C*DEPSE
C                        6X1
C REMARQUE: ON A DEJA DEPSE CALCULE AU I.1.
       CALL LCPRMV (HOOKNL, DEPSE, CDE)
       
       DO 210 I = 1, NDT
         LE(I) = YF(I) - YD(I) - CDE(I)
 210     CONTINUE

C       WRITE(6,'(6(1X,E12.5))')(LE(J),J=1,6)
       
C =====================================================================
C --- XVIII. CALCUL DE LEVP (1X1) -------------------------------------
C =====================================================================
        LEVP = YF(NDT+1) - YD(NDT+1)
        DO 220 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*(MDIL+Q(K)/P(K))
          ELSEIF (KK .EQ. 4) THEN
            LEVP = LEVP + DLAMBD(K)
            
Caf 15/05/07 Debut      
        
          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            CALL HUJPRJ(KK-4,SIGF,SIGD,COEF0,MUL)
            PS = 2*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3)
            
            IF((-Q(K)/PREF).GT.TOLE)THEN
              LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*
     &                     (MDIL+PS/(2.D0*Q(K)*P(K)))
            ELSE
              LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*MDIL
            ENDIF
            
          ELSEIF (KK .EQ. 8) THEN
            IF(VIND(22).GT.ZERO)THEN
              LEVP = LEVP - DLAMBD(K) 
            ELSE
              LEVP = LEVP + DLAMBD(K)
            ENDIF   
Caf 15/05/07 Fin  
                    
          ENDIF
 220      CONTINUE
C        WRITE(6,*)'LEVP =',LEVP
C =====================================================================
C --- XIX. CALCUL DE LR (NBMECX1) -------------------------------------
C =====================================================================
        DO 230 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/CMON*(UN-RC(K))**DEUX
     
Caf 15/05/07 Debut      
        
          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 8) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/CCYC*(UN-RC(K))**DEUX
Caf 15/05/07 Fin       
     
          ENDIF
 230      CONTINUE
C          WRITE(6,'(4(1X,E12.5))')(LR(J),J=1,NBMECA)
 
C =====================================================================
C --- XX. CALCUL DE LF (NBMECX1) --------------------------------------
C =====================================================================
        DO 240 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LF(K) = Q(K) + M*P(K)*RC(K)*( UN-B*LOG(P(K)/PC) )
          ELSEIF (KK .EQ. 4) THEN
            LF(K) = ABS(I1F) + RC(K)*D*PC
            
Caf 15/05/07 Debut      
        
          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            LF(K) = Q(K) + M*P(K)*RC(K)*( UN-B*LOG(P(K)/PC) )
          ELSEIF (KK .EQ. 8) THEN
            LF(K) = ABS(P(K)) + RC(K)*D*PC

Caf 15/05/07 Fin
            
          ENDIF
 240      CONTINUE
          
C         WRITE(6,'(4(1X,E12.5))')(LF(J),J=1,NBMECA)
 
C =====================================================================
C --- ASSEMBLAGE DE R : -----------------------------------------------
C =====================================================================
C     R    = -( LE       , LEVP       , LR       , LF       )
C =====================================================================
C --- ASSEMBLAGE DE DRDY
C =====================================================================
C     DRDY =  ( DLEDS    , DLEDEVP    , DLEDR    , DLEDLA   )
C             ( DLEVPDS  , DLEVPDEVP  , DLEVPDR  , DLEVPDLA )
C             ( DLRDS    , DLRDEVP    , DLRDR    , DLRDLA   )
C             ( DLFDS    , DLFDEVP    , DFLFDR   , DFLFDLA  )
C =====================================================================
C --- ASSEMBLAGE DE R -------------------------------------------------
C =====================================================================
        DO 850 I = 1, NDT
           R(I)  = -LE(I) /CCOND
 850       CONTINUE
        R(NDT+1) = -LEVP
        DO 950 K = 1, NBMECA
           R(NDT+1+K)        = -LR(K)
           R(NDT+1+NBMECA+K) = -LF(K) /CCOND
 950       CONTINUE
 
C =====================================================================
C --- ASSEMBLAGE DE DRDY ----------------------------------------------
C =====================================================================
C DLEDDY
        CALL LCICMA (DLEDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DLEDEV,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA (DLEDR,6,4,NDT,NBMECA,1,1,DRDY,NMOD,NMOD,1,NDT+2)
        CALL LCICMA (DLEDLA,6,4,NDT,NBMECA,1,1,DRDY,NMOD,NMOD,1,
     &               NDT+2+NBMECA)
C DLEVPDDY        
        CALL LCICMA (DLEVDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1,NDT+1) = DLEVDE
        CALL LCICMA (DLEVDR,1,4,1,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+1,NDT+2)
        CALL LCICMA (DLEVDL,1,4,1,NBMECA,1,1,DRDY,NMOD,NMOD,NDT+1,
     &               NDT+2+NBMECA)
C DLRDDY        
        CALL LCICMA (DLRDS,4,6,NBMECA,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
        CALL LCICMA (DLRDLE,4,1,NBMECA,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+1)
        CALL LCICMA (DLRDR,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+2)
        CALL LCICMA (DLRDLA,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,NDT+2,
     &               NDT+2+NBMECA)
C DLFDDY
        CALL LCICMA (DLFDS,4,6,NBMECA,NDT,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,1)
        CALL LCICMA (DLFDLE,4,1,NBMECA,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+1)
        CALL LCICMA (DLFDR,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2)
        CALL LCICMA (DLFDLA,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2+NBMECA)
     
        GOTO 1000

 999    CONTINUE
        IF (DEBUG) WRITE (IFM,'(A)') 'HUJJID :: LOG(PK/PC) NON DEFINI'
        IRET=1
 1000   CONTINUE
C       WRITE(6,'(A)')'DRDY ='
C       DO 1233 I=1,9
C         WRITE(6,'(9(1X,E12.5))')(DRDY(I,J),J=1,9)
C 1233  CONTINUE
C        WRITE(6,'(9(1X,E12.5))')(R(J),J=1,9)
C =====================================================================
C        CALL JEDEMA ()
C =====================================================================
        END
