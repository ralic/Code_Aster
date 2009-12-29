        SUBROUTINE HUJTID (MOD, IMAT, SIGR, VIN, DSDE, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/12/2009   AUTEUR KHAM M.KHAM 
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
C ---------------------------------------------------------------------
C CALCUL DE LA MATRICE TANGENTE DU PROBLEME CONTINU DE LA LOI DE HUJEUX
C POUR LE MECANISME PLASTIQUE DEVIATOIRE
C IN   MOD     :  MODELISATION
C      IMAT    :  ADRESSE DU MATERIAU CODE
C      SIG     :  CONTRAINTES
C      VIN     :  VARIABLES INTERNES
C OUT  DSDE    :  MATRICE TANGENTE
C ======================================================================
        INTEGER   NDT, NDI, I, J, K, KK, L, LL, NVI
        INTEGER   NBMECA, IND(7), IRET, IMAT, NBMECT
        REAL*8    N, BETA, DHUJ, M, PCO, PREF, PC
        REAL*8    PHI, ANGDIL, MDIL, DEGR, BHUJ
        REAL*8    RC(7), YD(18), DPSIDS(6,6), P(7), Q(7) 
        REAL*8    MATER(22,2), VIN(*), SIG(6), DSDE(6,6)
        REAL*8    HOOK(6,6), I1, E, NU, AL, DEMU
        REAL*8    COEF, ZERO, D13, UN, DEUX
        REAL*8    EPSV, TRACE, DFDEVP, EVL, TP, TP1, TEMPF
        REAL*8    PSI(42), DFDS(42), B1(7,7), B2(7,7), B(7,7)
        REAL*8    D(7,6), TE(6,6), SIGD(21), B3(7), LA
        REAL*8    ACYC, AMON, CMON, KSI(7), AD(7), X4, CCYC
        REAL*8    TOLE1, DET, XK(2), TH(2), PROD, PS, DEV(3)
        REAL*8    SIGR(6), PTRAC, PISO, PK, DPSI, PT, QT
        REAL*8    E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32,DELTA
        CHARACTER*8 MOD

        COMMON /TDIM/ NDT, NDI
C ======================================================================
        PARAMETER   ( TOLE1 = 1.D-7 )
        PARAMETER   ( ZERO = 0.D0 )
        PARAMETER   ( D13  = 0.333333333334D0 )
        PARAMETER   ( UN   = 1.D0 )
        PARAMETER   ( DEUX = 2.D0 )
        PARAMETER   ( DEGR = 0.0174532925199D0 )
C ======================================================================
        TEMPF = 0.D0
        CALL HUJMAT (MOD, IMAT, TEMPF, MATER, NDT, NDI, NVI)
        DO 5 I = 1, NDT
          SIG(I) = SIGR(I)
  5       CONTINUE        

        IF (NDT.LT.6) THEN
          SIG(5)=ZERO
          SIG(6)=ZERO
          NDT  = 6
        ENDIF        

        DO 6 I = 1, 7
          IND(I) = 0
   6    CONTINUE


C ======================================================================
C - RECUPERATION DES GRANDEURS UTILES : I1, VARIABLES INTERNES R ET X, -
C ======================================================================
        N      = MATER(1,2)
        BETA   = MATER(2,2)
        DHUJ   = MATER(3,2)
        BHUJ   = MATER(4,2)
        PHI    = MATER(5,2)
        ANGDIL = MATER(6,2)
        PCO    = MATER(7,2)
        PREF   = MATER(8,2)
        ACYC   = MATER(9,2)
        AMON   = MATER(10,2)
        CCYC   = DEUX*MATER(11,2)
        CMON   = MATER(12,2)     
        M      = SIN(DEGR*PHI)
        MDIL   = SIN(DEGR*ANGDIL)
        COEF   = MATER(20,2)
        PTRAC  = MATER(21,2)
C        PISO   = 1.5D0*MATER(21,2)
        PISO   = ZERO


C =====================================================================
C --- CALCUL DE LA TRACE DE SIG ---------------------------------------
C =====================================================================
        IRET = 0
        I1   = D13*TRACE(NDI,SIG)

  
C ---> INITIALISATION DE NBMECA, IND ET YD PAR VIN
        DO 11 K = 1, 6
          PSI(K)     = ZERO
          PSI(6+K)   = ZERO
          DFDS(12+K) = ZERO
          DFDS(18+K) = ZERO 
          DFDS(24+K) = ZERO 
          DFDS(30+K) = ZERO 
          DFDS(36+K) = ZERO 
  11      CONTINUE        

        DO 9 I = 1, 21
          SIGD(K)    = ZERO
   9      CONTINUE
        
        DO 12 K = 1, 7
          RC(K)  = ZERO
          P(K)   = ZERO
          Q(K)   = ZERO
          AD(K)  = ZERO
          KSI(K) = ZERO
  12      CONTINUE
        
        DO 13 K = 1, 18
          YD(K) = ZERO
  13      CONTINUE        
        
C --- MODIFICATION A APPORTER POUR MECANISMES CYCLIQUES
        YD(NDT+1) = VIN(23)
        NBMECA = 0      
        DO 16 K = 1, 8          
          IF (VIN(23+K) .EQ. UN) THEN
          
            NBMECA           = NBMECA+1
            YD(NDT+1+NBMECA) = VIN(K)       
            RC(NBMECA) = VIN(K)
            
            IF (K .LT. 4) THEN
              CALL HUJPRJ (K, SIG, SIGD(NBMECA*3-2), P(NBMECA), 
     &                     Q(NBMECA))    
              IF (((P(NBMECA) -PTRAC)/PREF) .LE. TOLE1) THEN
                IRET = 1
                GOTO 999
              ENDIF
              CALL HUJKSI('KSI   ',MATER,RC(NBMECA),KSI(NBMECA),IRET)
              IF(IRET.EQ.1) GOTO 999
              AD(NBMECA) = ACYC+KSI(NBMECA)*(AMON-ACYC)
            ENDIF
            
            IF ((K .GT.4) .AND. (K .LT. 8)) THEN            
              CALL HUJPRC(NBMECA, K-4, SIG, VIN, MATER, YD,
     &                      P(NBMECA), Q(NBMECA), SIGD(NBMECA*3-2))
              IF (((P(NBMECA) -PTRAC)/PREF) .LE. TOLE1) THEN
                IRET = 1
                GOTO 999
              ENDIF
              CALL HUJKSI('KSI   ',MATER,RC(NBMECA),KSI(NBMECA),IRET)
              IF(IRET.EQ.1) GOTO 999
              AD(NBMECA) = DEUX*(ACYC+KSI(NBMECA)*(AMON-ACYC))
            ENDIF
            
            IF (K .EQ. 8) THEN
              CALL HUJPIC(NBMECA, K, SIG, VIN, MATER, YD, P(NBMECA))
            ENDIF
            
            IND(NBMECA) = K
          ENDIF
  16    CONTINUE
         
        NBMECT = NBMECA
        DO 8 I = 1, 3
          CALL HUJPRJ(I,SIG,DEV,PT,QT)
          IF (ABS((PT+2*50.D0-PTRAC)/PREF).LT.TOLE1) THEN
            NBMECT = NBMECT + 1
            IND(NBMECT) = 8+I
          ENDIF
   8    CONTINUE

        CALL LCEQVN (NDT, SIG, YD)

        DO 17 K = 1, NBMECA
          CALL HUJDDD('DFDS  ', IND(K), MATER, IND, YD,
     &         VIN, DFDS((K-1)*NDT+1), DPSIDS, IRET)     
          IF (IRET.EQ.1) GOTO 999
          CALL HUJDDD('PSI   ', IND(K), MATER, IND, YD,
     &         VIN, PSI((K-1)*NDT+1), DPSIDS, IRET)
          IF (IRET.EQ.1) GOTO 999
 17       CONTINUE
        PC = PCO*EXP(-BETA*YD(NDT+1))
        CMON = CMON * PC/PREF
        CCYC = CCYC * PC/PREF

C =====================================================================
C --- OPERATEUR DE RIGIDITE CALCULE A ITERATION ----------------------
C =====================================================================
        CALL LCINMA (ZERO, HOOK)
        
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' .OR.
     &    MOD(1:4) .EQ. 'AXIS')  THEN
     
        IF (MATER(17,1).EQ.UN) THEN
      
          E    = MATER(1,1)*((I1 -PISO)/PREF)**N
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 30 I = 1, NDI
            DO 30 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
 30           CONTINUE
          DO 35 I = NDI+1, NDT
            HOOK(I,I) = DEMU
 35         CONTINUE
 
        ELSEIF (MATER(17,1).EQ.DEUX) THEN
      
          E1   = MATER(1,1)*((I1 -PISO)/PREF)**N
          E2   = MATER(2,1)*((I1 -PISO)/PREF)**N
          E3   = MATER(3,1)*((I1 -PISO)/PREF)**N
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)*((I1 -PISO)/PREF)**N
          G2   = MATER(8,1)*((I1 -PISO)/PREF)**N
          G3   = MATER(9,1)*((I1 -PISO)/PREF)**N
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DELTA= MATER(16,1)
         
          HOOK(1,1) = (UN - NU23*NU32)*E1/DELTA
          HOOK(1,2) = (NU21 + NU31*NU23)*E1/DELTA
          HOOK(1,3) = (NU31 + NU21*NU32)*E1/DELTA
          HOOK(2,2) = (UN - NU13*NU31)*E2/DELTA
          HOOK(2,3) = (NU32 + NU31*NU12)*E2/DELTA
          HOOK(3,3) = (UN - NU21*NU12)*E3/DELTA
          HOOK(2,1) = HOOK(1,2)
          HOOK(3,1) = HOOK(1,3)
          HOOK(3,2) = HOOK(2,3)
          HOOK(4,4) = G1
          HOOK(5,5) = G2
          HOOK(6,6) = G3
        
        ELSE
          CALL U2MESS('F', 'COMPOR1_34')
        ENDIF

      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
     
        CALL U2MESS('F', 'COMPOR1_4')
     
      ENDIF


C =====================================================================
C --- I. CALCUL DE B(K,L) (NBMECAXNBMECA) -----------------------------
C =====================================================================
C ---> I.1. CALCUL DE B1(K,L) = E(K)*HOOK*PSI(L)
C             (TERME SYMETRIQUE)
       DO 45 K = 1, NBMECT
         DO 45 L = 1, NBMECT
           B1(K,L) = ZERO
 45        CONTINUE
 
       DO 40 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 40 L = 1, NBMECA
           LL = (L-1)*NDT
           DO 40 I = 1, NDT
             DO 40 J = 1, NDT
               B1(K,L) = B1(K,L) - HOOK(I,J)*DFDS(KK+I)*PSI(LL+J)
 40            CONTINUE

C ------------ FIN I.1.
C ---> I.2. CALCUL DE B2(K,L) = DFDEVP(K)*EVL(L)
C           TERME NON SYMETRIQUE             
       DO 41 K = 1, NBMECA
       
         KK = IND(K)
         PK = P(K) -PTRAC
         DFDEVP = ZERO
         
         IF (KK .LT. 4) THEN
         
           DFDEVP = -M*BHUJ*BETA*RC(K)*PK
           
         ELSEIF (KK .EQ. 4) THEN
         
           DFDEVP = -BETA*RC(K)*DHUJ*PC
           
Caf 04/06/07 Debut
         ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
         
           XK(1) = VIN(4*KK-11)
           XK(2) = VIN(4*KK-10) 
           TH(1) = VIN(4*KK-9)
           TH(2) = VIN(4*KK-8)
           PROD  = SIGD(3*K-2)*(XK(1)-RC(K)*TH(1)) + 
     &             SIGD(3*K)*(XK(2)-RC(K)*TH(2))/DEUX  
           IF(Q(K).LT.TOLE1)THEN
             DFDEVP = -M*PK*BETA*BHUJ*RC(K)
           ELSE
             DFDEVP = -M*PK*BETA*BHUJ*(-PROD/Q(K)+RC(K))
           ENDIF

         ELSEIF (KK .EQ. 8) THEN
         
           X4 = VIN(21)
           IF(VIN(22).EQ.UN)THEN
             DFDEVP = -BETA*PC*DHUJ*(RC(K)-X4) 
           ELSE
             DFDEVP = -BETA*PC*DHUJ*(X4+RC(K)) 
           ENDIF
         
         ENDIF
         
         DO 41 L = 1, NBMECA
         
           LL = IND(L)
           EVL = ZERO
           
           IF (LL .LT. 4) THEN
           
Ckh --- traction
             IF ((P(L)/PREF).GT.TOLE1) THEN
                DPSI = MDIL+Q(L)/P(L)
              ELSE
                DPSI = MDIL+1.D+6*Q(L)/PREF
             ENDIF
             EVL = -KSI(L)*COEF*DPSI
             
           ELSEIF (LL .EQ. 4) THEN
           
             EVL = -UN
             
           ELSEIF ((LL .GT. 4) .AND. (LL .LT. 8)) THEN
           
             CALL HUJPRJ(LL-4, SIG, DEV, TP, TP1)
             PS = 2*SIGD(3*L-2)*DEV(1)+SIGD(3*L)*DEV(3)
Ckh --- traction
             IF ((P(L)/PREF).GT.TOLE1) THEN
               IF ((-Q(L)/PREF).LT.TOLE1) THEN
                 DPSI = MDIL
               ELSE
                 DPSI = MDIL+PS/(2.D0*P(L)*Q(L))
               ENDIF
             ELSE
               IF ((-Q(L)/PREF).LT.TOLE1) THEN
                 DPSI = MDIL
               ELSE
                 DPSI =MDIL+PS/(2.D-6*PREF*Q(L))
               ENDIF
             ENDIF
              
             EVL = -KSI(L)*COEF*DPSI
             
           ELSEIF (LL .EQ. 8) THEN
           
             IF (VIN(22).EQ.UN) THEN
               EVL = UN
             ELSE
               EVL = - UN
             ENDIF
             
           ENDIF
           
           B2(K,L) = DFDEVP*EVL
           
 41        CONTINUE
 
C ------------ FIN I.2.
C ---> I.3. CALCUL DE B3(K) = DFDR(K) * [ (1 -RK)**2 /AK ]
C           TERME DIAGONAL             
       DO 43 K = 1, NBMECA
       
         KK    = IND(K)
         PK    = P(K) -PTRAC
         B3(K) = ZERO
         
         IF (KK .LT. 4) THEN
         
           B3(K) = M*PK*(UN-BHUJ*LOG(PK/PC)) * (UN-RC(K))**DEUX /AD(K)
     
         ELSEIF (KK .EQ. 4) THEN
         
           B3(K) = DHUJ*PC * (UN-RC(K))**DEUX /CMON
           
         ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
         
           XK(1) = VIN(4*KK-11)
           XK(2) = VIN(4*KK-10) 
           TH(1) = VIN(4*KK-9)
           TH(2) = VIN(4*KK-8)
           PROD  = SIGD(3*K-2)*TH(1) + SIGD(3*K)*TH(2)/DEUX
           B3(K) = M*PK*(UN-BHUJ*LOG(PK/PC))*DEUX*
     &              (UN-RC(K))**DEUX /AD(K)

         ELSEIF (KK .EQ. 8) THEN
           
           B3(K) = DHUJ*PC*(UN-RC(K))**DEUX /CCYC   
                  
         ENDIF
         
 43      CONTINUE    
C ------------ FIN I.3.

         DO 42 K = 1, NBMECA
           DO 44 L = 1, NBMECA
             B(K,L) =  B1(K,L) + B2(K,L)
 44          CONTINUE
           B(K,K) =  B(K,K) + B3(K)
 42        CONTINUE

C =====================================================================
C --- II. CALCUL DE D(K,I) = E(K)*HOOK (NBMECAXNDT) -----------------
C =====================================================================
       DO 51 K = 1, NBMECA
         DO 51 I = 1, NDT
           D(K,I) = ZERO
 51      CONTINUE
 
       DO 50 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 50 I = 1, NDT
           DO 50 J = 1, NDT
             D(K,I) = D(K,I) - HOOK(J,I)*DFDS(KK+J)
 50          CONTINUE

C =====================================================================
C --- III. CALCUL DE D = B-1*D ----------------------------------------
C =====================================================================
       CALL MGAUSS('NCVP', B, D, 7, NBMECA, NDT, DET, IRET)
       IF (IRET.EQ.1) CALL U2MESS ('F', 'COMPOR1_6')
       
C =====================================================================
C --- IV. CALCUL DE TE = IDEN6 - E*D (6X6) ----------------------------
C =====================================================================
       CALL LCINMA (ZERO, TE)
       DO 61 I = 1, NDT
         TE(I,I) = UN
 61      CONTINUE

       DO 60 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 60 I = 1, NDT
           DO 60 J = 1, NDT
             TE(I,J) = TE(I,J) - PSI(KK+I)*D(K,J)
 60          CONTINUE


C =====================================================================
C --- V. CALCUL DE LA MATRICE TANGENTE EXPLICITE DSDE(I,J,K,L) = ------
C =====================================================================
C
C    HOOK(I,J,K,L) - HOOK(I,J,P,Q)*TE(P,Q,K,L)
C
C =====================================================================
      DO 820 J = 1, NDT
        DO 820 I = 1, NDT
          DSDE(I,J) = ZERO  
 820      CONTINUE  
      CALL LCPRMM (HOOK, TE, DSDE)
      
      GOTO 1000
      
C =====================================================================
C        CALL JEDEMA ()
C =====================================================================
 999  CONTINUE
      CALL U2MESS ('A', 'COMPOR1_14')
       
 1000 CONTINUE
       
      END
