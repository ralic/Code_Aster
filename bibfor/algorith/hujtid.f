        SUBROUTINE HUJTID (MOD, MATER, SIG, VIN, DSDE)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
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
C      MATER   :  COEFFICIENTS MATERIAU
C      SIG     :  CONTRAINTES
C      VIN     :  VARIABLES INTERNES
C OUT  DSDE    :  MATRICE TANGENTE
C ======================================================================
        INTEGER     NDT, NDI, I, J, K, KK, L, LL
        INTEGER     NBMECA, IND(4), IRET
        REAL*8      N, BETA, DHUJ, M, PCO, PREF, PC
        REAL*8      PHI, ANGDIL, MDIL, DEGR, BHUJ
        REAL*8      RC(4), YD(15), DPSIDS(6,6), P(3), Q(3) 
        REAL*8      MATER(20,2), VIN(*), SIG(6), DSDE(6,6)
        REAL*8      HOOK(6,6), I1, E, NU, AL, DEMU
        REAL*8      COEF, ZERO, D13, UN, DEUX
        REAL*8      EPSV, TRACE, DFDEVP, EVL
        REAL*8      PSI(24), DFDS(24), B1(4,4), B2(4,4), B(4,4)
        REAL*8      D(4,6), TE(6,6), SIGD(3), B3(4)
        REAL*8      ACYC, AMON, CMON, KSI(3), AD(3)
        REAL*8      TOLE, DET,BID(6)
        CHARACTER*8 MOD
C ======================================================================
        COMMON      /TDIM/ NDT, NDI
C ======================================================================
        PARAMETER   ( TOLE = 1.D-6 )
        PARAMETER   ( ZERO = 0.D0 )
        PARAMETER   ( D13 = 0.333333333334D0 )
        PARAMETER   ( UN = 1.D0 )
        PARAMETER   ( DEUX = 2.D0 )
        PARAMETER   ( DEGR = 0.0174532925199D0 )
C ======================================================================
C        CALL JEMARQ ()
        

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
        CMON   = MATER(12,2)
        M      = SIN(DEGR*PHI)
        MDIL   = SIN(DEGR*ANGDIL)


C =====================================================================
C --- CALCUL DE LA TRACE DE SIG ---------------------------------------
C =====================================================================
        IRET = 0
        I1   = D13*TRACE(NDI,SIG)

  
C ---> INITIALISATION DE NBMECA, IND ET YD PAR VIN
        NBMECA = 0
        DO 16 K = 1, 4
          IND(K) = 0
          RC(K) = ZERO
          IF (VIN(5+K) .EQ. UN) THEN
            NBMECA           = NBMECA+1
            YD(NDT+1+NBMECA) = VIN(K)
            RC(NBMECA) = VIN(K)
            IF (K .LT. 4) THEN
              CALL HUJPRJ (K, SIG, SIGD, P(NBMECA), Q(NBMECA))
              IF ((P(NBMECA)/PREF) .LE. TOLE) GOTO 999
              CALL HUJKSI('KSI   ',MATER,RC(NBMECA),KSI(NBMECA),IRET)
              IF (IRET .EQ. 1) GOTO 999
              AD(NBMECA) = ACYC+KSI(NBMECA)*(AMON-ACYC)
            ENDIF
            IND(NBMECA) = K
          ENDIF
 16       CONTINUE
 
        CALL LCEQVN (NDT, SIG, YD)
        YD(NDT+1) = VIN(5)
        
        DO 17 K = 1, NBMECA
          CALL HUJDDD('DFDS  ', IND(K), MATER, IND, YD,
     &         DFDS((K-1)*NDT+1), DPSIDS, IRET)
          IF (IRET.EQ.1) GOTO 999
          CALL HUJDDD('PSI   ', IND(K), MATER, IND, YD,
     &         PSI((K-1)*NDT+1), DPSIDS, IRET)
          IF (IRET.EQ.1) GOTO 999
 17       CONTINUE
        
        PC = PCO*EXP(-BETA*YD(NDT+1))
        
        CMON = CMON * PC/PREF
        
        COEF = UN
  
     
C =====================================================================
C --- OPERATEUR DE RIGIDITE CALCULE A ITERARTION ----------------------
C =====================================================================
        E  = MATER(1,1)*(I1/PREF)**N
        NU = MATER(2,1)
        AL = E *(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E        /(UN+NU)
        CALL LCINMA (ZERO, HOOK)
        
        
C =====================================================================
C --- 3D/DP/AX --------------------------------------------------------
C =====================================================================
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS')  THEN
          DO 20 I = 1, NDI
            DO 20 J = 1, NDI
              IF(I.EQ.J) HOOK(I,J) = AL
              IF(I.NE.J) HOOK(I,J) = DEMU
 20           CONTINUE

          DO 30 I = NDI+1, NDT
            HOOK(I,I) = DEMU
 30         CONTINUE
 
 
C =====================================================================
C --- CP/1D -----------------------------------------------------------
C =====================================================================
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' ) THEN
          CALL U2MESS('F','COMPOR1_4')
        ENDIF


C =====================================================================
C --- I. CALCUL DE B(K,L) (NBMECAXNBMECA) -----------------------------
C =====================================================================
C ---> I.1. CALCUL DE B1(K,L) = E(K)*HOOK*PSI(L)
C             (TERME SYMETRIQUE)
       DO 45 K = 1, NBMECA
         DO 45 L = 1, NBMECA
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
         IF (KK .LT. 4) THEN
           DFDEVP = -M*BHUJ*BETA*RC(K)*P(K)
         ELSEIF (KK .EQ. 4) THEN
           DFDEVP = -BETA*RC(K)*DHUJ*PC
         ENDIF
         
         DO 41 L = 1, NBMECA
           LL = IND(L)
           IF (LL .LT. 4) THEN
             EVL = -KSI(L)*COEF*(MDIL+Q(L)/P(L))
           ELSEIF (LL .EQ. 4) THEN
             EVL = -UN
           ENDIF
           
           B2(K,L) = DFDEVP*EVL
           
 41        CONTINUE
 
 
C ------------ FIN I.2.
C ---> I.3. CALCUL DE B3(K) = DFDR(K) * [ (1 -RK)**2 /AK ]
C           TERME DIAGONAL             
       DO 43 K = 1, NBMECA
         KK = IND(K)
         IF (KK .LT. 4) THEN
           B3(K) = M*P(K)*(UN-BHUJ*LOG(P(K)/PC)) *
     &            (UN-RC(K))**DEUX /AD(K)
         ELSEIF (KK .EQ. 4) THEN
           B3(K) = DHUJ*PC * (UN-RC(K))**DEUX /CMON
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
       CALL MGAUSS('NFVP', B, D, 4, NBMECA, NDT, DET, IRET)
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
        CALL LCPRMM (HOOK, TE, DSDE)
        GOTO 1000


C =====================================================================
C        CALL JEDEMA ()
C =====================================================================
 999  CONTINUE
      CALL U2MESS ('F', 'COMPOR1_14')
 1000 CONTINUE
      END
