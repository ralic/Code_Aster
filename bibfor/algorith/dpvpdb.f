      SUBROUTINE DPVPDB( NBMAT,MATER, CRIT,DT, VINM,VINP,  
     &                   NVI,SEQE, I1E, SEQM, I1M, 
     &                   DP, NBRE, RETCOM)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2009   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      INTEGER       NBMAT, NVI, NBRE, RETCOM
      REAL*8        MATER(NBMAT,2)
      REAL*8        CRIT(3), DT
      REAL*8        VINM(NVI), VINP(NVI)
      REAL*8        SEQE, I1E, SEQM, I1M, DP
C =====================================================================
C --- IN --- : NBMAT   NOMBRE DE PARAMETRES DU MODELE -----------------
C ---------- : MATER   COEFFICIENTS MATERIAU --------------------------
C ---------- : CRIT    TABLEAU DES PARAMETRES DE CONVERGENCE ----------
C ---------- : DT      PAS DE TEMPS -----------------------------------
C ---------- : VINM    VARIABLES INTERNES AU TEMPS MOINS --------------
C ---------- : VINP    VARIABLES INTERNES AU TEMPS PLUS ---------------
C ---------- : NVI     NOMBRE DE VI -----------------------------------
C ---------- : SEQE   CONTRAINTE EQUIVALENTE DE LA PREDICTION ELASTIQUE
C ---------- : I1E    TRACE DE LA PREDICTION ELASTIQUE ----------------
C ---------- : SEQM   CONTRAINTE EQUIVALENTE A l INSTANT MOINS --------
C ---------- : I1M    TRACE DE LA CONTRAINTE A L INSTANT MOINS---------
C ----OUT -- : DP     INCONNUE - DEFORMATION VISCOPLASTIQUE CUMULEE ---
C ---------  : NBRE   NOMBRE D ITERATIONS POUR LA CONVERGENCE LOCALE --
C ---------  : RETCOM  CODE RETOUR 0 OU 1 SI REDECOUPAGE NECESSAIRE  --
C =====================================================================
C =====================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE ------- 
C --- VISC_DRUC_PRAG --------------------------------------------------
C --- RESOLUTION NUMERIQUE DE L EQ NON LINEAIRE AVEC BRACKETING ET ----
C --------------LA METHODE DES CORDES (APPEL A ZEROCO)-----------------
C =====================================================================
      INTEGER  NITER, I
      REAL*8   MU, K
      REAL*8   TROIS, NEUF, ZERO, DIX
      REAL*8   PREF, A, N, CONST
      REAL*8   FONC1, FONC2, FONC3, FONC4
      REAL*8   F, FP, SEUIL, V0, XINF, XSUP, FINF, FSUP
      REAL*8   FONECP(3), FONECM(3), FONDER(3)
      REAL*8   DPVPEQ
      REAL*8   ALPHAM, RM, BETAM
      REAL*8   DALPDP, DRDP, DBETDP 
      REAL*8   X(4), Y(4), R8MIEM, TOL, DIFF
C =====================================================================
      PARAMETER ( TROIS  =  3.0D0 )
      PARAMETER ( NEUF   =  9.0D0 )
      PARAMETER ( DIX    = 10.0D0 )
      PARAMETER ( ZERO   =  0.0D0 )
      PARAMETER ( TOL    = 1.D-12)
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      MU          = MATER(4,1)
      K           = MATER(5,1)
      PREF        = MATER(1,2)
      A           = MATER(2,2)
      N           = MATER(3,2)
C =====================================================================

       CONST = A*DT/(PREF)**N
       RETCOM = 0

C =====================================================================
C --- CALCUL DE DP ----------------------------------------------------
C =====================================================================
       CALL DPVPVA(VINM, NBMAT, MATER, FONECM)
       CALL DPVPVA(VINP, NBMAT, MATER, FONECP)
       CALL DPVPDV(VINP, NBMAT, MATER, FONDER)
       

       ALPHAM = FONECM(1)
       RM     = FONECM(2)
       BETAM  = FONECM(3)

       DALPDP = FONDER(1)
       DRDP   = FONDER(2)
       DBETDP = FONDER(3)

       FONC1 = SEQE + ALPHAM*I1E - RM
C
       FONC2 = TROIS*MU + DRDP  - DALPDP*I1E 
     &         +NEUF*K *ALPHAM*BETAM 
C
       FONC3 = NEUF*K*(ALPHAM*DBETDP+BETAM*DALPDP)
C
       FONC4 = NEUF*K*DALPDP*DBETDP
       
C
       IF (FONC1 .GT. ZERO) THEN
           FONC1 = FONC1
         ELSE
           FONC1 = ZERO
       ENDIF    
C
       XINF = ZERO
       XSUP = A * (FONC1/PREF)**N * DT

       FINF   = DPVPEQ(XINF,N,CONST,FONC1,FONC2,FONC3,FONC4)
 
       FSUP   = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)

        
       NITER  = INT(CRIT(1))

       IF (ABS(FINF) .LE. CRIT(3)) THEN
           DP = XINF
           NBRE = 1
           GOTO 50
       ELSEIF (ABS(FSUP) .LE. CRIT(3)) THEN
           DP = XSUP
           NBRE = 1
           GOTO 50
       ELSEIF (FINF .GT. ZERO) THEN 
           X(2) = XINF
           Y(2) = FINF
       
        IF (FSUP  .LT. ZERO) THEN
          DO 31 I = 1, NITER
           XSUP = XSUP/DIX
           FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)
           
           IF (ABS(FSUP).LE.CRIT(3)) THEN
              DP = XSUP
              NBRE=I
              GOTO 50
           ELSEIF (FSUP.GT.ZERO) THEN
C             ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX 
              XSUP = XSUP*DIX
              FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)
              
              IF (ABS(FSUP).LE.CRIT(3)) THEN
               DP = XSUP
               NBRE=I
               GOTO 50
              ELSE
               X(1) = XSUP
               Y(1) = FSUP
               GOTO 20
              ENDIF
           ENDIF
  31      CONTINUE
           X(1) = XSUP
           Y(1) = FSUP
           GOTO 20
        ELSE
C        FSUP >0. On augmente DPMAX jusqu'à ce que F(DPMAX) < 0
           DO 30 I = 1, NITER
              FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)
              IF (ABS(FSUP).LE.CRIT(3)) THEN
                 DP = XSUP
                 NBRE=I
                 GOTO 50
              ELSEIF (FSUP.LT.ZERO) THEN
                 X(1) = XSUP
                 Y(1) = FSUP
                 GOTO 20
              ELSE
                 XSUP = XSUP*DIX
              ENDIF
  30       CONTINUE
           CALL U2MESS('A','ALGORITH6_79')
           GOTO 20
        ENDIF
        
       ELSEIF (FINF .LT. ZERO) THEN 
        X(1) = XINF
        Y(1) = FINF
       
         IF (FSUP  .GT. ZERO) THEN
          DO 32 I = 1, NITER
           XSUP = XSUP/DIX
           FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)
           IF (ABS(FSUP).LE.CRIT(3)) THEN
              DP = XSUP
              NBRE=I
              GOTO 50
           ELSEIF (FSUP.LT.ZERO) THEN
C             ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX 
              XSUP = XSUP*DIX
              FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)

              IF (ABS(FSUP).LE.CRIT(3)) THEN
              DP = XSUP
              NBRE=I
              GOTO 50
              ELSE
              X(2) = XSUP
              Y(2) = FSUP
              GOTO 20
              ENDIF
           ENDIF
  32      CONTINUE
           X(2) = XSUP
           Y(2) = FSUP
           GOTO 20
         ELSE
C        FSUP <0. On augmente DPMAX jusqu'à ce que F(DPMAX) > 0
           DO 33 I = 1, NITER
              FSUP = DPVPEQ(XSUP,N,CONST,FONC1,FONC2,FONC3,FONC4)
              IF (ABS(FSUP).LE.CRIT(3)) THEN
                 DP = XSUP
                 NBRE=I
                 GOTO 50
              ELSEIF (FSUP.GT.ZERO) THEN
                 X(2) = XSUP
                 Y(2) = FSUP
                 GOTO 20
              ELSE
                 XSUP = XSUP*DIX
              ENDIF
  33       CONTINUE
           CALL U2MESS('A','ALGORITH6_79')
           GOTO 20
        ENDIF

       ENDIF
C
   20 CONTINUE
C
C --- CALCUL DE X(4) SOLUTION DE L'EQUATION F = 0 :
C     ===========================================
      X(3) = X(1)
      Y(3) = Y(1)
      X(4) = X(2)
      Y(4) = Y(2)

      IF (ABS(Y(4)).LT.CRIT(3)) GOTO 50
      
      DO 40 I = 1, NITER
      
        IF (Y(1).GT.ZERO .OR. Y(2).LT.ZERO) THEN
         CALL U2MESS('A','ALGORITH6_78')
         GOTO 41
        ENDIF
        
        IF ((ABS(X(3)).LT.TOL).AND.(ABS(X(4)).LT.TOL)) THEN
         CALL U2MESS('A','ALGORITH9_84')
         DP = ZERO
         NBRE = I
         GOTO 50
        ENDIF
        
        IF (X(3).EQ.X(4)) THEN
         CALL U2MESS('A','ALGORITH9_84')
         GOTO 41
        ENDIF
        
        CALL ZEROCO(X,Y)
        DP = X(4)
        
        IF (ABS(DP).LT.TOL)  THEN
         DP = ZERO
         NBRE = I
         GOTO 50
        ENDIF 
        
        Y(4) = DPVPEQ(DP,N,CONST,FONC1,FONC2,FONC3,FONC4)


        IF ((ABS(Y(4)/FINF)).LT.CRIT(3)) THEN
         NBRE = I
         GOTO 50
        ENDIF 
    
  40  CONTINUE

  41  CONTINUE
       RETCOM = 1
C =====================================================================
 50   CONTINUE
C =====================================================================
      END
