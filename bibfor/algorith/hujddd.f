      SUBROUTINE HUJDDD (CARAC, K, MATER, IND, YF,
     &                   VEC, MAT, IRET)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D WWW.CODE-ASTER.ORG
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
C ---------------------------------------------------------------------
C CALCUL DE DIFFERENTES DERIVEES POUR LE MECANISME K
C ---------------------------------------------------------------------
C  IN
C   CARAC  :  = 'DFDS'   DERIVE PREMIERE DU SEUIL F PAR RAPPORT A SIGMA
C   	      = 'PSI'
C   	      = 'DPSIDS' DERIVE DE LA LOI D'ECOULEMENT (K=1,2,3)
C                        PAR RAPPORT A SIGMA
C   K	     :  NUMERO DU MECANISME
C   MATER  :  PARAMETRES MATERIAU
C   IND    :  TABLEAU DE CORRESPONDANCE
C             NUMERO D'ORDRE / NUMERO DE MECANISME
C   YF     :  VECTEUR DES INCONNUES
C  OUT
C   VEC    :  VECTEUR SOLUTION PSI OU DFDS
C   MAT    :  MATRICE SOLUTION DPSIDS
C   IRET   :  CODE RETOUR
C   		   = 0   OK
C   		   = 1   NOOK
C =====================================================================
C   CHARACTER*8        ZK8
C   CHARACTER*16                ZK16
C   CHARACTER*24                          ZK24
C   CHARACTER*32                                    ZK32
C   CHARACTER*80                                              ZK80
C   COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      
      INTEGER       NDT, NDI, I, J, K, MOD, IK
      INTEGER       IND(4), NBMECA, IRET, IADZI, IAZK24
      INTEGER       IFM, NIV
      REAL*8        MATER(20,2), COEF
      PARAMETER     (MOD = 15)
      REAL*8        BETA, M, PCO, PC, PREF
      REAL*8        EPSVPD, TRACE, B, PHI,ANGDIL
      REAL*8        DD, P, Q, SI, SI1, SIGKIJ
      REAL*8        YF(MOD), R, SIGF(6), SIGD(6)
      REAL*8        VEC(6), MAT(6,6), KSI
      REAL*8        D12, D13, UN, ZERO, DEUX, TROIS
      REAL*8        PSI(3), TOLE, DEGR, AEXP, EXPTOL, R8MAEM
      CHARACTER*6   CARAC
      CHARACTER*8   NOMAIL
      LOGICAL       CONSOL, TRACT, DEBUG
C =====================================================================
C      PARAMETER     ( DSQR  = 1.41421356237D0  )
      PARAMETER     ( D12   = 0.5D0  )
      PARAMETER     ( D13   = 0.3333333333334D0 )
      PARAMETER     ( UN    = 1.D0   )
      PARAMETER     ( ZERO  = 0.D0   )
      PARAMETER     ( DEUX  = 2.D0   )
      PARAMETER     ( TROIS = 3.D0   )
      PARAMETER     ( TOLE  = 1.D-6  )
      PARAMETER     ( DEGR  = 0.0174532925199D0 )
C =====================================================================
      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG

      CALL INFNIV (IFM, NIV)


C =====================================================================
C --- PROPRIETES HUJEUX MATERIAU --------------------------------------
C =====================================================================
      BETA    = MATER(2,2)
      B       = MATER(4,2)
      PHI     = MATER(5,2)
      ANGDIL  = MATER(6,2)
      PCO     = MATER(7,2)
      PREF    = MATER(8,2)
      M       = SIN(DEGR*PHI)
 

C =====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
C =====================================================================
      NBMECA = 0
      DO 4 I = 1, 4
        IF ( IND(I) .GT. 0 ) NBMECA = NBMECA+1
 4      CONTINUE
 
      DO 5 I = 1, NBMECA
        IF (IND(I) .EQ. K) R = YF(NDT+1+I)
 5      CONTINUE
 
      EPSVPD = YF(NDT+1)
      
      EXPTOL = LOG(R8MAEM())
      EXPTOL = MIN(EXPTOL, 40.D0)
      AEXP   = -BETA*EPSVPD
      
      IF (AEXP .GE. EXPTOL) THEN
        IRET = 1
        GOTO 999
      ENDIF
      
      PC = PCO*EXP(-BETA*EPSVPD)

      DO 25 I = 1, NDI
        SIGF(I) = YF(I)
 25     CONTINUE
      DO 26 I = NDI+1, NDT
C        SIGF(I) = YF(I)/DSQR
        SIGF(I) = YF(I)
 26     CONTINUE


C ---> PROJECTION DES CONTRAINTES DANS LE PLAN DEVIATEUR K
C      CALCUL DU DEVIATIEUR SIGDK (6X1), PK ET QK
C      CALL HUJPROJ ( K, SIGF, SIGK, SIGDK, PK, QK )
      IF (K .EQ. 4) GOTO 100
      
      DO 28 I = 1, 3
        PSI(I) = ZERO
 28     CONTINUE
 
      DO 27 I = 1, 6
        SIGD(I) = ZERO
 27     CONTINUE
  
      DD = ZERO
      P  = ZERO
      SI = UN
      DO 10 I = 1, NDI
        IF (I .NE. K) THEN
          PSI(I) = SI
          P  = P + SIGF(I)
          DD = DD + SIGF(I)*SI
          SI = -SI
        ENDIF
  10    CONTINUE
      DD = D12*DD
      P  = D12*P
      
      SI = UN
      DO 11 I = 1, NDI
        IF (I .NE. K) THEN
          SIGD(I) = DD*SI
          SI      = -SI
        ENDIF
  11    CONTINUE
      SIGKIJ          = SIGF( NDT+1-K )
      SIGD( NDT+1-K ) = SIGKIJ
       
      Q = DD**DEUX + SIGKIJ**DEUX
      Q = SQRT(Q)

      COEF = D12
C      COEF=UN

      CONSOL = (-Q/PREF) .LE. TOLE
      TRACT  = (P/PREF) .LE. TOLE

 100  CONTINUE


C =====================================================================
C --- CARAC = 'DPSIDS' :                                        -------
C --- CALCUL DE DPSIDS (6X6) POUR LE MECANISME DEVIATOIRE K (<4) ------
C =====================================================================
C ON NE CALCULE PAS POUR LE CAS ISOTROPE (K=4) CAR DPSIDS = [ 0 ]
      IF (CARAC(1:6) .EQ. 'DPSIDS') THEN
        IF (K .EQ. 4) CALL U2MESS('F', 'COMPOR1_2')
        
        CALL LCINMA(ZERO,MAT)
        IF (CONSOL) GOTO 600
        
        CALL HUJKSI('KSI   ', MATER, R, KSI, IRET)
        IF (IRET .EQ. 1) GOTO 999

        DO 50 I = 1, NDI
 
          DO 51 J = 1, NDI
            IF ((I.NE.K) .AND. (J.NE.K)) THEN
               MAT(I,J) = COEF *
     &            ( PSI(I)*PSI(J)*D12 /Q
     &            - SIGD(I)*D12*SIGD(J) /Q**TROIS
     &            - D12*KSI*SIGD(J) /P/Q
     &            + D12*KSI*Q /P**DEUX )
            ENDIF
  51        CONTINUE
  
          IF (I .NE. K) MAT(I,NDT+1-K) =
     &                  -COEF*KSI*SIGD(NDT+1-K) /P/Q
  
          DO 50 J = NDI+1, NDT
            MAT(I,J) = MAT(I,J) - 
     &                 COEF*SIGD(I)*SIGD(J) /Q**TROIS
  50        CONTINUE
  
        MAT(NDT+1-K,NDT+1-K) = COEF /Q
       
        DO 60 I = NDI+1, NDT
          DO 61 J = 1, NDI
            MAT(I,J) = MAT(I,J) - 
     &      COEF*SIGD(I)*D12*SIGD(J) /Q**TROIS
  61        CONTINUE
          DO 60 J = NDI+1, NDT
            MAT(I,J) = MAT(I,J) - 
     &      COEF*SIGD(I)*SIGD(J) /Q**TROIS
  60        CONTINUE
  
 600    CONTINUE


C =====================================================================
C --- CARAC = 'DFDS' :                                     ---------
C --- CALCUL DE DFDS (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
C =====================================================================
      ELSEIF (CARAC(1:4) .EQ. 'DFDS') THEN
        
        DO 70 I = 1, NDT
          VEC(I) = ZERO
 70       CONTINUE
 
        IF (K .LT. 4) THEN
       
          IF (TRACT) THEN
            IF (DEBUG) THEN
C              CALL TECAEL(IADZI,IAZK24)
C              NOMAIL = ZK24(IAZK24-1+3) (1:8)
              NOMAIL='#A FAIRE#'
              WRITE (IFM,'(A)')
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',NOMAIL
            ENDIF
            IRET = 1
            GOTO 999
          ENDIF
          
          DO 71 I = 1, NDI
            IF (I.NE.K) THEN
              VEC(I) = D12*M*R*(UN-B*(UN+LOG(P/PC)))
              IF(.NOT.CONSOL) VEC(I) = VEC(I)+D12*SIGD(I) /Q
            ENDIF
 71         CONTINUE
 
          IF (.NOT.CONSOL) THEN
            DO 72 I = NDI+1, NDT
              VEC(I) = SIGD(I)/Q
 72           CONTINUE
          ENDIF
 
        ELSEIF (K .EQ. 4) THEN
        
          DO 73 I = 1, NDI
            VEC(I) = -D13
 73         CONTINUE
 
        ENDIF
        

C =====================================================================
C --- CARAC = 'PSI' :                                     ---------
C --- CALCUL DE PSI (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
C =====================================================================
      ELSEIF (CARAC(1:3) .EQ. 'PSI') THEN
        
        DO 80 I = 1, NDT
          VEC(I) = ZERO
 80       CONTINUE
 
        IF (K .LT. 4) THEN
       
          IF (TRACT) THEN
            IF (DEBUG) THEN
C              CALL TECAEL(IADZI,IAZK24)
C              NOMAIL = ZK24(IAZK24-1+3) (1:8)
              NOMAIL='#A FAIRE#'
              WRITE (IFM,'(10(A))')
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',NOMAIL
            ENDIF
            IRET = 1
            GOTO 999
          ENDIF
          
          CALL HUJKSI ('KSI   ', MATER, R, KSI, IRET)
          IF (IRET .EQ. 1) GOTO 999
          
          DO 81 I = 1, NDI
            IF(I.NE.K) THEN
              VEC(I) = -COEF*KSI*(SIN(DEGR*ANGDIL) + Q/P)
              IF (.NOT.CONSOL) VEC(I) = VEC(I) + COEF*SIGD(I) /Q
            ENDIF
 81         CONTINUE
 
          IF (.NOT.CONSOL) VEC(NDT+1-K) = COEF*SIGD(NDT+1-K) /Q
 
        ELSEIF (K .EQ. 4) THEN
        
          DO 82 I = 1, NDI
            VEC(I) = -D13
 82         CONTINUE
 
        ENDIF
                
      ENDIF
 999  CONTINUE

      END
