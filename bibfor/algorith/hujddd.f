         SUBROUTINE HUJDDD (CARAC, K, MATER, IND, YF, VIN,
     &                      VEC, MAT, IRET)
         IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/02/2008   AUTEUR KHAM M.KHAM 
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
C             = 'PSI'
C             = 'DPSIDS' DERIVE DE LA LOI D'ECOULEMENT (K=1,2,3)
C                        PAR RAPPORT A SIGMA
C   K      :  NUMERO DU MECANISME (1 A 8)
C   MATER  :  PARAMETRES MATERIAU
C   IND    :  TABLEAU DE CORRESPONDANCE
C             NUMERO D'ORDRE / NUMERO DE MECANISME
C   YF     :  VECTEUR DES INCONNUES
C   VIN    :  VARIABLES INTERNES A T
C  OUT
C   VEC    :  VECTEUR SOLUTION PSI OU DFDS
C   MAT    :  MATRICE SOLUTION DPSIDS
C   IRET   :  CODE RETOUR
C                  = 0   OK
C                  = 1   NOOK
C =====================================================================
C      CHARACTER*8        ZK8
C      CHARACTER*16                ZK16
C      CHARACTER*24                          ZK24
C      CHARACTER*32                                    ZK32
C      CHARACTER*80                                              ZK80
C      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
        
        
      INTEGER       NDT, NDI, I, J, K, MOD, IK, KK
      INTEGER       IND(4), NBMECA, IRET, IADZI, IAZK24
      INTEGER       IFM, NIV
      REAL*8        MATER(22,2)
      PARAMETER     (MOD = 15)
      REAL*8        BETA, M, PCO, PC, PREF, ALPHA
      REAL*8        EPSVPD, TRACE, B, PHI,ANGDIL
      REAL*8        DD, P, Q, SI, SI1, SIGKIJ
      REAL*8        YF(MOD), R, SIGF(6), SIGD(6)
      REAL*8        VEC(6), MAT(6,6), KSI
      REAL*8        D12, D13, UN, ZERO, DEUX
      REAL*8        TOLE, DEGR, AEXP, EXPTOL, R8MAEM
      REAL*8        XK(2), TH(2), SIGDC(6), QC
      REAL*8        VIN(*), VH, D14, D40
      REAL*8        DSDDS(6,6), SXS(6,6), SXP(6,6), PXP(6,6)
      REAL*8        VP(6), PROD, SCXP(6,6), PS, PXH(6,6)
      REAL*8        VHIST(6), SXH, SCXH, FAC
      CHARACTER*6   CARAC
      CHARACTER*8   NOMAIL
      LOGICAL       CONSOL, TRACT, DEBUG
C =====================================================================
      PARAMETER     ( D12   = 0.5D0  )
      PARAMETER     ( D14   = 0.25D0 )
      PARAMETER     ( D13   = 0.3333333333334D0 )
      PARAMETER     ( UN    = 1.D0   )
      PARAMETER     ( ZERO  = 0.D0   )
      PARAMETER     ( DEUX  = 2.D0   )
      PARAMETER     ( TOLE  = 1.D-6  )
      PARAMETER     ( DEGR  = 0.0174532925199D0 )
      PARAMETER     ( D40   = 40.0D0 )
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
        ALPHA   = MATER(20,2)*D12
C =====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
C =====================================================================
        NBMECA = 0
        DO 4 I = 1, 4
          IF ( IND(I) .GT. 0 ) NBMECA = NBMECA+1
 4        CONTINUE
 
        DO 5 I = 1, NBMECA
          IF (IND(I) .EQ. K) R = YF(NDT+1+I)
 5        CONTINUE
 
        EPSVPD = YF(NDT+1)
        
        EXPTOL = LOG(R8MAEM())
        EXPTOL = MIN(EXPTOL, D40)
        AEXP   = -BETA*EPSVPD
        
        IF (AEXP .GE. EXPTOL) THEN
          IRET = 1
          GOTO 999
        ENDIF
        PC = PCO*EXP(-BETA*EPSVPD)
        IF(ABS(PC/PREF).LT.TOLE)THEN
          IRET = 1
          GOTO 999
        ENDIF

        DO 25 I = 1, NDT
          SIGF(I) = YF(I)
 25       CONTINUE


C ---> PROJECTION DES CONTRAINTES DANS LE PLAN DEVIATEUR K
C        CALCUL DU DEVIATIEUR SIGDK (6X1), PK ET QK
C        CALL HUJPROJ ( K, SIGF, SIGK, SIGDK, PK, QK )

Caf 09/05/07 Debut
C        IF (K .EQ. 4) GOTO 100
        IF (K .EQ. 4 .OR. K .EQ. 8) GOTO 100
Caf 09/05/07 Fin
        

        DO 27 I = 1, 6
          SIGD(I) = ZERO
 27       CONTINUE
  
        DD = ZERO
        P  = ZERO
        SI = UN
        DO 10 I = 1, NDI
          IF (K .LT. 4) THEN
            IF (I .NE. K) THEN
              P  = P + SIGF(I)
              DD = DD + SIGF(I)*SI
              SI = -SI
            ENDIF
          ELSE
            IF (I .NE. (K-4)) THEN
              P  = P + SIGF(I)
              DD = DD + SIGF(I)*SI
              SI = -SI
            ENDIF  
          ENDIF
  10      CONTINUE
        DD = D12*DD
        P  = D12*P
        
        SI = UN
        DO 11 I = 1, NDI
          IF (K .LT. 4) THEN
            IF (I .NE. K) THEN
              SIGD(I) = DD*SI
              SI      = -SI
            ENDIF
          ELSE
            IF (I .NE. (K-4)) THEN
              SIGD(I) = DD*SI
              SI      = -SI
            ENDIF
          ENDIF
  11      CONTINUE
        IF (K .LT. 4) THEN
          SIGKIJ          = SIGF( NDT+1-K )
          SIGD( NDT+1-K ) = SIGKIJ
        ELSE
          SIGKIJ          = SIGF( NDT+5-K )
          SIGD( NDT+5-K ) = SIGKIJ      
        ENDIF  
Caf 14/05/07 Debut
         
        IF (K .LT. 4) THEN
          Q = DD**DEUX + (SIGKIJ**DEUX)/DEUX
          Q = SQRT(Q)
          CONSOL = (-Q/PREF) .LE. TOLE
        ENDIF
 
        TRACT  = (P/PREF) .LE. TOLE
Caf 14/05/07 Fin
Caf 09/05/07 Debut
C ====================================================================
C --- CALCUL DE SIGDC ET QC POUR MECANISME DEVIATOIRE CYCLIQUE -------
C ====================================================================
        IF (TRACT) THEN
          GOTO 100
        ENDIF  
        IF ((K .GT. 4) .AND. (K .LT. 8)) THEN
          DO 29 I=1,NDT
            SIGDC(I)=ZERO
  29      CONTINUE
          
          XK(1) = VIN(4*K-11)
          XK(2) = VIN(4*K-10)
          TH(1) = VIN(4*K-9)
          TH(2) = VIN(4*K-8)
          SI=UN
          DO 30 I=1,3     
            IF (I .NE. (K-4)) THEN
              SIGDC(I) = SIGD(I)-(XK(1)-R*TH(1))*P*SI*
     &                  (UN-B*LOG(P/PC))*M
              SI = -SI
            ENDIF
  30      CONTINUE
          SIGDC(NDT+5-K)=SIGD(NDT+5-K)-(XK(2)-R*TH(2))*P*
     &                     (UN-B*LOG(P/PC))*M        
          QC = SQRT(D12*(SIGDC(1)**2+SIGDC(2)**2+
     &    SIGDC(3)**2+SIGDC(4)**2+SIGDC(5)**2+SIGDC(6)**2))
          CONSOL = (-QC/PREF) . LE. TOLE
        ENDIF
 100    CONTINUE        
C ====================================================================
C --- CALCUL DE PCK POUR MECANISME SPHERIQUE CYCLIQUE ----------------
C ====================================================================
        
        IF (K .EQ. 8) THEN  
          
          EXPTOL  = LOG(R8MAEM())
          EXPTOL  = MIN(EXPTOL, D40)
          AEXP    = -BETA*EPSVPD
          
          IF (AEXP .GE. EXPTOL) CALL U2MESS('F', 'COMPOR1_7')
          
          P       = (SIGF(1)+SIGF(2)+SIGF(3))*D13
        ENDIF
Caf 09/05/07 Fin
        
C =====================================================================
C --- CARAC = 'DPSIDS' :                                        -------
C --- CALCUL DE DPSIDS (6X6) POUR LE MECANISME DEVIATOIRE K (<4) ------
C =====================================================================
C ON NE CALCULE PAS POUR LE CAS ISOTROPE (K=4) CAR DPSIDS = [ 0 ]
      IF (CARAC(1:6) .EQ. 'DPSIDS') THEN
        IF (K .EQ. 4) CALL U2MESS('F', 'COMPOR1_2')
        
        CALL LCINMA(ZERO,MAT)
        IF (CONSOL) GOTO 600
        
Caf 15/05/07 Debut
        CALL LCINMA(ZERO,DSDDS)
        CALL LCINMA(ZERO,SXS)
        CALL LCINMA(ZERO,SXP)
        CALL LCINMA(ZERO,PXP)
        CALL LCINMA(ZERO,PXH)
        
        IF (K .GT. 4) THEN
          KK = K-4
        ELSE
          KK = K
        ENDIF
        
        DO 62 I = 1, NDT
          VP(I) = ZERO
          VHIST(I) = ZERO
  62    CONTINUE
         
        DO 63 I = 1, NDI
          IF (I .NE. (KK)) THEN
            VP(I) = UN
          ENDIF  
  63    CONTINUE
        
        DO 64 I = 1, NDI
          DO 64 J = 1, NDI
            IF ((I .NE. (KK)) .AND. 
     &                 (J .NE. (KK))) THEN
              IF (I .EQ. J) THEN 
                DSDDS(I,J) = D12
              ELSE
                DSDDS(I,J) = -D12
              ENDIF
            ENDIF
  64    CONTINUE
        DSDDS(NDT+1-KK,NDT+1-KK) = UN


        IF (K .LT. 4) THEN
          CALL HUJKSI('KSI   ', MATER, R, KSI, IRET)
          IF (IRET .EQ. 1) GOTO 999
  
          
          DO 65 I = 1, NDT
            DO 65 J = 1, NDT
              SXS(I,J) = SIGD(I)*SIGD(J)
              SXP(I,J) = SIGD(I)*VP(J)    
              PXP(I,J) = VP(I)*VP(J)
  65      CONTINUE  
          
          DO 66 I = 1, NDT
            DO 66 J = 1, NDT
              MAT(I,J) = D12*(DSDDS(I,J)/Q - D12*
     &                   SXS(I,J)/Q**3 - ALPHA*KSI*(SXP(I,J)/
     &                   (Q*P)-PXP(I,J)*Q/P**2))
  66      CONTINUE  
        
        
        ELSEIF ((K .GT. 4) .AND. (K .LT. 8)) THEN
C ---> MECANISME CYCLIQUE DEVIATOIRE

          CALL HUJKSI('KSI   ', MATER, R, KSI, IRET)
          IF (IRET .EQ. 1) GOTO 999
        
          SI = UN
          DO 670 I = 1, NDI
            IF(I.NE.(K-4))THEN
              VHIST(I) = SI*(XK(1)-TH(1)*R)
              SI = -SI
            ENDIF  
 670      CONTINUE
          VHIST(NDT+5-K) = XK(2)-TH(2)*R
          
          DO 67 I = 1, NDT
            DO 67 J = 1, NDT
              SXS(I,J)  = SIGDC(I)*SIGDC(J)           
              SXP(I,J)  = SIGD(I)*VP(J)
              SCXP(I,J) = SIGDC(I)*VP(J)
              PXP(I,J)  = VP(I)*VP(J)
              PXH(I,J)  = VHIST(I)*VP(J)
  67      CONTINUE   
          
          
C         WRITE(6,'(A)')'SXP ='
C         DO 927 I = 1, NDT
C           WRITE(6,'(6(1x,E16.9))')(SXP(I,J),J=1,6)  
C 927     CONTINUE
          PROD = ZERO
          PS   = ZERO
          SXH  = ZERO
          SCXH = ZERO
          DO 671 I = 1, NDT
            PS  = PS + SIGD(I)*SIGDC(I)
            SXH = SXH + SIGD(I)*VHIST(I) 
            SCXH = SCXH + SIGDC(I)*VHIST(I)
 671      CONTINUE
          IF(.NOT.CONSOL)THEN
            PROD = PS/(2.D0*QC**2)
          ELSE
            PROD = ZERO
          ENDIF
          FAC = D12*M*(UN-B*(UN+LOG(P/PC)))
          DO 68 I = 1, NDT
            DO 68 J = 1, NDT
              IF(.NOT.CONSOL)THEN
                MAT(I,J) = D12/QC*(DSDDS(I,J)-FAC*PXH(I,J))-
     &                    D14/QC**3*(SXS(I,J)-FAC*SCXH*SCXP(I,J))-
     &                    ALPHA*D12*KSI/(P*QC)*(SCXP(I,J)+SXP(I,J)-
     &                    FAC*SXH*PXP(I,J)-PS*D12/QC**2*(SCXP(I,J)-
     &                    FAC*SCXH*PXP(I,J))-PS*D12/P*PXP(I,J))


C               D14*(DSDDS(I,J)/QC -
C     &                   SXS(I,J)/QC**3 - 2.*ALPHA*KSI*((SXP(I,J)+
CC     &                   SCXP(I,J)*(UN-PROD))/(QC*P)
C     &                   -PXP(I,J)*PS/(2.*QC*P**2)))
              ELSE
                MAT(I,J) = ZERO
              
              ENDIF
  68      CONTINUE  
        
        ENDIF
 600    CONTINUE
Caf 15/05/07 Fin

C =====================================================================
C --- CARAC = 'DFDS' :                                     ---------
C --- CALCUL DE DFDS (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
C =====================================================================
      ELSEIF (CARAC(1:4) .EQ. 'DFDS') THEN
        
        DO 70 I = 1, NDT
          VEC(I)   = ZERO
          VHIST(I) = ZERO
 70     CONTINUE
 
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
 71       CONTINUE
 
          IF (.NOT.CONSOL) THEN
            DO 72 I = NDI+1, NDT
              VEC(I) = D12*SIGD(I)/Q
 72         CONTINUE
          ENDIF
          
        ELSEIF (K .EQ. 4) THEN
        
          DO 73 I = 1, NDI
            VEC(I) = -D13
 73       CONTINUE
 
Caf 09/05/07 Debut
       
        ELSEIF ((K .GT. 4) .AND. (K .LT. 8)) THEN
C --- MECANISMES DEVIATOIRES CYCLIQUES
          IF (TRACT) THEN
            IF (DEBUG) THEN
              NOMAIL='#A FAIRE#'
              WRITE (IFM,'(A)')
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',NOMAIL

            ENDIF           
            IRET = 1
            GOTO 999
          ENDIF   

          SI = UN
          DO 730 I = 1, NDI
            IF(I.NE.(K-4))THEN
              VHIST(I) = SI*(XK(1)-TH(1)*R)
              SI = -SI
            ENDIF  
 730      CONTINUE
 
          VHIST(NDT+5-K) = XK(2)-TH(2)*R
          SCXH = ZERO
          DO 731 I = 1, NDT
            SCXH = SCXH + SIGDC(I)*VHIST(I)
 731      CONTINUE  
 
          FAC = D12*M*(UN-B*(1+LOG(P/PC)))
          
          DO 74 I = 1, NDI
            IF (I . NE. (K-4)) THEN
               VEC(I) = FAC*(R-SCXH*D12/QC)
               IF( .NOT. CONSOL ) VEC(I) = VEC(I)+D12*SIGDC(I)/QC
            ENDIF
  74      CONTINUE
          IF ( .NOT. CONSOL ) VEC(NDT+5-K)= D12*SIGDC(NDT+5-K)/QC
        ELSEIF (K .EQ. 8) THEN
          DO 77 I = 1, NDI
            IF(VIN(22).EQ.UN)THEN 
              VEC(I)=-D13*ABS(P)/P
            ELSE
              VEC(I)=D13*ABS(P)/P
            ENDIF 
  77      CONTINUE
 
Caf 09/05/07 Fin 
 
        ENDIF
        
C =====================================================================
C --- CARAC = 'PSI' :                                     ---------
C --- CALCUL DE PSI (6X1) POUR LE MECANISME DEVIATOIRE K  ---------
C =====================================================================
      ELSEIF (CARAC(1:3) .EQ. 'PSI') THEN
        DO 80 I = 1, NDT
          VEC(I) = ZERO
 80     CONTINUE
 
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
              VEC(I) = -ALPHA*KSI*(SIN(DEGR*ANGDIL) + Q/P)
              IF (.NOT.CONSOL) VEC(I) = VEC(I) + SIGD(I) /Q/2.D0
            ENDIF
 81       CONTINUE
 
          IF (.NOT.CONSOL) VEC(NDT+1-K) = SIGD(NDT+1-K) /Q/2.D0
         
        ELSEIF (K .EQ. 4) THEN
        
          DO 82 I = 1, NDI
            VEC(I) = -D13
 82       CONTINUE

Caf 09/05/07 Debut
        ELSEIF (K .EQ. 8) THEN
        
          DO 83 I = 1, NDI
            IF(VIN(22).EQ.UN)THEN
              VEC(I) = -D13*ABS(P)/P 
            ELSE
              VEC(I) = D13*ABS(P)/P
            ENDIF  
  83      CONTINUE      
          
        ELSEIF ((K .GT. 4) .AND. (K .LT. 8)) THEN
C --- MECANISME DEVIATOIRE CYCLIQUE
C --- PREVOIR TEST POUR TRACTION ET DIVISION PAR ZERO
          IF (TRACT) THEN
            IF (DEBUG) THEN
              NOMAIL='#A FAIRE#'
              WRITE (IFM,'(10(A))')
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ',NOMAIL  
            ENDIF
              WRITE (IFM,*)
     &        'HUJDDD :: LOG(PK/PC) NON DEFINI DANS LA MAILLE ' 
              WRITE(6,*)'P =',P
              WRITE(6,'(A,6(1X,E16.9))')'SIGF =',(SIGF(I),I=1,6)
            IRET = 1
            GOTO 999
          ENDIF
          CALL HUJKSI ('KSI   ', MATER, R, KSI, IRET)
          PROD = ZERO
          DO 85 I = 1, NDT        
            PROD = PROD + SIGD(I)*SIGDC(I)  
  85      CONTINUE
          IF(.NOT.CONSOL)THEN
            PROD = PROD / (2.D0*QC)
          ELSE
            PROD = ZERO
          ENDIF
            
          DO 84 I =  1, NDI
          
            IF (I.NE.(K-4)) THEN
              IF (.NOT.CONSOL) THEN
                VEC(I) = -ALPHA*KSI*(SIN(DEGR*ANGDIL) + PROD/P)
     &                   + SIGDC(I)/QC/2.D0               
              ELSE
                VEC(I) = -ALPHA*KSI*SIN(DEGR*ANGDIL)
              ENDIF
            ENDIF
            
  84      CONTINUE        
          IF (.NOT.CONSOL) 
     &           VEC(NDT+5-K) = SIGDC(NDT+5-K)/QC/2.D0
Caf 09/05/07 Fin
C         WRITE(6,'(3(A,E16.9))')'PROD =',PROD,' --- SIGDC(1)=',SIGDC(1)
C     &                           ,' --- SIGD(1) =',SIGD(1)       
        ENDIF
                
      ENDIF
 999  CONTINUE
      END
