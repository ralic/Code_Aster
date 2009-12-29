        SUBROUTINE HUJCRD (K, MATER, SIG ,VIN, SEUILD)
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
C    ---------------------------------------------------------------
C    HUJEUX:  SEUIL DU MECANISME DEVIATOIRE K(=1 A 3)
C             FD(K) = QII(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
C    ---------------------------------------------------------------
C    IN  K      : PLAN DE PROJECTION (K = 1 A 3)
C        SIG    :  CONTRAINTE
C        VIN    :  VARIABLES INTERNES = ( Q, R, X )
C    OUT SEUILD :  SEUIL DU MECANISME DEVIATOIRE K
C    ---------------------------------------------------------------
        INTEGER      K, NDT, NDI
        INTEGER      IFM, NIV
        REAL*8       MATER(22,2), SIG(6), VIN(*), SEUILD
        REAL*8       UN, R, EPSVP, PCR, PA, TOLE
        REAL*8       DEGR, BETA, B, M, PHI, PCREF, PTRAC
        REAL*8       SIGD(3), P, Q
        LOGICAL      DEBUG
        PARAMETER    (UN = 1.D0)
        PARAMETER    (TOLE = 1.D-7)
        PARAMETER    (DEGR = 0.0174532925199D0)
        
C       ------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG

        CALL INFNIV (IFM, NIV)
        
C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
        EPSVP = VIN(23)
        R     = VIN(K)
        
C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================
        BETA  = MATER(2, 2)
        B     = MATER(4, 2)
        PHI   = MATER(5, 2)
        PCREF = MATER(7, 2)
        PA    = MATER(8, 2)
        PCR   = PCREF*EXP(-BETA*EPSVP)
        PTRAC = MATER(21,2)
        M     = SIN(DEGR*PHI)
        
        
C ==================================================================
C --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
C ==================================================================
        CALL HUJPRJ (K, SIG, SIGD, P, Q)
        
        P =P -PTRAC

        IF ((P/PA) .LE. TOLE) THEN        
           IF (DEBUG) WRITE (IFM,'(A)')
     &                'HUJCRD :: LOG(P/PA) NON DEFINI'
           SEUILD =-1.D0    
           GOTO 999
        ENDIF

C        IF(K.EQ.1)THEN
C         WRITE(6,*)'QK =',QK,' --- FR =',RK*(UN-B*LOG(PK/PCR))*M*PK
C        ENDIF
C ==================================================================
C --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE K ------------------
C ==================================================================
        SEUILD = -Q /M/P - R*(UN-B*LOG(P/PCR))

 999   CONTINUE
       END
