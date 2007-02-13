        SUBROUTINE HUJCRD (K, MATER, SIG ,VIN, SEUILD)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2007   AUTEUR KHAM M.KHAM 
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
C 	      FD(K) = QII(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
C    ---------------------------------------------------------------
C    IN  SIG	:  CONTRAINTE
C    IN  VIN	:  VARIABLES INTERNES = ( Q, R, X )
C    OUT SEUILD :  SEUIL  ELASTICITE DU MECANISME DEVIATOIRE
C    ---------------------------------------------------------------
        INTEGER      K, NDT, NDI
        REAL*8       MATER(20,2), SIG(6), VIN(*), SEUILD
        REAL*8       UN, RK, EPSVP, PCR, PA, TOLE
        REAL*8       DEGR, BETA, B, M, PHI, PCREF
        REAL*8       SIGD(3), PK, QK
        PARAMETER    (UN = 1.D0)
        PARAMETER    (TOLE = 1.D-6)
        PARAMETER    (DEGR = 0.0174532925199D0)
C       ------------------------------------------------------------
        COMMON /TDIM/ NDT, NDI
        
        
C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
        EPSVP = VIN(5)
        RK    = VIN(K)
    
        
C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================
        BETA  = MATER(2, 2)
        B     = MATER(4, 2)
        PHI   = MATER(5, 2)
        PCREF = MATER(7, 2)
        PA    = MATER(8, 2)
        PCR   = PCREF*EXP(-BETA*EPSVP)
        M     = SIN(DEGR*PHI)
        
        
C ==================================================================
C --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
C ==================================================================
        CALL HUJPRJ (K, SIG, SIGD, PK, QK)
        
        IF ((PK/PA) .LE. TOLE) THEN
C           CALL UTMESS('A','HUJEUX :: HUJCRD','PK DEVIENT TRES PETIT'//
C     &    ' OU POSITIF: LOG(PK/PC) NON DEFINI')
           SEUILD=-1.D0
           GOTO 999
        ENDIF

        
C ==================================================================
C --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE K ------------------
C ==================================================================
        SEUILD = -QK /M/PK - RK*(UN-B*LOG(PK/PCR))

 999   CONTINUE
       END
