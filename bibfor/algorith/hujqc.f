        SUBROUTINE HUJQC (K, TIN, VIN, MATER, P, Q)
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
C  ------------------------------------------------------
C  LOI DE HUJEUX: PROJECTION DANS LE PLAN DEVIATEUR K 
C  POUR UN MECANISME CYCLIQUE
C  IN  K        :  MECANISME K = 5 A 7
C      TIN      :  TENSEUR DES CONTRAINTES
C      VIN      :  VARIABLES INTERNES ASSOCIEES
C      MATER    :  COEFFICIENTS MATERIAU A T+DT
C
C  OUT 
C      P     :  PRESSION ISOTROPE K
C      Q     :  NORME DEVIATEUR CYCLIQUE K
C  ------------------------------------------------------
        INTEGER   NDT, NDI, I, J, K
        
        REAL*8    D12, DD, DEUX, VIN(*)
        REAL*8    RK, XK(2), TH(2)
        REAL*8    TIN(6), TOU(3), P, Q
        REAL*8    EPSVP, BETA, B, PHI, PCREF, PCR
        REAL*8    M, UN, DEGR, MATER(22,2) 
        PARAMETER     ( DEGR  = 0.0174532925199D0 )
        
        COMMON /TDIM/ NDT  , NDI

        DATA   D12, DEUX, UN /0.5D0, 2.D0, 1.D0/
        
C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
        
        EPSVP = VIN(23)
        RK    = VIN(K)
        XK(1) = VIN(4*K-11)
        XK(2) = VIN(4*K-10)
        TH(1) = VIN(4*K-9)
        TH(2) = VIN(4*K-8)

C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================
        
        BETA  = MATER(2, 2)
        B     = MATER(4, 2)
        PHI   = MATER(5, 2)
        PCREF = MATER(7, 2)
        PCR   = PCREF*EXP(-BETA*EPSVP)
        M     = SIN(DEGR*PHI)
        
C ==================================================================
C ----------------- CONSTRUCTION DU DEVIATEUR DES CONTRAINTES ------
C ==================================================================
        J = 1
        DO 10 I = 1, NDI
          IF (I .NE. K-4) THEN
            TOU(J) = TIN(I)
            J = J+1
          ENDIF
  10     CONTINUE

        TOU(3)  = TIN(NDT+5-K)

        DD      = D12*( TOU(1)-TOU(2) )
                
C ==================================================================
C ----------------- CONSTRUCTION DU DEVIATEUR CYCLIQUE -------------
C ==================================================================

        P = D12*( TOU(1)+TOU(2) )

        TOU(1)=DD
        TOU(2)=-DD
        
        TOU(1) = TOU(1)-(XK(1)-RK*TH(1))*P*(UN-B*LOG(P/PCR))*M
        TOU(2) = - TOU(1)        
        TOU(3) = TOU(3)-(XK(2)-RK*TH(2))*P*(UN-B*LOG(P/PCR))*M
        
        Q = TOU(1)**DEUX + (TOU(3)**DEUX)/DEUX
        Q = SQRT(Q)
        
        END
