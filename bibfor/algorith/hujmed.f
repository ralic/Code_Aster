        SUBROUTINE HUJMED (K, MATER, VIN, SIG)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/08/2008   AUTEUR KHAM M.KHAM 
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
C   --------------------------------------------------------------------
C   ENREGISTREMENT VARIABLE MEMOIRE POUR MECANISME DEVIATOIRE DU PLAN KP
C   IN  K      :  PLAN DE PROJECTION (1 A 3) OU (5 A 7)
C       MATER  :  COEFFICIENTS MATERIAU 
C       VIN    :  VARIABLES INTERNES A T 
C       SIG    :  CHAMPS DE CONTRAINTE A T
C
C   OUT VIN    :  VARIABLES INTERNES MODIFIEES 
C         (VARIABLES MEMOIRES + COMPOSANTES DE LA NORMALE A LA SURFACE)
C   --------------------------------------------------------------------
        INTEGER       NDT, NDI, K, I, J, KP
        REAL*8        DD, BETA, B, M, PCR, RC 
        REAL*8        SIG(6), PHI, PCREF, PTRAC
        REAL*8        VIN(*), TOU(3), P, D12
        REAL*8        MATER(22,2), DEGR, EPSVP, UN
        REAL*8        XK(2), TH(2), SC(2), DEUX, QSC
        LOGICAL       DEBUG
        
C ----------------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C ----------------------------------------------------------------------
        PARAMETER     (DEGR = 0.0174532925199D0)
        PARAMETER     (D12  = 0.5D0  )
        PARAMETER     (UN   = 1.0D0  )
        PARAMETER     (DEUX = 2.0D0  )


C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
        EPSVP = VIN(23)
        
        IF(K.GT.4)THEN 
          RC = VIN(K)    
        ELSE
          RC = VIN(K+4)
        ENDIF  


C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================
        BETA  = MATER(2, 2)
        B     = MATER(4, 2)
        PHI   = MATER(5, 2)
        PCREF = MATER(7, 2)
        PCR   = PCREF*EXP(-BETA*EPSVP)
        PTRAC = MATER(21,2)
        M     = SIN(DEGR*PHI)


C ==================================================================
C --- ON TRAVAILLE AVEC DES INDICES COMPRIS ENTRE 1 ET 3 -----------
C ==================================================================
        
        IF (K .GT. 4) THEN
          KP = K-4
        ELSE
          KP=K
        ENDIF


C ==================================================================
C --------------- VARIABLES MEMOIRES -------------------------------
C ==================================================================
        XK(1) = VIN(4*KP+5)
        XK(2) = VIN(4*KP+6)
        TH(1) = VIN(4*KP+7)
        TH(2) = VIN(4*KP+8)       


C ==================================================================
C --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
C ==================================================================
        J = 1
        DO 10 I = 1, NDI
          IF (I .NE. KP) THEN
            TOU(J) = SIG(I)
            J = J+1
          ENDIF
  10     CONTINUE
        TOU(3) = SIG(NDT+1-KP)
        
        DD= D12*( TOU(1)-TOU(2) )       
        P = D12*( TOU(1)+TOU(2) )
        P = P -PTRAC


C ==================================================================
C --- MISE A JOUR DES VARIABLES INTERNES DE MEMOIRE ----------------
C ==================================================================

C --- ENREGISTREMENT DE LA SURFACE DE CHARGE CYCLIQUE PRECEDENTE
        VIN(5*KP+31) = VIN(4*KP+5)
        VIN(5*KP+32) = VIN(4*KP+6)
        VIN(5*KP+33) = VIN(4*KP+7)
        VIN(5*KP+34) = VIN(4*KP+8)
        VIN(5*KP+35) = RC

C --- ENREGISTREMENT DU TENSEUR DIRECTION DE MEMOIRE
        SC(1) = DD-M*P*(UN-B*LOG(P/PCR))*(XK(1)-TH(1)*RC)
        SC(2) = TOU(3)-M*P*(UN-B*LOG(P/PCR))*(XK(2)-TH(2)*RC)
        QSC   = SQRT(SC(1)**2+(SC(2)**2)/DEUX)     

        VIN(4*KP+7)=-SC(1)/QSC
        VIN(4*KP+8)=-SC(2)/QSC

C --- ENREGISTREMENT DU TENSEUR DEVIATOIRE MEMOIRE
        VIN(4*KP+5)=DD/(M*P*(UN-B*LOG(P/PCR)))
        VIN(4*KP+6)=TOU(3)/(M*P*(UN-B*LOG(P/PCR)))

        END        
