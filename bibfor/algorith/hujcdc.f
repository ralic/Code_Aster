        SUBROUTINE HUJCDC (K, MATER, SIG ,VIN, SEUIL)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/04/2008   AUTEUR FOUCAULT A.FOUCAULT 
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
C    HUJEUX:  SEUIL DU MECANISME DEVIATOIRE CYCLIQUE K(=1 A 3)
C             FD(K) = QIIC(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
C             QIIC(K)=QII(TOU(K)+(X(K)-TH*RK)*PK*(1-B*LOG(PK/PC)*M))
C    ---------------------------------------------------------------
C    IN  K      :  PLAN DE PROJECTION K = 1, 2 OU 3
C        MATER  :  COEFFICIENTS MATERIAU 
C        SIG    :  CHAMPS DE CONTRAINTES A T
C        VIN    :  VARIABLES INTERNES A T
C    OUT SEUIL  :  SEUIL DU MECANISME DEVIATOIRE
C   ------------------------------------------------------------------
        INTEGER      K, NDT, NDI
        INTEGER      IFM, NIV
        INTEGER      I,J
        REAL*8       MATER(22,2), SIG(6), VIN(*), SEUIL
        REAL*8       UN, RK, EPSVP, PCR, PA, TOLE
        REAL*8       DEGR, BETA, B, M, PHI, PCREF
        REAL*8       SIGD(3), PK, QK, QXK, XK(2)
        REAL*8       TIN(6), TOU(3), TH(2), TOUC(2)
        REAL*8       D12, DD, DEUX      
        LOGICAL      DEBUG
        PARAMETER    (UN = 1.D0)
        PARAMETER    (TOLE = 1.D-6)
        PARAMETER    (DEGR = 0.0174532925199D0)
C       ------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG

        DATA   D12, DEUX /0.5D0, 2.D0/

        CALL INFNIV (IFM, NIV)
        
C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
        EPSVP = VIN(23)
        RK    = VIN(4+K)
        XK(1) = VIN(5+4*K)
        XK(2) = VIN(6+4*K)      
        TH(1) = VIN(7+4*K)
        TH(2) = VIN(8+4*K)      
                
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
        
        J = 1
        DO 10 I = 1, NDI
          IF (I .NE. K) THEN
            TOU(J) = SIG(I)
            J = J+1
          ENDIF
  10     CONTINUE

        TOU(3) = SIG(NDT+1-K)

        DD     = D12*( TOU(1)-TOU(2) )       
        
C ==================================================================
C --- CALCUL DE PK, QCK ---------------------------------------------
C ==================================================================

        PK      = D12*( TOU(1)+TOU(2) )
        TOU(1)  = DD
        TOU(2)  = -DD
        
        IF ((PK/PA) .LE. TOLE) THEN
           IF (DEBUG) WRITE (IFM,'(A)')
     &                'HUJCDC :: LOG(PK/PA) NON DEFINI'
           SEUIL=-1.D0
           GOTO 999
        ENDIF

        TOUC(2) = TOU(3)-(XK(2)-RK*TH(2))*PK*
     &             (UN-B*LOG(PK/PCR))*M
        TOUC(1) = TOU(1)-(XK(1)-RK*TH(1))*PK*
     &             (UN-B*LOG(PK/PCR))*M
        
        QK  = TOUC(1)**DEUX + (TOUC(2)**DEUX)/DEUX
        QK  = SQRT(QK)
                        
C ==================================================================
C --- CALCUL DU SEUIL DU MECANISME CYCLIQUE DEVIATOIRE K -----------
C ==================================================================
        SEUIL = -QK /M/PK - RK*(UN-B*LOG(PK/PCR))

 999   CONTINUE
       END
