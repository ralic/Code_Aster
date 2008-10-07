        SUBROUTINE HUJDRC (K,MATER,SIG,VIN,PSM,PST,DIST,IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/2008   AUTEUR DEVESA G.DEVESA 
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
C   -------------------------------------------------------------------
C   CALCUL DE PRODUIT SCALAIRE ENTRE LA NORME DES MECANISMES CYCLIQUES 
C   DEVIATOIRES ET DES VECTEURS DE RAYONS ISSUS VARIABLES D'HISTOIRE
C   ET LA POSITION ACTUELLE DANS LE PLAN DEVIATOIRE K.
C
C   IN  K      :  PLAN DE PROJECTION (K = 1 A 3)  
C       MATER  :  COEFFICIENTS MATERIAU A T+DT
C       VIN    :  VARIABLES INTERNES  A T
C       SIG    :  CONTRAINTE A T+DT
C
C   OUT PSM    : PRODUIT SCALAIRE ENTRE LA NORME DU SEUIL ET LE VECTEUR
C                DEFINI PAR LE RAYON MOBILISE ET LE CENTRE DU SEUIL
C       PST    : PRODUIT SCALAIRE ENTRE LA NORME DE LA SURFACE ET DR
C       SEUIL  : SEUIL DE LA SURFACE DE CHARGE ANTERIEURE 
C   -------------------------------------------------------------------
        INTEGER NDT, NDI, I, K, IRET
        REAL*8  MATER(22,2), SIG(6), VIN(*)
        REAL*8  B, PCO, BETA, PC, EPSVPM, PTRAC
        REAL*8  UN, ZERO, AEXP, EXPTOL, R8MAEM
        REAL*8  P, Q, M, PHI, DEGR, SIGD(3), PSM, REFM(2)
        REAL*8  POSF(3), REF(2), NORM(2), PST, TOLE
        REAL*8  DIST, RMEM
       
        PARAMETER     ( DEGR  = 0.0174532925199D0 )
       
        COMMON /TDIM/   NDT , NDI

        DATA      UN, ZERO, TOLE  /1.D0, 0.D0, 1.D-6/

        B      = MATER(4,2)
        PCO    = MATER(7,2)
        BETA   = MATER(2,2)
        EPSVPM = VIN(23)
        PHI    = MATER(5,2)
        M      = SIN(DEGR*PHI)
        PTRAC  = MATER(21,2)
        
        EXPTOL = LOG(1.D+20)
        EXPTOL = MIN(EXPTOL, 40.D0)
        AEXP   = -BETA*EPSVPM

        IF (AEXP .GE. EXPTOL) WRITE(6,'(A)') 'HUJDRC:: PB!!'  
              
        PC     = PCO*EXP(-BETA*EPSVPM)
        
        CALL HUJPRJ(K,SIG,SIGD,P,Q)
        
        P = P -PTRAC
        
        DO 5 I = 1, 3
          IF(Q.GT.TOLE)THEN
            POSF(I) = SIGD(I)/(M*P*(UN-B*LOG(P/PC)))
          ELSE
            POSF(I) = ZERO
          ENDIF
  5     CONTINUE        
        NORM(1) = VIN(4*K+7)
        NORM(2) = VIN(4*K+8)
        REF(1)  = VIN(4*K+5)
        REF(2)  = VIN(4*K+6)
        REFM(1) = VIN(4*K+5) - VIN(K+4)*VIN(4*K+7)
        REFM(2) = VIN(4*K+6) - VIN(K+4)*VIN(4*K+8)

        PST = 2.D0*NORM(1)*(POSF(1)-REF(1))+NORM(2)*(POSF(3)-REF(2))
        PSM = 2.D0*NORM(1)*(POSF(1)-REFM(1))+NORM(2)*(POSF(3)-REFM(2))

        Q  = SQRT(SIGD(1)**2 + (SIGD(3)**2)/2.D0)
        
C --- ON TESTE SI LA SURFACE ANTERIEURE N'EST PAS LA SURFACE MONOTONE
       
        RMEM = REF(1)**2+(REF(2)**2)/2.D0
        IF(RMEM.GT.ZERO)THEN
          RMEM = SQRT(RMEM)
        ELSE
          IRET = 1
          GOTO 100
        ENDIF
        
        DIST = SQRT((POSF(1)-REF(1))**2+((POSF(3)-REF(2))**2)/2)
        DIST = DIST/VIN(K+4)

 100    CONTINUE
        END
