        SUBROUTINE LCELIN ( MOD ,  NMAT,  MATERD, MATERF,
     1                      NVI,   DEPS,  SIGD,   VIND,   SIGF,   VINF )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C       ----------------------------------------------------------------
C       INTEGRATION ELASTIQUE LINEAIRE ISOTROPE SUR DT
C       IN  MOD    :  MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           VIND   :  VARIABLES INTERNES A T
C           SIGD   :  CONTRAINTE  A T
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C       ----------------------------------------------------------------
        INTEGER         NMAT , NVI
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          SIGD(6) ,       SIGF(6)
        REAL*8          VIND(*) ,       VINF(*)
        REAL*8          DKOOH(6,6),     HOOKF(6,6)
        REAL*8          EPSED(6),       EPSEF(6) ,   DEPS(6)
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
C
C --    OPERATEUR ELASTIQUE LINEAIRE ISOTROPE
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
C
C                                                        -1
C --    DEFORMATION ELASTIQUE A T ET T+DT : EPSEF = HOOKD  SIGD + DEPS
C
        CALL LCPRMV ( DKOOH, SIGD  , EPSED )
        CALL LCSOVE ( EPSED, DEPS  , EPSEF )
C
C --    CONTRAINTES PLANES
C       DEPS3 = - ( H31 EPSEF1 + H32 EPSEF2 + H34 EPSEF4 )/H33 - EPSED3
C
          IF(MOD(1:6).EQ.'C_PLAN')THEN
          DEPS(3) =  - ( HOOKF(3,1) * EPSEF(1) +
     1                   HOOKF(3,2) * EPSEF(2) +
     2                   HOOKF(3,4) * EPSEF(4) ) / HOOKF(3,3) - EPSED(3)
          CALL LCSOVE ( EPSED, DEPS  , EPSEF )
          ENDIF
C
C --    INTEGRATION ELASTIQUE : SIGF = HOOKF EPSEF (EPSEF MODIFIE EN CP)
C
        CALL LCPRMV ( HOOKF, EPSEF , SIGF  )
C
C --    VINF  = VIND ,  ETAT A T+DT = VINF(NVI) = 0 = ELASTIQUE
C
        CALL LCEQVN  ( NVI-1, VIND , VINF )
        VINF (NVI) = 0.D0
C
        END
