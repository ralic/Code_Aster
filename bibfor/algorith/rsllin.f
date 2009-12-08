        SUBROUTINE RSLLIN ( MOD ,  NMAT,  MATERD, MATERF, MATCST,
     1                      NVI,   DEPS,  SIGD,   VIND,   SIGF,   VINF,
     2                      THETA )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/12/2001   AUTEUR T2BAXJM R.MASSON 
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
C       INTEGRATION ELASTIQUE LINEAIRE ISOTROPE SUR DT POUR
C         LE MODELE DE ROUSSELIER (UTILISATION DES CONTRAINTES
C         EFFECTIVES)
C       IN  MOD    :  MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           MATCST :  'OUI' SI MATERIAU CONSTANT SUR DT
C           NVI    :  NB VARIABLES INTERNES
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           VIND   :  VARIABLES INTERNES A T
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C       ----------------------------------------------------------------
        INTEGER         NMAT , NVI
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          SIGD(6) ,       SIGF(6)
        REAL*8          VIND(*) ,       VINF(*)
        REAL*8          DKOOH(6,6), HOOKF(6,6), IDENT(6,6)
        REAL*8          DSIG(6), DEPS(6), THDE(6)
        REAL*8          RHO, F, F0, UN, THETA
C
        PARAMETER       ( UN     = 1.D0   )
C
        CHARACTER*8     MOD
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
C
C --    CALCUL DE RHO
C
        F   = VIND(2)
        F0  = MATERF(3,2)
        RHO = (UN-F)/(UN-F0)
C
        IF (MATCST(1:3).EQ.'OUI') THEN
C
C --INTEGRATION ELASTIQUE : SIGF = SIGD+ RHO HOOKF DEPS
          CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
          CALL LCPRSV ( THETA, DEPS , THDE  )
          CALL LCPRMV ( HOOKF, THDE , SIGF )
          CALL LCPRSV ( RHO  , SIGF , SIGF )
          CALL LCSOVE ( SIGD , SIGF , SIGF )
        ELSE
C                                                  -1
C --DEFORMATION ELASTIQUE A T ET T+DT : EPSEF = HOOKD/RHO  SIGD + DEPS
C --INTEGRATION ELASTIQUE :              SIGF = RHO*HOOKF EPSEF
          CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
          CALL LCOPIL ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
          CALL LCPRMM ( DKOOH, HOOKF, IDENT )
          CALL LCPRMV ( IDENT, SIGD , SIGF  )
          CALL LCPRSV ( THETA, DEPS , THDE  )
          CALL LCPRMV ( HOOKF, THDE , DSIG )
          CALL LCPRSV ( RHO  , DSIG , DSIG )
          CALL LCSOVE ( SIGF , DSIG , SIGF  )
        ENDIF
C
C --VINF  = VIND ,  ETAT A T+DT : VINF(NVI) = 0 = ELASTIQUE
        CALL LCEQVN  ( NVI-1, VIND , VINF )
        VINF (NVI) = 0.D0
C
        END
