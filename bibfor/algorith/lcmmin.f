      SUBROUTINE LCMMIN ( TYPESS, ESSAI, MOD, NMAT,
     &                      MATERF, NR, NVI,YD,  DEPS, DY  )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       CHABOCHE : CALCUL SOLUTION ESSAI DY = ( DSIG DX1 DX2 DP (DEPS3))
C                               AVEC     Y  = ( SIG  X1  X2  P  (EPS3))
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T   = ( SIG  VIN  (EPS3)  )
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C           TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE (=-1 INITIALEMENT)
C                               3 = ESSAI
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
C       ----------------------------------------------------------------
C
      INTEGER         NDT , NDI , TYPESS , NMAT,NR,NVI
C
      REAL*8          YD(*)     , DY(*),  ESSAI
      REAL*8          HOOK(6,6) , DFDS(6)
      REAL*8          DEPS(6)
      REAL*8          SIG(6)    , DSIG(6)
C
      REAL*8          MATERF(NMAT,2)
C
      CHARACTER*8     MOD
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
C
C      IF ( TYPESS .EQ. -1 ) TYPESS = 2
      TYPESS=0
C
C
C - SOLUTION INITIALE = NUL
C
      IF ( TYPESS .EQ. 0) THEN
         IF(MOD(1:6).EQ.'C_PLAN')THEN
            DEPS(3) = 0.D0
         ENDIF
C
C - SOLUTION INITIALE = ELASTIQUE
C
      ELSEIF ( TYPESS .EQ. 1 ) THEN
         CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
         CALL LCTRMA (HOOK , HOOK)
         CALL LCPRMV ( HOOK    , DEPS , DSIG  )
         CALL LCEQVN ( NDT     , DSIG , DY(1) )
C
C - SOLUTION INITIALE = EXPLICITE
C
C      ELSEIF ( TYPESS .EQ. 2 ) THEN
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
      ELSEIF ( TYPESS .EQ. 3 ) THEN
        CALL LCINVN ( NR  , ESSAI , DY )
        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
           DEPS(3) = ESSAI
           DY(3)   = 0.D0
        ENDIF
      ENDIF
C
      END
