        SUBROUTINE LCJPLC ( LOI  , MOD , IMAT,  NMAT, MATER, NVI,
     1                      TEMP,  TIME, DEPS,  EPSD, SIG ,  VIN, DSDE )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/01/98   AUTEUR JMBHH01 J.M.PROIX 
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
C       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTO-PLASTIQUE OU
C       VISCO-PLASTIQUE COHERENT A T+DT OU T
C       COHERENT A T+DT OU T
C       IN  LOI    :  MODELE DE COMPORTEMENT
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           NVI    :  NB VARIABLES INTERNES
C           TEMP   :  TEMPERATURE
C           TIME   :  INSTANT T
C           DEPS   :  INCREMENT DE DEFORMATION
C           EPSD   :  DEFORMATION A T
C           SIG    :  CONTRAINTE
C           VIN    :  VARIABLES INTERNES
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         IMAT, NMAT , NVI, NMOD
        REAL*8          DSDE(6,6)
        REAL*8          SIG(6)   , EPSD(6) , DEPS(6)
        REAL*8          VIN(*)
        REAL*8          TIME, TEMP
        REAL*8          MATER(NMAT,2)
        CHARACTER*3     INST
        CHARACTER*8     MOD
        CHARACTER*16    LOI
C       ----------------------------------------------------------------
         IF     ( LOI(1:9) .EQ. 'VISCOCHAB' ) THEN
            CALL  CVMJPL (MOD,IMAT,NMAT,MATER,DSDE)
         ELSEIF ( LOI(1:5) .EQ. 'LMARC'     ) THEN
            CALL  LMAJPL (MOD,IMAT,NMAT,MATER,DSDE)
         ENDIF
C
         END
