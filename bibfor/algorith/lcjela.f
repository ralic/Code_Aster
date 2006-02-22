        SUBROUTINE LCJELA ( LOI  , MOD ,  NMAT, MATER,  VIN, DSDE )
        IMPLICIT   NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
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
C       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTIQUE A T+DT OU T
C       IN  LOI    :  MODELE DE COMPORTEMENT
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           VIN    :  VARIABLES INTERNES
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT
C       ----------------------------------------------------------------
        INTEGER         NMAT
        REAL*8          DSDE(6,6)
        REAL*8          VIN(*)
        REAL*8          MATER(NMAT,2)
        CHARACTER*8     MOD
        CHARACTER*16    LOI
C       ----------------------------------------------------------------
      IF     ( LOI(1:8) .EQ. 'ROUSS_PR'.OR.
     1         LOI(1:10) .EQ. 'ROUSS_VISC'     ) THEN
         CALL RSLPLI ( 'ISOTROPE' , MOD , MATER , DSDE , NMAT, VIN )
C
      ELSEIF ( LOI(1:8)  .EQ. 'CHABOCHE'    .OR.
     1         LOI(1:5)  .EQ. 'LMARC'       .OR.
     1         LOI(1:9)  .EQ. 'VISCOCHAB'   .OR.
     1         LOI(1:4)  .EQ. 'OHNO'        .OR.
     1         LOI(1:7)  .EQ. 'NADAI_B'     .OR.
     1         LOI(1:10)  .EQ. 'HOEK_BROWN' .OR.
     1         LOI(1:14)  .EQ. 'HOEK_BROWN_EFF' .OR.
     1         LOI(1:7)  .EQ. 'IRRAD3M'     .OR.
     1         LOI(1:6)  .EQ. 'LAIGLE'           ) THEN
         CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , DSDE )
         
      ELSEIF ( LOI(1:8)  .EQ. 'MONOCRIS' ) THEN

         IF (MATER(NMAT,1).EQ.0) THEN
             CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , DSDE )
         ELSEIF (MATER(NMAT,1).EQ.1) THEN
             CALL LCOPLI ( 'ORTHOTRO' , MOD , MATER(1,1) , DSDE )
         ENDIF
             
             
      ENDIF
C
      END
