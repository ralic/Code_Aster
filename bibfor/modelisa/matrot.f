        SUBROUTINE MATROT ( ANGL , PGL )
      IMPLICIT NONE
        REAL*8              ANGL(*) , PGL(3,3)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       CALCUL DE LA MATRICE ROTATION A PARTIR DES ANGLES NAUTIQUES
C
C       LES ANGLES NAUTIQUES SONT DEFINIS COMME ETANT LES ROTATIONS
C       QU IL FAUT EFFECTUER AUTOUR DE ZO , Y1 , X POUR PASSER DU
C       REPERE INITIAL (X0,Y0,Z0) AU REPERE FINAL (X,Y,Z) :
C       (X0,Y0,Z0)     >    (X1,Y1,Z0)    >    (X,Y1,Z2)    >    (X,Y,Z)
C                    APLHA              BETA              GAMMA
C
C       IN      ANGL(1) = ROTATION SENS DIRECT AUTOUR DE ZO
C               ANGL(2) = ROTATION SENS ANTI-DIRECT AUTOUR DE Y1
C               ANGL(3) = ROTATION SENS DIRECT AUTOUR DE X
C
C       OUT     PGL   = MATRICE PASSAGE REPERE GLOBAL > FINAL
C       ----------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL*8 COSA ,COSB ,COSG ,SINA ,SINB ,SING 
C-----------------------------------------------------------------------
        COSA = COS( ANGL(1) )
        SINA = SIN( ANGL(1) )
        COSB = COS( ANGL(2) )
        SINB = SIN( ANGL(2) )
        COSG = COS( ANGL(3) )
        SING = SIN( ANGL(3) )
C
        PGL(1,1) = COSB*COSA
        PGL(2,1) = SING*SINB*COSA - COSG*SINA
        PGL(3,1) = SING*SINA + COSG*SINB*COSA
        PGL(1,2) = COSB*SINA
        PGL(2,2) = COSG*COSA + SING*SINB*SINA
        PGL(3,2) = COSG*SINB*SINA - COSA*SING
        PGL(1,3) = -SINB
        PGL(2,3) = SING*COSB
        PGL(3,3) = COSG*COSB
C
        END
