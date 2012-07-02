        SUBROUTINE ANGVXY ( GX , GN , ANGL )
      IMPLICIT NONE
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
C       CALCUL DES 3 ANGLES NAUTIQUES A PARTIR DU VECTEUR GX
C       ET D UN VECTEUR GN DONT LA PROJECTION NORMALE SUR LE PLAN NORMAL
C       A GX DONNE LE VECTEUR GY
C       IN      GX , GN
C       OUT     ALPHA , BETA , GAMMA
C       ----------------------------------------------------------------
        REAL*8  MRO(3,3), GX(3),  GY(3),  GN(3),  ANGL(*)
C
C-----------------------------------------------------------------------
      REAL*8 ALPHA ,BETA ,R8MIEM ,TST 
C-----------------------------------------------------------------------
        TST = R8MIEM()
C
        CALL ANGVX(GX,ALPHA,BETA)
        ANGL(1) = ALPHA
        ANGL(2) = BETA
        ANGL(3) = 0.D0
        CALL MATROT ( ANGL , MRO )
        CALL PMAVEC('ZERO',3,MRO,GN,GY)
          IF (ABS(GY(3)).LE.TST.AND.ABS(GY(2)).LE.TST)THEN
             ANGL(3) = 0.D0
          ELSE
             ANGL(3) = ATAN2(GY(3),GY(2))
          ENDIF
C
        END
