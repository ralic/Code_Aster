        SUBROUTINE ANGVXZ ( GX , GN , ANGL )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/11/98   AUTEUR CIBHHLV L.VIVAN 
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
C       CALCUL DES 3 ANGLES NAUTIQUES A PARTIR DU VECTEUR GX
C       ET D UN VECTEUR GN DONT LA PROJECTION NORMALE SUR LE PLAN NORMAL
C       A GX DONNE LE VECTEUR GZ
C       IN      GX , GN
C       OUT     ALPHA , BETA , GAMMA
C       ----------------------------------------------------------------
        REAL*8  MRO(3,3), GX(3),  GZ(3),  GN(3),  ANGL(*)
C
        TST = R8MIEM()
C
        CALL ANGVX ( GX, ALPHA, BETA )
        ANGL(1) = ALPHA
        ANGL(2) = BETA
        ANGL(3) = 0.D0
        CALL MATROT ( ANGL , MRO )
        CALL PMAVEC ( 'ZERO', 3, MRO, GN, GZ )
        IF ( ABS(GZ(3)).LE.TST .AND. ABS(GZ(2)).LE.TST )THEN
           ANGL(3) = 0.D0
        ELSE
           ANGL(3) = ATAN2(GZ(2),GZ(3))
        ENDIF
C
        END
