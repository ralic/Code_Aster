        SUBROUTINE ANGVX(GX,ALPHA,BETA)
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
C       CALCUL DE 2 ANGLES NAUTIQUES A PARTIR DU VECTEUR GX
C       IN      GX
C       OUT     ALPHA , BETA
C       ----------------------------------------------------------------
        REAL*8  GX(3),  ALPHA,  BETA,   P,      R8MIEM, TST
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        TST = R8MIEM()
C
        IF(ABS(GX(2)).LE.TST.AND.ABS(GX(1)).LE.TST)THEN
        ALPHA = 0.D0
        ELSE
        ALPHA = ATAN2(GX(2),GX(1))
        ENDIF
        P  = SQRT( GX(1)*GX(1) + GX(2)*GX(2) )
        IF(ABS(GX(3)).LE.TST.AND.ABS(P).LE.TST)THEN
        BETA = 0.D0
        ELSE
        BETA  = - ATAN2(GX(3),P)
        ENDIF
        END
