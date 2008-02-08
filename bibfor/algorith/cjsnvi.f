        SUBROUTINE CJSNVI ( MOD, NDT, NDI, NVI )
        IMPLICIT NONE

C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C       CJS        : DONNE NOMBRE CONTARINTES ET NOMBRE VARIABLES DE
C                    CJS
C       ----------------------------------------------------------------
C       IN
C           MOD    :  TYPE DE MODELISATION
C       OUT
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
        INTEGER         NDT, NDI, NVI
C
C VARIABLES LOALES POUR SE PREMUNIR D APPELS DU TYPE
C CALL CJSNVI(MOD,IBID,IBID,NVI)
C
        INTEGER         NDTLOC,NDILOC
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C

C - 3D
            IF (MOD(1:2).EQ.'3D') THEN
            NDTLOC = 6
            NDILOC = 3

C - D_PLAN AXIS
            ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS') THEN
            NDTLOC = 4
            NDILOC = 3

C - C_PLAN
            ELSE IF (MOD(1:6).EQ.'C_PLAN') THEN
            CALL U2MESS('F','ALGORITH2_15')

C - 1D
            ELSE IF (MOD(1:2).EQ.'1D') THEN
            CALL U2MESS('F','ALGORITH2_15')
C - 1D
            ELSE
             CALL U2MESS('F','ALGORITH2_20')
            ENDIF
            NVI = NDTLOC+10
            NDT = NDTLOC
            NDI = NDILOC
        END
