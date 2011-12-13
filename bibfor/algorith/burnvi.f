        SUBROUTINE BURNVI ( MOD, NDT, NDI, NR, NVI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C ----------------------------------------------------------------
C BETON_BURGER_FP :  NOMBRE DE COMPOSANTES DES CONTRAINTES ET
C                    NOMBRE VARIABLES
C=====================================================================
C
C IN  MOD   :  TYPE DE MODELISATION
C OUT NDT   :  NB TOTAL DE COMPOSANTES TENSEURS
C     NDI   :  NB DE COMPOSANTES DIRECTES TENSEURS
C     NR    :  NB DE COMPOSANTES DANS LE SYSTEME D'EQUATIONS
C              RESOLUTION PAR NEWTON DANS PLASTI : SIG + VINT
C              IL FAUT AJOUTER UN TERME POUR LES C_PLAN
C     NVI   :  NB DE VARIABLES INTERNES
C=====================================================================
      IMPLICIT NONE
      INTEGER         NDT , NDI, NR, NVI, NVINT
      CHARACTER*8     MOD

C === =================================================================
C --- NB DE COMPOSANTES / VARIABLES INTERNES / CATALOGUE MATERIAU
C === =================================================================
C --- ON INDIQUE NVI=20 
C === =================================================================
      NVI = 20
C === =================================================================
C --- NB VARIABLES INTERNES INTEGREES PAR NEWTON
C === =================================================================
      IF ((MOD(1:6).EQ.'C_PLAN').OR.(MOD(1:6).EQ.'D_PLAN').OR.
     &    (MOD(1:4).EQ.'AXIS')) THEN
        NVINT = 4
      ELSE
        NVINT = 6
      ENDIF
C === =================================================================
C --- 3D
C === =================================================================
      IF      (MOD(1:2).EQ.'3D')THEN
         NDT = 6
         NDI = 3
         NR  = NDT + NVINT
C === =================================================================
C --- D_PLAN AXIS C_PLAN
C === =================================================================
      ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS') THEN
         NDT = 4
         NDI = 3
         NR  = NDT + NVINT
      ELSE IF (MOD(1:6).EQ.'C_PLAN') THEN
         NDT = 4
         NDI = 3
         NR  = NDT + NVINT + 1
      ELSE IF (MOD(1:6).EQ.'POUTRE') THEN
C === =================================================================
C        MODELISATION DE TYPE POUTRE NON AUTORISEE
C === =================================================================
         CALL U2MESS('F','ALGORITH4_45')
      ELSE IF (MOD(1:2).EQ.'1D') THEN
C === =================================================================
C        MODELISATION DE TYPE 1D NON AUTORISEE
C === ==============================================================
         CALL U2MESS('F','ALGORITH4_45')
      ELSE
C === ==============================================================
C        MODELISATION INCONNUE
C === ==============================================================
         CALL U2MESS('F','ALGORITH2_20')
      ENDIF

      END
