        SUBROUTINE LGLNVI ( MOD, NDT, NDI, NVI )
C
        IMPLICIT     NONE
        INTEGER      NDT, NDI, NVI
        CHARACTER*8  MOD
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/02/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C =================================================================
C |---------------------------------------------------------------|
C |-- BUT : RECUPERATION DU NOMBRE DE VARIABLES INTERNES ---------|
C |---------------------------------------------------------------|
C =================================================================
C IN  : MOD    : TYPE DE MODELISATION -----------------------------
C OUT : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR -----------
C --- : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR --------
C --- : NVI    : NB DE VARIABLES INTERNES -------------------------
C =================================================================
C --- LES VARIABLES INTERNES --------------------------------------
C -----------------------------------------------------------------
C --- VIN(1)          : GAMP --------------------------------------
C --- VIN(2)          : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE ---
C --- VIN(3)          : DOMAINE DE COMPORTEMENT DU MATERIAU -------
C --- VIN(4)          : ETAT DU MATERIAU --------------------------
C =================================================================
      CALL JEMARQ ()
C =================================================================
C --- NB DE COMPOSANTES / VARIABLES INTERNES ----------------------
C =================================================================
      IF (MOD(1:2).EQ.'3D') THEN
C =================================================================
C - MODELISATION DE TYPE 3D ---------------------------------------
C =================================================================
          NDT = 6
          NDI = 3
      ELSE IF ( MOD(1:6).EQ.'D_PLAN'.OR.
     +          MOD(1:4).EQ.'AXIS'  .OR.
     +          MOD(1:6).EQ.'C_PLAN'     ) THEN
C =================================================================
C - D_PLAN AXIS C_PLAN --------------------------------------------
C =================================================================
          NDT = 4
          NDI = 3
      ELSE IF (MOD(1:2).EQ.'1D') THEN
C =================================================================
C - MODELISATION DE TYPE 1D NON AUTORISEE -------------------------
C =================================================================
          CALL UTMESS('F','LAIGLE',
     +                    'LA MODELISATION 1D N EST PAS AUTORISEE')
      ELSE 
C =================================================================
C - MODELISATION INCONNUE -----------------------------------------
C =================================================================
          CALL UTMESS('F','LAIGLE','MODELISATION INCONNUE')
      ENDIF
C =================================================================
C - NOMBRE DE VARIABLES INTERNES ET DES CONDITIONS NON-LINEAIRES --
C =================================================================
      NVI = 4
C =================================================================
      CALL JEDEMA ()
C =================================================================
      END
