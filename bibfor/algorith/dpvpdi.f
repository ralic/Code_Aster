      SUBROUTINE DPVPDI( NBMAT, MATER, TD, TF, TR, DEPST, DEPS)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/11/2010   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE - 
C --- VISC_DRUC_PRAG
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ---------
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2), TD, TF, TR, DEPST(6), DEPS(6)
C =====================================================================
C --- IN --- : NBMAT   NOMBRE DE PARAMETRES DU MODELE -----------------
C --- IN --- : MATER   COEFFICIENTS MATERIAU --------------------------
C ---------- : TD      TEMPERATURE DEBUT INCREMENT --------------------
C ---------- : TF      TEMPERATURE FIN INCREMENT ----------------------
C ---------- : TR      TEMPERATURE DE REFERENCE -----------------------
C ---------- : DEPST   INCREMENT DE DEFORMATION TOTALE ----------------
C --- OUT -- : DEPS   INCREMENT DE DEFORMATION MECANIQUE -------------
C =====================================================================
      INTEGER     II, NDT, NDI, IISNAN
      REAL*8      ALPHA
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- LES PARAMETRES MATERIAUX SONT SUPPOSES CONSTANT -----------------
C =====================================================================
      ALPHA = MATER(3,1)
      IF ((IISNAN(TR).EQ.0).AND.(IISNAN(TF).EQ.0).AND.(IISNAN(TD).EQ.0)
     &    .AND.(ALPHA.NE.0.D0))  THEN
        DO 10 II = 1, NDI
         DEPS(II) = DEPST(II) - ( ALPHA*(TF-TR) - ALPHA*(TD-TR))
 10     CONTINUE
        DO 20 II = NDI+1, NDT
         DEPS(II) = DEPST(II)
 20     CONTINUE
      ELSEIF (ALPHA.EQ.0.D0) THEN
        DO 11 II = 1, NDT
         DEPS(II) = DEPST(II)
 11     CONTINUE
      ELSEIF   (((IISNAN(TR).EQ.1).OR.(IISNAN(TD).EQ.1).OR.
     &         (IISNAN(TF).EQ.1)).AND.(ALPHA.NE.0.D0)) THEN
        CALL U2MESS('F','CALCULEL_15')
      ENDIF 
C =====================================================================
      END
