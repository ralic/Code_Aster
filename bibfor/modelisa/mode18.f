      SUBROUTINE MODE18 ( TYPELE, TYPEMO )
      IMPLICIT NONE
      CHARACTER*16        TYPELE, TYPEMO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/01/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C --- POUTRES, BARRES ET DISCRETS -------------------------------------
C
      IF ( TYPELE.EQ.'MECA_POU_D_E' ) THEN
         TYPEMO = 'POU_D_E'

      ELSEIF ( TYPELE.EQ.'MECA_POU_D_EM') THEN
         TYPEMO = 'POU_D_EM'

      ELSEIF ( TYPELE.EQ.'MECA_POU_D_T' ) THEN
         TYPEMO = 'POU_D_T'

      ELSEIF ( TYPELE.EQ.'MECA_POU_C_T' ) THEN
         TYPEMO = 'POU_C_T'

      ELSEIF ( TYPELE.EQ.'MECA_POU_D_TG' ) THEN
         TYPEMO = 'POU_D_TG'

      ELSEIF ( TYPELE.EQ.'MECA_POU_D_TGM' ) THEN
         TYPEMO = 'POU_D_TGM'

      ELSEIF ( TYPELE.EQ.'MECA_POU_D_T_GD' ) THEN
         TYPEMO =  'POU_D_T_GD'

      ELSEIF ( TYPELE.EQ.'MECA_BARRE' ) THEN
         TYPEMO = 'BARRE'

      ELSEIF ( TYPELE.EQ.'MECA_2D_BARRE' ) THEN
         TYPEMO = '2D_BARRE'

      ELSEIF ( TYPELE.EQ.'MECA_DIS_T_L' .OR. 
     +         TYPELE.EQ.'MECA_DIS_T_N' ) THEN
         TYPEMO = 'DIS_T'

      ELSEIF ( TYPELE.EQ.'MECA_2D_DIS_T_L' .OR. 
     +         TYPELE.EQ.'MECA_2D_DIS_T_N' ) THEN
         TYPEMO = '2D_DIS_T'

      ELSEIF ( TYPELE.EQ.'MECA_DIS_TR_L' .OR.
     +         TYPELE.EQ.'MECA_DIS_TR_N' ) THEN
         TYPEMO = 'DIS_TR'

      ELSEIF ( TYPELE.EQ.'MECA_2D_DIS_TR_L' .OR.
     +         TYPELE.EQ.'MECA_2D_DIS_TR_N' ) THEN
         TYPEMO = '2D_DIS_TR'
C
C --- 3D --------------------------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'MECA_HEXS20' ) THEN
         TYPEMO = '3D_SI'

      ELSEIF ( TYPELE(1:7).EQ.'MECA_XH' ) THEN
         TYPEMO = '3D_XFEM_H'

      ELSEIF ( TYPELE(1:7).EQ.'MECA_XT' ) THEN
         TYPEMO = '3D_XFEM_T'

      ELSEIF ( TYPELE(1:8).EQ.'MECA_XHT' ) THEN
         TYPEMO = '3D_XFEM_HT'

      ELSEIF ( TYPELE(1:9).EQ.'MECA_POHO' ) THEN
         TYPEMO = '3D_FAISCEAU'

      ELSEIF ( TYPELE(1:5).EQ.'MECA_' ) THEN
         TYPEMO = '3D'

      ELSEIF ( TYPELE(1:4).EQ.'MINC' ) THEN
         TYPEMO = '3D_INCO'

      ELSEIF ( TYPELE(1:4).EQ.'MGCA' ) THEN
         TYPEMO = '3D_GRAD_EPSI'

      ELSEIF ( TYPELE(1:5).EQ.'MVCA_' ) THEN
         TYPEMO = '3D_GRAD_VARI'

      ELSEIF ( TYPELE.EQ.'THM_LISU_FACE6' .OR.
     +         TYPELE.EQ.'THM_LISU_FACE8' .OR.
     +         TYPELE.EQ.'SOSU_ARETE3' ) THEN
         TYPEMO = '3D_JOINT_CT'
C
C --- LES THM ET COMPAGNIE --------------------------------------------
C
      ELSEIF ( TYPELE(1:5).EQ.'THHM_' ) THEN
         CALL THHM18 ( TYPELE, TYPEMO )

      ELSEIF ( TYPELE(1:4).EQ.'THH_' ) THEN
         CALL THH018 ( TYPELE, TYPEMO )

      ELSEIF ( TYPELE(1:4).EQ.'THM_' ) THEN
         CALL THM018 ( TYPELE, TYPEMO )

      ELSEIF ( TYPELE(1:4).EQ.'HHM_' ) THEN
         CALL HHM018 ( TYPELE, TYPEMO )

      ELSEIF ( TYPELE(1:3).EQ.'HM_' ) THEN
         CALL HM0018 ( TYPELE, TYPEMO )

      ELSEIF ( TYPELE(1:9).EQ.'THH2_AXIS' ) THEN
         TYPEMO = 'AXIS_THH2D'

      ELSEIF ( TYPELE(1:10).EQ.'THH2M_AXIS' ) THEN
         TYPEMO = 'AXIS_THH2MD'

      ELSEIF ( TYPELE(1:6).EQ.'THH2_D' ) THEN
         TYPEMO = 'D_PLAN_THH2D'

      ELSEIF ( TYPELE(1:7).EQ.'THH2M_D' ) THEN
         TYPEMO = 'D_PLAN_THH2MD'

      ELSEIF ( TYPELE(1:9).EQ.'HH2M_AXIS' ) THEN
         TYPEMO = 'AXIS_HH2MD'

      ELSEIF ( TYPELE(1:6).EQ.'HH2M_D' ) THEN
         TYPEMO = 'D_PLAN_HH2MD'

      ELSEIF ( TYPELE(1:5).EQ.'THV_D' ) THEN
         TYPEMO = 'D_PLAN_THVD'

      ELSEIF ( TYPELE(1:8).EQ.'THV_AXIS' ) THEN
         TYPEMO = 'AXIS_THVD'

      ELSEIF ( TYPELE(1:4).EQ.'THV_' ) THEN
         TYPEMO = '3D_THVD'
C
C --- PLAQUES ET COQUES -----------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'MEDKTR3' .OR. TYPELE.EQ.'MEDKQU4' .OR.
     +         TYPELE.EQ.'MEBODKT' ) THEN
         TYPEMO = 'DKT'

      ELSEIF ( TYPELE.EQ.'MEDKTG3' .OR. TYPELE.EQ.'MEDKQG4' .OR.
     +         TYPELE.EQ.'MEBODKT' ) THEN
         TYPEMO = 'DKTG'

      ELSEIF ( TYPELE.EQ.'MEDSTR3' .OR. TYPELE.EQ.'MEDSQU4' .OR.
     +         TYPELE.EQ.'MEBODST' ) THEN
         TYPEMO = 'DST'

      ELSEIF ( TYPELE.EQ.'MEQ4QU4' .OR. TYPELE.EQ.'MEBOQ4G' ) THEN
         TYPEMO = 'Q4G'

      ELSEIF ( TYPELE.EQ.'MEC3QU9H' .OR. TYPELE.EQ.'MEC3TR7H' .OR.
     +         TYPELE.EQ.'MEBOCQ3' ) THEN
         TYPEMO = 'COQUE_3D'

      ELSEIF ( TYPELE.EQ.'MECA_SHB8' ) THEN
         TYPEMO = 'SHB8'

      ELSEIF ( TYPELE.EQ.'MEGRDKT' ) THEN
         TYPEMO = 'GRILLE'

      ELSEIF ( TYPELE(1:4).EQ.'MEGM' ) THEN
         TYPEMO = 'GRILLE_MEMBRANE'

      ELSEIF ( TYPELE.EQ.'MECXSE3' ) THEN
         TYPEMO = 'COQUE_AXIS'

      ELSEIF ( TYPELE.EQ.'METCSE3' ) THEN
         TYPEMO = 'COQUE_C_PLAN'
C
C --- 2D AXIS ---------------------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'MEAXTR3' .OR. TYPELE.EQ.'MEAXQU4' .OR.
     +         TYPELE.EQ.'MEAXSE2' .OR. TYPELE.EQ.'MEAXTR6' .OR.
     +         TYPELE.EQ.'MEAXQU8' .OR. TYPELE.EQ.'MEAXQU9' .OR.
     +         TYPELE.EQ.'MEAXSE3' ) THEN
         TYPEMO = 'AXIS'

      ELSEIF ( TYPELE.EQ.'MEAXQS8' ) THEN
         TYPEMO = 'AXIS_SI'

      ELSEIF ( TYPELE.EQ.'MEFOTR3' .OR. TYPELE.EQ.'MEFOQU4' .OR.
     +         TYPELE.EQ.'MEFOSE2' .OR. TYPELE.EQ.'MEFOTR6' .OR.
     +         TYPELE.EQ.'MEFOQU8' .OR. TYPELE.EQ.'MEFOQU9' .OR.
     +         TYPELE.EQ.'MEFOSE3' ) THEN
         TYPEMO = 'AXIS_FOURIER'

      ELSEIF ( TYPELE.EQ.'MIAXTR6' .OR. TYPELE.EQ.'MIAXQU8' ) THEN
         TYPEMO = 'AXIS_INCO'

      ELSEIF ( TYPELE(1:4).EQ.'MVAX' ) THEN
         TYPEMO = 'AXIS_GRAD_VARI'

      ELSEIF ( TYPELE.EQ.'MFAXQU4' ) THEN
         TYPEMO = 'AXIS_FISSURE'
C
C --- 2D D_PLAN -------------------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'MEDPTR3' .OR. TYPELE.EQ.'MEDPTR6' .OR.
     +         TYPELE.EQ.'MEDPQU4' .OR. TYPELE.EQ.'MEDPQU8' .OR.
     +         TYPELE.EQ.'MEDPQU9' .OR. 
     +         TYPELE.EQ.'MEPLSE3' .OR. TYPELE.EQ.'MEPLSE2' ) THEN
         TYPEMO = 'D_PLAN'

      ELSEIF ( TYPELE.EQ.'MEDPQS8' .OR. TYPELE.EQ.'MEDPQS4' ) THEN
         TYPEMO = 'D_PLAN_SI'

      ELSEIF ( TYPELE.EQ.'MIPLTR6' .OR. TYPELE.EQ.'MIPLQU8' ) THEN
         TYPEMO = 'D_PLAN_INCO'

      ELSEIF ( TYPELE.EQ.'MGDPTR6' .OR. TYPELE.EQ.'MGDPQU8' ) THEN
         TYPEMO = 'D_PLAN_GRAD_EPSI'

      ELSEIF ( TYPELE(1:4).EQ.'MVDP' ) THEN
         TYPEMO = 'D_PLAN_GRAD_VARI'

      ELSEIF ( TYPELE.EQ.'MFPLQU4' ) THEN
         TYPEMO = 'PLAN_FISSURE'
C
C --- 2D C_PLAN -------------------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'MECPTR3' .OR. TYPELE.EQ.'MECPQU4' .OR.
     +         TYPELE.EQ.'MECPTR6' .OR. TYPELE.EQ.'MECPQU8' .OR.
     +         TYPELE.EQ.'MECPQU9' ) THEN
         TYPEMO = 'C_PLAN'

      ELSEIF ( TYPELE.EQ.'MECPQS8' .OR. TYPELE.EQ.'MECPQS4' ) THEN
         TYPEMO = 'C_PLAN_SI'

      ELSEIF ( TYPELE(1:4).EQ.'MGCP' ) THEN
         TYPEMO = 'C_PLAN_GRAD_EPSI'

      ELSEIF ( TYPELE(1:4).EQ.'MVCP' ) THEN
         TYPEMO = 'C_PLAN_GRAD_VARI'
C
C --- DIVERS ----------------------------------------------------------
C
      ELSEIF ( TYPELE(1:10).EQ.'MECA_APPUI' ) THEN
         TYPEMO = 'APPUI_REP'

      ELSEIF ( TYPELE.EQ.'MECABL2' ) THEN
         TYPEMO = 'CABLE'

      ELSEIF ( TYPELE.EQ.'MEPOULI' ) THEN
         TYPEMO = 'CABLE_POULIE'

      ELSEIF ( TYPELE.EQ.'MET3SEG3' .OR. TYPELE.EQ.'MET3SEG4' ) THEN
         TYPEMO = 'TUYAU_3M'

      ELSEIF ( TYPELE.EQ.'MET6SEG3' ) THEN
         TYPEMO = 'TUYAU_6M'

      ELSEIF ( TYPELE(1:5).EQ.'MEFL_' ) THEN
         TYPEMO = '3D_FLUIDE'

      ELSEIF ( TYPELE(1:4).EQ.'MEFL' ) THEN
         TYPEMO = '2D_FLUIDE'

      ELSEIF ( TYPELE(1:5).EQ.'MEFS_' ) THEN
         TYPEMO = 'FLUI_STRU'

      ELSEIF ( TYPELE(1:5).EQ.'MEFSS' ) THEN
         TYPEMO = '2D_FLUI_STRU'

      ELSEIF ( TYPELE(1:5).EQ.'MEFP_' ) THEN
         TYPEMO = '2D_FLUI_PESA'

      ELSEIF ( TYPELE(1:6).EQ.'MEAXFL' ) THEN
         TYPEMO = 'AXIS_FLUIDE'

      ELSEIF ( TYPELE(1:6).EQ.'MEAXFS' ) THEN
         TYPEMO = 'AXIS_FLUI_STRU'

      ELSEIF ( TYPELE.EQ.'MEGRQU4' ) THEN
         TYPEMO = 'ASSE_GRIL'

      ELSEIF ( TYPELE(1:9).EQ.'MEAB_FACE' ) THEN
         TYPEMO = '3D_ABSO'

      ELSEIF ( TYPELE(1:6).EQ.'MEPASE' ) THEN
         TYPEMO = 'D_PLAN_ABSO'

      ELSEIF ( TYPELE(1:9).EQ.'MEFA_FACE' ) THEN
         TYPEMO = '3D_FLUI_ABSO'

      ELSEIF ( TYPELE(1:6).EQ.'MEFASE' ) THEN
         TYPEMO = '2D_FLUI_ABSO'

      ELSEIF ( TYPELE.EQ.'CFS2E2D' .OR. TYPELE.EQ.'CFS3E2D') THEN
         TYPEMO = 'CONT_DVP_2D'

      ELSEIF ( TYPELE.EQ.'CFT3E3D' .OR. TYPELE.EQ.'CFQ4E3D' .OR.
     +         TYPELE.EQ.'CFQ8E3D' .OR. TYPELE.EQ.'CFT6E3D' .OR.
     +         TYPELE.EQ.'CFQ9E3D' ) THEN
         TYPEMO = 'CONT_DVP_3D'
C
C --- THERMIQUE -------------------------------------------------------
C
      ELSEIF ( TYPELE.EQ.'THCPSE3' ) THEN
         TYPEMO = 'COQUE_PLAN'

      ELSEIF ( TYPELE.EQ.'THCASE3' ) THEN
         TYPEMO = 'COQUE_AXIS'

      ELSEIF ( TYPELE.EQ.'THER_HEXA8_D'  .OR.
     +         TYPELE.EQ.'THER_PENTA6_D' .OR.
     +         TYPELE.EQ.'THER_TETRA4_D' .OR.
     +         TYPELE.EQ.'THER_PYRAM5_D' .OR.
     +         TYPELE.EQ.'THER_FACE4_D'  .OR.
     +         TYPELE.EQ.'THER_FACE3_D'  ) THEN
         TYPEMO = '3D_DIAG'

      ELSEIF ( TYPELE(1:5).EQ.'THER_' ) THEN
         TYPEMO = '3D'

      ELSEIF ( TYPELE.EQ.'THAXTR3' .OR. TYPELE.EQ.'THAXQU4' .OR.
     +         TYPELE.EQ.'THAXSE2' .OR. TYPELE.EQ.'THAXTR6' .OR.
     +         TYPELE.EQ.'THAXQU8' .OR. TYPELE.EQ.'THAXQU9' .OR.
     +         TYPELE.EQ.'THAXSE3' ) THEN
         TYPEMO = 'AXIS'

      ELSEIF ( TYPELE.EQ.'THAXTL3' .OR. TYPELE.EQ.'THAXQL4' .OR.
     +         TYPELE.EQ.'THAXSL2' .OR. TYPELE.EQ.'THAXTL6' .OR.
     +         TYPELE.EQ.'THAXQL9' .OR. TYPELE.EQ.'THAXSL3' ) THEN
         TYPEMO = 'AXIS_DIAG'

      ELSEIF ( TYPELE(1:4).EQ.'THFO') THEN
         TYPEMO = 'AXIS_FOURIER'

      ELSEIF ( TYPELE(1:4).EQ.'THCO') THEN
         TYPEMO = 'COQUE'

      ELSEIF ( TYPELE.EQ.'THPLTR3' .OR. TYPELE.EQ.'THPLQU4' .OR.
     +         TYPELE.EQ.'THPLSE2' .OR. TYPELE.EQ.'THPLTR6' .OR.
     +         TYPELE.EQ.'THPLQU8' .OR. TYPELE.EQ.'THPLQU9' .OR.
     +         TYPELE.EQ.'THPLSE3' ) THEN
         TYPEMO = 'PLAN'

      ELSEIF ( TYPELE.EQ.'THPLTL3' .OR. TYPELE.EQ.'THPLQL4' .OR.
     +         TYPELE.EQ.'THPLSL2' .OR. TYPELE.EQ.'THPLTL6' .OR.
     +         TYPELE.EQ.'THPLQL9' .OR. TYPELE.EQ.'THPLSL3' ) THEN
         TYPEMO = 'PLAN_DIAG'
C
C --- ACOUSTIQUE ------------------------------------------------------
C
      ELSEIF ( TYPELE(1:5).EQ.'ACOU_' ) THEN
         TYPEMO = '3D'

      ELSEIF ( TYPELE(1:4).EQ.'ACPL' ) THEN
         TYPEMO = 'PLAN'

      ELSE
         TYPEMO = 'XXXX'
      ENDIF
C
      END
