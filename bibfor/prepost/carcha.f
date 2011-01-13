      SUBROUTINE CARCHA(NOCH,NOMGD,TYPCHA,OPTION,PARAM)
      IMPLICIT  NONE
      CHARACTER*8  NOMGD,TYPCHA,PARAM
      CHARACTER*16 NOCH
      CHARACTER*24 OPTION
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/01/2011   AUTEUR PELLET J.PELLET 
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
C
C     BUT:
C       RECUPERER DES CARACTERISTIQUES LIEES A UN NOM DE CHAMP
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   NOCH     : NOM DU CHAMP
C
C      SORTIE :
C-------------
C OUT  NOMGD    : NOM DE LA GRANDEUR ASSOCIEE
C OUT  TYPCHA   : TYPE DU CHAMP
C OUT  OPTION   : OPTION CALCULANT CE CHAMP
C OUT  PARA     : NOM D'UN MODE LOCAL ASSOCIE
C
C ......................................................................
C
      IF (NOCH.EQ.'TEMP') THEN
         NOMGD  = 'TEMP_R  '
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'PRES') THEN
         NOMGD  = 'PRES_R  '
         TYPCHA = 'ELEM'
      ELSE IF (NOCH.EQ.'IRRA') THEN
         NOMGD  = 'IRRA_R  '
         TYPCHA = 'NOEU'
C
C     CHAMP DE GRANDEUR "DEPL_R"
      ELSE IF (NOCH.EQ.'DEPL') THEN
         NOMGD  = 'DEPL_R  '
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'VITE') THEN
         NOMGD  = 'DEPL_R  '
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'ACCE') THEN
         NOMGD  = 'DEPL_R  '
         TYPCHA = 'NOEU'
C
C     CHAMP DE GRANDEUR "SIEF_R"
      ELSE IF (NOCH.EQ.'SIEF_ELGA') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELGA'
         OPTION = 'RAPH_MECA'
         PARAM  = 'PCONTPR'
      ELSE IF (NOCH.EQ.'SIEF_ELGA') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELGA'
         OPTION = 'SIEF_ELGA'
         PARAM  = 'PCONTPR'
      ELSE IF (NOCH.EQ.'EQUI_ELGA_SIGM') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELGA'
         OPTION = 'EQUI_ELGA_SIGM'
         PARAM  = 'PCONTEQ'
      ELSE IF (NOCH.EQ.'SIEF_ELNO') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELNO'
         OPTION = 'SIEF_ELNO'
         PARAM  = 'PSIEFNOR'
      ELSE IF (NOCH.EQ.'SIEF_ELNO') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELNO'
         OPTION = 'SIEF_ELNO'
         PARAM  = 'PSIEFNOR'
      ELSE IF (NOCH.EQ.'EQUI_ELNO_SIGM') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'ELNO'
         OPTION = 'EQUI_ELNO_SIGM'
      ELSE IF (NOCH.EQ.'SIEF_NOEU') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'SIEF_NOEU') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'SIGM_NOEU_DEPL') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'EQUI_NOEU_SIGM') THEN
         NOMGD  = 'SIEF_R'
         TYPCHA = 'NOEU'
         OPTION = 'EQUI_NOEU_SIGM'
C
C     CHAMP DE GRANDEUR "EPSI_R"
      ELSE IF (NOCH.EQ.'EPSI_ELGA') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELGA'
         OPTION = 'EPSI_ELGA'
         PARAM  = 'PDEFORR'
      ELSE IF (NOCH.EQ.'EPSI_ELGA') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELGA'
         OPTION = 'EPSI_ELGA'
         PARAM  = 'PDEFORR'
      ELSE IF (NOCH.EQ.'EQUI_ELGA_EPME') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELGA'
         OPTION = 'EQUI_ELGA_EPME'
         PARAM  = 'PDEFOEQ'
      ELSE IF (NOCH.EQ.'EQUI_ELGA_EPSI') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELGA'
         OPTION = 'EQUI_ELGA_EPSI'
         PARAM  = 'PDEFOEQ'
      ELSE IF (NOCH.EQ.'EPSG_ELGA') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELGA'
         OPTION = 'EPSG_ELGA'
         PARAM  = 'PDEFORR'
      ELSE IF (NOCH.EQ.'EPSI_ELNO_DEPL') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELNO'
         OPTION = 'EPSI_ELNO_DEPL'
         PARAM  = 'PDEFORR'
      ELSE IF (NOCH.EQ.'EPSI_ELNO_TUYO') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELNO'
         OPTION = 'EPSI_ELNO_TUYO'
         PARAM  = 'PDEFONO'
      ELSE IF (NOCH.EQ.'EPSA_ELNO') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELNO'
         OPTION = 'EPSI_ELNO_DEPL'
         PARAM  = 'PDEFORR'
      ELSE IF (NOCH.EQ.'EPSP_ELNO') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELNO'
         OPTION = 'EPSP_ELNO'
         PARAM  = 'PDEFOPL'
      ELSE IF (NOCH.EQ.'EPSI_NOEU') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'EPSA_NOEU') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'EPME_ELNO') THEN
         NOMGD  = 'EPSI_R'
         TYPCHA = 'ELNO'
C
C     CHAMP DE GRANDEUR "VARI_R"
      ELSE IF (NOCH.EQ.'VARI_ELGA') THEN
         NOMGD  = 'VARI_R'
         TYPCHA = 'ELGA'
         OPTION = 'RAPH_MECA'
         PARAM  = 'PVARIPR'
      ELSE IF (NOCH.EQ.'VARI_ELNO') THEN
         NOMGD  = 'VARI_R'
         TYPCHA = 'ELNO'
         OPTION = 'VARI_ELNO_ELGA'
         PARAM  = 'PVARINR'
      ELSE IF (NOCH.EQ.'VARI_ELNO_ELGA') THEN
         NOMGD  = 'VARI_R'
         TYPCHA = 'ELNO'
         OPTION = 'VARI_ELNO_ELGA'
         PARAM  = 'PVARINR'
      ELSE IF (NOCH.EQ.'VARI_NOEU_ELGA') THEN
         NOMGD  = 'VAR2_R'
         TYPCHA = 'NOEU'
      ELSE IF (NOCH.EQ.'HYDR_ELNO') THEN
         NOMGD  = 'HYDR_R'
         TYPCHA = 'ELNO'
      ELSE IF (NOCH.EQ.'HYDR_NOEU') THEN
         NOMGD  = 'HYDR_R'
         TYPCHA = 'NOEU'

C
C     ERREUR
      ELSE
         CALL U2MESK('F','UTILITAI2_94',1,NOCH)
      END IF
C
C     VERIFICATION DE LA PRESENCE DE 'PARAM' ET 'OPTION'
C     POUR LES CHAMPS ELGA
      IF(NOCH(6:9).EQ.'ELGA') THEN
        CALL ASSERT(OPTION.NE.' ')
        CALL ASSERT(PARAM .NE.' ')
      ENDIF
C
      END
