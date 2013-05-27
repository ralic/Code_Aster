subroutine carcha(noch, nomgd, typcha, option, param)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: nomgd, typcha, param
    character(len=16) :: noch
    character(len=24) :: option
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       RECUPERER DES CARACTERISTIQUES LIEES A UN NOM DE CHAMP
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOCH     : NOM DU CHAMP
!
!      SORTIE :
!-------------
! OUT  NOMGD    : NOM DE LA GRANDEUR ASSOCIEE
! OUT  TYPCHA   : TYPE DU CHAMP
! OUT  OPTION   : OPTION CALCULANT CE CHAMP
! OUT  PARA     : NOM D'UN MODE LOCAL ASSOCIE
!
! ......................................................................
!
    if (noch .eq. 'TEMP') then
        nomgd = 'TEMP_R  '
        typcha = 'NOEU'
    else if (noch.eq.'PRES') then
        nomgd = 'PRES_R  '
        typcha = 'ELEM'
    else if (noch.eq.'IRRA') then
        nomgd = 'IRRA_R  '
        typcha = 'NOEU'
!
!     CHAMP DE GRANDEUR "DEPL_R"
    else if (noch.eq.'DEPL') then
        nomgd = 'DEPL_R  '
        typcha = 'NOEU'
    else if (noch.eq.'PTOT') then
        nomgd = 'DEPL_R  '
        typcha = 'NOEU'
    else if (noch.eq.'VITE') then
        nomgd = 'DEPL_R  '
        typcha = 'NOEU'
    else if (noch.eq.'ACCE') then
        nomgd = 'DEPL_R  '
        typcha = 'NOEU'
!
!     CHAMP DE GRANDEUR "SIEF_R"
    else if (noch.eq.'SIEF_ELGA') then
        nomgd = 'SIEF_R'
        typcha = 'ELGA'
        option = 'RAPH_MECA'
        param = 'PCONTPR'
    else if (noch.eq.'SIEF_ELGA') then
        nomgd = 'SIEF_R'
        typcha = 'ELGA'
        option = 'SIEF_ELGA'
        param = 'PCONTPR'
    else if (noch.eq.'SIEQ_ELGA') then
        nomgd = 'SIEF_R'
        typcha = 'ELGA'
        option = 'SIEQ_ELGA'
        param = 'PCONTEQ'
    else if (noch.eq.'SIEF_ELNO') then
        nomgd = 'SIEF_R'
        typcha = 'ELNO'
        option = 'SIEF_ELNO'
        param = 'PSIEFNOR'
    else if (noch.eq.'SIEQ_ELNO') then
        nomgd = 'SIEF_R'
        typcha = 'ELNO'
        option = 'SIEQ_ELNO'
    else if (noch.eq.'SIEF_NOEU') then
        nomgd = 'SIEF_R'
        typcha = 'NOEU'
    else if (noch.eq.'SIGM_ELNO') then
        nomgd = 'SIEF_R'
        typcha = 'ELNO'
    else if (noch.eq.'SIGM_NOEU') then
        nomgd = 'SIEF_R'
        typcha = 'NOEU'
    else if (noch.eq.'SIEQ_NOEU') then
        nomgd = 'SIEF_R'
        typcha = 'NOEU'
        option = 'SIEQ_NOEU'
!
!
!     CHAMP DE GRANDEUR "EPSI_R"
    else if (noch.eq.'EPSI_ELGA') then
        nomgd = 'EPSI_R'
        typcha = 'ELGA'
        option = 'EPSI_ELGA'
        param = 'PDEFOPG'
    else if (noch.eq.'EPMQ_ELGA') then
        nomgd = 'EPSI_R'
        typcha = 'ELGA'
        option = 'EPMQ_ELGA'
        param = 'PDEFOEQ'
    else if (noch.eq.'EPEQ_ELGA') then
        nomgd = 'EPSI_R'
        typcha = 'ELGA'
        option = 'EPEQ_ELGA'
        param = 'PDEFOEQ'
    else if (noch.eq.'EPSG_ELGA') then
        nomgd = 'EPSI_R'
        typcha = 'ELGA'
        option = 'EPSG_ELGA'
        param = 'PDEFOPG'
    else if (noch.eq.'EPSI_ELNO') then
        nomgd = 'EPSI_R'
        typcha = 'ELNO'
        option = 'EPSI_ELNO'
        param = 'PDEFONO'
    else if (noch.eq.'EPSA_ELNO') then
        nomgd = 'EPSI_R'
        typcha = 'ELNO'
        option = 'EPSI_ELNO'
        param = 'PDEFONO'
    else if (noch.eq.'EPSP_ELNO') then
        nomgd = 'EPSI_R'
        typcha = 'ELNO'
        option = 'EPSP_ELNO'
        param = 'PDEFOPL'
    else if (noch.eq.'EPSI_NOEU') then
        nomgd = 'EPSI_R'
        typcha = 'NOEU'
    else if (noch.eq.'DIVU') then
        nomgd = 'EPSI_R'
        typcha = 'NOEU'
    else if (noch.eq.'EPSA_NOEU') then
        nomgd = 'EPSI_R'
        typcha = 'NOEU'
    else if (noch.eq.'EPME_ELNO') then
        nomgd = 'EPSI_R'
        typcha = 'ELNO'
!
!     CHAMP DE GRANDEUR "VARI_R"
    else if (noch.eq.'VARI_ELGA') then
        nomgd = 'VARI_R'
        typcha = 'ELGA'
        option = 'RAPH_MECA'
        param = 'PVARIPR'
    else if (noch.eq.'VARI_ELNO') then
        nomgd = 'VARI_R'
        typcha = 'ELNO'
        option = 'VARI_ELNO'
        param = 'PVARINR'
    else if (noch.eq.'VARI_NOEU') then
        nomgd = 'VAR2_R'
        typcha = 'NOEU'
    else if (noch.eq.'HYDR_ELNO') then
        nomgd = 'HYDR_R'
        typcha = 'ELNO'
    else if (noch.eq.'HYDR_NOEU') then
        nomgd = 'HYDR_R'
        typcha = 'NOEU'
    else if (noch.eq.'ERME_ELEM') then
        nomgd = 'ERRE_R'
        typcha = 'ELEM'
!
!     ERREUR
    else
        call u2mesk('F', 'UTILITAI2_94', 1, noch)
    endif
!
!     VERIFICATION DE LA PRESENCE DE 'PARAM' ET 'OPTION'
!     POUR LES CHAMPS ELGA
    if (noch(6:9) .eq. 'ELGA') then
        call assert(option.ne.' ')
        call assert(param .ne.' ')
    endif
!
end subroutine
