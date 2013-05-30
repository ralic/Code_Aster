subroutine rsutc2(typres, nomch, nomgd, typsd)
    implicit none
    include 'asterfort/u2mess.h'
    character(len=*) :: typres, nomch, nomgd, typsd
!
! person_in_charge: jacques.pellet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!----------------------------------------------------------------------
!
!     RECHERCHE :
!       - DE LA GRANDEUR ASSOCIEE AU NOM_CHAM
!       - DU TYPE DE LA SD
!
! IN  : TYPRES : K16  : TYPE DE RESULTAT ('EVOL_THER', 'EVOL_ELAS',...)
! IN  : NOMCH  : K16  : NOM DU CHAMP ('DEPL', 'EPSA_ELNO',...)
! OUT : NOMGD  : K8   : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!                       ('DEPL_R','SIEF_R')
! OUT : TYPSD  : K4   : TYPE DE LA SD ('NOEU', 'ELNO', 'ELGA')
!
!----------------------------------------------------------------------
!
    if (nomch .eq. 'DEPL') then
        nomgd = 'DEPL_R'
        typsd = 'NOEU'
    else if (nomch.eq.'VITE') then
        nomgd = 'DEPL_R'
        typsd = 'NOEU'
    else if (nomch.eq.'ACCE') then
        nomgd = 'DEPL_R'
        typsd = 'NOEU'
    else if (nomch.eq.'TEMP') then
        nomgd = 'TEMP_R'
        typsd = 'NOEU'
    else if (nomch.eq.'VARI_ELNO') then
        nomgd = 'VARI_R'
        typsd = 'ELNO'
    else if (nomch.eq.'EPSA_ELNO') then
        nomgd = 'EPSI_R'
        typsd = 'ELNO'
    else if (nomch.eq.'SIEF_ELNO') then
        nomgd = 'SIEF_R'
        typsd = 'ELNO'
    else if (nomch.eq.'SIGM_ELNO') then
        nomgd = 'SIEF_R'
        typsd = 'ELNO'
    else if (nomch.eq.'PRES') then
        nomgd = 'PRES_R'
        typsd = 'ELNO'
    else if (nomch.eq.'FVOL_3D') then
        nomgd = 'FORC_R'
        typsd = 'NOEU'
    else if (nomch.eq.'FVOL_2D') then
        nomgd = 'FORC_R'
        typsd = 'NOEU'
    else if (nomch.eq.'FSUR_3D') then
        nomgd = 'FORC_R'
        typsd = 'NOEU'
    else if (nomch.eq.'FSUR_2D') then
        nomgd = 'FORC_R'
        typsd = 'NOEU'
    else if (nomch.eq.'EPSI_NOEU') then
        nomgd = 'EPSI_R'
        typsd = 'NOEU'
    else if (nomch.eq.'VITE_VENT') then
        nomgd = 'DEPL_R'
        typsd = 'NOEU'
    else
!
        call u2mess('F', 'PREPOST4_76')
    endif
!
!--- TRAITEMENT DES CHAMPS DE DEPLACEMENTS COMPLEXES
!
    if (nomgd .eq. 'DEPL_R') then
        if (typres .eq. 'DYNA_HARMO' .or. typres .eq. 'HARM_GENE' .or. typres .eq.&
            'MODE_MECA_C') then
            nomgd = 'DEPL_C'
        endif
    endif
!
end subroutine
