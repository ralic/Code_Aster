subroutine uttgel(nomte, ndim, typgeo)
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
! person_in_charge: olivier.boiteau at edf.fr
!  UTILITAIRE - TYPE GEOMETRIQUE D'UN ELEMENT FINI
!  **           *    *                **
! =====================================================================
! IN  NOMTE  : NOM DU TYPE D'ELEMENT FINI
! IN  NDIM   : DIMENSION DE L'ELEMENT FINI
! OUT TYPGEO : TYPE GEOMETRIQUE CORRESPONDANT
!              EN 2D : 'TR', 'QU'
!              EN 3D : 'HE', 'TE', 'PE', 'PY'
! ----------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    integer :: ndim
    character(len=2) :: typgeo
    character(len=16) :: nomte
!
! 0.2. ==> VARIABLES LOCALES
!
    integer :: iaux
    character(len=8) :: k8bid
!
!====
! 1. EN 2D
!====
!
    if (ndim .eq. 2) then
!
! 1.1. ==> CAS PARTICULIER DES ELEMENTS EN THM
!
        if (nomte(1:11) .eq. 'THH2M_AXIS_') then
            iaux = 12
            elseif ( nomte(1:10).eq.'THHM_AXIS_' .or. nomte(1:10)&
        .eq.'HH2M_AXIS_' .or. nomte(1:10).eq.'THH2_AXIS_' ) then
            iaux = 11
            elseif ( nomte(1:9).eq.'THH_AXIS_' .or. nomte(1:9)&
        .eq.'HHM_AXIS_' .or. nomte(1:9).eq.'THM_AXIS_' .or. nomte(1:9)&
        .eq.'THV_AXIS_' .or. nomte(1:9).eq.'HH2_AXIS_' ) then
            iaux = 10
            elseif ( nomte(1:8).eq.'THH2M_DP' .or. nomte(1:8)&
        .eq.'HM_AXIS_' .or. nomte(1:8).eq.'HH_AXIS_' ) then
            iaux = 9
            elseif ( nomte(1:7).eq.'THHM_DP' .or. nomte(1:7).eq.'HH2M_DP'&
        .or. nomte(1:7).eq.'THH2_DP' ) then
            iaux = 8
            elseif ( nomte(1:6).eq.'THM_DP' .or. nomte(1:6).eq.'THH_DP'&
        .or. nomte(1:6).eq.'HHM_DP' .or. nomte(1:6).eq.'THV_DP' .or.&
        nomte(1:6).eq.'HH2_DP' ) then
            iaux = 7
            elseif ( nomte(1:5).eq.'HM_DP' .or. nomte(1:5).eq.'HH_DP' )&
        then
            iaux = 6
!
! 1.2. ==> CAS PARTICULIER DES ELEMENTS OSGS
!
            elseif ( nomte(1:6).eq.'MIPLOS' .or. nomte(1:6).eq.'MIAXOS' )&
        then
            iaux = 7
!
! 1.3. ==> CAS GENERAL
!
        else
            iaux = 5
        endif
!
!====
! 2. EN 3D
!====
!
    else if (ndim.eq.3) then
!
! 2.1. ==> CAS PARTICULIER DES ELEMENTS EN THM
!
        if (nomte(1:6) .eq. 'THH2M_') then
            iaux = 7
            elseif ( nomte(1:5).eq.'THHM_' .or. nomte(1:5).eq.'THH2_'&
        .or. nomte(1:5).eq.'HH2M_' ) then
            iaux = 6
            elseif ( nomte(1:4).eq.'THM_' .or. nomte(1:4).eq.'THH_' .or.&
        nomte(1:4).eq.'HHM_' .or. nomte(1:4).eq.'THV_' .or. nomte(1:4)&
        .eq.'HH2_' ) then
            iaux = 5
        else if (nomte(1:3).eq.'HM_' .or. nomte(1:3).eq.'HH_') then
            iaux = 4
!
! 2.2. ==> CAS PARTICULIER DES ELEMENTS OSGS
!
        else if (nomte(1:7).eq.'MINCOS_') then
            iaux = 8
!
! 2.3. ==> CAS GENERAL
!
        else
            iaux = 6
        endif
!
!====
! 3. SINON ON NE SAIT PAS FAIRE
!====
!
    else
!
        call codent(ndim, 'G', k8bid)
        call u2mesk('F', 'UTILITAI_9', 1, k8bid)
!
    endif
!
!====
! 4. DECODAGE
!====
!
    typgeo = nomte(iaux:iaux+1)
    if (typgeo .eq. 'Q8') then
        typgeo = 'QU'
    endif
!
end subroutine
