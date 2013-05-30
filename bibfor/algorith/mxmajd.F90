subroutine mxmajd(deltat, sddyna)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynin.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/ndynlo.h'
    character(len=19) :: sddyna
    real(kind=8) :: deltat
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EXPLICITE)
!
! MET A JOUR LES VITESSES ET LES DEPL/VITE/ACCE GENERALISEES
! (PROJ_MODAL)
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! IN  DELTAT : INCREMENT DE TEMPS
!
!
!
!
    character(len=19) :: depgep, vitgep, accgep
    integer :: jdepgp, jvitgp, jaccgp
    character(len=19) :: depgem, vitgem, accgem
    integer :: jdepgm, jvitgm, jaccgm
    integer :: imode
    real(kind=8) :: coevi1, coevi2
    integer :: nbmodp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- COEFFICIENTS
!
    if (ndynlo(sddyna,'DIFF_CENT')) then
        coevi1 = 0.5d0*deltat
        coevi2 = 0.5d0*deltat
    else if (ndynlo(sddyna,'TCHAMWA')) then
        coevi1 = 0.d0
        coevi2 = deltat
    endif
!
! --- NOM DES OBJETS POUR PROJECTION MODALE
!
    call ndynkk(sddyna, 'PRMO_DEPGEM', depgem)
    call ndynkk(sddyna, 'PRMO_VITGEM', vitgem)
    call ndynkk(sddyna, 'PRMO_ACCGEM', accgem)
    call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
    call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
    call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
    nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
!
    call jeveuo(depgep, 'E', jdepgp)
    call jeveuo(depgem, 'E', jdepgm)
    call jeveuo(vitgep, 'E', jvitgp)
    call jeveuo(vitgem, 'E', jvitgm)
    call jeveuo(accgep, 'E', jaccgp)
    call jeveuo(accgem, 'E', jaccgm)
!
    do 11 imode = 1, nbmodp
        zr(jvitgp+imode-1) = zr(jvitgm+imode-1) + coevi1*zr(jaccgp+ imode-1) + coevi2*zr(jaccgm+i&
                             &mode-1)
        zr(jdepgm+imode-1) = zr(jdepgp+imode-1)
        zr(jvitgm+imode-1) = zr(jvitgp+imode-1)
        zr(jaccgm+imode-1) = zr(jaccgp+imode-1)
!
!
11  end do
!
!
    call jedema()
end subroutine
