subroutine cbpres(char, noma, ligrmo, ndim, fonree)
    implicit   none
    include 'asterc/getfac.h'
    include 'asterfort/cafond.h'
    include 'asterfort/cafotu.h'
    include 'asterfort/cafthm.h'
    include 'asterfort/capres.h'
    integer :: ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
    integer :: nbca1, nbca2, nbca3, nbca4, ialloc
    character(len=16) :: motclf
!-----------------------------------------------------------------------
!
!
    motclf = 'PRES_REP'
    call getfac(motclf, nbca1)
!
    motclf = 'FLUX_THM_REP'
    call getfac(motclf, nbca2)
!
    motclf = 'FORCE_TUYAU'
    call getfac(motclf, nbca3)
!
    motclf = 'EFFE_FOND'
    call getfac(motclf, nbca4)
!
!     MOT CLE PRES_REP :
!     ---------------------
    if (nbca1 .ne. 0) then
        if ((nbca3.eq.0) .and. (nbca4.eq.0)) then
            call capres(char, ligrmo, noma, ndim, fonree)
        endif
    endif
!
!     MOT CLE EFFE_FOND :
!     ---------------------
    if (nbca4 .ne. 0) then
        ialloc=0
        if (nbca1 .ne. 0) then
!
!     EFFET DE FOND ET PRES_REP
!     ON ALLOUE UNE SEULE CARTE CHAR//'.CHME.PRESS'
!
            call capres(char, ligrmo, noma, ndim, fonree)
            ialloc=1
        endif
        call cafond(char, ligrmo, ialloc, noma, fonree)
    endif
!
!     MOT CLE FORCE_TUYAU :
!     ---------------------
    if (nbca3 .ne. 0) then
        ialloc=0
        if (nbca1 .ne. 0) then
!
!     PRESSION DANS LES TUYAUX ET PRES_REP SUR D'AUTRES ELEMENTS
!     ON ALLOUE UNE SEULE CARTE CHAR//'.CHME.PRESS'
!
            call capres(char, ligrmo, noma, ndim, fonree)
            ialloc=1
        endif
        call cafotu(char, ligrmo, ialloc, noma, fonree)
    endif
!
!     MOT CLE FLUX_THM_REP :
!     ----------------------
    if (nbca2 .ne. 0) then
        call cafthm(char, ligrmo, noma, fonree)
    endif
!
!-----------------------------------------------------------------------
!
end subroutine
