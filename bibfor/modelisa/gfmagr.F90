subroutine gfmagr(noma, nomgrf, nbfigr)
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
!     ------------------------------------------------------------------
!
    implicit none
!     IN
    include 'jeveux.h'
!
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jexnom.h'
    integer :: nbfigr
    character(len=8) :: noma
    character(len=24) :: nomgrf
!
!     ------------------------------------------------------------------
!     CREATION D'UN GROUPE DE MAILLES QUI CONTIENDRA LES MAILLES D'UN
!     GROUPE DE FIBRES (MOT CLE SECTION ET FIBRE)
!     (DEFI_GEOM_FIBRE)
!
! ----- DECLARATIONS
!
    character(len=24) :: grpmai
!
    call jemarq()
!
    grpmai = noma// '.GROUPEMA       '
!
    call jecroc(jexnom(grpmai, nomgrf))
    call jeecra(jexnom(grpmai, nomgrf), 'LONMAX', nbfigr, ' ')
    call jeecra(jexnom(grpmai, nomgrf), 'LONUTI', nbfigr, ' ')
!
!
    call jedema()
end subroutine
