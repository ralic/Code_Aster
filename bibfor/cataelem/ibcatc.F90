subroutine ibcatc(type, iunit, ier)
    implicit none
    include 'asterfort/ibcael.h'
    include 'asterfort/inccat.h'
    include 'asterfort/lccata.h'
    character(len=*) :: type
    integer :: iunit, ier
!     -----------------------------------------------------------------
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
! MAIN DE LA LECTURE DES CATALOGUE
!     -----------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (type .eq. 'CATAELEM') then
        call inccat()
        call lccata(iunit)
    else if (type .eq. 'ELEMBASE') then
        call ibcael('LIRE')
    endif
!
    ier=0
end subroutine
