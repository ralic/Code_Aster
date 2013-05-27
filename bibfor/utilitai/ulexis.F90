function ulexis(iul)
    implicit none
    logical :: ulexis
    include 'asterfort/codent.h'
    include 'asterfort/ulopen.h'
    integer :: iul, i, unit
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     VERIFIE QUE L'UNITE LOGIQUE EST RATTACHE A UN FICHIER
!
!     ------------------------------------------------------------------
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
    logical :: ficexi
    character(len=8) :: k8b
    character(len=255) :: namell
!     ------------------------------------------------------------------
!
    ulexis = .false.
!
    do 10 i = 1, nbfile
        unit = unitfi(i)
        if (unit .eq. iul) then
            ulexis = .true.
            goto 12
        endif
10  end do
    call codent(iul, 'G', k8b)
    namell = 'fort.'//k8b
    inquire(file=namell,exist=ficexi)
    if (ficexi) then
        call ulopen(iul, ' ', ' ', 'A', 'O')
        ulexis = .true.
    endif
12  continue
!
end function
