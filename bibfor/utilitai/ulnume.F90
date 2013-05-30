function ulnume()
    implicit none
    integer :: ulnume
!     ------------------------------------------------------------------
!     RETOURNE UN NUMERO D'UNITE LOGIQUE NON UTILISE
!              -1 SI AUCUN DE DISPONIBLE
!     LA RECHERCHE EST LIMITEE A L'INTERVALLE 70-99
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
!
    include 'asterfort/u2mess.h'
    include 'asterfort/ulinit.h'
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    integer :: i, ival, k
!
    if (first .ne. 17111990) call ulinit()
!
    ival = -1
    do 1 i = 99, 70, -1
        do 10 k = 1, nbfile
            if (unitfi(k) .eq. i) then
                goto 1
            endif
10      continue
        ival = i
        goto 2
 1  end do
    call u2mess('A', 'UTILITAI5_10')
 2  continue
    ulnume = ival
end function
