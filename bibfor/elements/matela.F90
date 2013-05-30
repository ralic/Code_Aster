subroutine matela(icodma, materi, itemp, temp, e,&
                  nu)
    implicit none
    include 'asterfort/rcvalb.h'
    real(kind=8) :: temp, e, nu
    integer :: icodma, itemp
    character(len=*) :: materi
!     ------------------------------------------------------------------
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
!
!     RECUPERATION DES VALEURS DE E, NU
!     FONCTION EVENTUELLEMENT DE LA TEMPERATURE TEMP
!     COMPORTEMENT : 'ELAS'
!     ------------------------------------------------------------------
! IN  ICODMA : IS  : ADRESSE DU MATERIAU CODE
! IN  ITEMP  : IS  : =0 : PAS DE TEMPERATURE
! IN  TEMP   : R8  : VALEUR DE LA TEMPERATURE
!
! OUT E      : R8  : MODULE D'YOUNG
! OUT NU     : R8  : COEFFICIENT DE POISSON
!
    integer :: nbres, nbpar, i
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter    (nbres=2)
    real(kind=8) :: valpar, valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar, nomres(nbres)
    data nomres / 'E', 'NU'/
!
    do 10 i = 1, nbres
        valres(i) = 0.d0
10  end do
    if (itemp .eq. 0) then
        nbpar = 0
        nompar = ' '
        valpar = 0.d0
    else
        nbpar = 1
        nompar = 'TEMP'
        valpar = temp
    endif
    call rcvalb('RIGI', 1, 1, '+', icodma,&
                materi, 'ELAS', nbpar, nompar, valpar,&
                2, nomres, valres, codres, 1)
    e = valres(1)
    nu = valres(2)
end subroutine
