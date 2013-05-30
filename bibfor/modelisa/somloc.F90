subroutine somloc(m, adco, nbso, nusglo, nusloc)
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
!     DONNE LE NUMERO LOCAL : NUSLOC DU SOMMET DONT LE
!     NUMERO GLOBAL EST NUSGLO POUR LA MAILLE M
!     LA MAILLE A NBSO SOMMETS ET L ADRESSE DE SA CONNECTIVITE
!     EST ADCO
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/u2mesi.h'
    integer :: m, adco, nbso, nusglo, nusloc
!
    logical :: trouve
    integer :: is
    integer :: vali(2)
!
    trouve=.false.
    do 10 is = 1, nbso
        if (zi(adco+is-1) .eq. nusglo) then
            trouve=.true.
            nusloc=is
        endif
10  end do
    if (.not.trouve) then
        vali(1)=nusglo
        vali(2)=m
        call u2mesi('F', 'VOLUFINI_2', 2, vali)
    endif
!
end subroutine
