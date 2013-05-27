subroutine convi4(vari, vari4, nb)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_4
!
!     UTILITAIRE DE CONVERSION DE TABLEAUX INTEGER <---> INTEGER*4
!
!     SI NB > 0, REMPLIT VARI4 AVEC LES VALEURS DE VARI
!     SI NB < 0, REMPLIT VARI AVEC LES VALEURS DE VARI4
!
    implicit none
    integer :: nb
    integer :: vari(nb)
    integer(kind=4) :: vari4(nb)
    integer :: i
    if (nb .gt. 0) then
        do 10 i = 1, nb
            vari4(i) = vari(i)
10      continue
    else
        do 11 i = 1, -nb
            vari(i) = vari4(i)
11      continue
    endif
end subroutine
