subroutine mm2onf(ndim, nno, alias, ksi1, ksi2,&
                  ddff)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/assert.h"
#include "asterfort/elrfd2.h"
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: ddff(3, 9)
    integer :: nno, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! CALCUL DES DERIVEES SECONDES DES FONCTIONS DE FORME EN UN POINT
! DE L'ELEMENT DE REFERENCE
!
! ----------------------------------------------------------------------
!
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  NNO    : NOMBRE DE NOEUD DE L'ELEMENT
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  KSI1   : POINT DE CONTACT SUIVANT KSI1 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! IN  KSI2   : POINT DE CONTACT SUIVANT KSI2 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! OUT DDFF   : DERIVEES SECONDES DES FONCTIONS DE FORME EN XI YI
!
! ----------------------------------------------------------------------
!
    integer :: ibid1, ibid2, i
    real(kind=8) :: ksi(2)
    real(kind=8) :: d2ff(3, 3, 9)
    character(len=8) :: elrefe
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    do 10 i = 1, 9
        ddff(1,i) = 0.d0
        ddff(2,i) = 0.d0
        ddff(3,i) = 0.d0
        d2ff(1,1,i) = 0.d0
        d2ff(1,2,i) = 0.d0
        d2ff(1,3,i) = 0.d0
        d2ff(2,1,i) = 0.d0
        d2ff(2,2,i) = 0.d0
        d2ff(2,3,i) = 0.d0
        d2ff(3,1,i) = 0.d0
        d2ff(3,2,i) = 0.d0
        d2ff(3,3,i) = 0.d0
10  end do
!
    ksi(1) = ksi1
    ksi(2) = ksi2
!
    elrefe = alias
!
    if ((nno.lt.1) .or. (nno.gt.9)) then
        ASSERT(.false.)
    endif
!
    if ((ndim.lt.1) .or. (ndim.gt.3)) then
        ASSERT(.false.)
    endif
!
! --- RECUP DERIVEES SECONDES DES FONCTIONS DE FORME
!
    call elrfd2(elrefe, ksi, nno*ndim*ndim, d2ff, ibid1,&
                ibid2)
!
! --- CONVERSION XI-YI/YI-XI -> KSI1-KSI2
!
    do 16 i = 1, nno
        ddff(1,i) = d2ff(1,1,i)
        ddff(2,i) = d2ff(2,2,i)
        ddff(3,i) = d2ff(1,2,i)
16  end do
!
end subroutine
