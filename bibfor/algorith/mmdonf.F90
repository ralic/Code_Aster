subroutine mmdonf(ndim, nno, alias, ksi1, ksi2,&
                  dff)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/assert.h'
    include 'asterfort/elrfdf.h'
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: dff(2, 9)
    integer :: nno, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! CALCUL DES DERIVEES PREMIERES DES FONCTIONS DE FORME EN UN POINT
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
! OUT DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
!
! ----------------------------------------------------------------------
!
    integer :: ibid, jbid, i
    real(kind=8) :: ksi(2)
    real(kind=8) :: d2f(3, 9)
    character(len=8) :: elrefe
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    do 10 i = 1, 9
        dff(1,i) = 0.d0
        dff(2,i) = 0.d0
        d2f(1,i) = 0.d0
        d2f(2,i) = 0.d0
        d2f(3,i) = 0.d0
10  end do
!
    ksi(1) = ksi1
    ksi(2) = ksi2
!
    elrefe = alias
!
    if ((nno.lt.1) .or. (nno.gt.9)) then
        call assert(.false.)
    endif
!
    if ((ndim.lt.1) .or. (ndim.gt.3)) then
        call assert(.false.)
    endif
!
! --- RECUP DERIVEES PREMIERES DES FONCTIONS DE FORME
!
    call elrfdf(elrefe, ksi, nno*ndim, d2f, ibid,&
                jbid)
!
! --- CONVERSION 3D -> 2D
!
    do 15 i = 1, nno
        dff(1,i) = d2f(1,i)
        dff(2,i) = d2f(2,i)
15  end do
!
end subroutine
