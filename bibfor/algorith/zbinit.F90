subroutine zbinit(f0, coef, dimmem, mem)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: dimmem
    real(kind=8) :: f0, coef, mem(2, dimmem)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
!
! INITIALISATION DU COMMON
!
! ----------------------------------------------------------------------
!
!
! IN  F0     : VALEUR DE LA FONCTION EN X0
! IN  COEF   : COEFFICIENT D'AMPLIFICATION POUR LA RECHERCHE DE BORNE
! IN  DIMMEM : NOMBRE MAX DE COUPLES MEMORISES
! IN  MEM    : COUPLES MEMORISES
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhoneg, rhopos
    real(kind=8) :: parmul, fneg, fpos
    integer :: dimcpl, nbcpl
    logical :: bpos, lopti
    common /zbpar/ rhoneg,rhopos,&
     &               parmul,fneg  ,fpos  ,&
     &               dimcpl,nbcpl ,bpos  ,lopti
!
! ----------------------------------------------------------------------
!
    if (f0 .gt. 0) then
        call assert(.false.)
    endif
    parmul = coef
    dimcpl = dimmem
    mem(1,1) = 0.d0
    mem(2,1) = f0
    nbcpl = 1
    bpos = .false.
    rhoneg = 0.d0
    fneg = f0
    lopti = .false.
!
end subroutine
