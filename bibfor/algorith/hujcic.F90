subroutine hujcic(mater, sig, vin, seuil)
    implicit none
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
!       ---------------------------------------------------------------
!       HUJEUX:  SEUIL DU MECANISME DE CONSOLIDATION CYCLIQUE
!       FIC = ABS( I1 + D*PC*X4) + R4*D*PC
!       ---------------------------------------------------------------
!       IN  MATER  :  COEFFICIENTS MATERIAU
!           SIG    :  CHAMPS DE CONTRAINTES
!           VIN    :  VARIABLES INTERNES
!       OUT SEUIL  :  SEUIL DU MECANISME CYCLIQUE DE CONSOLIDATION
!       ---------------------------------------------------------------
    include 'asterc/r8maem.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, i
    real(kind=8) :: mater(22, 2), r4c, i1, sig(6), vin(*)
    real(kind=8) :: d, pco, beta, seuil, pc, epsvpm
    real(kind=8) :: d13, zero, aexp, exptol, x4
!
    common /tdim/   ndt , ndi
!
    data      d13, zero  /0.333333333334d0, 0.d0/
!
    d = mater(3,2)
    pco = mater(7,2)
    beta = mater(2,2)
    r4c = vin(8)
    epsvpm = vin(23)
    x4 = vin(21)
!
    exptol = log(r8maem())
    exptol = min(exptol, 40.d0)
    aexp = -beta*epsvpm
    if (aexp .ge. exptol) call u2mess('F', 'COMPOR1_7')
!
    pc = pco*exp(-beta*epsvpm)
!
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + d13*sig(i)
10  continue
!
    seuil = - abs(abs(i1)+d*pc*x4)/(d*pc) - r4c
!
!
end subroutine
