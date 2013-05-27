subroutine hujrmo(mater, sig, vin, riso)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   ------------------------------------------------------------------
!   CALCUL DE LA VALEUR DE RISO = PISO/(D*PCO*EXP(-BETA*EPSVP))
!   IN  MATER  :  COEFFICIENTS MATERIAU A T+DT
!       VIN    :  VARIABLES INTERNES  A T
!       SIG    :  CONTRAINTE A T+DT
!
!   OUT RISO   : FACTEUR DE MOBILISATION ACTUEL DU MECANISME ISOTROPE
!   ------------------------------------------------------------------
    integer :: ndt, ndi, i
    real(kind=8) :: mater(22, 2), riso, i1, sig(6), vin(*)
    real(kind=8) :: d, pco, beta, pc, epsvpm
    real(kind=8) :: d13, zero, aexp, exptol
!
    common /tdim/   ndt , ndi
!
    data      d13, zero  /0.333333333334D0, 0.d0/
!
    d = mater(3,2)
    pco = mater(7,2)
    beta = mater(2,2)
    epsvpm = vin(23)
!
    exptol = log(1.d+20)
    exptol = min(exptol, 40.d0)
    aexp = -beta*epsvpm
!
    if (aexp .ge. exptol) write(6,'(A)') 'HUJRMO :: PB!!'
!
    pc = pco*exp(-beta*epsvpm)
!
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + d13*sig(i)
10  continue
!
    riso = abs(i1)/abs(d*pc)
!
end subroutine
