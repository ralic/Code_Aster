subroutine hujpxs(mater, sig, vin, prox)
    implicit none
#include "asterf_types.h"
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
!   PASSAGE ENTRE LE SEUIL CYCLIQUE ET LE SEUIL MONOTONE
!   IN  MATER  :  COEFFICIENTS MATERIAU
!       VIN    :  VARIABLES INTERNES
!       SIG    :  TENSEUR DES CONTRAINTES
!
!   OUT PROX   = .TRUE. POUR PASSAGE CYCLIQUE - MONOTONE
!   ------------------------------------------------------------------
    integer :: ndt, ndi, i
    real(kind=8) :: mater(22, 2), r4, i1, sig(6), vin(*)
    real(kind=8) :: d, pco, beta, pc, epsvpm, dist
    real(kind=8) :: d13, zero, aexp, exptol, rh
    aster_logical :: prox
!
    common /tdim/   ndt , ndi
!
    data      d13, zero  /0.333333333334d0, 0.d0/
!
    d = mater(3,2)
    pco = mater(7,2)
    beta = mater(2,2)
    rh = vin(4)
    epsvpm = vin(23)
!
    exptol = log(1.d+20)
    exptol = min(exptol, 40.d0)
    aexp = -beta*epsvpm
!
    if (aexp .ge. exptol) write(6,'(A)') 'HUJPXS :: PB!!'
!
    pc = pco*exp(-beta*epsvpm)
!
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + d13*sig(i)
 10 continue
!
    r4 = abs(i1)/abs(d*pc)
!
    dist = abs(r4-rh)/rh
!       WRITE(6,*)'RMOB =',R4,' --- RH =',RH
!        WRITE(6,*)'DIST =',(R4-RH)/RH
    if (dist .lt. 1.d-4) then
        prox = .true.
    else
        prox = .false.
    endif
!        WRITE(6,*)'PROX =',PROX
!
end subroutine
