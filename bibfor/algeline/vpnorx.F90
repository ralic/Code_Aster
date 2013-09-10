subroutine vpnorx(nbmode, neq, exclus, vecp, resufk)
    implicit   none
#include "asterfort/vecink.h"
#include "blas/dscal.h"
    integer :: nbmode, neq, exclus(*)
    real(kind=8) :: vecp(neq, *)
    character(len=*) :: resufk(*)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     NORMALISE A LA PLUS GRANDE DES VALEURS SUR UN DDL QUI N'EST PAS
!     EXCLUS
!     ------------------------------------------------------------------
!
    integer :: imode, ieq
    real(kind=8) :: normx, invx, absnx, rexc, arexc
    character(len=24) :: k24b
!
    k24b='SANS_CMP: LAGR'
    do 100 imode = 1, nbmode
        normx = vecp(1,imode)*exclus(1)
        absnx=abs(normx)
        do 110 ieq = 2, neq
            rexc=vecp(ieq,imode)*exclus(ieq)
            arexc=abs(rexc)
            if (absnx .lt. arexc) then
                normx = rexc
                absnx = arexc
            endif
110      continue
        if (normx .ne. 0.d0) then
            invx=1.d0/normx
            call dscal(neq, invx, vecp(1, imode), 1)
        endif
100  end do
    call vecink(nbmode, k24b, resufk)
!
end subroutine
