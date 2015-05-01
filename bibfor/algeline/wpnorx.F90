subroutine wpnorx(nbmode, neq, exclus, vecp, resufk)
    implicit   none
#include "asterc/r8miem.h"
    integer :: nbmode, neq, exclus(*)
    complex(kind=8) :: vecp(neq, nbmode)
    character(len=*) :: resufk(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     NORMALISE A LA PLUS GRANDE DES VALEURS SUR UN DDL QUI N'EST PAS
!     EXCLUS
!     ------------------------------------------------------------------
! IN  NBMODE : I : NOMBRE  DE  MODE
! IN  NEQ    : I : TAILLE  DES MODES
! VAR VECP   : C : MATRICE DES MODES
! IN  EXCLUS : I : TABLE   DES DDL EXCLUS (0 <=> EXCLUS)
!     ------------------------------------------------------------------
    integer :: imode, ieq
    complex(kind=8) :: normx, zero
    real(kind=8) :: prec
!     ------------------------------------------------------------------
!
    prec=r8miem()*10.d0
    zero = dcmplx(0.0d0,0.0d0)
    do 100 imode = 1, nbmode
        normx = zero
        do 110 ieq = 1, neq
            if (abs(vecp(ieq,imode)*exclus(ieq)) .gt. abs(normx)) then
                normx = vecp(ieq,imode)
            endif
110      continue
        if (abs(normx) .gt. prec) then
            normx = 1.d0 / normx
            do 120 ieq = 1, neq
                vecp(ieq,imode) = vecp(ieq,imode) * normx
120          continue
        endif
        resufk(imode) = 'SANS_CMP: LAGR'
100  end do
!
end subroutine
