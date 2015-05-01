subroutine d0mpfn(zimat, nmnbn, nmdpla)
    implicit none
! ======================================================================
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
!======================================================================
!
!     RECUPERE LES VALEURS DES DERIVEES DES MOMENTS LIMITES DE PLAST
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
!
! OUT NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
!
#include "asterfort/cdnfon.h"
    integer :: i, ier0, ier1, zimat
!
    real(kind=8) :: nmnbn(6)
    real(kind=8) :: nmdpla(2, 2)
!
    character(len=8) :: nomres(4), domres(4)
!
    nomres(1) = 'FMEX1'
    nomres(2) = 'FMEX2'
    nomres(3) = 'FMEY1'
    nomres(4) = 'FMEY2'
!
    domres(1) = 'DFMEX1'
    domres(2) = 'DFMEX2'
    domres(3) = 'DFMEY1'
    domres(4) = 'DFMEY2'
!
    do 10, i=1,2
    call cdnfon(zimat, domres(2*(i-1)+1), nmnbn(i), 0, nmdpla(1, i),&
                ier0)
!
    if (ier0 .gt. 0) then
        call cdnfon(zimat, nomres(2*(i-1)+1), nmnbn(i), 1, nmdpla(1, i),&
                    ier1)
    endif
!
    call cdnfon(zimat, domres(2*i), nmnbn(i), 0, nmdpla(2, i),&
                ier0)
!
    if (ier0 .gt. 0) then
        call cdnfon(zimat, nomres(2*i), nmnbn(i), 1, nmdpla(2, i),&
                    ier1)
    endif
    10 end do
!
end subroutine
