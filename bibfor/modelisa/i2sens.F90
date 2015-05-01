subroutine i2sens(chemin, nbrma2, limail, nbrma, connex,&
                  typmai, abscis)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
#include "asterfort/assert.h"
#include "asterfort/i2extf.h"
    integer :: nbrma, nbrma2
    integer :: chemin(nbrma2), limail(nbrma)
    character(len=*) :: connex, typmai
    real(kind=8) :: abscis(2), delta
!
!-----------------------------------------------------------------------
    integer :: j, mi, mj, nid, nig, njd
    integer :: njg
!-----------------------------------------------------------------------
!
    mi = limail(chemin(1))
    call i2extf(mi, 1, connex, typmai, nig,&
                nid)
    delta = abscis(2)-abscis(1)
    mi = int(sign(1.d0*mi,delta))
    chemin(1) = mi

    do j = 2, nbrma
        mj = limail(chemin(j))
        call i2extf(mj, 1, connex, typmai, njg,&
                    njd)
!
        if ((nid .eq. njd) .or. (nig .eq. njg)) then
            mj = -mj*mi/abs(mi) 
        endif
!
        mi = mj
        nig = njg
        nid = njd
        chemin(j) = mi
    end do
end subroutine
