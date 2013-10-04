subroutine mmstaf(noma, ndim, chdepd, coef_augm_frot, lpenaf,&
                  nummae, aliase, nne, nummam, ksipc1,&
                  ksipc2, ksipr1, ksipr2, mult_lagr_f1, mult_lagr_f2,&
                  tang_1, tang_2, norm, indi_cont, indi_frot,&
                  pres_frot,dist_frot)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/matini.h"
#include "asterfort/mcopco.h"
#include "asterfort/mmvalp.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
    character(len=8) :: noma, aliase
    integer :: ndim, nne
    real(kind=8) :: ksipc1, ksipc2
    real(kind=8) :: ksipr1, ksipr2
    integer :: nummae, nummam
    character(len=19) :: chdepd
    real(kind=8) :: tang_1(3), tang_2(3), norm(3)
    real(kind=8) :: coef_augm_frot
    logical :: lpenaf
    integer :: indi_frot, indi_cont
    real(kind=8) :: mult_lagr_f1(9), mult_lagr_f2(9)
    real(kind=8) :: pres_frot(3),dist_frot(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! STATUT DU FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! In  noma           : name of mesh
! In  ndim           : space size
! In  chdepd         : cumulated displacement
! In  coef_augm_frot : augmented ratio for friction
! In  lpenaf         : .true. if penalized friction
! In  nummae         : number of slave element
! In  aliase         : type of slave element
! In  nne            : number of nodes of slave element
! In  nummam         : number of master element
! In  kspic1         : first parametric coord. of contact point (in slave element)
! In  kspic2         : second parametric coord. of contact point (in slave element)
! In  kspir1         : first parametric coord. of projection of contact point (in master element)
! In  kspir2         : second parametric coord. of projection of contact point (in master element)
! In  mult_lagr_f1   : first lagrange multiplier for friction at nodes
! In  mult_lagr_f2   : second lagrange multiplier for friction at nodes
! In  tang_1         : first tangent vector
! In  tang_2         : second tangent vector
! In  norm           : normal
! In  indi_cont      : contact status
! Out indi_frot      : friction status
! Out pres_frot      : friction "pressure"
! Out dist_frot      : tangent distance
!
! ----------------------------------------------------------------------
!
    integer :: idim, idim1, idim2
    real(kind=8) :: nrese
    real(kind=8) :: dlagrf(2), dist_total(3)
    real(kind=8) :: ddeple(3), ddeplm(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: lagr_augm_frot(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nrese = 0.d0
    indi_frot = 0
    do idim = 1, 3
        lagr_augm_frot(idim) = 0.d0
        dist_frot(idim) = 0.d0
        dist_total(idim) = 0.d0
        pres_frot(idim) = 0.d0
    end do
    dlagrf(1) = 0.d0
    dlagrf(2) = 0.d0
    call matini(3, 3, 0.d0, mprojt)
    if (indi_cont .eq. 0) goto 99
!
! - Tangent projection matrix
!
    do idim1 = 1, ndim
        do idim2 = 1, ndim
            mprojt(idim1,idim2) = -1.d0*norm(idim1)*norm(idim2)
        end do
    end do
    do idim1 = 1, ndim
        mprojt(idim1,idim1) = 1.d0 + mprojt(idim1,idim1)
    end do
!
! - Lagrange multiplier for friction at current contact point
!
    call mmvalp(ndim, aliase, nne, 1, ksipc1,&
                ksipc2, mult_lagr_f1, dlagrf(1))
    if (ndim .eq. 3) then
        call mmvalp(ndim, aliase, nne, 1, ksipc1,&
                    ksipc2, mult_lagr_f2, dlagrf(2))
    endif
!
! - Displacement increment
!
    call mcopco(noma, chdepd, ndim, nummae, ksipc1,&
                ksipc2, ddeple)
    call mcopco(noma, chdepd, ndim, nummam, ksipr1,&
                ksipr2, ddeplm)
!
! - Gap increment
!
    do idim = 1, 3
        dist_total(idim) = ddeple(idim) - ddeplm(idim)
    end do
!
! - Projection of gap increment on tangent plane
!
    do idim1 = 1, ndim
        do idim2 = 1, ndim
            dist_frot(idim1) = mprojt(idim1,idim2)*dist_total(idim2)+dist_frot(idim1)
        end do
    end do
!
! - Friction "pressure"
!
    if (ndim .eq. 2) then
        do idim = 1, 2
            pres_frot(idim) = dlagrf(1)*tang_1(idim)
        end do
    else if (ndim.eq.3) then
        do idim = 1, 3
            pres_frot(idim) = dlagrf(1)*tang_1(idim) + dlagrf(2)*tang_2(idim)
        end do
    else
        ASSERT(.false.)
    endif 
!
! - Semi-multiplicator for friction SEMI-MULTIPLICATEUR DE FROTTEMENT
!
    if (lpenaf) then
        do idim = 1, 3
            lagr_augm_frot(idim) = coef_augm_frot*dist_frot(idim)
        end do
    else
        do idim = 1, 3
            lagr_augm_frot(idim) = pres_frot(idim) + coef_augm_frot*dist_frot(idim)
        end do
    endif
!
! - Friction ratio
!
    do idim = 1, 3
        nrese = lagr_augm_frot(idim)*lagr_augm_frot(idim) + nrese
    end do
    nrese = sqrt(nrese)
!
! - Normalization
!
    if (nrese.ne.0.d0) then
      do idim = 1,3
        lagr_augm_frot(idim) = lagr_augm_frot(idim)/nrese
      end do
    endif
!
! - Sliding or not ?
!
    if (nrese .le. 1.d0) then
        indi_frot = 0
    else
        indi_frot = 1
    endif
!
99  continue
!
    call jedema()
end subroutine
