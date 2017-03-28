subroutine lcsmelas(fm    , df     , dtaudf, &
                    nmat  , materd_, young_, nu_)
!
implicit none
!
#include "blas/ddot.h"
#include "asterfort/assert.h"
#include "asterfort/verift.h"
#include "asterfort/gdsmin.h"
#include "asterfort/gdsmci.h"
#include "asterfort/gdsmtg.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    real(kind=8), intent(in) :: fm(3, 3)
    real(kind=8), intent(in) :: df(3, 3)
    real(kind=8), intent(out) :: dtaudf(6, 3, 3)
    integer, intent(in) :: nmat
    real(kind=8), optional, intent(in) :: materd_(nmat, 2)
    real(kind=8), optional, intent(in) :: young_
    real(kind=8), optional, intent(in) :: nu_
!
! --------------------------------------------------------------------------------------------------
!
! Elastic matrix for SIMO-MIEHE
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: young, nu, unk, troisk, mu
    integer :: ij, kl, k, l
    real(kind=8) :: dtaudj, dtaudb
    real(kind=8) :: dvbbtr(6, 6), dvbedf(6, 3, 3)
    real(kind=8) :: em(6)

! - Common for SIMO-MIEHE
    integer :: ind(3, 3), ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6), id(6, 6)
    real(kind=8) :: bem(6), betr(6), dvbetr(6), eqbetr, trbetr
    real(kind=8) :: jp, dj, jm, dfb(3, 3)
    real(kind=8) :: djdf(3, 3), dbtrdf(6, 3, 3)
!
    common /gdsmc/&
     &            bem,betr,dvbetr,eqbetr,trbetr,&
     &            jp,dj,jm,dfb,&
     &            djdf,dbtrdf,&
     &            kr,id,rac2,rc,ind,ind1,ind2
!
! --------------------------------------------------------------------------------------------------
!
    dtaudf(:,:,:) = 0.d0
    em(:)         = 0.d0
!
! - Some initialisations
!
    call gdsmin()
!
! - Kinematic
!
    call gdsmci(fm, df, em)
!
! - Material properties
!
    if (present(materd_)) then
        young  = materd_(1,1) 
        nu     = materd_(2,1)
    else
        young  = young_
        nu     = nu_
    endif
    troisk = young/(1.d0-2.d0*nu)
    mu     = young/(2.d0*(1.d0+nu))
    unk    = troisk/3.d0
!
! - Prepare matrix
!
    call gdsmtg()
!
    do ij = 1, 6
        do kl = 1, 6
            dvbbtr(ij,kl)= (id(ij,kl)-kr(ij)*kr(kl)/3.d0) 
        end do
    end do
    do ij = 1, 6
        do k = 1, 3
            do l = 1, 3
                dvbedf(ij,k,l) = ddot(6,dvbbtr(ij,1),6,dbtrdf(1,k,l), 1)
            end do
        end do
    end do
!
! - Matrice tangente
!
    dtaudb = mu
    dtaudj = 0.5d0*(2.d0*unk*jp)
    do ij = 1, 6
        do k = 1, 3
            do l = 1, 3
                dtaudf(ij,k,l) = dtaudb*dvbedf(ij,k,l) + dtaudj*kr(ij)*djdf(k,l)
            end do
        end do
    end do
    
!
end subroutine
