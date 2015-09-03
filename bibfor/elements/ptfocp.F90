subroutine ptfocp(itype, option, xl, nno, nc, pgl, fer, fei)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    integer :: itype, nno, nc
    character(len=*) :: option
    real(kind=8) :: fer(12), fei(12), pgl(3, 3)
    real(kind=8) :: xl
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/ptfop1.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, icoec, icoer, iret, lforc, lx, ncc, nnoc
    real(kind=8) :: coef1, coef2, s, s2, xxx, s3, s4, s5
    real(kind=8) :: u(3), v(3), w(6), w2(3)
    real(kind=8) :: qr(12), qqr(12), qi(12), qqi(12)
    character(len=16) :: ch16
    aster_logical :: global, normal
!
! --------------------------------------------------------------------------------------------------
!
    qr(:) = 0.0d0
    qqr(:) = 0.0d0
    fer(1:12) = 0.0d0
    qi(:) = 0.0d0
    qqi(:) = 0.0d0
    fei(1:12) = 0.0d0
    nnoc = 1
    ncc = 6
    global = .false.
!
    if (option .eq. 'CHAR_MECA_FC1D1D') then
        call jevech('PGEOMER', 'L', lx)
        lx = lx-1
        do i = 1, 3
            w(i)   = zr(lx+i)
            w(i+3) = zr(lx+i+3)
            w2(i)  = w(i+3) - w(i)
        enddo
!
!       force poutre à valeurs complexes
        call jevech('PFC1D1D', 'L', lforc)
        xxx = abs( dble( zc(lforc+6) ) )
        global = xxx .lt. 1.d-3
        normal = xxx .gt. 1.001d0
        do i = 1, 3
            qr(i)   = dble( zc(lforc-1+i) )
            qr(i+6) = qr(i)
            qi(i)   = dimag( zc(lforc-1+i) )
            qi(i+6) = qi(i)
            xxx = abs( dble( zc(lforc-1+3+i) ) )
            if (xxx .gt. 1.d-20) then
                call utmess('F', 'ELEMENTS2_46')
            endif
        enddo
!
        if (normal) then
            s=ddot(3,w2,1,w2,1)
            s2=1.d0/s
            call provec(w2, qr(1), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s=ddot(3,qr(1),1,qr(1),1)
            s4 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, qr(1))
            call provec(w2, qr(7), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s=ddot(3,qr(7),1,qr(7),1)
            s4 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, qr(7))
!
            s=ddot(3,w2,1,w2,1)
            s2=1.d0/s
            call provec(w2, qi(1), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s=ddot(3,qi(1),1,qi(1),1)
            s4 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, qi(1))
            call provec(w2, qi(7), u)
            s=ddot(3,u,1,u,1)
            s3 = sqrt(s)
            s=ddot(3,qi(7),1,qi(7),1)
            s4 = sqrt(s)
            s5 = s3*sqrt(s2)/s4
            call provec(u, w2, v)
            call pscvec(3, s2, v, u)
            call pscvec(3, s5, u, qi(7))
        endif
!       passage repere local du vecteur force (si necessaire)
        if (global .or. normal) then
            call utpvgl(nno, nc, pgl, qr(1), qqr(1))
            call utpvgl(nno, nc, pgl, qi(1), qqi(1))
        else
            do i = 1, 12
                qqr(i) = qr(i)
                qqi(i) = qi(i)
            enddo
        endif
!       a cause des chargements variables
        coef1 = 1.0d0
        coef2 = 1.0d0
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
! --------------------------------------------------------------------------------------------------
!   recuperation du coef_mult
    call tecach('NNO', 'PCOEFFR', 'L', iret, iad=icoer)
    call tecach('NNO', 'PCOEFFC', 'L', iret, iad=icoec)
!
    if (icoer .ne. 0) then
        do i = 1, 12
            qqr(i) = qqr(i) * zr(icoer)
            qqi(i) = qqi(i) * zr(icoer)
        enddo
    else if (icoec .ne. 0) then
        do i = 1, 12
            qqr(i) = qqr(i) * dble( zc(icoec) )
            qqi(i) = qqi(i) * dimag( zc(icoec) )
        enddo
    endif
!
    call ptfop1(itype, coef1, coef2, xl, qqr, fer)
    call ptfop1(itype, coef1, coef2, xl, qqi, fei)
!
end subroutine
