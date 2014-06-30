subroutine ptfocp(itype, option, nomte, xl, rad,&
                  angs2, nno, nc, pgl, pgl1,&
                  pgl2, fer, fei)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/ptfop1.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "blas/ddot.h"
    integer :: itype
    character(len=*) :: option, nomte
    real(kind=8) :: fer(12), fei(12), pgl(3, 3), pgl1(3, 3), pgl2(3, 3)
    real(kind=8) :: xl, rad, angs2
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
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: coef1, coef2, s, s2
    real(kind=8) :: zero, un, xxx
    real(kind=8) :: u(3), v(3), w(6), w2(3)
    real(kind=8) :: qr(12), qqr(12), qi(12), qqi(12)
    character(len=16) :: ch16
    logical(kind=1) :: global, normal
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     --- INITIALISATION  ---
!-----------------------------------------------------------------------
    integer :: i, icoec, icoer, iret, lforc, lx, nc
    integer :: ncc, nno, nnoc
    real(kind=8) :: s3, s4, s5
!-----------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
    do 10 i = 1, 12
        qr(i) = zero
        qqr(i) = zero
        fer(i) = zero
        qi(i) = zero
        qqi(i) = zero
        fei(i) = zero
10  end do
    nnoc = 1
    ncc = 6
    global = .false.
!
    if (option .eq. 'CHAR_MECA_FC1D1D') then
        call jevech('PGEOMER', 'L', lx)
        lx = lx-1
        do 20 i = 1, 3
            w(i) = zr(lx+i)
            w(i+3) = zr(lx+i+3)
            w2(i) = w(i+3) - w(i)
20      continue
!
!         --- FORCE POUTRE A VALEURS COMPLEXES ---
        call jevech('PFC1D1D', 'L', lforc)
        xxx = abs( dble( zc(lforc+6) ) )
        global = xxx .lt. 1.d-3
        normal = xxx .gt. 1.001d0
        do 30 i = 1, 3
            qr(i) = dble( zc(lforc-1+i) )
            qr(i+6) = qr(i)
            qi(i) = dimag( zc(lforc-1+i) )
            qi(i+6) = qi(i)
            xxx = abs( dble( zc(lforc-1+3+i) ) )
            if (xxx .gt. 1.d-20) then
                call utmess('F', 'ELEMENTS2_46')
            endif
30      continue
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
!        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE (SI NECESSAIRE) ---
        if (global .or. normal) then
            if (nomte .eq. 'MECA_POU_C_T') then
                call utpvgl(nnoc, ncc, pgl1, qr(1), qqr(1))
                call utpvgl(nnoc, ncc, pgl2, qr(7), qqr(7))
                call utpvgl(nnoc, ncc, pgl1, qi(1), qqi(1))
                call utpvgl(nnoc, ncc, pgl2, qi(7), qqi(7))
            else
                call utpvgl(nno, nc, pgl, qr(1), qqr(1))
                call utpvgl(nno, nc, pgl, qi(1), qqi(1))
            endif
        else
            do 40 i = 1, 12
                qqr(i) = qr(i)
                qqi(i) = qi(i)
40          continue
        endif
!
!        ---A CAUSE DES CHARGEMENTS VARIABLES ---
        coef1 = un
        coef2 = un
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
! *********************************************************************
!
!     --- RECUPERATION DU COEF_MULT ---
!
    call tecach('NNN', 'PCOEFFR', 'L', iret, iad=icoer)
    call tecach('NNN', 'PCOEFFC', 'L', iret, iad=icoec)
!
    if (icoer .ne. 0) then
        do 400 i = 1, 12
            qqr(i) = qqr(i) * zr(icoer)
            qqi(i) = qqi(i) * zr(icoer)
400      continue
!
    else if (icoec .ne. 0) then
        do 410 i = 1, 12
            qqr(i) = qqr(i) * dble( zc(icoec) )
            qqi(i) = qqi(i) * dimag( zc(icoec) )
410      continue
!
!
    endif
!
    call ptfop1(itype, coef1, coef2, xl, rad,&
                angs2, global, qqr, fer)
    call ptfop1(itype, coef1, coef2, xl, rad,&
                angs2, global, qqi, fei)
!
end subroutine
