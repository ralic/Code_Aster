subroutine te0082(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/grdthm.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/pmavec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vecma.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - CALCULE DES MATRICES ELEMENTAIRES
!                          OPTION : 'MASS_MECA'
!    - CALCULE DES VECTEURS ELEMENTAIRES
!                          OPTION : 'M_GAMMA'
!    - CALCULE DES GRANDEURS ELEMENTAIRES
!                          OPTION : 'ECIN_ELEM'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=16) :: phenom
    character(len=3) :: stopz
    integer :: icodre(1)
!      CHARACTER*4        FAMI
    real(kind=8) :: valres(1), poids, r, vfi, vfj
    real(kind=8) :: matp(18, 18), matv(171), masvit(18), masdep(18)
    real(kind=8) :: vect1(18), vect2(18)
    integer :: nno, kp, nnos, npg2, ii, jj, i, j, k, imatuu, jgano
    integer :: l, n1, n2, i2, j2
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: kd1, kd2, ij1, ij2, nddl, nvec, iacce, ivect, ndim
    integer :: idepl, ivite, ifreq, iecin
    integer :: mecani(5), press1(7), press2(7), tempe(5), ibi
    integer :: idec, iret
! ......................................................................
!
    call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    nddl = 2 * nno
    nvec = nddl * ( nddl + 1 ) / 2
    press1(1) = 0
    press2(1) = 0
    tempe(1) = 0
    call grdthm(nomte, .false., .false., 2, mecani,&
                press1, press2, tempe, ibi, ibi,&
                ibi, ibi, ibi, ibi)
    idec = press1(1) + press2(1) + tempe(1)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
    call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', [0.d0],&
                1, 'RHO', valres, icodre(1), 1)
!
    do k = 1, nvec
        matv(k) = 0.0d0
    end do
!
    do kp = 1, npg2
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids)
        if (lteatt('AXIS','OUI')) then
            r = 0.0d0
            do i = 1, nno
                r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
        poids = poids*valres(1)
!
        kd1 = 2
        kd2 = 1
        do i = 1, 2*nno, 2
            kd1 = kd1+2*i-3
            kd2 = kd2+2*i-1
            ii = (i+1)/2
            do j = 1, i, 2
                jj = (j+1)/2
                ij1 = kd1+j-1
                ij2 = kd2+j-1
                vfi = zr(ivf+k+ii-1)
                vfj = zr(ivf+k+jj-1)
                matv(ij1 ) = matv(ij1 ) + poids*vfi*vfj
                matv(ij2+1) = matv(ij2+1) + poids*vfi*vfj
            end do
        end do
    end do
!
    if (option .eq. 'MASS_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
        if (idec .eq. 0) then
            do i = 1, nvec
                zr(imatuu+i-1) = matv(i)
            end do
        else
            do k = 1, nno
                do n1 = 1, 2
                    i = 2*k+n1-2
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    do l = 1, nno
                        do n2 = 1, 2
                            j = 2*l+n2-2
                            if (j .gt. i) goto 105
                            if (l .le. nnos) then
                                j2 = j+idec*(l-1)
                            else
                                j2 = j+idec*nnos
                            endif
                            zr(imatuu+i2*(i2-1)/2+j2-1) = matv(i*(i-1) /2+j)
                        end do
                    end do
105                 continue
                end do
            end do
        endif
!
    else if (option .eq. 'M_GAMMA') then
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        call vecma(matv, nvec, matp, nddl)
        if (idec .eq. 0) then
            call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
        else
            do k = 1, nddl
                vect1(k) = 0.0d0
                vect2(k) = 0.0d0
            end do
            do k = 1, nno
                do n1 = 1, 2
                    i = 2*k+n1-2
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    vect1(i) = zr(iacce+i2-1)
                end do
            end do
            call pmavec('ZERO', nddl, matp, vect1, vect2)
            do k = 1, nno
                do n1 = 1, 2
                    i = 2*k+n1-2
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    zr(ivect+i2-1) = vect2(i)
                end do
            end do
        endif
!
! OPTION ECIN_ELEM : CALCUL DE L'ENERGIE CINETIQUE
!
    else if (option .eq. 'ECIN_ELEM') then
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iret, iad=ivite)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
            call jevech('PENERCR', 'E', iecin)
            call vecma(matv, nvec, matp, nddl)
            call pmavec('ZERO', nddl, matp, zr(ivite), masvit)
            zr(iecin) = .5d0*ddot(nddl,zr(ivite),1,masvit,1)
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=idepl)
            if (iret .eq. 0) then
                call jevech('PENERCR', 'E', iecin)
                call jevech('POMEGA2', 'L', ifreq)
                call vecma(matv, nvec, matp, nddl)
                call pmavec('ZERO', nddl, matp, zr(idepl), masdep)
                zr(iecin) = .5d0*ddot(nddl,zr(idepl),1,masdep,1)*zr( ifreq)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
!
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
end subroutine
