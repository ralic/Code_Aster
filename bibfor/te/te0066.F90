subroutine te0066(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/rcangm.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIOBRE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'ETHE_ELEM'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    character(len=8) :: nompar, fami, poum, nomres(3)
    character(len=16) :: phenom
    integer :: icodre(3)
    real(kind=8) :: valpar, lambda(1), poids, epot, valres(3), lambor(3)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), flux, fluy, fluz
    real(kind=8) :: angmas(7), point(3), fluglo(3), fluloc(3), p(3, 3)
    integer :: i, ipoids, ivf, idfde, igeom, imate, kpg, spt, ino
    integer :: ndim, jgano, nno, kp, npg1, iener, itemp, itempe, l
    logical :: aniso
!
!-----------------------------------------------------------------------
    integer :: iret, nbpar, nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PENERDR', 'E', iener)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemp)
    if (itemp .eq. 0) then
        nbpar = 0
        nompar = ' '
        valpar = 0.d0
    else
        nbpar = 1
        nompar = 'INST'
        valpar = zr(itemp)
    endif
!
    call rccoma(zi(imate),'THER', 1, phenom, iret)
    if (phenom .ne. 'THER_ORTH') then
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER', nbpar, nompar, [valpar],&
                    1, 'LAMBDA', lambda, icodre, 1)
        aniso = .false.
    else
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        nomres(3) = 'LAMBDA_N'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER_ORTH', nbpar, nompar, [valpar],&
                    3, nomres, valres, icodre, 1)
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        lambor(3) = valres(3)
        aniso = .true.
    endif
!
!====
! PREALABLES LIES A L'ANISOTROPIE
!====
!
    epot = 0.d0
    do kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
        flux = 0.d0
        fluy = 0.d0
        fluz = 0.d0
        do i = 1, nno
            flux = flux + zr(itempe-1+i)*dfdx(i)
            fluy = fluy + zr(itempe-1+i)*dfdy(i)
            fluz = fluz + zr(itempe-1+i)*dfdz(i)
        enddo

        if (.not.aniso) then
            fluglo(1) = lambda(1)*flux
            fluglo(2) = lambda(1)*fluy
            fluglo(3) = lambda(1)*fluz
        else
            point(1) = 0.d0
            point(2) = 0.d0
            point(3) = 0.d0
            do ino = 1, nno
                point(1) = point(1) + zr(ivf+l+ino-1)*zr(igeom+3* ino-3)
                point(2) = point(2) + zr(ivf+l+ino-1)*zr(igeom+3* ino-2)
                point(3) = point(3) + zr(ivf+l+ino-1)*zr(igeom+3* ino-1)
            enddo
            call rcangm(ndim, point, angmas)
            call matrot(angmas, p)
            fluglo(1) = flux
            fluglo(2) = fluy
            fluglo(3) = fluz
            call utpvgl(1, 3, p, fluglo, fluloc)
            fluloc(1) = lambor(1)*fluloc(1)
            fluloc(2) = lambor(2)*fluloc(2)
            fluloc(3) = lambor(3)*fluloc(3)
            call utpvlg(1, 3, p, fluloc, fluglo)
        endif
!
        epot = epot - (flux*fluglo(1)+fluy*fluglo(2)+fluz*fluglo(3))*poids
!
    end do
    zr(iener) = epot/2.d0
end subroutine
