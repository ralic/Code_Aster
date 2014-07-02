subroutine te0220(option, nomte)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
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
! ......................................................................
!    - FONCTION REALISEE:
!                         CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIBRE
!                         OPTION : 'ETHE_ELEM'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: icodre(2), kpg, spt
    character(len=8) :: nompar, fami, poum, nomres(2)
    character(len=16) :: phenom
    real(kind=8) :: valres(2), valpar
    real(kind=8) :: dfdx(9), dfdy(9), poids, flux, fluy, epot
    real(kind=8) :: angmas(7), rbid(3), fluglo(2), fluloc(2), p(2, 2)
    integer :: ndim, nno, nnos, npg, kp, j, itempe, itemp, iener
    integer :: ipoids, ivf, idfde, jgano, igeom, imate, iret, nbpar
    aster_logical :: aniso
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PENERDR', 'E', iener)
!
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
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rccoma(zi(imate), 'THER', 1, phenom, iret)
    if (phenom .ne. 'THER_ORTH') then
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER', nbpar, nompar, [valpar],&
                    1, 'LAMBDA', valres, icodre, 1)
        aniso = .false.
    else
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THER_ORTH', nbpar, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
        aniso = .true.
!       pas de repere cylindrique en 2d -> rbid
        call rcangm(ndim, rbid, angmas)
        p(1,1) = cos(angmas(1))
        p(2,1) = sin(angmas(1))
        p(1,2) = -sin(angmas(1))
        p(2,2) = cos(angmas(1))
    endif
!
    epot = 0.d0
    do kp = 1, npg
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
        flux = 0.d0
        fluy = 0.d0
        do j = 1, nno
            flux = flux + zr(itempe+j-1)*dfdx(j)
            fluy = fluy + zr(itempe+j-1)*dfdy(j)
        enddo
        if (.not.aniso) then
            fluglo(1) = valres(1)*flux
            fluglo(2) = valres(1)*fluy
        else
            fluglo(1) = flux
            fluglo(2) = fluy
!
            fluloc(1) = p(1,1)*fluglo(1) + p(2,1)*fluglo(2)
            fluloc(2) = p(1,2)*fluglo(1) + p(2,2)*fluglo(2)
!
            fluloc(1) = valres(1)*fluloc(1)
            fluloc(2) = valres(2)*fluloc(2)
!
            fluglo(1) = p(1,1)*fluloc(1) + p(1,2)*fluloc(2)
            fluglo(2) = p(2,1)*fluloc(1) + p(2,2)*fluloc(2)
        endif
!
        epot = epot - (flux*fluglo(1)+fluy*fluglo(2))*poids
    enddo
    zr(iener) = epot/ 2.d0
!
end subroutine
