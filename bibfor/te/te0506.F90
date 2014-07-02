subroutine te0506(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/foderi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_FLUTNL'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
    character(len=8) :: coef
    real(kind=8) :: poids, r, tpg
    real(kind=8) :: alpha, alphap, nx, ny
    integer :: nno, nnos, jgano, ndim, kp, npg, i, k, itemps, itemp, itempi
    integer :: iflux
    integer :: ipoids, ivf, idfde, igeom
    integer :: iveres
    aster_logical :: laxi
!
!
    call jemarq()
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PRESIDU', 'E', iveres)
!
    coef = zk8(iflux)
    if (coef(1:7) .eq. '&FOZERO') goto 40
!
!
    do 30 kp = 1, npg
        k = (kp-1)*nno
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        r = 0.d0
        tpg = 0.d0
        do 10 i = 1, nno
            r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
            tpg = tpg + zr(itempi+i-1)*zr(ivf+k+i-1)
 10     continue
        call foderi(coef, tpg, alpha, alphap)
        if (laxi) poids = poids*r
!
!
        do 20 i = 1, nno
            zr(iveres+i-1) = zr(iveres+i-1) + poids*zr(ivf+k+i-1)* ( alpha-alphap*tpg)
 20     continue
 30 end do
 40 continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
