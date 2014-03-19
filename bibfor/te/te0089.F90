subroutine te0089(option, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_PRES_F  '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, jgano, nbres, ndim, nnos
    real(kind=8) :: z
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nompar(nbres)
    real(kind=8) :: valpar(nbres), poids, r, tx, ty, nx, ny
    real(kind=8) :: pres, cisa
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: itemps, ipres, ivectu, i, l
    logical :: laxi
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPRESSF', 'L', ipres)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTUR', 'E', ivectu)
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
    valpar(3) = zr(itemps)
!
    do 30 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        r = 0.d0
        z = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r = r + zr(igeom+2*i-2)*zr(ivf+l-1)
            z = z + zr(igeom+2*i-1)*zr(ivf+l-1)
10      continue
        if (laxi) poids = poids*r
        valpar(1) = r
        valpar(2) = z
        call fointe('FM', zk8(ipres), 3, nompar, valpar,&
                    pres, icode)
        call fointe('FM', zk8(ipres+1), 3, nompar, valpar,&
                    cisa, icode)
        tx = -nx*pres - ny*cisa
        ty = -ny*pres + nx*cisa
        do 20 i = 1, nno
            l = (kp-1)*nno + i
            zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + tx*zr(ivf+l-1)* poids
            zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + ty*zr(ivf+l-1)* poids
20      continue
30  end do
end subroutine
