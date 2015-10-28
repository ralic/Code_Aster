subroutine te0224(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/dfdm1d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          COQUE 1D
!                          OPTION : 'CHAR_MECA_FFCO2D  '
!                          ELEMENT: MECXSE3
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, jgano, nbres, ndim, nnos
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nompar(nbres), elrefe
    real(kind=8) :: valpar(nbres), pres
    real(kind=8) :: poids, r, z, fx, fy, mz, f1, f3, m2, nx, ny, dfdx(3), cour
    integer :: nno, nddl, kp, npg, ipoids, ivf, idfdk, igeom
    integer :: itemps, ivectu, k, i, l, iforc
    aster_logical :: global, locapr
!
!
    call elref1(elrefe)
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdk, jgano=jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTUR', 'E', ivectu)
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
    valpar(3) = zr(itemps)
    call jevech('PFFCO2D', 'L', iforc)
    nddl = 3
!
    global = zk8(iforc+5) .eq. 'GLOBAL'
    locapr = zk8(iforc+5) .eq. 'LOCAL_PR'
!
    do 30 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, poids, nx, ny)
        r = 0.d0
        z = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r = r + zr(igeom+2*i-2)*zr(ivf+l-1)
!
            z = z + zr(igeom+2*i-1)*zr(ivf+l-1)
 10     continue
        if (nomte .eq. 'MECXSE3') poids = poids*r
        valpar(1) = r
        valpar(2) = z
        if (global) then
            call fointe('FM', zk8(iforc), 3, nompar, valpar,&
                        fx, icode)
            call fointe('FM', zk8(iforc+1), 3, nompar, valpar,&
                        fy, icode)
            call fointe('FM', zk8(iforc+4), 3, nompar, valpar,&
                        mz, icode)
        else if (locapr) then
            f1 = 0.d0
            call fointe('FM', zk8(iforc+2), 3, nompar, valpar,&
                        pres, icode)
!-----------------------------------------------------
!       LE SIGNE MOINS DE FOR(3,J+1) CORRESPOND A LA CONVENTION :
!          UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
            f3 = -pres
            m2 = 0.d0
!-----------------------------------------------------
            fx = nx*f3 - ny*f1
            fy = ny*f3 + nx*f1
            mz = m2
        else
            call fointe('FM', zk8(iforc), 3, nompar, valpar,&
                        f1, icode)
            call fointe('FM', zk8(iforc+2), 3, nompar, valpar,&
                        f3, icode)
            call fointe('FM', zk8(iforc+3), 3, nompar, valpar,&
                        m2, icode)
            fx = nx*f3 - ny*f1
            fy = ny*f3 + nx*f1
            mz = m2
        endif
        do 20 i = 1, nno
            l = (kp-1)*nno + i
            zr(ivectu+nddl* (i-1)) = zr(ivectu+nddl* (i-1)) + fx*zr( ivf+l-1 )*poids
            zr(ivectu+nddl* (i-1)+1) = zr(ivectu+nddl* (i-1)+1) + fy*zr(ivf+l-1 )*poids
            zr(ivectu+nddl* (i-1)+2) = zr(ivectu+nddl* (i-1)+2) + mz*zr(ivf+l-1 )*poids
 20     continue
 30 end do
end subroutine
