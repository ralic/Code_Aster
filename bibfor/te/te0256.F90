subroutine te0256(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 1D
!
!          OPTION : 'CHAR_MECA_VNOR_F '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
    integer :: icodre(1)
    character(len=8) :: nompar(2), fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: poids, nx, ny, valpar(2)
    integer :: ipoids, ivf, idfde, igeom, ivnor, kpg, spt
    integer :: nno, kp, npg, ivectu, imate, ldec
    aster_logical :: laxi
!
!-----------------------------------------------------------------------
    integer :: i, ier, ii, jgano, n, ndim, nnos
!
    real(kind=8) :: r, rho(1), vnorf, x, y
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PMATERC', 'L', imate)
    call jevech('PSOURCF', 'L', ivnor)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
    do 10 i = 1, 2*nno
        zr(ivectu+i-1) = 0.0d0
 10 end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    do 50 kp = 1, npg
        ldec = (kp-1)*nno
!
!        COORDONNEES DU POINT DE GAUSS
        x = 0.d0
        y = 0.d0
        do 20 n = 0, nno - 1
            x = x + zr(igeom+2*n)*zr(ivf+ldec+n)
            y = y + zr(igeom+2*n+1)*zr(ivf+ldec+n)
 20     continue
!
!        VALEUR DE LA VITESSE
        valpar(1) = x
        valpar(2) = y
        call fointe('FM', zk8(ivnor), 2, nompar, valpar,&
                    vnorf, ier)
        nx = 0.0d0
        ny = 0.0d0
!
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!
        if (laxi) then
            r = 0.d0
            do 30 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
 30         continue
            poids = poids*r
        endif
!
        do 40 i = 1, nno
            ii = 2*i
            zr(ivectu+ii-1) = zr(ivectu+ii-1) - poids*vnorf*rho(1)*zr( ivf+ldec+i-1)
 40     continue
!
 50 end do
!
end subroutine
