subroutine te0217(option, nomte)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DU SECOND MEMBRE ELEMENTAIRE EN THERMIQUE CORRESPON-
!          DANT A UN GRADIENT IMPOSE DE TEMPERATURE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_GRAI_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: icodre(1), kpg, spt
    character(len=8) :: nompar(4), grxf, gryf, grzf, fami, poum
!
    real(kind=8) :: valres(1), valpar(4), x, y, z
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, grx, gry, grz
    integer :: ipoids, ivf, idfde, igeom
    integer :: jgano, nno, ndim, kp, npg1, i, l, ivectt, igrai, imate
!
    aster_logical :: fonc
!
!
!-----------------------------------------------------------------------
    integer :: ier, itemps, nnos
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg1,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVECTTR', 'E', ivectt)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 1, 'INST', [0.d0],&
                1, 'LAMBDA', valres, icodre, 1)
!
    if (option .eq. 'CHAR_THER_GRAI_R') then
        fonc = .false.
        call jevech('PGRAINR', 'L', igrai)
        grx = zr(igrai)
        gry = zr(igrai+1)
        grz = zr(igrai+2)
    else if (option.eq.'CHAR_THER_GRAI_F') then
        fonc = .true.
        call jevech('PTEMPSR', 'L', itemps)
        call jevech('PGRAINF', 'L', igrai)
        grxf = zk8(igrai)
        gryf = zk8(igrai+1)
        grzf = zk8(igrai+2)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itemps)
    endif
!
    do 40 kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
        x = 0.d0
        y = 0.d0
        z = 0.d0
        do 20 i = 1, nno
            x = x + zr(igeom-1+3* (i-1)+1)*zr(ivf+l+i-1)
            y = y + zr(igeom-1+3* (i-1)+2)*zr(ivf+l+i-1)
            z = z + zr(igeom-1+3* (i-1)+3)*zr(ivf+l+i-1)
 20     continue
!
        poids = poids*valres(1)
!
        if (fonc) then
            valpar(1) = x
            valpar(2) = y
            valpar(3) = z
            call fointe('FM', grxf, 4, nompar, valpar,&
                        grx, ier)
            call fointe('FM', gryf, 4, nompar, valpar,&
                        gry, ier)
            call fointe('FM', grzf, 4, nompar, valpar,&
                        grz, ier)
        endif
!
        do 30 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids* (+grx*dfdx(i)+ gry*dfdy(i)+grz*dfdz(i))
 30     continue
!
 40 end do
!
end subroutine
