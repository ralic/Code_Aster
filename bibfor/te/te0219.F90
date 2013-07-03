subroutine te0219(option, nomte)
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
#include "asterfort/connec.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/teattr.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_GRAI_R/F  '
!                          EN 2D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre, kpg, spt
    character(len=8) :: grxf, gryf, nompar(3), elrefe, alias8, fami, poum
    real(kind=8) :: dfdx(9), dfdy(9), poids, x, y, valres
    real(kind=8) :: coorse(18), vectt(9), grx, gry, valpar(3)
    integer :: ndim, nno, nnos, kp, npg, i, k, ivectt, igrai
    integer :: ipoids, ivf, idfde, igeom, imate, jgano
    integer :: nnop2, c(6, 9), ise, nse, itemps, j, ier, ibid
!
!
    logical :: fonc
!
!
    call elref1(elrefe)
!
    if (lteatt(' ','LUMPE','OUI')) then
        call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'QU9') elrefe='QU4'
        if (alias8(6:8) .eq. 'TR6') elrefe='TR3'
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'CHAR_THER_GRAI_R') then
        fonc=.false.
        call jevech('PGRAINR', 'L', igrai)
        grx=zr(igrai)
        gry=zr(igrai+1)
    else if (option.eq.'CHAR_THER_GRAI_F') then
        fonc=.true.
        call jevech('PTEMPSR', 'L', itemps)
        call jevech('PGRAINF', 'L', igrai)
        grxf=zk8(igrai)
        gryf=zk8(igrai+1)
        nompar(1)='X'
        nompar(2)='Y'
        nompar(3)='INST'
        valpar(3) = zr(itemps)
    endif
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PVECTTR', 'E', ivectt)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 1, 'INST', 0.d0,&
                1, 'LAMBDA', valres, icodre, 1)
!
    call connec(nomte, nse, nnop2, c)
!
    do 10 i = 1, nnop2
        vectt(i)=0.d0
10  end do
!
!     BOUCLE SUR LES SOUS-ELEMENTS
    do 100 ise = 1, nse
!
        do 105 i = 1, nno
            do 105 j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
105          continue
!
        do 101 kp = 1, npg
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                        dfdx, dfdy, poids)
            x = 0.d0
            y = 0.d0
            do 102 i = 1, nno
                x = x + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
                y = y + coorse(2*(i-1)+2) * zr(ivf+k+i-1)
102          continue
!
            if (fonc) then
                valpar(1) = x
                valpar(2) = y
                call fointe('FM', grxf, 3, nompar, valpar,&
                            grx, ier)
                call fointe('FM', gryf, 3, nompar, valpar,&
                            gry, ier)
            endif
!
            if (lteatt(' ','AXIS','OUI')) poids = poids*x
            poids = poids*valres
!
            do 103 i = 1, nno
                vectt(c(ise,i)) = vectt( c(ise,i)) + poids*( dfdx(i)* grx+dfdy(i)*gry)
103          continue
101      continue
100  end do
!
    do 200 i = 1, nnop2
        zr(ivectt-1+i)=vectt(i)
200  end do
!
end subroutine
