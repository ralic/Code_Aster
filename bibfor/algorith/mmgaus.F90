subroutine mmgaus(alias, typi, nord, xpg, ypg,&
                  hpg)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1501
    implicit none
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
    character(len=8) :: alias
    integer :: typi, nord
    real(kind=8) :: xpg, ypg, hpg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (UTILITAIRE)
!
! RETOURNE LES COORDONNEES ET LE POIDS DU POINT D'INTEGRATION
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  TYPI   : TYPE D'INTEGRATION
!     1 'AUTO'    (ON CHOISIT LE SCHEMA LE PLUS ADAPTE)
!    X2 'GAUSS'   (X EST LE DEGRE DES POLYNOMES DE LEGENDRE)
!    Y3 'SIMPSON' (Y EST LE NOMBRE DE SUBDIVISIONS)
!    Z4 'NCOTES'  (Z EST LE DEGRE DU POLYNOME INTERPOLATEUR)
! IN  NORD   : NUMERO DU POINT D'INTEGRATION
! OUT XPG    : COORDONNEE X DU POINT D'INTEGRATION
! OUT YPG    : COORDONNEE Y DU POINT D'INTEGRATION
! OUT HPG    : POIDS DU POINT D'INTEGRATION
!
! ----------------------------------------------------------------------
!
    integer :: zgauss
    parameter   (zgauss=6)
!
    integer :: znpgse
    parameter   (znpgse=6)
    integer :: zseg
    parameter   (zseg  =2)
!
    integer :: znpgtr
    parameter   (znpgtr=12)
    integer :: ztri
    parameter   (ztri  =3)
!
    real(kind=8) :: fpgseg(zgauss, znpgse, zseg)
    real(kind=8) :: fpgtri(zgauss, znpgtr, ztri)
!
    integer :: zncots
    parameter   (zncots=8)
!
    integer :: znpncs
    parameter   (znpncs=5)
    integer :: znpnct
    parameter   (znpnct=10)
!
    real(kind=8) :: pncseg(zncots, znpncs)
    real(kind=8) :: pnctri(zncots, znpnct)
!
    integer :: param
    integer :: i, j, h, n, incseg, jncseg
    real(kind=8) :: a, b, c, d, p1, p2, p3
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    call r8inir(zgauss*znpgse*zseg, 0.d0, fpgseg, 1)
    call r8inir(zgauss*znpgtr*ztri, 0.d0, fpgtri, 1)
    call r8inir(zncots*znpncs, 0.d0, pncseg, 1)
    call r8inir(zncots*znpnct, 0.d0, pnctri, 1)
!
! --- POINTS DE GAUSS (SEGMENT)
!
    fpgseg(1,1,1) = 0.d0
    fpgseg(1,1,2) = 2.d0
!
    fpgseg(2,1,1) = +0.577350269189626d0
    fpgseg(2,1,2) = 1.d0
    fpgseg(2,2,1) = -fpgseg(2,1,1)
    fpgseg(2,2,2) = fpgseg(2,1,2)
!
    fpgseg(3,1,1) = -0.774596669241483d0
    fpgseg(3,1,2) = 0.555555555555556d0
    fpgseg(3,2,1) = 0.d0
    fpgseg(3,2,2) = 0.888888888888889d0
    fpgseg(3,3,1) = -fpgseg(3,1,1)
    fpgseg(3,3,2) = fpgseg(3,1,2)
!
    fpgseg(4,1,1) = +0.339981043584856d0
    fpgseg(4,1,2) = 0.652145154862546d0
    fpgseg(4,2,1) = -fpgseg(4,1,1)
    fpgseg(4,2,2) = fpgseg(4,1,2)
    fpgseg(4,3,1) = +0.861136311594053d0
    fpgseg(4,3,2) = 0.347854845137454d0
    fpgseg(4,4,1) = -fpgseg(4,3,1)
    fpgseg(4,4,2) = fpgseg(4,3,2)
!
    fpgseg(5,1,1) = -0.906179845938664d0
    fpgseg(5,1,2) = 0.236926885056189d0
    fpgseg(5,2,1) = -0.538469310105683d0
    fpgseg(5,2,2) = 0.478628670499366d0
    fpgseg(5,3,1) = 0.d0
    fpgseg(5,3,2) = 0.568888888888889d0
    fpgseg(5,4,1) = -fpgseg(5,2,1)
    fpgseg(5,4,2) = fpgseg(5,2,2)
    fpgseg(5,5,1) = -fpgseg(5,1,1)
    fpgseg(5,5,2) = fpgseg(5,1,2)
!
    fpgseg(6,1,1) = +0.238619186083197d0
    fpgseg(6,1,2) = 0.467913934572691d0
    fpgseg(6,2,1) = -fpgseg(6,1,1)
    fpgseg(6,2,2) = fpgseg(6,1,2)
    fpgseg(6,3,1) = +0.661209386466265d0
    fpgseg(6,3,2) = 0.360761573048139d0
    fpgseg(6,4,1) = -fpgseg(6,3,1)
    fpgseg(6,4,2) = fpgseg(6,3,2)
    fpgseg(6,5,1) = +0.932469514203152d0
    fpgseg(6,5,2) = 0.171324492379170d0
    fpgseg(6,6,1) = -fpgseg(6,5,1)
    fpgseg(6,6,2) = fpgseg(6,5,2)
!
! --- POINTS DE GAUSS (TRIANGLE)
!
    fpgtri(1,1,1) = 1.d0/3.d0
    fpgtri(1,1,2) = 1.d0/3.d0
    fpgtri(1,1,3) = 0.5d0
!
    a = 1.d0/6.d0
    b = 2.d0/3.d0
    p1 = 1.d0/6.d0
    fpgtri(2,1,1) = a
    fpgtri(2,1,2) = a
    fpgtri(2,1,3) = p1
    fpgtri(2,2,1) = b
    fpgtri(2,2,2) = a
    fpgtri(2,2,3) = p1
    fpgtri(2,3,1) = a
    fpgtri(2,3,2) = b
    fpgtri(2,3,3) = p1
!
    fpgtri(3,1,1) = 0.2d0
    fpgtri(3,1,2) = 0.2d0
    fpgtri(3,1,3) = 25.d0/96.d0
    fpgtri(3,2,1) = 0.6d0
    fpgtri(3,2,2) = 0.2d0
    fpgtri(3,2,3) = 25.d0/96.d0
    fpgtri(3,3,1) = 0.2d0
    fpgtri(3,3,2) = 0.6d0
    fpgtri(3,3,3) = 25.d0/96.d0
    fpgtri(3,4,1) = 1.d0/3.d0
    fpgtri(3,4,2) = 1.d0/3.d0
    fpgtri(3,4,3) = -27.d0/96.d0
!
    a = 0.445948490915965d0
    b = 0.091576213509771d0
    p1 = 0.111690794839005d0
    p2 = 0.054975871827661d0
    fpgtri(4,1,1) = b
    fpgtri(4,1,2) = b
    fpgtri(4,1,3) = p2
    fpgtri(4,2,1) = 1.d0-2.d0*b
    fpgtri(4,2,2) = b
    fpgtri(4,2,3) = p2
    fpgtri(4,3,1) = b
    fpgtri(4,3,2) = 1.d0-2.d0*b
    fpgtri(4,3,3) = p2
    fpgtri(4,4,1) = a
    fpgtri(4,4,2) = 1.d0-2.d0*a
    fpgtri(4,4,3) = p1
    fpgtri(4,5,1) = a
    fpgtri(4,5,2) = a
    fpgtri(4,5,3) = p1
    fpgtri(4,6,1) = 1.d0-2.d0*a
    fpgtri(4,6,2) = a
    fpgtri(4,6,3) = p1
!
    a = 0.470142064105115d0
    b = 0.101286507323456d0
    p1 = 0.066197076394253d0
    p2 = 0.062969590272413d0
    fpgtri(5,1,1) = 1.d0/3.d0
    fpgtri(5,1,2) = 1.d0/3.d0
    fpgtri(5,1,3) = 9.d0/80.d0
    fpgtri(5,2,1) = a
    fpgtri(5,2,2) = a
    fpgtri(5,2,3) = p1
    fpgtri(5,3,1) = 1.d0-2.d0*a
    fpgtri(5,3,2) = a
    fpgtri(5,3,3) = p1
    fpgtri(5,4,1) = a
    fpgtri(5,4,2) = 1.d0-2.d0*a
    fpgtri(5,4,3) = p1
    fpgtri(5,5,1) = b
    fpgtri(5,5,2) = b
    fpgtri(5,5,3) = p2
    fpgtri(5,6,1) = 1.d0-2.d0*b
    fpgtri(5,6,2) = b
    fpgtri(5,6,3) = p2
    fpgtri(5,7,1) = b
    fpgtri(5,7,2) = 1.d0-2.d0*b
    fpgtri(5,7,3) = p2
!
    a = 0.063089014491502d0
    b = 0.249286745170910d0
    c = 0.310352451033785d0
    d = 0.053145049844816d0
    p1 = 0.025422453185103d0
    p2 = 0.058393137863189d0
    p3 = 0.041425537809187d0
    fpgtri(6,1 ,1) = a
    fpgtri(6,1 ,2) = a
    fpgtri(6,1 ,3) = p1
    fpgtri(6,2 ,1) = 1.d0-2.d0*a
    fpgtri(6,2 ,2) = a
    fpgtri(6,2 ,3) = p1
    fpgtri(6,3 ,1) = a
    fpgtri(6,3 ,2) = 1.d0-2.d0*a
    fpgtri(6,3 ,3) = p1
    fpgtri(6,4 ,1) = b
    fpgtri(6,4 ,2) = b
    fpgtri(6,4 ,3) = p2
    fpgtri(6,5 ,1) = 1.d0-2.d0*b
    fpgtri(6,5 ,2) = b
    fpgtri(6,5 ,3) = p2
    fpgtri(6,6 ,1) = b
    fpgtri(6,6 ,2) = 1.d0-2.d0*b
    fpgtri(6,6 ,3) = p2
    fpgtri(6,7 ,1) = c
    fpgtri(6,7 ,2) = d
    fpgtri(6,7 ,3) = p3
    fpgtri(6,8 ,1) = d
    fpgtri(6,8 ,2) = c
    fpgtri(6,8 ,3) = p3
    fpgtri(6,9 ,1) = 1-c-d
    fpgtri(6,9 ,2) = c
    fpgtri(6,9 ,3) = p3
    fpgtri(6,10,1) = 1-c-d
    fpgtri(6,10,2) = d
    fpgtri(6,10,3) = p3
    fpgtri(6,11,1) = c
    fpgtri(6,11,2) = 1-c-d
    fpgtri(6,11,3) = p3
    fpgtri(6,12,1) = d
    fpgtri(6,12,2) = 1-c-d
    fpgtri(6,12,3) = p3
!
! --- POIDS NEWTON-COTES (SEGMENT)
!
    pncseg(3,1) = 0.25d0
    pncseg(3,2) = 0.75d0
!
    pncseg(4,1) = 0.155555555555556d0
    pncseg(4,2) = 0.711111111111111d0
    pncseg(4,3) = 0.266666666666667d0
!
    pncseg(5,1) = 0.131944444444444d0
    pncseg(5,2) = 0.520833333333333d0
    pncseg(5,3) = 0.347222222222222d0
!
    pncseg(6,1) = 0.097619047619048d0
    pncseg(6,2) = 0.514285714285714d0
    pncseg(6,3) = 0.064285714285714d0
    pncseg(6,4) = 0.647619047619048d0
!
    pncseg(7,1) = 0.086921296296296d0
    pncseg(7,2) = 0.414004629629630d0
    pncseg(7,3) = 0.153125d0
    pncseg(7,4) = 0.345949074074074d0
!
    pncseg(8,1) = 0.069770723104057d0
    pncseg(8,2) = 0.415379188712522d0
    pncseg(8,3) = -0.065467372134039d0
    pncseg(8,4) = 0.740458553791887d0
    pncseg(8,5) = -0.320282186948854d0
!
! --- POIDS NEWTON-COTES (TRIANGLE)
!
    pnctri(3,1) = 0.016666666666667d0
    pnctri(3,2) = 0.0375d0
    pnctri(3,3) = 0.225d0
!
    pnctri(4,1) = 0.d0
    pnctri(4,2) = 0.044444444444445d0
    pnctri(4,3) = -0.011111111111111d0
    pnctri(4,4) = 0.088888888888889d0
!
    pnctri(5,1) = 0.005456349206349d0
    pnctri(5,2) = 0.012400793650794d0
    pnctri(5,3) = 0.012400793650794d0
    pnctri(5,4) = 0.099206349206349d0
    pnctri(5,5) = 0.012400793650794d0
!
    pnctri(6,1) = 0.d0
    pnctri(6,2) = 0.021428571428572d0
    pnctri(6,3) = -0.016071428571429d0
    pnctri(6,4) = 0.042857142857143d0
    pnctri(6,5) = 0.038095238095238d0
    pnctri(6,6) = 0.042857142857143d0
    pnctri(6,7) = -0.032142857142857d0
!
    pnctri(7,1) = 0.002577160493827d0
    pnctri(7,2) = 0.005765817901235d0
    pnctri(7,3) = 0.006900077160494d0
    pnctri(7,4) = 0.062195216049383d0
    pnctri(7,5) = 0.005198688271605d0
    pnctri(7,6) = -0.013233024691358d0
    pnctri(7,7) = 0.086014660493827d0
    pnctri(7,8) = 0.006616512345679d0
!
    pnctri(8,1) = 0.d0
    pnctri(8,2) = 0.012980599647266d0
    pnctri(8,3) = -0.016507936507937d0
    pnctri(8,4) = 0.024832451499118d0
    pnctri(8,5) = 0.040070546737213d0
    pnctri(8,6) = 0.029347442680776d0
    pnctri(8,7) = -0.038201058201058d0
    pnctri(8,8) = 0.023703703703704d0
    pnctri(8,9) = -0.051075837742504d0
    pnctri(8,10) = 0.051922398589065d0
!
!_______________________________________________________________________
!
! 'AUTO'
!
! TOUS LES SCHEMAS SONT DE TYPE TRAPEZE SAUF TR6/TR7/QU8/QU9
!
    if (typi .eq. 1) then
        if (alias(1:3) .eq. 'SE2') then
            if (nord .eq. 1) then
                xpg = -1.d0
                ypg = 0.d0
                hpg = 1.d0
            else if (nord .eq. 2) then
                xpg = 1.d0
                ypg = 0.d0
                hpg = 1.d0
            else
                call assert(.false.)
            endif
        else if (alias(1:3) .eq. 'SE3') then
            if (nord .eq. 1) then
                xpg = -1.d0
                ypg = 0.d0
                hpg = 1.d0 / 3.d0
            else if (nord .eq. 2) then
                xpg = 1.d0
                ypg = 0.d0
                hpg = 1.d0 / 3.d0
            else if (nord .eq. 3) then
                xpg = 0.d0
                ypg = 0.d0
                hpg = 4.d0 / 3.d0
            else
                call assert(.false.)
            endif
        else if (alias(1:3) .eq. 'TR3') then
            if (nord .eq. 1) then
                xpg = 0.d0
                ypg = 0.d0
                hpg = 1.d0/6.d0
            else if (nord .eq. 2) then
                xpg = 1.d0
                ypg = 0.d0
                hpg = 1.d0/6.d0
            else if (nord .eq. 3) then
                xpg = 0.d0
                ypg = 1.d0
                hpg = 1.d0/6.d0
            else
                call assert(.false.)
            endif
        else if ((alias(1:3).eq.'TR6').or.(alias(1:3).eq.'TR7')) then
            call assert((nord.ge.1).and.(nord.le.6))
            xpg = fpgtri(4,nord,1)
            ypg = fpgtri(4,nord,2)
            hpg = fpgtri(4,nord,3)
        else if ((alias(1:3) .eq. 'QU4')) then
            if (nord .eq. 1) then
                xpg = -1.d0
                ypg = -1.d0
                hpg = 1.d0
            else if (nord .eq. 2) then
                xpg = 1.d0
                ypg = -1.d0
                hpg = 1.d0
            else if (nord .eq. 3) then
                xpg = 1.d0
                ypg = 1.d0
                hpg = 1.d0
            else if (nord .eq. 4) then
                xpg = -1.d0
                ypg = 1.d0
                hpg = 1.d0
            else
                call assert(.false.)
            endif
        else if ((alias(1:3).eq.'QU8').or.(alias(1:3).eq.'QU9')) then
            if (nord .eq. 1) then
                xpg = -1.d0
                ypg = -1.d0
                hpg = 1.d0 / 9.d0
            else if (nord .eq. 2) then
                xpg = 1.d0
                ypg = -1.d0
                hpg = 1.d0 / 9.d0
            else if (nord .eq. 3) then
                xpg = 1.d0
                ypg = 1.d0
                hpg = 1.d0 / 9.d0
            else if (nord .eq. 4) then
                xpg = -1.d0
                ypg = 1.d0
                hpg = 1.d0 / 9.d0
            else if (nord .eq. 5) then
                xpg = 0.d0
                ypg = -1.d0
                hpg = 4.d0 / 9.d0
            else if (nord .eq. 6) then
                xpg = 1.d0
                ypg = 0.d0
                hpg = 4.d0 / 9.d0
            else if (nord .eq. 7) then
                xpg = 0.d0
                ypg = 1.d0
                hpg = 4.d0 / 9.d0
            else if (nord .eq. 8) then
                xpg = -1.d0
                ypg = 0.d0
                hpg = 4.d0 / 9.d0
            else if (nord .eq. 9) then
                xpg = 0.d0
                ypg = 0.d0
                hpg = 16.d0 / 9.d0
            else
                call assert(.false.)
            endif
        else
            call assert(.false.)
        endif
!_______________________________________________________________________
!
! 'GAUSS'
!
    else if (mod(typi,10) .eq. 2) then
        param = typi/10
        if (alias(1:2) .eq. 'SE') then
            call assert((nord.ge.1).and.(nord.le.param))
            xpg = fpgseg(param,nord,1)
            ypg = 0.d0
            hpg = fpgseg(param,nord,2)
        else if (alias(1:2) .eq. 'TR') then
            call assert((nord.ge.1).and.(nord.le.znpgtr))
            xpg = fpgtri(param,nord,1)
            ypg = fpgtri(param,nord,2)
            hpg = fpgtri(param,nord,3)
        else if (alias(1:2) .eq. 'QU') then
!           POINTS DE GAUSS ARRANGES EN LIGNE EN PARTANT DU BAS GAUCHE
            i = mod((nord-1),param)+1
            j = ((nord-1)/param)+1
!
            xpg = fpgseg(param,i,1)
            ypg = fpgseg(param,j,1)
            hpg = fpgseg(param,i,2)*fpgseg(param,j,2)
        else
            call assert(.false.)
        endif
!_______________________________________________________________________
!
! 'SIMPSON'
!
    else if (mod(typi,10) .eq. 3) then
        param = typi/10
!
! SEGMENTS
!
! EXEMPLE A 2 SUBDIVISIONS   NUMEROTATION            POIDS (X6)
!
!                         1---2---3---4---5      1---4---2---4---1
!
!   SUBDIVISIONS   | 1 | 2 | 3 | 4 |
! -----------------+---+---+---+---+
! NOMBRE DE POINTS | 3 | 5 | 7 | 9 |
!
        if (alias(1:2) .eq. 'SE') then
            n = nord-1
            xpg = -1.d0 + n*(1.d0/param)
            ypg = 0.d0
!
            if ((n .eq. 0) .or. (n .eq. 2*param)) then
                hpg = 1.d0/(param*3.d0)
            else
                if (mod(n,2) .eq. 0) then
                    hpg = 2.d0/(param*3.d0)
                else
                    hpg = 4.d0/(param*3.d0)
                endif
            endif
!
! TRIANGLES
!
! EXEMPLE A 2 SUBDIVISIONS  NUMEROTATION            POIDS (X120)
!
!                           15                      1
!                           | \                     | \
!                           10  14                  4   4
!                           |     \                 |     \
!                           6   9   13              3---8---3
!                           |         \             | \     | \
!                           3   5   8   12          4   8   8   4
!                           |             \         |     \ |     \
!                           1---2---4---7--11       1---4---3---4---1
!
!    SUBDIVISIONS  | 1 | 2 | 3 | 4 |
! -----------------+---+---+---+---+
! NOMBRE DE POINTS | 6 | 15| 28| 45|
!
        else if (alias(1:2) .eq. 'TR') then
            h=0
            n=nord
50          continue
            if (n .gt. 0) then
                h=h+1
                n=n-h
                goto 50
            endif
            h = h-1
            i = -n
            j = h+n
            h = 2*param
!
            xpg = 0.5d0*i/param
            ypg = 0.5d0*j/param
!
            if ((i .eq. 0) .or. (j .eq. 0) .or. (i+j .eq. h)) then
                if (i .eq. 0) then
                    if ((j .eq. 0) .or. (j .eq. h)) then
                        hpg = 1.d0
                    else
                        if (mod(j,2) .eq. 0) then
                            hpg = 3.d0
                        else
                            hpg = 4.d0
                        endif
                    endif
                else if (j .eq. 0) then
                    if (i .eq. h) then
                        hpg = 1.d0
                    else
                        if (mod(i,2) .eq. 0) then
                            hpg = 3.d0
                        else
                            hpg = 4.d0
                        endif
                    endif
                else
                    if (mod(j,2) .eq. 0) then
                        hpg = 3.d0
                    else
                        hpg = 4.d0
                    endif
                endif
            else
                if ((mod(i,2) .eq. 0) .and. (mod(j,2) .eq. 0)) then
                    hpg = 6.d0
                else
                    hpg = 8.d0
                endif
            endif
            hpg = hpg/((param**2)*30.d0)
!
! QUADRANGLES
!
! EXEMPLE A 2 SUBDIVISIONS    NUMEROTATION           POIDS (X36)
!
!                          21--22--23--24--25     1---4---2---4---1
!                           |               |     |       |       |
!                          16  17  18  19  20     4  16   8  16   4
!                           |               |     |       |       |
!                          11  12  13  14  15     2---8---4---8---2
!                           |               |     |       |       |
!                           6   7   8   9  10     4  16   8  16   4
!                           |               |     |       |       |
!                           1---2---3---4---5     1---4---2---4---1
!
!   SUBDIVISIONS   | 1 | 2 | 3 | 4 |
! -----------------+---+---+---+---+
! NOMBRE DE POINTS | 9 | 25| 49| 81|
!
        else if (alias(1:2) .eq. 'QU') then
!
            i = mod((nord-1),(2*param+1))
            j = (nord-1)/(2*param+1)
!
            xpg = -1.d0 +i*(1.d0/param)
            ypg = -1.d0 +j*(1.d0/param)
!
            if ((i.eq.0) .or. (j.eq.0) .or. (i.eq.2*param) .or. (j.eq.2* param)) then
                if ((i .eq. 0) .or. (i .eq. 2*param)) then
                    if ((j .eq. 0) .or. (j .eq. 2*param)) then
                        hpg = 1.d0/((param**2)*9.d0)
                    else
                        if (mod(j,2) .eq. 0) then
                            hpg = 2.d0/((param**2)*9.d0)
                        else
                            hpg = 4.d0/((param**2)*9.d0)
                        endif
                    endif
                else
                    if (mod(i,2) .eq. 0) then
                        hpg = 2.d0/((param**2)*9.d0)
                    else
                        hpg = 4.d0/((param**2)*9.d0)
                    endif
                endif
            else
                if ((mod(i,2) .eq. 0) .and. (mod(j,2) .eq. 0)) then
                    hpg = 4.d0/((param**2)*9.d0)
                else if ((mod(i,2).eq.1).and.(mod(j,2).eq.1)) then
                    hpg = 16.d0/((param**2)*9.d0)
                else
                    hpg = 8.d0/((param**2)*9.d0)
                endif
            endif
        else
            call assert(.false.)
        endif
!
!_______________________________________________________________________
!
!
! NCOTES
!
    else if (mod(typi,10) .eq. 4) then
        param = typi/10
!
! SEGMENTS
!
! EXEMPLE D'ORDRE 4          NUMEROTATION            POIDS (X45)
!
!                         1---2---3---4---5      7---32--12--32--7
!
!      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
! -----------------+---+---+---+---+---+---+
! NOMBRE DE POINTS | 4 | 5 | 6 | 7 | 8 | 9 |
!
        if (alias(1:2) .eq. 'SE') then
            h = (param/2)+1
!
            if (nord .le. h) then
                incseg = nord
            else
                incseg = (param+1)-(nord-1)
            endif
!
            xpg = -1.d0 + (nord-1)*(2.d0/param)
            ypg = 0.d0
            hpg = pncseg(param,incseg)
!
! TRIANGLES
!
! EXEMPLE D'ORDRE 4   NUMEROTATION            POIDS (X90)
!
!                     15                      0
!                     | \                     | \
!                     10  14                  4   4
!                     |     \                 |     \
!                     6   9   13             -1   8  -1
!                     |         \             |         \
!                     3   5   8   12          4   8   8   4
!                     |             \         |             \
!                     1---2---4---7--11       0---4-(-1)--4---0
!
!      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
! -----------------+---+---+---+---+---+---+
! NOMBRE DE POINTS | 10| 15| 21| 28| 36| 45|
!
        else if (alias(1:2) .eq. 'TR') then
            h=0
            n=nord
60          continue
            if (n .gt. 0) then
                h=h+1
                n=n-h
                goto 60
            endif
            i = -n
            j = h-1+n
!
            xpg = (i*1.d0)/param
            ypg = (j*1.d0)/param
!
            h=min(i,j,param-i-j)
            i=param-max(i,j,param-i-j)
!
            j = param/2
            if (i .le. j) then
                hpg=pnctri(param,((i+1)/2)*(i/2+1)+1+h)
            else
                hpg=pnctri(param,((i+1)/2)*(i/2+1)+1+h -(i-(param/2))*&
                (i-((param-1)/2)))
            endif
!
! QUADRANGLES
!
! EXEMPLE D'ORDRE 4
!    NUMEROTATION           POIDS (X2025)
!
!    21--22--23--24--25    49--224--84-224--49
!     |               |     |               |
!    16  17  18  19  20   224 1024 384 1024 224
!     |               |     |               |
!    11  12  13  14  15    84 384  144 384  84
!     |               |     |               |
!     6   7   8   9  10   224 1024 384 1024 224
!     |               |     |               |
!     1---2---3---4---5    49--224--84-224--49
!
!      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
! -----------------+---+---+---+---+---+---+
! NOMBRE DE POINTS | 16| 25| 36| 49| 64| 81|
!
        else if (alias(1:2) .eq. 'QU') then
            i = mod((nord-1),(param+1))+1
            j = (nord-1)/(param+1) +1
            h = (param/2)+1
!
            if (i .le. h) then
                incseg = i
            else
                incseg = (param+1)-(i-1)
            endif
            xpg = -1.d0 + (i-1)*(2.d0/param)
!
            if (j .le. h) then
                jncseg = j
            else
                jncseg = (param+1)-(j-1)
            endif
            ypg = -1.d0 + (j-1)*(2.d0/param)
!
            hpg = pncseg(param,incseg)*pncseg(param,jncseg)
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
end subroutine
