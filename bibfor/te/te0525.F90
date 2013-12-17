subroutine te0525(option, nomte)
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
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/ntfcma.h"
#include "asterfort/projet.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcfodi.h"
!
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_TNL'
!                          ELEMENTS 3D ISO PARAMETRIQUES
!                            - PROBLEME  DE  TRANSPORT  -
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!
!
    real(kind=8) :: uloc(3, 50), ul(3, 50), jacob(50), rbid, rr, tpg
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, xkpt, xkptt(50)
    real(kind=8) :: dtpgdx(50), dtpgdy(50), dtpgdz(50), vect(50)
    real(kind=8) :: dbpgdx(50), dbpgdy(50), dbpgdz(50), dupgdz(50)
    real(kind=8) :: betaa, tpn, betai, dupgdx(50), dupgdy(50), res(50)
    real(kind=8) :: xr, xrr, xaux, tpg0, xk0, pn, pnp1, xk1
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, npg1, i, ivectt, itemps, ifon(3)
    integer :: itemp, itempi, ilagrm, ivite, ilagrp, iveres
    integer :: nbvf, jvalf, k, l, idim, ndim, nnos
!
! DEB ------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PLAGRM ', 'L', ilagrm)
    call jevech('PVITESR', 'L', ivite)
    call jevech('PLAGRP ', 'E', ilagrp)
    call jevech('PVECTTR', 'E', ivectt)
    call jevech('PRESIDU', 'E', iveres)
!
    call ntfcma(' ',zi(imate), ifon)
    nbvf = zi(ifon(1))
    jvalf = zi(ifon(1)+2)
    xr = 0.d0
    do 10 i = 1, nbvf
        xaux = zr(jvalf+i-1)
        call rcfodi(ifon(1), xaux, rbid, xrr)
        if (xrr .gt. xr) then
            xr = xrr
        endif
10  end do
    rr = 0.6d0/xr
!
    k = 0
    do 30 i = 1, nno
        do 20 idim = 1, 3
            k = k + 1
            uloc(idim,i) = zr(ivite+k-1)
20      continue
30  end do
!
    do 50 kp = 1, npg1
        ul(1,kp) = 0.d0
        ul(2,kp) = 0.d0
        ul(3,kp) = 0.d0
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
        tpg = 0.d0
        tpg0 = 0.d0
        dtpgdx(kp) = 0.d0
        dtpgdy(kp) = 0.d0
        dtpgdz(kp) = 0.d0
!
        do 40 i = 1, nno
            tpg = tpg + zr(itempi+i-1)*zr(ivf+l+i-1)
            tpg0 = tpg0 + zr(itemp+i-1)*zr(ivf+l+i-1)
            ul(1,kp) = ul(1,kp) + uloc(1,i)*zr(ivf+l+i-1)
            ul(2,kp) = ul(2,kp) + uloc(2,i)*zr(ivf+l+i-1)
            ul(3,kp) = ul(3,kp) + uloc(3,i)*zr(ivf+l+i-1)
            dtpgdx(kp) = dtpgdx(kp) + zr(itempi+i-1)*dfdx(i)
            dtpgdy(kp) = dtpgdy(kp) + zr(itempi+i-1)*dfdy(i)
            dtpgdz(kp) = dtpgdz(kp) + zr(itempi+i-1)*dfdz(i)
40      continue
!
        call rcfode(ifon(2), tpg, xk1, xkpt)
        call rcfode(ifon(2), tpg0, xk0, xkpt)
        pn = zr(ilagrm+kp-1)
        call rcfodi(ifon(1), pn, betaa, rbid)
        pnp1 = pn + ((tpg-betaa)*rr)
        zr(ilagrp+kp-1) = pnp1
        vect(kp) = pnp1
        jacob(kp) = poids
        xkptt(kp) = xk1 - xk0
50  end do
    call projet(3, npg1, nno, vect, res)
!
    do 70 kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
        dbpgdx(kp) = 0.d0
        dbpgdy(kp) = 0.d0
        dbpgdz(kp) = 0.d0
        dupgdx(kp) = 0.d0
        dupgdy(kp) = 0.d0
        dupgdz(kp) = 0.d0
!
        do 60 i = 1, nno
            dupgdx(kp) = dupgdx(kp) + res(i)*dfdx(i)
            dupgdy(kp) = dupgdy(kp) + res(i)*dfdy(i)
            dupgdz(kp) = dupgdz(kp) + res(i)*dfdz(i)
            tpn = res(i)
            call rcfodi(ifon(1), tpn, betai, rbid)
            dbpgdx(kp) = dbpgdx(kp) + betai*dfdx(i)
            dbpgdy(kp) = dbpgdy(kp) + betai*dfdy(i)
            dbpgdz(kp) = dbpgdz(kp) + betai*dfdz(i)
60      continue
!
70  end do
!
    do 90 kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
        do 80 i = 1, nno
            zr(iveres+i-1) = zr(iveres+i-1) + jacob(kp)*zr(ivf+l+i-1)* (rr* (ul(1,kp)*dbpgdx(kp)+&
                             &ul(2, kp)*dbpgdy(kp)+ul(3,kp)* dbpgdz(kp))- (ul(1,kp)*dupgdx(kp)+ul&
                             &(2, kp)*dupgdy(kp)+ul( 3,kp)*dupgdz(kp))) + jacob(kp)*xkptt(kp)* (d&
                             &fdx(i)*dtpgdx( kp)+ dfdy(i)*dtpgdy(kp)+dfdz(i)*dtpgdz(kp))
!
80      continue
!
90  end do
!
! FIN ------------------------------------------------------------------
end subroutine
