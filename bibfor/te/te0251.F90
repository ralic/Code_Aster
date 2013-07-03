subroutine te0251(option, nomte)
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
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/foderi.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES TANGENTES ELEMENTAIRES
!                          OPTION : 'MTAN_THER_FLUXNL'
!                          ELEMENTS DE FACE 2D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! ......................................................................
!
    real(kind=8) :: poids, r, nx, ny, theta, alphap, rbid, tpg
    real(kind=8) :: mrigt(9, 9), coorse(18)
    integer :: nno, nnos, ndim, kp, npg, ipoids, ivf, idfde, jgano
    integer :: imattt, i, j, ij, l, li, lj, ibid
    integer :: c(6, 9), ise, nse, nnop2
    integer :: igeom, iflux, itempi, itemps
    logical :: laxi
    character(len=8) :: elrefe, alias8
!
!
    call elref1(elrefe)
!
    if (lteatt(' ','LUMPE','OUI')) then
        call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    if (zk8(iflux) (1:7) .eq. '&FOZERO') goto 120
    theta = zr(itemps+2)
!
    call connec(nomte, nse, nnop2, c)
!
    do 20 i = 1, nnop2
        do 10 j = 1, nnop2
            mrigt(i,j) = 0.d0
10      continue
20  end do
!
! --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------
!
    do 90 ise = 1, nse
!
        do 40 i = 1, nno
            do 30 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
30          continue
40      continue
!
        do 80 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            r = 0.d0
            tpg = 0.d0
            do 50 i = 1, nno
                l = (kp-1)*nno + i
                r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
                tpg = tpg + zr(itempi-1+c(ise,i))*zr(ivf+l-1)
50          continue
            if (laxi) poids = poids*r
            call foderi(zk8(iflux), tpg, rbid, alphap)
            ij = imattt - 1
            do 70 i = 1, nno
                li = ivf + (kp-1)*nno + i - 1
!CDIR$ IVDEP
                do 60 j = 1, i
                    lj = ivf + (kp-1)*nno + j - 1
                    ij = ij + 1
                    mrigt(c(ise,i),c(ise,j)) = mrigt(&
                                               c(ise, i),&
                                               c(ise, j) ) - poids*theta*alphap*zr(li)* zr(lj&
                                               )
60              continue
70          continue
80      continue
90  end do
!
! MISE SOUS FORME DE VECTEUR
!
    ij = imattt - 1
    do 110 i = 1, nnop2
        do 100 j = 1, i
            ij = ij + 1
            zr(ij) = mrigt(i,j)
100      continue
110  end do
120  continue
end subroutine
