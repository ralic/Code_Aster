subroutine te0249(option, nomte)
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
!----------------------------------------------------------------------
!
!     BUT: CALCUL DES MATRICES TANGENTES ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'MTAN_THER_COEF_R'
!          OPTION : 'MTAN_THER_RAYO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       04/04/02 (OB): CORRECTION BUG CALCUL TPG EN LUMPE
!       + MODIFS FORMELLES: IMPLICIT NONE, LAXI, LCOEF, ...
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! PARAMETRES D'APPEL
#include "jeveux.h"
#include "asterc/r8t0.h"
#include "asterfort/assert.h"
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
!
!
    real(kind=8) :: poids, r, nx, ny, theta, mrigt(9, 9), coorse(18), hech
    real(kind=8) :: sigma, epsil, tpg, tz0
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: c(6, 9), imattt, i, j, ij, l, li, lj, iray, itemp, ise, nse
    integer :: nnop2, iech, itemps, ibid
    logical :: laxi, lcoef
    character(len=8) :: elrefe, alias8
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    tz0 = r8t0()
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! INITS.
    if (option(11:14) .eq. 'COEF') then
        lcoef = .true.
        call jevech('PCOEFHR', 'L', iech)
        hech = zr(iech)
    else if (option(11:14).eq.'RAYO') then
        lcoef = .false.
        call jevech('PRAYONR', 'L', iray)
        call jevech('PTEMPEI', 'L', itemp)
        sigma = zr(iray)
        epsil = zr(iray+1)
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
    if (lteatt('AXIS','OUI')) then
        laxi = .true.
    else
        laxi = .false.
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
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
    do 120 ise = 1, nse
!
        do 40 i = 1, nno
            do 30 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
30          continue
40      continue
!
        do 110 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            if (laxi) then
                r = 0.d0
                do 50 i = 1, nno
                    l = (kp-1)*nno + i
                    r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
50              continue
                poids = poids*r
            endif
            ij = imattt - 1
            if (lcoef) then
                do 70 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    do 60 j = 1, i
                        lj = ivf + (kp-1)*nno + j - 1
                        ij = ij + 1
                        mrigt(c(ise,i),c(ise,j)) = mrigt(&
                                                   c(ise, i),&
                                                   c( ise, j)) + poids*theta*zr(li)*zr(lj&
                                                   )* hech
60                  continue
70              continue
            else
                tpg = 0.d0
                do 80 i = 1, nno
                    l = (kp-1)*nno + i
                    tpg = tpg + zr(itemp-1+c(ise,i))*zr(ivf+l-1)
80              continue
                do 100 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    do 90 j = 1, i
                        lj = ivf + (kp-1)*nno + j - 1
                        ij = ij + 1
                        mrigt(c(ise,i),c(ise,j)) = mrigt(&
                                                   c(ise, i),&
                                                   c( ise, j)) + poids*theta*zr(li)*zr(lj)* 4.d0*&
                                                   & sigma*epsil* (tpg+tz0&
                                                   )**3
90                  continue
100              continue
            endif
110      continue
120  end do
!
! MISE SOUS FORME DE VECTEUR
!
    ij = imattt - 1
    do 140 i = 1, nnop2
        do 130 j = 1, i
            ij = ij + 1
            zr(ij) = mrigt(i,j)
130      continue
140  end do
end subroutine
