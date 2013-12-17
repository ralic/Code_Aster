subroutine te0242(option, nomte)
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
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rcdiff.h"
#include "asterfort/rcfode.h"
#include "asterfort/teattr.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'MTAN_RIGI_MASS'
!                          ELEMENTS 2D LUMPES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE
!
    character(len=8) :: elrefe, alias8
    real(kind=8) :: lambda, r8bid, rhocp, deltat
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, theta, khi, tpgi
    real(kind=8) :: mt(9, 9), coorse(18), diff, tpsec, tpg
    integer :: ndim, nno, nnos, kp, npg, i, j, ij, k, itemps, ifon(3)
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icomp, itempi, imattt, jgano, ipoid2, npg2
    integer :: c(6, 9), ise, nse, nnop2, ivf2, idfde2
    integer :: isechf, isechi, ibid
! DEB ------------------------------------------------------------------
    call elref1(elrefe)
!
    if (lteatt(' ','LUMPE','OUI')) then
        call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'QU9') elrefe='QU4'
        if (alias8(6:8) .eq. 'TR6') elrefe='TR3'
        call elref4(elrefe, 'NOEU', ndim, nno, nnos,&
                    npg2, ipoid2, ivf2, idfde2, jgano)
    else
        call elref4(elrefe, 'MASS', ndim, nno, nnos,&
                    npg2, ipoid2, ivf2, idfde2, jgano)
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PMATTTR', 'E', imattt)
!
!
!
    if ((zk16(icomp)(1:5).eq.'SECH_')) then
        if (zk16(icomp)(1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp)(1:10) .eq. 'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!            POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!            ISECHI ET ISECHF SONT FICTIFS
            isechi = itempi
            isechf = itempi
        endif
    endif
!
    deltat= zr(itemps+1)
    theta = zr(itemps+2)
    khi = zr(itemps+3)
!
!     CALCUL LUMPE
!     ------------
!  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
!
    call connec(nomte, nse, nnop2, c)
!
    do 10 i = 1, nnop2
        do 10 j = 1, nnop2
            mt(i,j)=0.d0
10      continue
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 200 ise = 1, nse
!
        do 205 i = 1, nno
            do 205 j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
205          continue
!
!
        if (zk16(icomp)(1:5) .eq. 'THER_') then
!
! ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
!
            call ntfcma(zk16(icomp), zi(imate), ifon)
            do 101 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpgi = 0.d0
                do 102 i = 1, nno
                    r = r + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
                    tpgi = tpgi + zr(itempi-1+c(ise,i)) * zr(ivf+k+i- 1)
102              continue
                if (lteatt(' ','AXIS','OUI')) poids = poids*r
                call rcfode(ifon(2), tpgi, lambda, r8bid)
!
                ij = imattt - 1
                do 103 i = 1, nno
!DIR$ IVDEP
                    do 103 j = 1, nno
                        ij = ij + 1
                        mt(c(ise,i),c(ise,j)) = mt(&
                                                c(ise, i),&
                                                c(ise, j))+ poids* lambda*theta*(dfdx(i)*dfdx(j)+&
                                                &dfdy(i)* dfdy(j)&
                                                )
103                  continue
101          continue
!
! ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------
!
            do 405 i = 1, nno
                do 405 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
405              continue
!
            call ntfcma(zk16(icomp), zi(imate), ifon)
            do 401 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpgi = 0.d0
                do 402 i = 1, nno
                    r = r + coorse(2*(i-1)+1) * zr(ivf2+k+i-1)
                    tpgi = tpgi + zr(itempi-1+c(ise,i)) * zr(ivf2+k+i- 1)
402              continue
                if (lteatt(' ','AXIS','OUI')) poids = poids*r
                call rcfode(ifon(1), tpgi, r8bid, rhocp)
!
                ij = imattt - 1
                do 403 i = 1, nno
!DIR$ IVDEP
                    do 403 j = 1, nno
                        ij = ij + 1
                        mt(c(ise,i),c(ise,j)) = mt(&
                                                c(ise, i),&
                                                c(ise, j))+ poids* khi*rhocp*zr(ivf2+k+i-1)*zr(iv&
                                                &f2+k+j-1&
                                                ) /deltat
403                  continue
401          continue
!
! --- SECHAGE
!
        else if (zk16(icomp)(1:5).eq.'SECH_') then
!
! ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
!
            do 203 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                tpsec = 0.d0
                do 201 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf+k+i-1)
                    tpg = tpg + zr(itempi-1+c(ise,i)) *zr(ivf+k+i-1)
                    tpsec = tpsec + zr(isechf-1+c(ise,i)) *zr(ivf+k+i- 1)
201              continue
                if (lteatt(' ','AXIS','OUI')) poids = poids*r
                call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
!
                ij = imattt - 1
                do 202 i = 1, nno
!
                    do 202 j = 1, nno
                        ij = ij + 1
                        mt(c(ise,i),c(ise,j)) = mt(&
                                                c(ise, i),&
                                                c(ise, j)) + poids*( diff*theta*(dfdx(i)*dfdx(j)+&
                                                &dfdy(i)* dfdy(j))&
                                                )
202                  continue
203          continue
!
! ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------
!
            do 301 i = 1, nno
                do 301 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
301              continue
!
            do 304 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                do 302 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf2+k+i-1)
302              continue
                if (lteatt(' ','AXIS','OUI')) poids = poids*r
!
                ij = imattt - 1
                do 303 i = 1, nno
!
                    do 303 j = 1, nno
                        ij = ij + 1
                        mt(c(ise,i),c(ise,j)) = mt(&
                                                c(ise, i),&
                                                c(ise, j)) + poids*( khi*zr(ivf2+k+i-1)*zr(ivf2+k&
                                                &+j-1)/ deltat&
                                                )
303                  continue
304          continue
        endif
!
! FIN DE LA BOUCLE SUR LES SOUS-ELEMENTS
!
200  end do
!
! MISE SOUS FORME DE VECTEUR
    ij = imattt-1
    do 406 i = 1, nnop2
        do 406 j = 1, i
            ij = ij +1
            zr(ij)=mt(i,j)
406      continue
! FIN ------------------------------------------------------------------
end subroutine
