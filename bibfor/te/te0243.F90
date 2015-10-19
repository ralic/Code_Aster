subroutine te0243(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/connec.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/ppgan2.h"
#include "asterfort/rcdiff.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvalb.h"
#include "asterfort/runge6.h"
#include "asterfort/teattr.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
!                          OPTION : 'RESI_RIGI_MASS'
!                          ELEMENTS 2D LUMPES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE
!
    real(kind=8) :: beta, lambda, theta, deltat, khi, tpg, tpsec
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, r8bid, diff
    real(kind=8) :: dtpgdx, dtpgdy, hydrgm(9), hydrgp(9)
    real(kind=8) :: coorse(18), vectt(9), err
    real(kind=8) :: chal(1), tpgm
    integer :: icodre(1)
    character(len=8) :: elrefe, alias8
    integer :: ndim, nno, nnos, kp, npg, i, j, k, itemps, ifon(3)
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icomp, itempi, iveres, jgano, ipoid2, npg2
    integer :: c(6, 9), ise, nse, nnop2, ivf2, idfde2
    integer :: isechi, isechf, ibid, jgano2
    integer :: ihydr, ihydrp, itempr
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
! --- INDMAT : INDICE SAUVEGARDE POUR LE MATERIAU
!
!C      PARAMETER        ( INDMAT = 8 )
!
! DEB ------------------------------------------------------------------
!
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'QU9') elrefe='QU4'
        if (alias8(6:8) .eq. 'TR6') elrefe='TR3'
        call elrefe_info(elrefe=elrefe,fami='NOEU',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano2)
    else
        call elrefe_info(elrefe=elrefe,fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano2)
    endif
!
    call elrefe_info(elrefe=elrefe,fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PRESIDU', 'E', iveres)
!
!
!
    if ((zk16(icomp)(1:5).eq.'SECH_')) then
        if (zk16(icomp)(1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp)(1:10) .eq. 'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!          ISECHI ET ISECHF SONT FICTIFS
            isechi = itempi
            isechf = itempi
        endif
    endif
!
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
    khi = zr(itemps+3)
!
    if (zk16(icomp)(1:5) .ne. 'SECH_') then
        call ntfcma(zk16(icomp), zi(imate), ifon)
    endif
!
!
! --  -RECUPERATION DES PARAMETRES POUR L HYDRATATION
!
    if (zk16(icomp)(1:9) .eq. 'THER_HYDR') then
        call jevech('PHYDRPM', 'L', ihydr)
        call jevech('PHYDRPP', 'E', ihydrp)
        call jevech('PTEMPER', 'L', itempr)
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'THER_HYDR', 0, ' ', [r8bid],&
                    1, 'CHALHYDR', chal, icodre, 1)
        do 150 kp = 1, npg2
            k = nno*(kp-1)
            hydrgm(kp)=0.d0
            do 160 i = 1, nno
                hydrgm(kp)=hydrgm(kp)+zr(ihydr)*zr(ivf2+k+i-1)
160          continue
150      continue
    endif
!
!     CALCUL LUMPE
!     ------------
!  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
!
    call connec(nomte, nse, nnop2, c)
    do 10 i = 1, nnop2
        vectt(i)=0.d0
10  end do
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 200 ise = 1, nse
!
        do i = 1, nno
            do j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
            enddo
        enddo
!
        if (zk16(icomp)(1:5) .eq. 'THER_') then
!
!
! ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
!
            do 101 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                dtpgdx = 0.d0
                dtpgdy = 0.d0
                do 102 i = 1, nno
                    r = r + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
                    tpg = tpg + zr(itempi-1+c(ise,i)) * zr(ivf+k+i-1)
                    dtpgdx = dtpgdx + zr(itempi-1+c(ise,i)) * dfdx(i)
                    dtpgdy = dtpgdy + zr(itempi-1+c(ise,i)) * dfdy(i)
102              continue
!
! --------------
!
                call rcfode(ifon(2), tpg, lambda, r8bid)
!
                if (lteatt('AXIS','OUI')) poids = poids*r
!!DIR$_IVDEP
                do 105 i = 1, nno
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids * theta*lambda* (dfdx(i)*dtpgdx+dfdy(i)*d&
                                      &tpgdy&
                                      )
105              continue
101          continue
!
! ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------
!
            do i = 1, nno
                do j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
                enddo
            enddo
!
            call ntfcma(zk16(icomp), zi(imate), ifon)
            do 401 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                do 402 i = 1, nno
                    r = r + coorse(2*(i-1)+1) * zr(ivf2+k+i-1)
                    tpg = tpg + zr(itempi-1+c(ise,i)) * zr(ivf2+k+i-1)
402              continue
!
! ---  RESOLUTION DE L EQUATION D HYDRATATION
!
                if (zk16(icomp)(1:9) .eq. 'THER_HYDR') then
                    tpgm = 0.d0
                    do 103 i = 1, nno
                        tpgm = tpgm + zr(itempr+i-1)*zr(ivf2+k+i-1)
103                  continue
                    call runge6(ifon(3), deltat, tpg, tpgm, hydrgm(kp),&
                                hydrgp(kp), err)
                endif
!
                call rcfode(ifon(1), tpg, beta, r8bid)
                if (lteatt('AXIS','OUI')) poids = poids*r
                if (zk16(icomp)(1:9) .eq. 'THER_HYDR') then
! --- THERMIQUE NON LINEAIRE AVEC HYDRATATION
                    do 104 i = 1, nno
                        k=(kp-1)*nno
                        vectt(c(ise,i)) = vectt(&
                                          c(ise,i)) + poids * (beta-chal(1)*hydrgp(kp))/deltat* k&
                                          &hi*zr(ivf2+k+i- 1&
                                          )
104                  continue
                else
! --- THERMIQUE NON LINEAIRE SEULE
                    do 404 i = 1, nno
                        vectt(c(ise,i)) = vectt(&
                                          c(ise, i) ) + poids * beta/deltat*khi*zr(ivf2+k+i-1)
404                  continue
                endif
401          continue
!
        else if (zk16(icomp)(1:5).eq.'SECH_') then
!
            do 203 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                dtpgdx = 0.d0
                dtpgdy = 0.d0
                tpsec = 0.d0
                do 201 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf+k+i-1)
                    tpg = tpg + zr(itempi-1+c(ise,i)) *zr(ivf+k+i-1)
                    dtpgdx = dtpgdx + zr(itempi-1+c(ise,i)) *dfdx(i)
                    dtpgdy = dtpgdy + zr(itempi-1+c(ise,i)) *dfdy(i)
                    tpsec = tpsec + zr(isechf-1+c(ise,i)) *zr(ivf+k+i- 1)
201              continue
                call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
                if (lteatt('AXIS','OUI')) poids = poids*r
!
                do 202 i = 1, nno
                    k=(kp-1)*nno
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids * theta*diff*(dfdx(i)*dtpgdx+dfdy(i)*dtpg&
                                      &dy&
                                      )
202              continue
203          continue
!
! ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------
!
            do 303 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                do 301 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf2+k+i-1)
                    tpg = tpg + zr(itempi-1+c(ise,i)) *zr(ivf2+k+i-1)
301              continue
                if (lteatt('AXIS','OUI')) poids = poids*r
!
                do 302 i = 1, nno
                    k=(kp-1)*nno
                    vectt(c(ise,i)) = vectt(c(ise,i)) +poids*(1.d0/ deltat*khi*zr(ivf2+k+i-1)*tpg&
                                      )
302              continue
303          continue
!
        endif
!
200  end do
!
! MISE SOUS FORME DE VECTEUR
    do 306 i = 1, nnop2
        zr(iveres-1+i)=vectt(i)
306  end do
    if (zk16(icomp) (1:9) .eq. 'THER_HYDR') call ppgan2(jgano2, 1, 1, hydrgp, zr(ihydrp))
! FIN ------------------------------------------------------------------
end subroutine
