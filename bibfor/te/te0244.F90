subroutine te0244(option, nomte)
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
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_EVOLNI'
!                          ELEMENTS 2D ISOPARAMETRIQUES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE LUMPE SANS HYDRATATION, NI SECHAGE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! PARAMETRES D'APPEL
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/connec.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rcdiff.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvalb.h"
#include "asterfort/teattr.h"
!
    character(len=16) :: nomte, option
!
!
    character(len=8) :: elrefe, alias8
    integer :: icodre(1)
    real(kind=8) :: beta, dbeta, lambda, dfdx(9), dfdy(9), poids, r, tpg
    real(kind=8) :: theta, deltat, dtpgdx, dtpgdy, coorse(18), tpgbuf, vectt(9)
    real(kind=8) :: vecti(9), dlambd, diff, tpsec, chal(1), hydrpg(9)
    integer :: ndim, nno, nnos, kp, npg, i, j, k, itemps, jgano, ipoids, ivf
    integer :: idfde, igeom, imate, icomp, ifon(3), itemp, ivectt, ivecti
    integer :: c(6, 9), ise, nse, nnop2, npg2, ipoid2, ivf2, idfde2, isechf
    integer :: isechi, ibid, ihydr
    aster_logical :: laxi, lhyd
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'QU9') elrefe='QU4'
        if (alias8(6:8) .eq. 'TR6') elrefe='TR3'
        call elrefe_info(elrefe=elrefe, fami='NOEU', ndim=ndim, nno=nno, nnos=nnos,&
                         npg=npg2, jpoids=ipoid2, jvf=ivf2, jdfde=idfde2, jgano=jgano)
    else
        call elrefe_info(elrefe=elrefe, fami='MASS', ndim=ndim, nno=nno, nnos=nnos,&
                         npg=npg2, jpoids=ipoid2, jvf=ivf2, jdfde=idfde2, jgano=jgano)
    endif
!
    call elrefe_info(elrefe=elrefe, fami='RIGI', ndim=ndim, nno=nno, nnos=nnos,&
                     npg=npg, jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    if (lteatt('AXIS','OUI')) then
        laxi = .true.
    else
        laxi = .false.
    endif
!
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PVECTTR', 'E', ivectt)
    call jevech('PVECTTI', 'E', ivecti)
!
!====
! 1.3 PREALABLES LIES A L'HYDRATATION ET AU SECHAGE
!====
    if (zk16(icomp)(1:5) .eq. 'SECH_') then
        if (zk16(icomp)(1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp)(1:10) .eq. 'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!          ISECHI ET ISECHF SONT FICTIFS
            isechi = itemp
            isechf = itemp
        endif
    endif
!
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
    if (zk16(icomp)(1:5) .ne. 'SECH_') then
        call ntfcma(zk16(icomp), zi(imate), ifon)
    endif
!====
! 1.4 PREALABLES LIES A L'HYDRATATION
!====
    if (zk16(icomp)(1:9) .eq. 'THER_HYDR') then
        lhyd = .true.
        call jevech('PHYDRPM', 'L', ihydr)
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'THER_HYDR', 0, ' ', [0.d0],&
                    1, 'CHALHYDR', chal, icodre, 1)
        do 150 kp = 1, npg2
            k = nno*(kp-1)
            hydrpg(kp)=0.d0
            do 160 i = 1, nno
                hydrpg(kp)=hydrpg(kp)+zr(ihydr)*zr(ivf2+k+i-1)
160         continue
150     continue
    else
        lhyd = .false.
    endif
!====
! 1.5 PREALABLES LIES AUX ELEMENTS LUMPES
!====
!  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
!
    call connec(nomte, nse, nnop2, c)
    do 10 i = 1, nnop2
        vectt(i)=0.d0
        vecti(i)=0.d0
 10 end do
!
!====
! 2. CALCULS DU TERME DE RIGIDITE DE L'OPTION
!====
! ----- 2EME FAMILLE DE PTS DE GAUSS/BOUCLE SUR LES SOUS-ELEMENTS
!
    do 200 ise = 1, nse
!
        if (zk16(icomp)(1:5) .eq. 'THER_') then
!
            do 305 i = 1, nno
                do 305 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
305             continue
!
            do 301 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                dtpgdx = 0.d0
                dtpgdy = 0.d0
                do 302 i = 1, nno
! CALCUL DE T- ET DE SON GRADIENT
                    tpg = tpg + zr(itemp-1+c(ise,i)) * zr(ivf+k+i-1)
                    dtpgdx = dtpgdx + zr(itemp-1+c(ise,i)) * dfdx(i)
                    dtpgdy = dtpgdy + zr(itemp-1+c(ise,i)) * dfdy(i)
302             continue
                if (laxi) then
                    do 303 i = 1, nno
! CALCUL DE R POUR JACOBIEN
                        r = r + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
303                 continue
                    poids = poids*r
                endif
!
! CALCUL DES CARACTERISTIQUES MATERIAUX
! ON LES EVALUE AVEC TPG=T-
                tpgbuf = tpg
                call rcfode(ifon(2), tpgbuf, lambda, dlambd)
!
! CALCUL STD A 2 OUTPUTS (LE DEUXIEME NE SERT QUE POUR LA PREDICTION)
!
                do 320 i = 1, nno
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i))- poids * (1.0d0-theta)*lambda*(dfdx(i)*dtpgdx+dfd&
                                      &y(i)* dtpgdy&
                                      )
                    vecti(c(ise,i)) = vecti(&
                                      c(ise,i))- poids * (1.0d0-theta)*lambda*(dfdx(i)*dtpgdx+dfd&
                                      &y(i)* dtpgdy&
                                      )
320             continue
! FIN DE LA BOUCLE SUR LES PT DE GAUSS
301         continue
!
!====
! 3. CALCULS DU TERME DE RIGIDITE DE L'OPTION
!====
! ------- 3EME FAMILLE DE PTS DE GAUSS -----------
!
            do 405 i = 1, nno
                do 405 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
405             continue
!
            do 401 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                do 402 i = 1, nno
! CALCUL DE T-
                    tpg = tpg + zr(itemp-1+c(ise,i)) * zr(ivf2+k+i-1)
402             continue
                if (laxi) then
                    do 403 i = 1, nno
! CALCUL DE R POUR JACOBIEN
                        r = r + coorse(2*(i-1)+1) * zr(ivf2+k+i-1)
403                 continue
                    poids = poids*r
                endif
!
! CALCUL DES CARACTERISTIQUES MATERIAUX
! ON LES EVALUE AVEC TPG=T-
                tpgbuf = tpg
                call rcfode(ifon(1), tpgbuf, beta, dbeta)
                if (lhyd) then
! THER_HYDR
                    do 420 i = 1, nno
                        vectt(c(ise,i)) = vectt(&
                                          c(ise,i)) + poids * ((beta-chal(1)*hydrpg(kp))* zr(ivf2&
                                          &+k+i-1)/deltat&
                                          )
                        vecti(c(ise,i)) = vecti(&
                                          c(ise,i)) + poids * ((dbeta*tpg-chal(1)*hydrpg(kp))* zr&
                                          &(ivf2+k+i-1)/ deltat&
                                          )
420                 continue
                else
! THER_NL
! CALCUL A 2 OUTPUTS (LE DEUXIEME NE SERT QUE POUR LA PREDICTION)
!
                    do 421 i = 1, nno
                        vectt(c(ise,i)) = vectt(c(ise, i) ) + poids * beta/deltat*zr(ivf2+k+i-1)
                        vecti(c(ise,i)) = vecti(&
                                          c(ise, i) ) + poids * dbeta*tpg/deltat*zr(ivf2+k+i-1)
421                 continue
! FIN BOUCLE LHYD
                endif
! FIN DE BOUCLE SUR LES PT DE GAUSS
401         continue
!
        else if (zk16(icomp)(1:5).eq.'SECH_') then
!
!        CALCULS DU TERME DE RIGIDITE DE L'OPTION
!
            do 307 i = 1, nno
                do 307 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
307             continue
!
            do 310 kp = 1, npg
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                dtpgdx = 0.d0
                dtpgdy = 0.d0
                tpsec = 0.d0
                do 308 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf+k+i-1)
                    tpg = tpg + zr(itemp-1+c(ise,i)) *zr(ivf+k+i-1)
                    dtpgdx = dtpgdx + zr(itemp-1+c(ise,i)) *dfdx(i)
                    dtpgdy = dtpgdy + zr(itemp-1+c(ise,i)) *dfdy(i)
                    tpsec = tpsec + zr(isechi-1+c(ise,i)) *zr(ivf+k+i- 1)
308             continue
                call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
                if (laxi) poids = poids*r
!
                do 309 i = 1, nno
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids *( -(1.0d0-theta)*diff*(dfdx(i)*dtpgdx+df&
                                      &dy(i)* dtpgdy)&
                                      )
                    vecti(c(ise,i)) = vectt(c(ise,i))
309             continue
310         continue
!
!  CALCULS DU TERME DE MASSE DE L'OPTION
!
            do 311 i = 1, nno
                do 311 j = 1, 2
                    coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
311             continue
!
            do 314 kp = 1, npg2
                k=(kp-1)*nno
                call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                            poids, dfdx, dfdy)
                r = 0.d0
                tpg = 0.d0
                do 312 i = 1, nno
                    r = r + coorse(2*(i-1)+1) *zr(ivf2+k+i-1)
                    tpg = tpg + zr(itemp-1+c(ise,i)) *zr(ivf2+k+i-1)
312             continue
                if (laxi) poids = poids*r
!
                do 313 i = 1, nno
                    vectt(c(ise,i)) = vectt( c(ise,i)) + poids * ( tpg/deltat*zr(ivf2+k+i-1) )
                    vecti(c(ise,i)) = vectt(c(ise,i))
313             continue
314         continue
!
        endif
! FIN DE BOUCLE SUR LES SOUS-ELEMENTS
200 end do
!
! MISE SOUS FORME DE VECTEUR
    do 500 i = 1, nnop2
        zr(ivectt-1+i)=vectt(i)
        zr(ivecti-1+i)=vecti(i)
500 end do
! FIN ------------------------------------------------------------------
end subroutine
