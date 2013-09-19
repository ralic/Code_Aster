subroutine te0078(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_EVOL'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/connec.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
!
    integer :: nbres
    parameter     (nbres=3)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), elrefe, alias8
    character(len=16) :: phenom, option, nomte
    real(kind=8) :: valres(nbres), dfdx(9), dfdy(9), poids, r, tpg, theta, cp
    real(kind=8) :: orig(2), lambor(2), lambda, fluglo(2), fluloc(2), p(2, 2)
    real(kind=8) :: point(2), coorse(18), vectt(9), deltat, alpha, dtpgdx
    real(kind=8) :: dtpgdy, xnorm, xu, yu
    integer :: ndim, nno, nnos, kp, npg, i, j, k, itemps, ivectt, jgano, nnop2
    integer :: c(6, 9), ise, nse, nuno, ipoids, ivf, idfde, igeom, imate, itemp
    integer :: icamas, npg2, ipoid2, ivf2, idfde2, ibid
    logical :: aniso, global
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
!
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
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PVECTTR', 'E', ivectt)
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
!
!====
! 1.3 PREALABLES LIES A LA RECUPERATION DES DONNEES MATERIAUX
!====
    if (phenom .eq. 'THER') then
        nomres(1) = 'LAMBDA'
        nomres(2) = 'RHO_CP'
        aniso = .false.
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', [zr(itemps)],&
                    2, nomres, valres, icodre, 1)
        lambda = valres(1)
        cp = valres(2)
    else if (phenom .eq. 'THER_ORTH') then
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        nomres(3) = 'RHO_CP'
        aniso = .true.
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', [zr(itemps)],&
                    3, nomres, valres, icodre, 1)
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        cp = valres(3)
    else
        call utmess('F', 'ELEMENTS2_63')
    endif
!====
! 1.4 PREALABLES LIES A L'ANISOTROPIE
!====
    if (aniso) then
        call jevech('PCAMASS', 'L', icamas)
        if (zr(icamas) .gt. 0.d0) then
            global = .true.
            alpha = zr(icamas+1)*r8dgrd()
            p(1,1) = cos(alpha)
            p(2,1) = sin(alpha)
            p(1,2) = -sin(alpha)
            p(2,2) = cos(alpha)
        else
            global = .false.
            orig(1) = zr(icamas+4)
            orig(2) = zr(icamas+5)
        endif
    endif
!
!====
! 3.1 CALCULS TERMES DE RIGIDITE
!    POUR LES ELEMENTS LUMPES ET NON LUMPES
!====
!
!  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
    call connec(nomte, nse, nnop2, c)
    do 10 i = 1, nnop2
        vectt(i)=0.d0
10  continue
!
! ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 200 ise = 1, nse
        do 205 i = 1, nno
            do 205 j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
205          continue
        do 201 kp = 1, npg
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                        poids, dfdx, dfdy)
            r = 0.d0
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            do 202 i = 1, nno
! CALCUL DE T- ET DE GRAD(T-)
                r = r + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
                dtpgdx = dtpgdx + zr(itemp-1+c(ise,i)) * dfdx(i)
                dtpgdy = dtpgdy + zr(itemp-1+c(ise,i)) * dfdy(i)
202          continue
            if (lteatt(' ','AXIS','OUI')) poids = poids*r
            if (.not.aniso) then
                fluglo(1) = lambda*dtpgdx
                fluglo(2) = lambda*dtpgdy
            else
                if (.not.global) then
                    point(1)=0.d0
                    point(2)=0.d0
                    do 204 nuno = 1, nno
                        point(1)= point(1)+ zr(ivf+k+nuno-1)*coorse(2*&
                        (nuno-1)+1)
                        point(2)= point(2)+ zr(ivf+k+nuno-1)*coorse(2*&
                        (nuno-1)+2)
204                  continue
                    xu = orig(1) - point(1)
                    yu = orig(2) - point(2)
                    xnorm = sqrt( xu**2 + yu**2 )
                    xu = xu / xnorm
                    yu = yu / xnorm
                    p(1,1) = xu
                    p(2,1) = yu
                    p(1,2) = -yu
                    p(2,2) = xu
                endif
                fluglo(1) = dtpgdx
                fluglo(2) = dtpgdy
                fluloc(1) = p(1,1)*dtpgdx + p(2,1)*dtpgdy
                fluloc(2) = p(1,2)*dtpgdx + p(2,2)*dtpgdy
                fluloc(1) = lambor(1)*fluloc(1)
                fluloc(2) = lambor(2)*fluloc(2)
                fluglo(1) = p(1,1)*fluloc(1) + p(1,2)*fluloc(2)
                fluglo(2) = p(2,1)*fluloc(1) + p(2,2)*fluloc(2)
            endif
            do 203 i = 1, nno
                vectt(c(ise,i)) = vectt(&
                                  c(ise,i)) + poids * (theta- 1.0d0)*( fluglo(1)*dfdx(i) + fluglo&
                                  &(2)*dfdy(i)&
                                  )
203          continue
201      continue
!
!====
! 3.2 CALCULS TERMES DE MASSE
!    POUR LES ELEMENTS LUMPES
!====
!
        do 305 i = 1, nno
            do 305 j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
305          continue
        do 301 kp = 1, npg2
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoid2, idfde2, coorse,&
                        poids, dfdx, dfdy)
            r = 0.d0
            tpg = 0.d0
            do 302 i = 1, nno
! CALCUL DE T-
                r = r + coorse(2*(i-1)+1) * zr(ivf2+k+i-1)
                tpg = tpg + zr(itemp-1+c(ise,i)) * zr(ivf2+k+i-1)
302          continue
            if (lteatt(' ','AXIS','OUI')) then
                poids = poids*r
                if (r .eq. 0.d0) then
                    call utmess('F', 'ELEMENTS3_10')
                endif
            endif
!
            do 303 i = 1, nno
                vectt(c(ise,i)) = vectt( c(ise,i)) + poids * ( cp/ deltat*zr(ivf2+k+i-1)*tpg )
303          continue
301      continue
200  continue
!
! MISE SOUS FORME DE VECTEUR
!
    do 306 i = 1, nnop2
        zr(ivectt-1+i)=vectt(i)
306  continue
!
end subroutine
