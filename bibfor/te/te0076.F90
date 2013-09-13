subroutine te0076(option, nomte)
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
    implicit none
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
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!-----------------------------------------------------------------------
    integer :: icamas, ij, nbres, nuno
    real(kind=8) :: alpha, xnorm, xu, yu
!-----------------------------------------------------------------------
    parameter         (       nbres=2 )
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), elrefe, alias8
    character(len=16) :: phenom
    real(kind=8) :: valres(nbres)
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, theta, fluglo(2)
    real(kind=8) :: lambor(2), orig(2), p(2, 2), point(2)
    real(kind=8) :: fluloc(2), lambda
    real(kind=8) :: mrigt(9, 9), coorse(18)
    integer :: nno, kp, npg, i, j, k, itemps, imattt, nnos
    integer :: ipoids, ivf, idfde, igeom, imate, jgano, ndim
    integer :: c(6, 9), ise, nse, nnop2, ibid
    logical :: aniso, global
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
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATTTR', 'E', imattt)
    call jevech('PTEMPSR', 'L', itemps)
    theta = zr(itemps+2)
!
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
!
    aniso = .false.
    if (phenom .eq. 'THER') then
        nomres(1) = 'LAMBDA'
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', zr(itemps),&
                    1, nomres, valres, icodre, 1)
        lambda = valres(1)
    else if (phenom .eq. 'THER_ORTH') then
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', zr(itemps),&
                    2, nomres, valres, icodre, 1)
!
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        aniso = .true.
    else
        call utmess('F', 'ELEMENTS2_63')
    endif
!
    global = .false.
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
            orig(1) = zr(icamas+4)
            orig(2) = zr(icamas+5)
        endif
    endif
!
    call connec(nomte, nse, nnop2, c)
!
    do 11 i = 1, nnop2
        do 11 j = 1, nnop2
            mrigt(i,j)=0.d0
11      continue
!
! --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------
!
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
            if (lteatt(' ','AXIS','OUI')) then
                r = 0.d0
                do 102 i = 1, nno
                    r = r + coorse(2*(i-1)+1)*zr(ivf+k+i-1)
102              continue
                poids = poids*r
            endif
!
            if (.not.global .and. aniso) then
                point(1)=0.d0
                point(2)=0.d0
                do 104 nuno = 1, nno
                    point(1)= point(1) + zr(ivf+k+nuno-1)*coorse(2*(&
                    nuno-1)+1)
                    point(2)= point(2) + zr(ivf+k+nuno-1)*coorse(2*(&
                    nuno-1)+2)
104              continue
!
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
!
            do 103 i = 1, nno
                if (.not.aniso) then
                    fluglo(1) = lambda*dfdx(i)
                    fluglo(2) = lambda*dfdy(i)
                else
                    fluloc(1) = p(1,1)*dfdx(i) + p(2,1)*dfdy(i)
                    fluloc(2) = p(1,2)*dfdx(i) + p(2,2)*dfdy(i)
                    fluloc(1) = lambor(1)*fluloc(1)
                    fluloc(2) = lambor(2)*fluloc(2)
                    fluglo(1) = p(1,1)*fluloc(1) + p(1,2)*fluloc(2)
                    fluglo(2) = p(2,1)*fluloc(1) + p(2,2)*fluloc(2)
                endif
!
                do 103 j = 1, nno
                    mrigt(c(ise,i),c(ise,j)) = mrigt(&
                                               c(ise, i),&
                                               c(ise, j) ) + poids*theta* ( fluglo(1)*dfdx(j) + f&
                                               &luglo(2)* dfdy(j)&
                                               )
!
103              continue
101      continue
!
100  end do
!
! MISE SOUS FORME DE VECTEUR
!
    ij = imattt-1
    do 106 i = 1, nnop2
        do 106 j = 1, i
            ij = ij + 1
            zr(ij)=mrigt(i,j)
106      continue
end subroutine
