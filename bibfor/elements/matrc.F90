subroutine matrc(nno, kcis, matc, vectt)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
!
    integer :: nno
    real(kind=8) :: kcis, matc(5, 5), vectt(3, 3)
!
    real(kind=8) :: valres(5), valpar(1)
    integer :: icodre(5)
    character(len=4) :: fami
    character(len=8) :: nomres(5), nompar
    character(len=10) :: phenom
    real(kind=8) :: young, nu, nult, nutl, alpha, beta
    real(kind=8) :: passag(3, 3), pas2(2, 2), dorth(3, 3), work(3, 3), d(3, 3)
    real(kind=8) :: dcis(2, 2), c, s, d2(2, 2), el, et, glt, gtn, delta
    real(kind=8) :: r8bid4(4)
    integer :: i, j, jmate, nbv, nbpar, jcoqu, iret
    integer :: ndim, nnos, npg, ipoids, ivf, idfde, jgano, jcou
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    do i = 1, 5
        do j = 1, 5
            matc(i,j) = 0.d0
        end do
    end do
!
    call jevech('PMATERC', 'L', jmate)
    call jevech('PNBSP_I', 'L', jcou)
!
    nbpar = 1
    nompar = 'TEMP'
    call moytem(fami, npg, 3*zi(jcou), '+', valpar(1),&
                iret)
!
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS') then
        nbv = 2
        nomres(1) = 'E'
        nomres(2) = 'NU'
!
!        ------ MATERIAU ISOTROPE --------------------------------------
!
        call rcvala(zi(jmate), ' ', phenom, nbpar, nompar,&
                    valpar(1), nbv, nomres, valres, icodre,&
                    1)
!
        young = valres(1)
        nu = valres(2)
!
! ------ CONSTRUCTION DE LA MATRICE DE COMPORTEMENT MATC : (5,5)
!
        matc(1,1) = young/ (1.d0-nu*nu)
        matc(1,2) = matc(1,1)*nu
        matc(2,1) = matc(1,2)
        matc(2,2) = matc(1,1)
        matc(3,3) = young/2.d0/ (1.d0+nu)
        matc(4,4) = matc(3,3)*kcis
        matc(5,5) = matc(4,4)
!
    else if (phenom.eq.'ELAS_ORTH') then
!
        nomres(1) = 'E_L'
        nomres(2) = 'E_T'
        nomres(3) = 'NU_LT'
        nomres(4) = 'G_LT'
        nomres(5) = 'G_TN'
        nbv = 5
!
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----   ET DU TEMPS
!        -----------
        call rcvala(zi(jmate), ' ', phenom, nbpar, nompar,&
                    valpar(1), nbv, nomres, valres, icodre,&
                    1)
!
        el = valres(1)
        et = valres(2)
        nult = valres(3)
        glt = valres(4)
        gtn = valres(5)
        nutl = et*nult/el
        delta = 1.d0 - nult*nutl
        dorth(1,1) = el/delta
        dorth(1,2) = nult*et/delta
        dorth(1,3) = 0.d0
        dorth(2,2) = et/delta
        dorth(2,1) = dorth(1,2)
        dorth(2,3) = 0.d0
        dorth(3,1) = 0.d0
        dorth(3,2) = 0.d0
        dorth(3,3) = glt
!
! ---   DETERMINATION DES MATRICE DE PASSAGE DES REPERES INTRINSEQUES
! ---   AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
! ---   AU REPERE UTILISATEUR :
!
! ---   RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
! ---   PAR RAPPORT AU REPERE GLOBAL :
!
        call jevech('PCACOQU', 'L', jcoqu)
!
        alpha = zr(jcoqu+1)*r8dgrd()
        beta = zr(jcoqu+2)*r8dgrd()
!
!       CALCUL DU COSINUS ET DU SINUS DE L'ANGLE ENTRE LE REPERE
!       INTRINSEQUE ET LE REPERE UTILISATEUR
        call coqrep(vectt, alpha, beta, r8bid4, r8bid4,&
                    c, s)
!
! ----   TENSEUR D'ELASTICITE DANS LE REPERE INTRINSEQUE :
! ----   D_GLOB = PASSAG_T * D_ORTH * PASSAG
!
        do i = 1, 3
            do j = 1, 3
                passag(i,j) = 0.d0
            end do
        end do
        passag(1,1) = c*c
        passag(2,2) = c*c
        passag(1,2) = s*s
        passag(2,1) = s*s
        passag(1,3) = c*s
        passag(3,1) = -2.d0*c*s
        passag(2,3) = -c*s
        passag(3,2) = 2.d0*c*s
        passag(3,3) = c*c - s*s
        call utbtab('ZERO', 3, 3, dorth, passag,&
                    work, d)
!
        do i = 1, 3
            do j = 1, 3
                matc(i,j) = d(i,j)
            end do
        end do
!
        dcis(1,1) = glt
        dcis(1,2) = 0.d0
        dcis(2,1) = 0.d0
        dcis(2,2) = gtn
        pas2(1,1) = c
        pas2(2,2) = c
        pas2(1,2) = s
        pas2(2,1) = -s
!
        call utbtab('ZERO', 2, 2, dcis, pas2,&
                    work, d2)
        do i = 1, 2
            do j = 1, 2
                matc(3+i,3+j) = d2(i,j)
            end do
        end do
!
    else
        call utmess('F', 'ELEMENTS_45', sk=phenom)
    endif
!
end subroutine
