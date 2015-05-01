subroutine nmvmpo(fami, npg, nno, option, nc,&
                  xl, wgauss, icodma, sect, u,&
                  du, contm, contp, fl, klv)
!
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
!
    implicit none
    character(len=*) :: fami, option
    integer :: npg, nno, nc, icodma
    real(kind=8) :: xl, sect(*), u(nno*nc), du(nno*nc), fl(nno*nc), klv(*)
    real(kind=8) :: contm(npg*nc), contp(npg*nc), wgauss(npg)
!
#include "asterf_types.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/mavec.h"
#include "asterfort/moytem.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utbtab.h"
#include "asterfort/utlcal.h"
#include "asterfort/verifm.h"
#include "blas/dscal.h"
!
! --------------------------------------------------------------------------------------------------
!
!                       COMPORTEMENT ELAS POUR LES MECA_POU_D_TG
!
! --------------------------------------------------------------------------------------------------
!
!   IN :
!       fami        : famille de point de gauss
!       npg         : nombre de point de gauss
!       nno         : nombre de noeuds
!       option      : RAPH_MECA  FULL_MECA RIGI_MECA_TANG
!       nc          : nombre de composantes
!       xl          : longueur de l element
!       wgauss      : poids des points de Gauss
!       icodma      : adresse du materiau code
!       sect        : caracteristiques de la section
!       u           : vecteur deplacement a l'instant precedent
!       du          : vecteur accroissement de deplacement
!       contm       : contraintes a l'instant precedent
!   OUT :
!       contp       : contraintes à l'instant actuel
!       fl          : force nodale = bt*contp
!       klv         : matrice de rigidité tangente
!
! --------------------------------------------------------------------------------------------------
!
    integer :: codres(2), itemp, iret
    character(len=2) :: nomres(2)
    aster_logical :: vecteu, matric
    integer :: dimklv, kp, kk, i, j, k
    real(kind=8) :: eps(nc), deps(nc), fg(nno*nc), sigp(nc), sigm(nc)
    real(kind=8) :: e, nu, g, phiy, phiz, xls2, epsthf(1), epsthd(1)
    real(kind=8) :: aa, xiy, xiz, alfay, alfaz, xjx, xjg
    real(kind=8) :: valres(2)
    real(kind=8) :: temp
    real(kind=8) :: hoel(nc, nc), hota(nc, nc), d1b(nc, nno*nc)
    real(kind=8) :: work(nc, nno*nc), rg0(nno*nc, nno*nc)
!   pour la thermique
    real(kind=8) :: temm, em, num, f, df
!
! --------------------------------------------------------------------------------------------------
!
    xls2 = xl/2.d0
    dimklv = 2*nc*(2*nc+1)/2
!
!   booléens pratiques
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
    fl(1:nno*nc) = 0.0d0
!
    hoel(:,:) = 0.0d0
    hota(:,:) = 0.0d0
    d1b(:,:) = 0.0d0
    work(:,:) = 0.0d0
    rg0(:,:) = 0.0d0
    fg(:) = 0.0d0
!
!   Température
    call verifm(fami, npg, 1, '-', icodma,&
                'ELAS', 1, epsthf, iret)
    call verifm(fami, npg, 1, 'T', icodma,&
                'ELAS', 1, epsthd, iret)
    itemp = 0
    if (iret .eq. 0) itemp = 1
    nomres(1) = 'E'
    nomres(2) = 'NU'
!   Thermique à T+
    call moytem(fami, npg, 1, '+', temp,&
                iret)
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'ELAS', 1, 'TEMP', [temp],&
                2, nomres, valres, codres, 1)
    e = valres(1)
    nu = valres(2)
    g = e / (2.d0*(1.d0+nu))
!   thermique à T-
    call moytem(fami, npg, 1, '-', temm,&
                iret)
    call rcvalb(fami, 1, 1, '-', icodma,&
                ' ', 'ELAS', 1, 'TEMP', [temm],&
                2, nomres, valres, codres, 1)
    em = valres(1)
    num = valres(2)
!
!   caractéristiques de la section
    aa = sect(1)
    xiy = sect(2)
    xiz = sect(3)
    alfay = sect(4)
    alfaz = sect(5)
    xjx = sect(8)
    xjg = sect(9)
!
!   matériau integré sur la section
    hoel(1,1) = e*aa
    hoel(2,2) = g*aa/alfay
    hoel(3,3) = g*aa/alfaz
    phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
    phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
    hoel(4,4) = g*xjx
    hoel(5,5) = e*xiy
    hoel(6,6) = e*xiz
    hoel(7,7) = e*xjg
!
!   boucle sur les points de gauss
    do kp = 1, npg
!       calcul de d1b ( epsi = d1b * u )
        call jsd1ff(kp, xl, phiy, phiz, d1b)
!       calcul de eps, deps et sigm (effort au pt de gauss)
!       et de dsigm = incrément d'effort élastique
        eps(:) = 0.d0
        deps(:)= 0.0d0
        sigm(:) = 0.d0
        do i = 1, nc
            do j = 1, nno*nc
                eps(i) = eps(i) + d1b(i,j)* u(j)
                deps(i) = deps(i) + d1b(i,j)*du(j)
            enddo
            sigm(i) = contm(nc*(kp-1)+i)*e/em
        enddo
        if ((epsthd(1).ne.0.d0) .and. (itemp.ne.0)) then
            f = epsthf(1)
            df= epsthd(1)
            eps(1) = eps(1)- f
            deps(1)=deps(1)-df
        endif
!       Élasticité
        do i = 1, nc
            hota(i,i) = hoel(i,i)
            sigp(i)=sigm(i)+hoel(i,i)*deps(i)
        enddo
!       calcul de bt*h*b
        if (matric) then
            call dscal(nc*nc, xls2, hota, 1)
            call dscal(nc*nc, wgauss(kp), hota, 1)
            call utbtab('CUMU', nc, nno*nc, hota, d1b,&
                        work, rg0)
        endif
!       Les contraintes "+" et fl
        if (vecteu) then
            do i = 1, nc
                contp(nc*(kp-1)+i) = sigp(i)
            enddo
            do k = 1, nno*nc
                do kk = 1, nc
                    fl(k)=fl(k) + xls2*sigp(kk)*d1b(kk,k)*wgauss(kp)
                enddo
            enddo
        endif
!
    enddo
!
    if (matric) then
        call mavec(rg0, nno*nc, klv, dimklv)
    endif
!
end subroutine
