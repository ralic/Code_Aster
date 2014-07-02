subroutine te0234(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/defgen.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/effi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/moytpg.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 1D
!
!     OPTION : FORC_NODA (REPRISE)
!          -----------------------------------------------------------
!
!
    integer :: nbres, jnbspi, nbsp, itab(7)
!
    integer :: nbcou, npge, icontm, ideplm, ivectu, icou, inte, kpki, k1
!
    real(kind=8) :: cisail, zic, coef, rhos, rhot, epsx3, gsx3, sgmsx3
!
!---- DECLARATIONS LOCALES ( RAMENEES DE TE0239.F FULL_MECA )
!
    parameter (nbres=2)
    character(len=8) :: nomres(nbres), elrefe
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: dfdx(3), zero, un, deux
    real(kind=8) :: test, test2, eps, nu, h, cosa, sina, cour, r, tpg
    real(kind=8) :: jacp, kappa, correc
    real(kind=8) :: eps2d(4), sigtdi(5), sigmtd(5)
    real(kind=8) :: x3
    integer :: nno, nnos, jgano, ndim, kp, npg, i, k, icaco, iret
    integer :: ipoids, ivf, idfdk, igeom, imate
    aster_logical :: testl1, testl2
    real(kind=8) :: zmin, hic
!
!
    data zero,un,deux/0.d0,1.d0,2.d0/
!
!-- SHIFT POUR LES COURBURES
    call elref1(elrefe)
    eps = 1.d-3
!
!DEB
!
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdk, jgano=jgano)
!
!
!-- LECTURE DU COMPORTEMENT
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
    if (nbcou .gt. 30) then
        call utmess('F', 'ELEMENTS3_50')
    endif
!
    npge = 3
!
!---- LECTURES STANDARDS ( RAMENEES DE TE0239.F FULL_MECA )
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icaco)
    h = zr(icaco)
    kappa = zr(icaco+1)
    correc = zr(icaco+2)
!---- COTE MINIMALE SUR L'EPAISSEUR
!
    zmin = -h/2.d0
!---- EPAISSEUR DE CHAQUE COUCHE
!
    hic = h/nbcou
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call tecach('OOO', 'PCONTMR', 'L', iret, nval=7,&
                itab=itab)
    icontm=itab(1)
    nbsp=itab(7)
    if (nbsp .ne. npge*nbcou) then
        call utmess('F', 'ELEMENTS_4')
    endif
!
    call jevech('PDEPLMR', 'L', ideplm)
!---- INITIALISATION DU VECTEUR FORCE INTERNE
!
    call jevech('PVECTUR', 'E', ivectu)
    do 10 i = 1, 3*nno
        zr(ivectu+i-1) = 0.d0
 10 end do
!
    kpki = 0
    do 60 kp = 1, npg
!-- BOUCLE SUR LES POINTS D'INTEGRATION SUR LA SURFACE
!
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, jacp, cosa, sina)
!
        call r8inir(5, 0.d0, sigmtd, 1)
        r = zero
        call moytpg('RIGI', kp, npge, '+', tpg,&
                    iret)
!
        do 20 i = 1, nno
            r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
 20     continue
!
        call rcvala(zi(imate), ' ', 'ELAS', 1, 'TEMP',&
                    [tpg], 2, nomres, valres, icodre,&
                    1)
        nu = valres(2)
        cisail = valres(1)/ (un+nu)
        if (nomte .eq. 'MECXSE3') jacp = jacp*r
        test = abs(h*cour/deux)
        if (test .ge. un) correc = zero
        test2 = abs(h*cosa/ (deux*r))
        if (test2 .ge. un) correc = zero
!
        testl1 = (test.le.eps .or. correc.eq.zero)
        testl2 = (&
                 test2 .le. eps .or. correc .eq. zero .or. abs(cosa) .le. eps .or. abs(cour*r)&
                 .le. eps .or. abs(cosa-cour*r) .le. eps&
                 )
!
        do 50 icou = 1, nbcou
            do 40 inte = 1, npge
                if (inte .eq. 1) then
                    zic = zmin + (icou-1)*hic
                    coef = 1.d0/3.d0
                else if (inte.eq.2) then
                    zic = zmin + hic/2.d0 + (icou-1)*hic
                    coef = 4.d0/3.d0
                else
                    zic = zmin + hic + (icou-1)*hic
                    coef = 1.d0/3.d0
                endif
                x3 = zic
!
                if (testl1) then
                    rhos = 1.d0
                else
                    rhos = 1.d0 + x3*cour
                endif
                if (testl2) then
                    rhot = 1.d0
                else
                    rhot = 1.d0 + x3*cosa/r
                endif
!
!-- CALCULS DES COMPOSANTES DE DEFORMATIONS TRIDIMENSIONNELLES :
!-- EPSSS, EPSTT, EPSSX3
!-- (EN FONCTION DES DEFORMATIONS GENERALISEES :ESS,KSS,ETT,KTT,GS)
!-- DE L'INSTANT PRECEDANT ET DES DEFORMATIONS INCREMENTALES
!-- DE L'INSTANT PRESENT
!
                call defgen(testl1, testl2, nno, r, x3,&
                            sina, cosa, cour, zr(ivf+k), dfdx,&
                            zr(ideplm), eps2d, epsx3)
!
                if (nomte .eq. 'METDSE3' .or. nomte .eq. 'METCSE3') then
                    eps2d(2) = 0.d0
                endif
!
!-- CONSTRUCTION DE LA DEFORMATION GSX3 ET DE LA CONTRAINTE SGMSX3
!
                gsx3 = 2.d0* epsx3
                sgmsx3 = cisail*kappa*gsx3/2.d0
!-- JEU D'INDICES DANS LA BOUCLE SUR LES POINTS D'INTEGRATION
!                                  DE LA SURFACE MOYENNE
!
                kpki = kpki + 1
                k1 = 4* (kpki-1)
!-- CALCUL DES CONTRAINTES TILDE, ON A REMPLACE ICONTP PAR ICONTM
!
                if (nomte .eq. 'MECXSE3') then
!                                                    AXISYM
                    sigtdi(1) = zr(icontm-1+k1+1)/rhos
                    sigtdi(2) = x3*zr(icontm-1+k1+1)/rhos
                    sigtdi(3) = zr(icontm-1+k1+2)/rhot
                    sigtdi(4) = x3*zr(icontm-1+k1+2)/rhot
                    sigtdi(5) = sgmsx3/rhos
                else
                    sigtdi(1) = zr(icontm-1+k1+1)/rhos
                    sigtdi(2) = x3*zr(icontm-1+k1+1)/rhos
                    sigtdi(3) = sgmsx3/rhos
                    sigtdi(4) = 0.d0
                    sigtdi(5) = 0.d0
                endif
!
                do 30 i = 1, 5
                    sigmtd(i) = sigmtd(i) + sigtdi(i)*0.5d0*hic*coef
 30             continue
!
 40         continue
 50     continue
!
!-- CALCUL DES EFFORTS INTERIEURS
!
        call effi(nomte, sigmtd, zr(ivf+k), dfdx, jacp,&
                  sina, cosa, r, zr(ivectu))
!
 60 continue
!
end subroutine
