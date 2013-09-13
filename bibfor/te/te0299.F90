subroutine te0299(option, nomte)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       CALCUL DES FACTEURS D'INTENSITÉ DES CONTRAINTES
!       A PARTIR DE LA FORME BILINEAIRE SYMETRIQUE G ET
!       DES DEPLACEMENTS SINGULIERS EN FOND DE FISSURE
!      POUR LES ELEMENTS ISOPARAMETRIQUES 2D
!
!       OPTION : 'CALC_K_G'    (CHARGES REELLES)
!                'CALC_K_G_F'  (CHARGES FONCTIONS)
!
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/chauxi.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/gbilin.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmgeom.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
!
    integer :: icodre(3)
    integer :: codrho
    integer :: ipoids, ivf, idfde, nno, kp, npg, compt, ier, nnos, jgano, icomp
    integer :: igeom, ithet, irota, ipesa, ificg, idepl, iret, ipuls
    integer :: imate, iforc, iforf, ifond, itemps, k, i, j, kk, l, ndim, jtab(7)
    integer :: ino
!
    real(kind=8) :: dfdi(18), f(3, 3), eps(6), fno(18)
    real(kind=8) :: dudm(3, 4), dfdm(3, 4), dtdm(3, 4), der(4)
    real(kind=8) :: du1dm(3, 4), du2dm(3, 4)
    real(kind=8) :: rho, om, omo, rbid, e, nu, rbid2(3, 3, 3)
    real(kind=8) :: thet, tno(20), tgdm(3)
    real(kind=8) :: xag, yag, xg, yg, xa, ya, norm, a, b
    real(kind=8) :: c1, c2, c3, cs, u1(2), u2(2)
    real(kind=8) :: e1(3), e2(3), e3(3), p(3, 3), invp(3, 3), rg, phig
    real(kind=8) :: u1l(3), u2l(3), rbid3(3), mu, ka, rbid4(3, 4)
    real(kind=8) :: th, valres(3), valpar(4)
    real(kind=8) :: coefk
    real(kind=8) :: guv, guv1, guv2, k1, k2, g, poids, ray, puls
!
    character(len=4) :: fami
    character(len=8) :: nomres(3), nompar(4)
    character(len=16) :: phenom, compor(4)
!
    logical :: lcour, lmoda, fonc, lpesa, lrota
    logical :: axi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! --- RECUPERATION DES CHAMPS IN
!
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PFISSR', 'L', ifond)
!
!
! --- RECUPERATION DU CHAMP OUT
!
    call jevech('PGTHETA', 'E', ificg)
!
    axi = .false.
    if (lteatt(' ','AXIS','OUI')) axi = .true.
!
    g = 0.d0
    k1 = 0.d0
    k2 = 0.d0
    rho = 0.d0
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
! --- PAS DE CALCUL DE G POUR LES ELEMENTS OU THETA EST NULLE
!
    compt = 0
    do i = 1, nno
        thet = 0.d0
        do j = 1, ndim
            thet = thet + abs(zr(ithet+ndim*(i-1)+j-1))
        end do
        if (thet .lt. r8prem()) compt = compt + 1
    end do
    if (compt .eq. nno) goto 999
!
!
! --- RECUPERATION DES FORCES
!
    if (option .eq. 'CALC_K_G_F') then
        fonc = .true.
        call jevech('PFFVOLU', 'L', iforf)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        if (ndim .eq. 2) then
            nompar(3) = 'INST'
        else if (ndim.eq.3) then
            nompar(3) = 'Z'
            nompar(4) = 'INST'
        endif
        valpar(ndim+1) = zr(itemps)
    else if (option.eq.'CALC_K_G') then
        fonc =.false.
        call jevech('PFRVOLU', 'L', iforc)
    else
        ASSERT(.false.)
    endif
!
    lpesa = .false.
    call tecach('ONN', 'PPESANR', 'L', 7, jtab,&
                iret)
    ipesa=jtab(1)
    if (iret .eq. 0) then
        lpesa = .true.
    endif
!
    lrota = .false.
    call tecach('ONN', 'PROTATR', 'L', 7, jtab,&
                iret)
    irota=jtab(1)
    if (iret .eq. 0) then
        lrota = .true.
    endif
!
! --- VERFICATION DU COMPORTEMENT
!
    do i = 1, 4
        compor(i) = zk16(icomp+i-1)
    end do
!
    if ((compor(1).ne.'ELAS' ) .or. (compor(3).eq.'GROT_GDEP') .or.&
        (compor(4).eq.'COMP_INCR')) then
        call utmess('F', 'RUPTURE1_24')
    endif
!
! --- RECUPERATION DE LA PULSATION
!
    lmoda = .false.
    call tecach('ONN', 'PPULPRO', 'L', 7, jtab,&
                iret)
    ipuls=jtab(1)
    if (iret .eq. 0) then
        puls = zr(ipuls)
        lmoda = .true.
    else
        puls = 0.d0
    endif
!
! --- RECUPERATION DES CHARGES
!
    if (fonc) then
        do i = 1, nno
            do j = 1, ndim
                valpar(j) = zr(igeom+ndim*(i-1)+j-1)
            end do
            do j = 1, ndim
                kk = ndim*(i-1) + j
                call fointe('FM', zk8(iforf+j-1), ndim+1, nompar, valpar,&
                            fno(kk), ier)
            end do
        end do
    else
        do i = 1, nno
            do j = 1, ndim
                fno(ndim*(i-1)+j) = zr(iforc+ndim*(i-1)+j-1)
            end do
        end do
    endif
!
! --- RECUPERATION DE LA PESANTEUR ET DE LA ROTATION
!
    if (lpesa .or. lrota) then
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, ' ', rbid,&
                    1, 'RHO', rho, icodre, 1)
        if (lpesa) then
            do i = 1, nno
                do j = 1, ndim
                    kk = ndim*(i-1)+j
                    fno(kk)=fno(kk)+rho*zr(ipesa)*zr(ipesa+j)
                end do
            end do
        endif
!
        if (lrota) then
            om = zr(irota)
            do i = 1, nno
                omo = 0.d0
                do j = 1, ndim
                    omo = omo + zr(irota+j)* zr(igeom+ndim*(i-1)+j-1)
                end do
                do j = 1, ndim
                    kk = ndim*(i-1)+j
                    fno(kk)=fno(kk)+rho*om*om*(zr(igeom+kk-1)-omo*zr(&
                    irota+j))
                end do
            end do
        endif
    endif
!
! --- RECUPERATION DE LA TEMPERATURE
!
    do ino = 1, nno
        call rcvarc(' ', 'TEMP', '+', 'NOEU', ino,&
                    1, tno(ino), iret)
        if (iret .ne. 0) tno(ino) = 0.d0
    end do
!
! ----------------------------------------------------------------------
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
    do kp = 1, npg
!
        l = (kp-1) * nno
        xg = 0.d0
        yg = 0.d0
        do i = 1, 3
            tgdm(i) = 0.d0
            do j = 1, 4
                dudm(i,j) = 0.d0
                du1dm(i,j)= 0.d0
                du2dm(i,j)= 0.d0
                dtdm(i,j) = 0.d0
                dfdm(i,j) = 0.d0
            end do
        end do
!
! ----- CALCUL DES ELEMENTS CINEMATIQUES (MATRICES F ET E)
!       EN UN PT DE GAUSS
!
        call nmgeom(ndim, nno, axi, .false., zr(igeom),&
                    kp, ipoids, ivf, idfde, zr(idepl),&
                    .true., poids, dfdi, f, eps,&
                    ray)
!
! ----- CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
! ----- DU GRADIENT DE TEMPERATURE AUX POINTS DE GAUSS (TGDM)
!
        do i = 1, nno
            der(1) = dfdi(i)
            der(2) = dfdi(i+nno)
            der(3) = 0.d0
            der(4) = zr(ivf+l+i-1)
!
            xg = xg + zr(igeom-1+ndim*(i-1)+1)*der(4)
            yg = yg + zr(igeom-1+ndim*(i-1)+2)*der(4)
!
            do j = 1, ndim
                tgdm(j) = tgdm(j) + tno(i) * der(j)
                do k = 1, ndim
                    dudm(j,k) = dudm(j,k) + zr(idepl+ndim*(i-1)+j-1)* der(k)
                    dtdm(j,k) = dtdm(j,k) + zr(ithet+ndim*(i-1)+j-1)* der(k)
                    dfdm(j,k) = dfdm(j,k) + fno(ndim*(i-1)+j)*der(k)
                end do
                dudm(j,4) = dudm(j,4) + zr(idepl+ndim*(i-1)+j-1)*der( 4)
                dtdm(j,4) = dtdm(j,4) + zr(ithet+ndim*(i-1)+j-1)*der( 4)
                dfdm(j,4) = dfdm(j,4) + fno(ndim*(i-1)+j)*der(4)
            end do
        end do
!
        if (axi) then
            if (ray .lt. r8prem()) then
                call utmess('F', 'RUPTURE0_56')
            endif
            dudm(3,3)= dudm(1,4)/ray
            dtdm(3,3)= dtdm(1,4)/ray
            dfdm(3,3)= dfdm(1,4)/ray
        endif
!
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
! ----- RECUPERATION DE E, NU
!
        call rcvalb(fami, kp, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', 0.d0,&
                    2, nomres, valres, icodre, 0)
        ASSERT(icodre(1)+icodre(2).eq.0)
!
!
! ----- RECUPERATION DE RHO
!
        call rcvalb(fami, kp, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', 0.d0,&
                    1, 'RHO', rho, codrho, 0)
!
        if ((codrho.ne.0) .and. lmoda) then
            call utmess('F', 'RUPTURE1_26')
        endif
!
        e = valres(1)
        nu = valres(2)
!
        c3 = e/(2.d0*(1.d0+nu))
        if (lteatt(' ','D_PLAN','OUI') .or. lteatt(' ','AXIS','OUI')) then
            mu = e/(2.d0*(1.d0+nu))
            ka = 3.d0-4.d0*nu
            c1 = e*(1.d0-nu)/((1.d0+nu)*(1.d0-2.d0*nu))
            c2 = nu/(1.d0-nu)*c1
            th = 1.d0
            coefk = e/(1.d0-nu*nu)
        else
            ka = (3.d0-nu)/(1.d0+nu)
            mu = e/(2.d0*(1.d0+nu))
            c1 = e/(1.d0-nu*nu)
            c2 = nu*c1
            th = (1.d0-2.d0*nu)/(1.d0-nu)
            coefk = e
        endif
!
!
! ----- CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
!
        norm = sqrt(zr(ifond-1+3)**2+zr(ifond-1+4)**2)
        a = zr(ifond-1+4)/norm
        b = -zr(ifond-1+3)/norm
        xa = zr(ifond-1+1)
        ya = zr(ifond-1+2)
!
!       COORDONNEES POLAIRES DU POINT
        xag = a*(xg-xa)+b*(yg-ya)
        yag = -b*(xg-xa)+a*(yg-ya)
        rg = sqrt(xag*xag+yag*yag)
!
        if (rg .gt. r8prem()) then
!         LE POINT N'EST PAS SUR LE FOND DE FISSURE
            phig = atan2(yag,xag)
            iret=1
        else
!         LE POINT EST SUR LE FOND DE FISSURE :
!         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
!         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
            phig=0.d0
            iret=0
        endif
!
! ----- ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
! ----- CAR ON SE TROUVE SUR LE FOND DE FISSURE
        ASSERT(iret.ne.0)
!
! ----- BASE LOCALE ASSOCIÉE AU POINT DE GAUSS KP
!
        e1(1) = a
        e1(2) = b
        e1(3) = 0
        e2(1) = -b
        e2(2) = a
        e2(3) = 0
        e3(1) = 0
        e3(2) = 0
        e3(3) = 0
!
!
! ----- CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
!
        do i = 1, 3
            p(i,1)=e1(i)
            p(i,2)=e2(i)
            p(i,3)=e3(i)
        end do
!
! ----- CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
!
        do i = 1, 3
            do j = 1, 3
                invp(i,j)=p(j,i)
            end do
        end do
!
!       PRISE EN COMPTE DE LA COURBURE : NON
!
        lcour=.false.
!
        call chauxi(ndim, mu, ka, rg, phig,&
                    invp, lcour, rbid2, du1dm, du2dm,&
                    rbid4, u1l, u2l, rbid3)
!
        if (axi) then
!         CHAMPS SINGULIERS DANS LA BASE GLOBALE
            call vecini(ndim, 0.d0, u1)
            call vecini(ndim, 0.d0, u2)
            do i = 1, ndim
                do j = 1, ndim
                    u1(i) = u1(i) + p(i,j) * u1l(j)
                    u2(i) = u2(i) + p(i,j) * u2l(j)
                end do
            end do
!
            du1dm(3,3)= u1(1)/ray
            du2dm(3,3)= u2(1)/ray
        endif
!
!-----------------------------------------------------------------------
!       CALCUL DE G, K1, K2 AU POINT DE GAUSS
!-----------------------------------------------------------------------
!
        guv = 0.d0
        cs = 1.d0
        call gbilin(fami, kp, zi(imate), dudm, dudm,&
                    dtdm, dfdm, tgdm, poids, c1,&
                    c2, c3, cs, th, 2.d0,&
                    rho, puls, axi, guv)
        g = g + guv
!
        guv1 = 0.d0
        cs = 0.5d0
        call gbilin(fami, kp, zi(imate), dudm, du1dm,&
                    dtdm, dfdm, tgdm, poids, c1,&
                    c2, c3, cs, th, 1.d0,&
                    rho, puls, axi, guv1)
        k1 = k1 + guv1
!
        guv2 = 0.d0
        cs = 0.5d0
        call gbilin(fami, kp, zi(imate), dudm, du2dm,&
                    dtdm, dfdm, tgdm, poids, c1,&
                    c2, c3, cs, th, 1.d0,&
                    rho, puls, axi, guv2)
        k2 = k2 + guv2
!
    end do
!
    k1 = k1 * coefk
    k2 = k2 * coefk
!
    zr(ificg) = g
    zr(ificg+1) = k1 / sqrt(coefk)
    zr(ificg+2) = k2 / sqrt(coefk)
    zr(ificg+3) = k1
    zr(ificg+4) = k2
!
999  continue
!
    call jedema()
!
end subroutine
