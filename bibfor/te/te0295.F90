subroutine te0295(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/chauxi.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/gbil3d.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/nmgeom.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       CALCUL DES FACTEURS D'INTENSITÉ DES CONTRAINTES
!       A PARTIR DE LA FORME BILINEAIRE SYMETRIQUE G ET
!       DES DEPLACEMENTS SINGULIERS EN FOND DE FISSURE
!      POUR LES ELEMENTS ISOPARAMETRIQUES 3D
!
!       OPTION : 'CALC_K_G'    (CHARGES REELLES)
!                'CALC_K_G_F'  (CHARGES FONCTIONS)
!
!
! ----------------------------------------------------------------------
!
!
    integer :: icodre(3)
    integer :: codrho
    integer :: ipoids, ivf, idfde, nno, kp, npg, compt, ier, nnos
    integer :: jgano, icomp, ibalo, icour
    integer :: igeom, ithet, ificg, irota, ipesa, idepl, iret
    integer :: imate, iforc, iforf, itemps, k, i, j, kk, l, ndim, ino, ipuls
    integer :: jlsn, jlst, jtab(7)
!
!
    real(kind=8) :: r8bid
    real(kind=8) :: dfdi(60), f(3, 3), eps(6), fno(81), e1(3), e2(3), e3(3)
    real(kind=8) :: dudm(3, 4), dfdm(3, 4), dtdm(3, 4), der(4)
    real(kind=8) :: u1l(3), u2l(3), u3l(3), dfvdm(3, 4)
    real(kind=8) :: du1dm(3, 4), du2dm(3, 4), du3dm(3, 4)
    real(kind=8) :: p(3, 3), invp(3, 3)
    real(kind=8) :: courb(3, 3, 3)
    real(kind=8) :: rho, om, omo, rbid, e, nu, alpha, tref
    real(kind=8) :: thet, tpg(27), tno(20), tgdm(3), ttrg, la, mu, ka
    real(kind=8) :: xg, yg, zg, ff
    real(kind=8) :: c1, c2, c3, rg, phig
    real(kind=8) :: valres(3)
    real(kind=8) :: coeff, coeff3
    real(kind=8) :: guv, guv1, guv2, guv3, k1, k2, k3, g, poids
    real(kind=8) :: norme, k3a, ttrgv, tgvdm(3)
    real(kind=8) :: valpar(4), lsng, lstg, puls
!
    character(len=4) :: fami
    character(len=8) :: nomres(3), nompar(4)
    character(len=16) :: phenom, compor(4)
!
    logical :: lcour, lmoda, fonc, lpesa, lrota
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
    call jevech('PBASLOR', 'L', ibalo)
    call jevech('PCOURB', 'L', icour)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!
! --- RECUPERATION DU CHAMP OUT
!
    call jevech('PGTHETA', 'E', ificg)
!
    g = 0.d0
    k1 = 0.d0
    k2 = 0.d0
    k3 = 0.d0
    coeff = 1.d0
    coeff3 = 1.d0
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
!
! --- PAS DE CALCUL DE G POUR LES ELEMENTS OU THETA EST NULLE
!
    compt = 0
    do 10 i = 1, nno
        thet = 0.d0
        do 11 j = 1, ndim
            thet = thet + abs(zr(ithet+ndim*(i-1)+j-1))
11      continue
        if (thet .lt. r8prem()) compt = compt + 1
10  end do
    if (compt .eq. nno) goto 9999
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
    do 20 i = 1, 4
        compor(i) = zk16(icomp+i-1)
20  end do
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
        do 50 i = 1, nno
            do 30 j = 1, ndim
                valpar(j) = zr(igeom+ndim*(i-1)+j-1)
30          continue
            do 40 j = 1, ndim
                kk = ndim*(i-1) + j
                call fointe('FM', zk8(iforf+j-1), ndim+1, nompar, valpar,&
                            fno(kk), ier)
40          continue
50      continue
    else
        do 80 i = 1, nno
            do 60 j = 1, ndim
                fno(ndim*(i-1)+j) = zr(iforc+ndim*(i-1)+j-1)
60          continue
80      end do
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
            do 95 i = 1, nno
                do 90 j = 1, ndim
                    kk = ndim*(i-1)+j
                    fno(kk)=fno(kk)+rho*zr(ipesa)*zr(ipesa+j)
90              continue
95          continue
        endif
!
        if (lrota) then
            om = zr(irota)
            do 105 i = 1, nno
                omo = 0.d0
                do 100 j = 1, ndim
                    omo = omo + zr(irota+j)* zr(igeom+ndim*(i-1)+j-1)
100              continue
                do 103 j = 1, ndim
                    kk = ndim*(i-1)+j
                    fno(kk)=fno(kk)+rho*om*om*(zr(igeom+kk-1)-omo*zr(&
                    irota+j))
103              continue
105          continue
        endif
    endif
!
! --- RECUPERATION DE LA TEMPERATURE
!
    call rcvarc(' ', 'TEMP', 'REF', 'RIGI', 1,&
                1, tref, iret)
    if (iret .ne. 0) tref = 0.d0
    do 645 kp = 1, npg
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    1, tpg(kp), iret)
        if (iret .ne. 0) tpg(kp) = 0.d0
645  end do
!
    do 646 ino = 1, nno
        call rcvarc(' ', 'TEMP', '+', 'NOEU', ino,&
                    1, tno(ino), iret)
        if (iret .ne. 0) tno(ino) = 0.d0
646  end do
!
! ----------------------------------------------------------------------
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
    do 800 kp = 1, npg
!
        l = (kp-1) * nno
        xg = 0.d0
        yg = 0.d0
        zg = 0.d0
        lsng=0.d0
        lstg=0.d0
        do 110 i = 1, 3
            tgdm(i) = 0.d0
            tgvdm(i) = 0.d0
            do 111 j = 1, 4
                dudm(i,j) = 0.d0
                du1dm(i,j)= 0.d0
                du2dm(i,j)= 0.d0
                du3dm(i,j)= 0.d0
                dtdm(i,j) = 0.d0
                dfdm(i,j) = 0.d0
                dfvdm(i,j) = 0.d0
111          continue
110      continue
!
! ----- CALCUL DES ELEMENTS CINEMATIQUES (MATRICES F ET E)
!       EN UN PT DE GAUSS
!
        call nmgeom(ndim, nno, .false., .false., zr(igeom),&
                    kp, ipoids, ivf, idfde, zr(idepl),&
                    .true., poids, dfdi, f, eps,&
                    rbid)
!
! ----- CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
! ----- DU GRADIENT DE TEMPERATURE AUX POINTS DE GAUSS (TGDM)
! ----- ET LEVEL SETS
!
        do 320 i = 1, nno
            der(1) = dfdi(i)
            der(2) = dfdi(i+nno)
            der(3) = dfdi(i+2*nno)
            der(4) = zr(ivf+l+i-1)
!
            xg = xg + zr(igeom-1+ndim*(i-1)+1)*der(4)
            yg = yg + zr(igeom-1+ndim*(i-1)+2)*der(4)
            zg = zg + zr(igeom-1+ndim*(i-1)+3)*der(4)
!
            lsng = lsng + zr(jlsn-1+i) * der(4)
            lstg = lstg + zr(jlst-1+i) * der(4)
!
            do 310 j = 1, ndim
                tgdm(j) = tgdm(j) + tno(i) * der(j)
                do 300 k = 1, ndim
                    dudm(j,k) = dudm(j,k) + zr(idepl+ndim*(i-1)+j-1)* der(k)
                    dtdm(j,k) = dtdm(j,k) + zr(ithet+ndim*(i-1)+j-1)* der(k)
                    dfdm(j,k) = dfdm(j,k) + fno(ndim*(i-1)+j)*der(k)
300              continue
                dudm(j,4) = dudm(j,4) + zr(idepl+ndim*(i-1)+j-1)*der( 4)
                dtdm(j,4) = dtdm(j,4) + zr(ithet+ndim*(i-1)+j-1)*der( 4)
                dfdm(j,4) = dfdm(j,4) + fno(ndim*(i-1)+j)*der(4)
310          continue
320      continue
!
        ttrg = tpg(kp) - tref
        ttrgv = 0.d0
!
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
! ----- RECUPERATION DE E, NU ET ALPHA
!
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    1, r8bid, iret)
        call rcvalb(fami, kp, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', 0.d0,&
                    3, nomres, valres, icodre, 0)
        ASSERT(icodre(1)+icodre(2).eq.0)
        if (icodre(3) .ne. 0) then
            ASSERT(iret.ne.0)
            valres(3) = 0.d0
        endif
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
        alpha = valres(3)
        k3a = alpha * e / (1.d0-2.d0*nu)
!
        la = nu*e/((1.d0+nu)*(1.d0-2.d0*nu))
        mu = e/(2.d0*(1.d0+nu))
!       EN DP
        ka=3.d0-4.d0*nu
        coeff=e/(1.d0-nu*nu)
        coeff3=2.d0 * mu
!       EN CP
!       KA=(3.D0-NU)/(1.D0+NU)
!       COEFF=E
!       COEFF3=2.D0 * MU
!
        c1 = la + 2.d0 * mu
        c2 = la
        c3 = mu
!
! ----- CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
!
!       COORDONNEES POLAIRES DU POINT
        rg = sqrt(lsng**2+lstg**2)
!
        if (rg .gt. r8prem()) then
!         LE POINT N'EST PAS SUR LE FOND DE FISSURE
            phig = sign(1.d0,lsng) * abs(atan2(lsng,lstg))
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
!       (E1=GRLT,E2=GRLN,E3=E1^E2)
        do 124 i = 1, 3
            e1(i)=0.d0
            e2(i)=0.d0
            do 125 ino = 1, nno
                ff=zr(ivf-1+nno*(kp-1)+ino)
                e1(i) = e1(i)+zr(ibalo-1+9*(ino-1)+i+3)* ff
                e2(i) = e2(i)+zr(ibalo-1+9*(ino-1)+i+6)* ff
125          continue
124      continue
!
!       NORMALISATION DE LA BASE
        call normev(e1, norme)
        call normev(e2, norme)
        call provec(e1, e2, e3)
!
! ----- CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
!
        do 120 i = 1, 3
            p(i,1)=e1(i)
            p(i,2)=e2(i)
            p(i,3)=e3(i)
120      continue
!
! ----- CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
!
        do 130 i = 1, 3
            do 131 j = 1, 3
                invp(i,j)=p(j,i)
131          continue
130      continue
!
!       PRISE EN COMPTE DE LA COURBURE : OUI
!
        lcour=.true.
!       RECUPERATION DU TENSEUR DE COURBURE
        call jevech('PCOURB', 'L', icour)
        do 500 i = 1, ndim
            do 501 j = 1, ndim
                courb(i,1,j)=zr(icour-1+ndim*(i-1)+j)
                courb(i,2,j)=zr(icour-1+ndim*(i+3-1)+j)
                courb(i,3,j)=zr(icour-1+ndim*(i+6-1)+j)
501          continue
500      continue
!
!
        call chauxi(ndim, mu, ka, rg, phig,&
                    invp, lcour, courb, du1dm, du2dm,&
                    du3dm, u1l, u2l, u3l)
!
!-----------------------------------------------------------------------
!       CALCUL DE G, K1, K2, K3 AU POINT DE GAUSS
!-----------------------------------------------------------------------
!
        guv = 0.d0
        call gbil3d(dudm, dudm, dtdm, dfdm, dfdm,&
                    tgdm, tgdm, ttrg, ttrg, poids,&
                    c1, c2, c3, k3a, rho,&
                    puls, guv)
        g = g + guv
!
        guv1 = 0.d0
        call gbil3d(dudm, du1dm, dtdm, dfdm, dfvdm,&
                    tgdm, tgvdm, ttrg, ttrgv, poids,&
                    c1, c2, c3, k3a, rho,&
                    puls, guv1)
        k1 = k1 + guv1
!
        guv2 = 0.d0
        call gbil3d(dudm, du2dm, dtdm, dfdm, dfvdm,&
                    tgdm, tgvdm, ttrg, ttrgv, poids,&
                    c1, c2, c3, k3a, rho,&
                    puls, guv2)
        k2 = k2 + guv2
!
        guv3 = 0.d0
        call gbil3d(dudm, du3dm, dtdm, dfdm, dfvdm,&
                    tgdm, tgvdm, ttrg, ttrgv, poids,&
                    c1, c2, c3, k3a, rho,&
                    puls, guv3)
        k3 = k3 + guv3
!
800  end do
!
    k1 = k1 * coeff
    k2 = k2 * coeff
    k3 = k3 * coeff3
!
    zr(ificg) = g
    zr(ificg+1) = k1 / sqrt(coeff)
    zr(ificg+2) = k2 / sqrt(coeff)
    zr(ificg+3) = k3 / sqrt(coeff3)
    zr(ificg+4) = k1
    zr(ificg+5) = k2
    zr(ificg+6) = k3
!
9999  continue
!
    call jedema()
!
end subroutine
