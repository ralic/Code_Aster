subroutine te0299(option, nomte)
    implicit none
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cgverho.h"
#include "asterfort/chauxi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/gbilin.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmgeom.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/coor_cyl.h"
#include "asterfort/vecini.h"
#include "asterfort/provec.h"
!
    integer :: icodre(3),matcod,ncmp, i1, ij
    integer :: ipoids, ivf, idfde, nno, kp, npg, compt, ier, nnos, jgano, icomp
    integer :: igeom, ithet, irota, ipesa, ificg, idepl, iret, ipuls, isigi
    integer :: imate, iforc, iforf, ifond, itemps, k, i, j, kk, l, ndim, jtab(7)
    integer :: ino
!
    real(kind=8) :: dfdi(18), f(3, 3), eps(6), fno(18), sigin(6), dsigin(6,3)
    real(kind=8) :: dudm(3, 4), dfdm(3, 4), dtdm(3, 4), der(4)
    real(kind=8) :: du1dm(3, 4), du2dm(3, 4), epsref(6)
    real(kind=8) :: rhocst, rho, om, omo, rbid=0.d0, e, nu, rbid2(3, 3, 3)
    real(kind=8) :: thet, tno(20), tgdm(3),rac2
    real(kind=8) :: xg, yg, e1(3), e2(3), e3(3)
    real(kind=8) :: c1, c2, c3, cs, u1(2), u2(2)
    real(kind=8) :: basloc(9*6), p(3, 3), invp(3, 3), rg, phig, ffp(9)
    real(kind=8) :: u1l(3), u2l(3), rbid3(3), mu, ka, rbid4(3, 4)
    real(kind=8) :: th, val(1), valres(3), valpar(4)
    real(kind=8) :: coefk
    real(kind=8) :: guv, guv1, guv2, k1, k2, g, poids, ray, puls
!
    character(len=4) :: fami
    character(len=8) :: nompar(4), elrefp
    character(len=16) :: nomres(3), compor(4)
    character(len=32) :: phenom
!
    aster_logical :: lcour, fonc, lpesa, lrota, l_not_zero
    aster_logical :: axi
!
! ----------------------------------------------------------------------
!

!

    fami = 'RIGI'
    call elref1(elrefp)
    call elrefe_info(fami=fami, ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! --- RECUPERATION DES CHAMPS IN
!
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PFISSR', 'L', ifond)
    matcod = zi(imate)
!
! NOMBRE DE COMPOSANTES DES TENSEURS
    ncmp = 2*ndim
!
! --- RECUPERATION DU CHAMP OUT
!
    call jevech('PGTHETA', 'E', ificg)
!
    axi = .false.
    if (lteatt('AXIS','OUI')) axi = .true.
!
    g = 0.d0
    k1 = 0.d0
    k2 = 0.d0
    rac2 = sqrt(2.d0)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
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
! --- VERIFS DE COHERENCE RHO <-> PESANTEUR, ROTATION, PULSATION
!
    if (.not. cgverho(imate)) call utmess('F', 'RUPTURE1_26')
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
    call tecach('ONO', 'PPESANR', 'L', iret, nval=7,&
                itab=jtab)
    ipesa=jtab(1)
    if (iret .eq. 0) then
        lpesa = .true.
    endif
!
    lrota = .false.
    call tecach('ONO', 'PROTATR', 'L', iret, nval=7,&
                itab=jtab)
    irota=jtab(1)
    if (iret .eq. 0) then
        lrota = .true.
    endif
!
! --- VERFICATION DU COMPORTEMENT : ELASTICITE INCREMENTALE AUTORISEE
!  
    do i = 1, 4
        compor(i) = zk16(icomp+i-1)
    end do
!
    if (compor(3).eq.'GROT_GDEP') then
        call utmess('F', 'RUPTURE1_24')
    end if
    if ((compor(1).ne.'ELAS' ) .or.  (compor(4).eq.'COMP_INCR')) then
        if (compor(1).ne.'ELAS' ) then
            call utmess('F', 'RUPTURE1_24')
        end if
    endif
!
! --- RECUPERATION DE LA PULSATION
!
    call tecach('ONO', 'PPULPRO', 'L', iret, nval=7,&
                itab=jtab)
    ipuls=jtab(1)
    if (iret .eq. 0) then
        puls = zr(ipuls)
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
        call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
        call rcvalb('RIGI', 1, 1, '+', matcod,&
                    ' ', phenom, 1, ' ', [rbid],&
                    1, 'RHO', val, icodre, 1)
        rhocst = val(1)
        if (lpesa) then
            do i = 1, nno
                do j = 1, ndim
                    kk = ndim*(i-1)+j
                    fno(kk)=fno(kk)+rhocst*zr(ipesa)*zr(ipesa+j)
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
                    fno(kk)=fno(kk)+rhocst*om*om*(zr(igeom+kk-1)-omo*zr(&
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
! --- RECUPERATION DE LA CONTRAINTE INITIALE
    call tecach('ONO', 'PSIGINR', 'L', iret, iad=isigi)
    
    
! ----------------------------------------------------------------------
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
    do kp = 1, npg
!INITIALISATIONS
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
        do i = 1, 6
            sigin(i) = 0.d0
            epsref(i)= 0.d0
            do j = 1, 3
                dsigin(i,j) = 0.d0
            end do
         end do
!
! ----- CALCUL DES ELEMENTS CINEMATIQUES (MATRICES F ET E)
!       EN UN PT DE GAUSS
!
        call nmgeom(ndim, nno, axi, .false._1, zr(igeom),&
                    kp, ipoids, ivf, idfde, zr(idepl),&
                    .true._1, poids, dfdi, f, eps,&
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
        call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
       

!
! ----- RECUPERATION DE E, NU
!
        call rcvalb(fami, kp, 1, '+', matcod,&
                    ' ', phenom, 0, ' ', [0.d0],&
                    3, nomres, valres, icodre, 0)
        ASSERT(icodre(1)+icodre(2).eq.0)
        if (icodre(3) .ne. 0) then
            valres(3) = 0.d0
        endif
!
        e = valres(1)
        nu = valres(2)
        rho = valres(3)
!
        c3 = e/(2.d0*(1.d0+nu))
        if (lteatt('D_PLAN','OUI') .or. lteatt('AXIS','OUI')) then
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
! ---- DETERMINATION DES GRANDEURS UTILES A LA PRISE EN COMPTE DE LA CONTRAINTE INITIALE
! ---- CONTRAINTE INITIALE, SA DERIVEE ET LA DEFORMATION ASSOCIEE EPSREF
!
        if (isigi .ne. 0) then
            do i = 1, nno
                i1 = i-1
                der(1) = dfdi(i)
                der(2) = dfdi(i+nno)
                der(3) = 0.d0
                der(4) = zr(ivf+l+i1)

! CALCUL DE SIGMA INITIAL
                ij = isigi+ncmp*i1-1
                do j = 1, ncmp
                    sigin(j) = sigin(j)+ zr(ij+j)*der(4)
                end do
!
! CALCUL DU GRADIENT DE SIGMA INITIAL
                do j = 1, ncmp
                    do k = 1, ndim
                        dsigin(j,k)=dsigin(j,k)+zr(ij+j)*der(k)
                    end do
                end do
            end do
!
! TRAITEMENTS PARTICULIERS DES TERMES CROISES
            do  i = 4, ncmp
                sigin(i) = sigin(i)*rac2
                do j = 1, ndim
                    dsigin(i,j) = dsigin(i,j)*rac2
                end do
            end do
!
! CALCUL DE LA DEFORMATION DE REFERENCE
!
            epsref(1)=-(1.d0/e)*(sigin(1)-(nu*(sigin(2)+sigin(3))))
            epsref(2)=-(1.d0/e)*(sigin(2)-(nu*(sigin(3)+sigin(1))))
            epsref(3)=-(1.d0/e)*(sigin(3)-(nu*(sigin(1)+sigin(2))))
            epsref(4)=-(1.d0/mu)*sigin(4)  
        endif
!
!
! ----- CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
!
!
! ----- BASE LOCALE ASSOCIÉE AU POINT DE GAUSS KP
!
        p(:,:)=0.d0
        invp(:,:)=0.d0
        do ino = 1, nno
          ffp(ino)=zr(ivf-1+nno*(kp-1)+ino)
          basloc((6*(ino-1)+1):(6*(ino-1)+6))=zr((ifond-1+1):(ifond-1+6))
        enddo
        call coor_cyl(ndim, nno, basloc, zr(igeom), ffp,&
                      p(1:ndim,1:ndim), invp(1:ndim,1:ndim), rg, phig,&
                      l_not_zero)
! BRICOLAGE POUR CALCULER LE SIGNE DE K2 QUAND NDIM=2
        e1(:)=0.d0
        e1(1:ndim)=p(1:ndim,1)
        e2(:)=0.d0
        e2(1:ndim)=p(1:ndim,2)
        call provec(e1, e2, e3)
        p(3,3)=e3(3)
        invp(3,3)=e3(3)
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
                    dtdm, dfdm, tgdm, poids, sigin,&
                    dsigin, epsref, c1,&
                    c2, c3, cs, th, 2.d0,&
                    rho, puls, axi, guv)

          g = g + guv
!

          guv1 = 0.d0

        cs = 0.5d0
        call gbilin(fami, kp, zi(imate), dudm, du1dm,&
                    dtdm, dfdm, tgdm, poids, sigin,&
                    dsigin, epsref, c1,&
                    c2, c3, cs, th, 1.d0,&
                    rho, puls, axi, guv1)
        k1 = k1 + guv1
!

          guv2 = 0.d0

        cs = 0.5d0
        
        call gbilin(fami, kp, zi(imate), dudm, du2dm,&
                    dtdm, dfdm, tgdm, poids, sigin,&
                    dsigin, epsref, c1,&
                    c2, c3, cs, th, 1.d0,&
                    rho, puls, axi, guv2)
        k2 = k2 + guv2

!
    end do
!
    k1 = k1 * coefk
    k2 = k2 * coefk
    if (e3(3) .lt. 0) k2=-k2
!



    zr(ificg) = g
    zr(ificg+1) = k1 / sqrt(coefk)
    zr(ificg+2) = k2 / sqrt(coefk)
    zr(ificg+3) = k1
    zr(ificg+4) = k2
!
999 continue
!
!
end subroutine
