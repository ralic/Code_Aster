subroutine rairep(noma, ioc, km, rigi, nbgr,&
                  ligrma, nbno, tabnoe, rignoe, rigto,&
                  amoto, rirot, ndim)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/compma.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
    integer :: ioc, nbgr, nbno, ndim
    character(len=8) :: noma, tabnoe(*), km
    character(len=24) :: ligrma(nbgr)
    real(kind=8) :: rignoe(*), rigto(*), amoto(*), rirot(3)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
    character(len=8) :: k8b
    character(len=8) :: nomnoe, typm
    character(len=24) :: nomgr, magrno, manono, magrma, manoma, matyma
    real(kind=8) :: zero, x(9), y(9), z(9), rigi(6)
    real(kind=8) :: a(3), b(3), c(3), u(3)
    logical :: lfonc, trans
    integer :: iarg, appui
!
!-----------------------------------------------------------------------
    integer :: i,     ii
    integer :: ij, im, in, inoe, iret,  jcoor
    integer :: ldgm, ldgn, ldnm, ltyp, nb, nbma, ncg
    integer :: nfg, ngn, nm, nn, nno, noemax, ntopo
    integer :: numa
    real(kind=8) :: coef, dist, hc, r1, r2, r3
    real(kind=8) :: r4, r5, r6, rig3, rig4, rig45, rig46
    real(kind=8) :: rig5, rig56, rig6, surf, surtot, xc, xg
    real(kind=8) :: xx, yc, yg, yy, zg, zz
    real(kind=8), pointer :: coegro(:) => null()
    real(kind=8), pointer :: coeno(:) => null()
    character(len=8), pointer :: fongro(:) => null()
    integer, pointer :: parno(:) => null()
    real(kind=8), pointer :: surmai(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    lfonc = .false.
!
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
!
!        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
    call compma(noma, nbgr, ligrma, nbma)
    magrno = noma//'.GROUPENO'
    manono = noma//'.NOMNOE'
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    matyma = noma//'.TYPMAIL'
!
    noemax = 0
!
!
!     --- DESCRIPTION NOEUDS STRUCTURE ---
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!       RECUPERATION DU CENTRE
!
    call getvr8('RIGI_PARASOL', 'COOR_CENTRE', iocc=ioc, nbval=0, nbret=ncg)
    call getvem(noma, 'NOEUD', 'RIGI_PARASOL', 'NOEUD_CENTRE', ioc,&
                iarg, 0, k8b, nno)
    call getvem(noma, 'GROUP_NO', 'RIGI_PARASOL', 'GROUP_NO_CENTRE', ioc,&
                iarg, 0, k8b, ngn)
    if (ncg .ne. 0) then
        call getvr8('RIGI_PARASOL', 'COOR_CENTRE', iocc=ioc, nbval=3, vect=c,&
                    nbret=ncg)
        xg = c(1)
        yg = c(2)
        zg = c(3)
    else if (nno.ne.0) then
        call getvem(noma, 'NOEUD', 'RIGI_PARASOL', 'NOEUD_CENTRE', ioc,&
                    iarg, 1, nomnoe, nno)
        call jenonu(jexnom(manono, nomnoe), inoe)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    else if (ngn.ne.0) then
        call getvem(noma, 'GROUP_NO', 'RIGI_PARASOL', 'GROUP_NO_CENTRE', ioc,&
                    iarg, 1, nomgr, ngn)
        call jeveuo(jexnom(magrno, nomgr), 'L', ldgn)
        inoe = zi(ldgn)
        call jenuno(jexnum(manono, inoe), nomnoe)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    endif
!
!       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
!
    call getvr8('RIGI_PARASOL', 'COEF_GROUP', iocc=ioc, nbval=0, nbret=ncg)
    if (ncg .ne. 0) then
        AS_ALLOCATE(vr=coegro, size=nbgr)
        call getvr8('RIGI_PARASOL', 'COEF_GROUP', iocc=ioc, nbval=nbgr, vect=coegro,&
                    nbret=ncg)
    else
        AS_ALLOCATE(vk8=fongro, size=nbgr)
        lfonc = .true.
        call getvid('RIGI_PARASOL', 'FONC_GROUP', iocc=ioc, nbval=nbgr, vect=fongro,&
                    nbret=nfg)
    endif
!
!
!
    if (ndim .eq. 2) then
        appui=1
    else
!     LA DIMENSION DE L'APPUI N'EST PAS ENCORE DETERMINEE
        appui=-1
    endif
!
    call jeveuo(matyma, 'L', ltyp)
    do i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do in = 0, nb-1
            numa=zi(ldgm+in)
            ASSERT(numa.gt.0)
            call jelira(jexnum(manoma, numa), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, numa), 'L', ldnm)
!
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(ltyp-1+numa)), typm)
            call dismoi('DIM_TOPO', typm, 'TYPE_MAILLE', repi=ntopo)
!
            if (appui .eq. -1) then
!            LA DIMENSION DE LA PREMIERE MAILLE DEFINIT L'APPUI
                appui=ntopo
            else if ((appui.eq.1).or.(appui.eq.2)) then
                if (appui .ne. ntopo) then
                    call utmess('F', 'MODELISA6_35')
                endif
            else
                call utmess('F', 'MODELISA6_29')
            endif
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                noemax = max(noemax,inoe)
            end do
        end do
    end do
    ASSERT(appui.ne.-1)
!
    AS_ALLOCATE(vr=coeno, size=noemax)
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    AS_ALLOCATE(vi=parno, size=noemax)
!
!
!     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
!
    AS_ALLOCATE(vr=surmai, size=nbma)
    im = 0
    surtot = zero
    do i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do in = 0, nb-1
            im = im + 1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            xc = zero
            yc = zero
            hc = zero
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                parno(inoe) = parno(inoe) + 1
                x(nn) = zr(jcoor+3*(inoe-1)+1-1)
                y(nn) = zr(jcoor+3*(inoe-1)+2-1)
                z(nn) = zr(jcoor+3*(inoe-1)+3-1)
                xc = xc + x(nn)
                yc = yc + y(nn)
                hc = hc + z(nn)
            end do
            xc = xc/nm
            yc = yc/nm
            hc = hc/nm
!
            if (appui .eq. 1) then
                a(1) = x(2) - x(1)
                a(2) = y(2) - y(1)
                a(3) = z(2) - z(1)
                surf = ddot(2,a,1,a,1)
                surmai(im)= sqrt(surf)
            else if (appui.eq.2) then
                a(1) = x(3) - x(1)
                a(2) = y(3) - y(1)
                a(3) = z(3) - z(1)
                if (nm .eq. 3 .or. nm .eq. 6 .or. nm .eq. 7) then
                    b(1) = x(2) - x(1)
                    b(2) = y(2) - y(1)
                    b(3) = z(2) - z(1)
                else if (nm.eq.4.or.nm.eq.8.or.nm.eq.9) then
                    b(1) = x(4) - x(2)
                    b(2) = y(4) - y(2)
                    b(3) = z(4) - z(2)
                else
                    ASSERT(.false.)
                endif
                call provec(a, b, c)
                surf=ddot(3,c,1,c,1)
                surmai(im) = sqrt(surf)*0.5d0
            else
                ASSERT(.false.)
            endif
            if (lfonc) then
                u(1) = xg - xc
                u(2) = yg - yc
                u(3) = zg - hc
                dist=ddot(3,u,1,u,1)
                dist = sqrt(dist)
                call fointe('F ', fongro(i), 1, ['X'], [dist],&
                            coef, iret)
                surmai(im) = surmai(im)*coef
            else
                surmai(im) = surmai(im)*coegro(i)
            endif
            surtot = surtot + surmai(im)
            surmai(im) = surmai(im)/nm
        end do
    end do
!
!     CALCUL DES PONDERATIONS ELEMENTAIRES
!
    im = 0
    do i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do in = 0, nb-1
            im = im + 1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                do ij = 1, noemax
                    if (parno(ij) .eq. 0) goto 37
                    if (zi(ldnm+nn-1) .eq. ij) then
                        coeno(ij) = coeno(ij) + surmai(im)/surtot
                    endif
 37                 continue
                end do
            end do
        end do
    end do
    nbma = im
!
!     CALCUL DES RAIDEURS DE ROTATION
!
    ii = 0
    rig4 = zero
    rig5 = zero
    rig6 = zero
    rig45 = zero
    rig46 = zero
    rig56 = zero
    rig3 = 0.d0
    do ij = 1, noemax
        if (parno(ij) .eq. 0) goto 50
        ii = ii + 1
        xx = zr(jcoor+3*(ij-1)+1-1) - xg
        yy = zr(jcoor+3*(ij-1)+2-1) - yg
        zz = zr(jcoor+3*(ij-1)+3-1) - zg
        if (ndim .eq. 3) then
            rig4 = rig4 + (rigi(2)*zz**2+rigi(3)*yy**2)*coeno(ij)
            rig5 = rig5 + (rigi(1)*zz**2+rigi(3)*xx**2)*coeno(ij)
            rig6 = rig6 + (rigi(2)*xx**2+rigi(1)*yy**2)*coeno(ij)
            rig45 = rig45 - rigi(3)*xx*yy*coeno(ij)
            rig46 = rig46 - rigi(2)*xx*zz*coeno(ij)
            rig56 = rig56 - rigi(1)*yy*zz*coeno(ij)
            rig3 = 0.d0
        else
            rig3 = rig3 + (rigi(2)*xx**2+rigi(1)*yy**2)*coeno(ij)
        endif
!
 50     continue
    end do
    nbno = ii
!
    trans=(km(1:7) .eq. 'K_T_D_N').or.(km(1:7) .eq. 'K_T_D_L').or.&
     &      (km(1:7) .eq. 'A_T_D_N').or.(km(1:7) .eq. 'A_T_D_L')
!
    if (trans) then
!        PAS DE RAIDEUR EN ROTATION SUR LES DISCRETS
        if (ndim .eq. 2) then
            rigi(3)=zero
            rig3 =zero
        endif
        rigi(4) = zero
        rigi(5) = zero
        rigi(6) = zero
        rig4 = zero
        rig5 = zero
        rig6 = zero
        rirot(1) = zero
        rirot(2) = zero
        rirot(3) = zero
    else
        rig3 = rigi(3) - rig3
        rig4 = rigi(4) - rig4
        rig5 = rigi(5) - rig5
        rig6 = rigi(6) - rig6
        if (ndim .eq. 3) then
            rirot(1) = rig4
            rirot(2) = rig5
            rirot(3) = rig6
        else
            rirot(1) = rig3
        endif
    endif
!
    ii = 0
    do ij = 1, noemax
        if (parno(ij) .eq. 0) goto 51
        ii = ii + 1
        r1 = rigi(1)*coeno(ij)
        r2 = rigi(2)*coeno(ij)
        if (ndim .eq. 3) then
            r3 = rigi(3)*coeno(ij)
            r4 = rig4*coeno(ij)
            r5 = rig5*coeno(ij)
            r6 = rig6*coeno(ij)
        else
            r3 = rig3*coeno(ij)
            r4 = zero
            r5 = zero
            r6 = zero
        endif
        call jenuno(jexnum(manono, ij), nomnoe)
        if (km(1:1) .eq. 'K') then
            rigto(6*(ij-1)+1) = r1 + rigto(6*(ij-1)+1)
            rigto(6*(ij-1)+2) = r2 + rigto(6*(ij-1)+2)
            rigto(6*(ij-1)+3) = r3 + rigto(6*(ij-1)+3)
            rigto(6*(ij-1)+4) = r4 + rigto(6*(ij-1)+4)
            rigto(6*(ij-1)+5) = r5 + rigto(6*(ij-1)+5)
            rigto(6*(ij-1)+6) = r6 + rigto(6*(ij-1)+6)
            r1 = rigto(6*(ij-1)+1)
            r2 = rigto(6*(ij-1)+2)
            r3 = rigto(6*(ij-1)+3)
            r4 = rigto(6*(ij-1)+4)
            r5 = rigto(6*(ij-1)+5)
            r6 = rigto(6*(ij-1)+6)
        else if (km(1:1).eq.'A') then
            amoto(6*(ij-1)+1) = r1 + amoto(6*(ij-1)+1)
            amoto(6*(ij-1)+2) = r2 + amoto(6*(ij-1)+2)
            amoto(6*(ij-1)+3) = r3 + amoto(6*(ij-1)+3)
            amoto(6*(ij-1)+4) = r4 + amoto(6*(ij-1)+4)
            amoto(6*(ij-1)+5) = r5 + amoto(6*(ij-1)+5)
            amoto(6*(ij-1)+6) = r6 + amoto(6*(ij-1)+6)
            r1 = amoto(6*(ij-1)+1)
            r2 = amoto(6*(ij-1)+2)
            r3 = amoto(6*(ij-1)+3)
            r4 = amoto(6*(ij-1)+4)
            r5 = amoto(6*(ij-1)+5)
            r6 = amoto(6*(ij-1)+6)
        endif
        rignoe(6*(ii-1)+1) = r1
        rignoe(6*(ii-1)+2) = r2
        rignoe(6*(ii-1)+3) = r3
        rignoe(6*(ii-1)+4) = r4
        rignoe(6*(ii-1)+5) = r5
        rignoe(6*(ii-1)+6) = r6
        tabnoe(ii) = nomnoe
 51     continue
    end do
!
    AS_DEALLOCATE(vr=coegro)
    AS_DEALLOCATE(vk8=fongro)
    AS_DEALLOCATE(vr=coeno)
    AS_DEALLOCATE(vi=parno)
    AS_DEALLOCATE(vr=surmai)
!
    call jedema()
end subroutine
