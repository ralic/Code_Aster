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
    integer :: i, icoef, icoegr, idno, ier, ifongr, ii
    integer :: ij, im, in, inoe, iret, isurma, jcoor
    integer :: ldgm, ldgn, ldnm, ltyp, nb, nbma, ncg
    integer :: nfg, ngn, nm, nn, nno, noemax, ntopo
    integer :: numa
    real(kind=8) :: coef, dist, hc, r1, r2, r3
    real(kind=8) :: r4, r5, r6, rig3, rig4, rig45, rig46
    real(kind=8) :: rig5, rig56, rig6, surf, surtot, xc, xg
    real(kind=8) :: xx, yc, yg, yy, zg, zz
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
        call wkvect('&&RAIREP.COEGRO', 'V V R', nbgr, icoegr)
        call getvr8('RIGI_PARASOL', 'COEF_GROUP', iocc=ioc, nbval=nbgr, vect=zr(icoegr),&
                    nbret=ncg)
    else
        call wkvect('&&RAIREP.FONGRO', 'V V K8', nbgr, ifongr)
        lfonc = .true.
        call getvid('RIGI_PARASOL', 'FONC_GROUP', iocc=ioc, nbval=nbgr, vect=zk8(ifongr),&
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
    do 20 i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do 22 in = 0, nb-1
            numa=zi(ldgm+in)
            ASSERT(numa.gt.0)
            call jelira(jexnum(manoma, numa), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, numa), 'L', ldnm)
!
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(ltyp-1+numa)), typm)
            call dismoi('F', 'DIM_TOPO', typm, 'TYPE_MAILLE', ntopo,&
                        k8b, ier)
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
            do 24 nn = 1, nm
                inoe = zi(ldnm+nn-1)
                noemax = max(noemax,inoe)
24          continue
22      continue
20  end do
    ASSERT(appui.ne.-1)
!
    call wkvect('&&RAIREP.COENO', 'V V R', noemax, icoef)
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    call wkvect('&&RAIREP.PARNO', 'V V I', noemax, idno)
!
!
!     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
!
    call wkvect('&&RAIREP.SURMAI', 'V V R', nbma, isurma)
    im = 0
    surtot = zero
    do 21 i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do 23 in = 0, nb-1
            im = im + 1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            xc = zero
            yc = zero
            hc = zero
            do 25 nn = 1, nm
                inoe = zi(ldnm+nn-1)
                zi(idno+inoe-1) = zi(idno+inoe-1) + 1
                x(nn) = zr(jcoor+3*(inoe-1)+1-1)
                y(nn) = zr(jcoor+3*(inoe-1)+2-1)
                z(nn) = zr(jcoor+3*(inoe-1)+3-1)
                xc = xc + x(nn)
                yc = yc + y(nn)
                hc = hc + z(nn)
25          continue
            xc = xc/nm
            yc = yc/nm
            hc = hc/nm
!
            if (appui .eq. 1) then
                a(1) = x(2) - x(1)
                a(2) = y(2) - y(1)
                a(3) = z(2) - z(1)
                surf = ddot(2,a,1,a,1)
                zr(isurma+im-1)= sqrt(surf)
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
                zr(isurma+im-1) = sqrt(surf)*0.5d0
            else
                ASSERT(.false.)
            endif
            if (lfonc) then
                u(1) = xg - xc
                u(2) = yg - yc
                u(3) = zg - hc
                dist=ddot(3,u,1,u,1)
                dist = sqrt(dist)
                call fointe('F ', zk8(ifongr+i-1), 1, 'X', dist,&
                            coef, iret)
                zr(isurma+im-1) = zr(isurma+im-1)*coef
            else
                zr(isurma+im-1) = zr(isurma+im-1)*zr(icoegr+i-1)
            endif
            surtot = surtot + zr(isurma+im-1)
            zr(isurma+im-1) = zr(isurma+im-1)/nm
23      continue
21  end do
!
!     CALCUL DES PONDERATIONS ELEMENTAIRES
!
    im = 0
    do 31 i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do 33 in = 0, nb-1
            im = im + 1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do 35 nn = 1, nm
                do 37 ij = 1, noemax
                    if (zi(idno+ij-1) .eq. 0) goto 37
                    if (zi(ldnm+nn-1) .eq. ij) then
                        zr(icoef+ij-1) = zr(icoef+ij-1) + zr(isurma+ im-1)/surtot
                    endif
37              continue
35          continue
33      continue
31  end do
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
    do 50 ij = 1, noemax
        if (zi(idno+ij-1) .eq. 0) goto 50
        ii = ii + 1
        xx = zr(jcoor+3*(ij-1)+1-1) - xg
        yy = zr(jcoor+3*(ij-1)+2-1) - yg
        zz = zr(jcoor+3*(ij-1)+3-1) - zg
        if (ndim .eq. 3) then
            rig4 = rig4 + (rigi(2)*zz**2+rigi(3)*yy**2)*zr(icoef+ij-1)
            rig5 = rig5 + (rigi(1)*zz**2+rigi(3)*xx**2)*zr(icoef+ij-1)
            rig6 = rig6 + (rigi(2)*xx**2+rigi(1)*yy**2)*zr(icoef+ij-1)
            rig45 = rig45 - rigi(3)*xx*yy*zr(icoef+ij-1)
            rig46 = rig46 - rigi(2)*xx*zz*zr(icoef+ij-1)
            rig56 = rig56 - rigi(1)*yy*zz*zr(icoef+ij-1)
            rig3 = 0.d0
        else
            rig3 = rig3 + (rigi(2)*xx**2+rigi(1)*yy**2)*zr(icoef+ij-1)
        endif
!
50  end do
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
    do 51 ij = 1, noemax
        if (zi(idno+ij-1) .eq. 0) goto 51
        ii = ii + 1
        r1 = rigi(1)*zr(icoef+ij-1)
        r2 = rigi(2)*zr(icoef+ij-1)
        if (ndim .eq. 3) then
            r3 = rigi(3)*zr(icoef+ij-1)
            r4 = rig4*zr(icoef+ij-1)
            r5 = rig5*zr(icoef+ij-1)
            r6 = rig6*zr(icoef+ij-1)
        else
            r3 = rig3*zr(icoef+ij-1)
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
51  end do
!
    call jedetr('&&RAIREP.COEGRO')
    call jedetr('&&RAIREP.FONGRO')
    call jedetr('&&RAIREP.COENO')
    call jedetr('&&RAIREP.PARNO')
    call jedetr('&&RAIREP.SURMAI')
!
    call jedema()
end subroutine
