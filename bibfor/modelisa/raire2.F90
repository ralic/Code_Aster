subroutine raire2(noma, rigi, nbgr, ligrma, nbnoeu,&
                  nbno, tabnoe, rignoe)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterfort/compma.h"
#include "asterfort/fointe.h"
#include "asterfort/getvem.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/provec.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: nbgr, nbno, nbnoeu, tabnoe(nbnoeu)
    character(len=8) :: noma
    character(len=24) :: ligrma(nbgr)
    real(kind=8) :: rignoe(6*nbnoeu)
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
    character(len=8) :: nomnoe
    character(len=24) :: nomgr, magrno, manono, magrma, manoma
    real(kind=8) :: r8b, zero, x(8), y(8), z(8), rigi(6)
    real(kind=8) :: a(3), b(3), c(3), u(3)
    logical :: lfonc
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i, icoef, icoegr, idno, ifongr, ifr, ii
    integer :: ij, im, in, inoe, iret, isurma
    integer :: jcoor, ldgm, ldgn, ldnm, nb, nbma, ncf
    integer :: ncg, nfg, ngn, nm, nn, nno, noemax
!
    real(kind=8) :: coef, dist, hc, r1, r2, r3
    real(kind=8) :: r4, r5, r6, rig4, rig45, rig46, rig5
    real(kind=8) :: rig56, rig6, surf, surtot, xc, xg, xx
    real(kind=8) :: yc, yg, yy, zg, zz
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    ifr = iunifi('RESULTAT')
    lfonc = .false.
!
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
!
!
!        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
    call compma(noma, nbgr, ligrma, nbma)
    magrno = noma//'.GROUPENO'
    manono = noma//'.NOMNOE'
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    noemax = 0
!
!     --- DESCRIPTION NOEUDS STRUCTURE ---
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!       RECUPERATION DU CENTRE
!
    xg = zero
    yg = zero
    zg = zero
    call getvr8('ENER_SOL', 'COOR_CENTRE', 1, iarg, 0,&
                r8b, ncg)
    call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                iarg, 0, k8b, nno)
    call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                iarg, 0, k8b, ngn)
    if (ncg .ne. 0) then
        call getvr8('ENER_SOL', 'COOR_CENTRE', 1, iarg, 3,&
                    c, ncg)
        xg = c(1)
        yg = c(2)
        zg = c(3)
    else if (nno.ne.0) then
        call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                    iarg, 1, nomnoe, nno)
        call jenonu(jexnom(manono, nomnoe), inoe)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    else if (ngn.ne.0) then
        call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                    iarg, 1, nomgr, ngn)
        call jeveuo(jexnom(magrno, nomgr), 'L', ldgn)
        inoe = zi(ldgn)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    endif
!
!       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
!
    call getvr8('ENER_SOL', 'COEF_GROUP', 1, iarg, 0,&
                r8b, ncg)
    if (ncg .ne. 0) then
        call wkvect('&&RAIRE2.COEGRO', 'V V R', nbgr, icoegr)
        call getvr8('ENER_SOL', 'COEF_GROUP', 1, iarg, nbgr,&
                    zr(icoegr), ncg)
    else
        call getvid('ENER_SOL', 'FONC_GROUP', 1, iarg, 0,&
                    k8b, ncf)
        if (ncf .eq. 0) call u2mess('F', 'MODELISA6_33')
        call wkvect('&&RAIRE2.FONGRO', 'V V K8', nbgr, ifongr)
        lfonc = .true.
        call getvid('ENER_SOL', 'FONC_GROUP', 1, iarg, nbgr,&
                    zk8(ifongr), nfg)
    endif
!
    do 20 i = 1, nbgr
        call jelira(jexnom(magrma, ligrma(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, ligrma(i)), 'L', ldgm)
        do 22 in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do 24 nn = 1, nm
                inoe = zi(ldnm+nn-1)
                noemax = max(noemax,inoe)
24          continue
22      continue
20  end do
    call wkvect('&&RAIRE2.COENO', 'V V R', noemax, icoef)
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    call wkvect('&&RAIRE2.PARNO', 'V V I', noemax, idno)
!
!
!     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
!
    call wkvect('&&RAIRE2.SURMAI', 'V V R', nbma, isurma)
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
            a(1) = x(3) - x(1)
            a(2) = y(3) - y(1)
            a(3) = z(3) - z(1)
            if (nm .eq. 3 .or. nm .eq. 6) then
                b(1) = x(2) - x(1)
                b(2) = y(2) - y(1)
                b(3) = z(2) - z(1)
            else if (nm.eq.4.or.nm.eq.8) then
                b(1) = x(4) - x(2)
                b(2) = y(4) - y(2)
                b(3) = z(4) - z(2)
            else
                call u2mess('F', 'MODELISA6_34')
            endif
            call provec(a, b, c)
            surf=ddot(3,c,1,c,1)
            zr(isurma+im-1) = sqrt(surf)*0.5d0
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
!     CALCUL DES RAIDEURS DE TORSION
!
    ii = 0
    rig4 = zero
    rig5 = zero
    rig6 = zero
    rig45 = zero
    rig46 = zero
    rig56 = zero
    do 50 ij = 1, noemax
        if (zi(idno+ij-1) .eq. 0) goto 50
        ii = ii + 1
        xx = zr(jcoor+3*(ij-1)+1-1) - xg
        yy = zr(jcoor+3*(ij-1)+2-1) - yg
        zz = zr(jcoor+3*(ij-1)+3-1) - zg
        rig4 = rig4 + (rigi(2)*zz**2+rigi(3)*yy**2)*zr(icoef+ij-1)
        rig5 = rig5 + (rigi(1)*zz**2+rigi(3)*xx**2)*zr(icoef+ij-1)
        rig6 = rig6 + (rigi(2)*xx**2+rigi(1)*yy**2)*zr(icoef+ij-1)
        rig45 = rig45 - rigi(3)*xx*yy*zr(icoef+ij-1)
        rig46 = rig46 - rigi(2)*xx*zz*zr(icoef+ij-1)
        rig56 = rig56 - rigi(1)*yy*zz*zr(icoef+ij-1)
50  end do
    nbno = ii
    rig4 = rigi(4) - rig4
    rig5 = rigi(5) - rig5
    rig6 = rigi(6) - rig6
    write(ifr,1001) rig4,rig5,rig6
!
    ii = 0
    do 51 ij = 1, noemax
        if (zi(idno+ij-1) .eq. 0) goto 51
        ii = ii + 1
        r1 = rigi(1)*zr(icoef+ij-1)
        r2 = rigi(2)*zr(icoef+ij-1)
        r3 = rigi(3)*zr(icoef+ij-1)
        r4 = rig4*zr(icoef+ij-1)
        r5 = rig5*zr(icoef+ij-1)
        r6 = rig6*zr(icoef+ij-1)
        rignoe(6*(ii-1)+1) = r1
        rignoe(6*(ii-1)+2) = r2
        rignoe(6*(ii-1)+3) = r3
        rignoe(6*(ii-1)+4) = r4
        rignoe(6*(ii-1)+5) = r5
        rignoe(6*(ii-1)+6) = r6
        tabnoe(ii) = ij
51  end do
!
    1001 format(1x,'RAIDEURS DE ROTATION A REPARTIR:',/&
     &      1x,' KRX: ',1x,1pe12.5,' KRY: ',1x,1pe12.5,&
     &      ' KRZ: ',1x,1pe12.5)
    call jedetr('&&RAIRE2.COEGRO')
    call jedetr('&&RAIRE2.FONGRO')
    call jedetr('&&RAIRE2.COENO')
    call jedetr('&&RAIRE2.PARNO')
    call jedetr('&&RAIRE2.SURMAI')
!
    call jedema()
end subroutine
