subroutine raire2(noma, rigi, nbgr, ligrma, nbnoeu,&
                  nbno, tabnoe, rignoe)
    implicit none
#include "jeveux.h"
#include "asterfort/compma.h"
#include "asterfort/fointe.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
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
    real(kind=8) :: zero, x(8), y(8), z(8), rigi(6)
    real(kind=8) :: a(3), b(3), c(3), u(3)
    logical :: lfonc
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i,     ifr, ii
    integer :: ij, im, in, inoe, iret
    integer ::  ldgm, ldgn, ldnm, nb, nbma, ncf
    integer :: ncg, nfg, ngn, nm, nn, nno, noemax
!
    real(kind=8) :: coef, dist, hc, r1, r2, r3
    real(kind=8) :: r4, r5, r6, rig4, rig45, rig46, rig5
    real(kind=8) :: rig56, rig6, surf, surtot, xc, xg, xx
    real(kind=8) :: yc, yg, yy, zg, zz
    real(kind=8), pointer :: coegro(:) => null()
    real(kind=8), pointer :: coeno(:) => null()
    character(len=8), pointer :: fongro(:) => null()
    integer, pointer :: parno(:) => null()
    real(kind=8), pointer :: surmai(:) => null()
    real(kind=8), pointer :: vale(:) => null()
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
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
!       RECUPERATION DU CENTRE
!
    xg = zero
    yg = zero
    zg = zero
    call getvr8('ENER_SOL', 'COOR_CENTRE', iocc=1, nbval=0, nbret=ncg)
    call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                iarg, 0, k8b, nno)
    call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                iarg, 0, k8b, ngn)
    if (ncg .ne. 0) then
        call getvr8('ENER_SOL', 'COOR_CENTRE', iocc=1, nbval=3, vect=c,&
                    nbret=ncg)
        xg = c(1)
        yg = c(2)
        zg = c(3)
    else if (nno.ne.0) then
        call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                    iarg, 1, nomnoe, nno)
        call jenonu(jexnom(manono, nomnoe), inoe)
        xg = vale(1+3*(inoe-1)+1-1)
        yg = vale(1+3*(inoe-1)+2-1)
        zg = vale(1+3*(inoe-1)+3-1)
    else if (ngn.ne.0) then
        call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                    iarg, 1, nomgr, ngn)
        call jeveuo(jexnom(magrno, nomgr), 'L', ldgn)
        inoe = zi(ldgn)
        xg = vale(1+3*(inoe-1)+1-1)
        yg = vale(1+3*(inoe-1)+2-1)
        zg = vale(1+3*(inoe-1)+3-1)
    endif
!
!       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
!
    call getvr8('ENER_SOL', 'COEF_GROUP', iocc=1, nbval=0, nbret=ncg)
    if (ncg .ne. 0) then
        AS_ALLOCATE(vr=coegro, size=nbgr)
        call getvr8('ENER_SOL', 'COEF_GROUP', iocc=1, nbval=nbgr, vect=coegro,&
                    nbret=ncg)
    else
        call getvid('ENER_SOL', 'FONC_GROUP', iocc=1, nbval=0, nbret=ncf)
        if (ncf .eq. 0) then
            call utmess('F', 'MODELISA6_33')
        endif
        AS_ALLOCATE(vk8=fongro, size=nbgr)
        lfonc = .true.
        call getvid('ENER_SOL', 'FONC_GROUP', iocc=1, nbval=nbgr, vect=fongro,&
                    nbret=nfg)
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
                parno(inoe) = parno(inoe) + 1
                x(nn) = vale(1+3*(inoe-1)+1-1)
                y(nn) = vale(1+3*(inoe-1)+2-1)
                z(nn) = vale(1+3*(inoe-1)+3-1)
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
                call utmess('F', 'MODELISA6_34')
            endif
            call provec(a, b, c)
            surf=ddot(3,c,1,c,1)
            surmai(im) = sqrt(surf)*0.5d0
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
                    if (parno(ij) .eq. 0) goto 37
                    if (zi(ldnm+nn-1) .eq. ij) then
                        coeno(ij) = coeno(ij) + surmai(im)/surtot
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
        if (parno(ij) .eq. 0) goto 50
        ii = ii + 1
        xx = vale(1+3*(ij-1)+1-1) - xg
        yy = vale(1+3*(ij-1)+2-1) - yg
        zz = vale(1+3*(ij-1)+3-1) - zg
        rig4 = rig4 + (rigi(2)*zz**2+rigi(3)*yy**2)*coeno(ij)
        rig5 = rig5 + (rigi(1)*zz**2+rigi(3)*xx**2)*coeno(ij)
        rig6 = rig6 + (rigi(2)*xx**2+rigi(1)*yy**2)*coeno(ij)
        rig45 = rig45 - rigi(3)*xx*yy*coeno(ij)
        rig46 = rig46 - rigi(2)*xx*zz*coeno(ij)
        rig56 = rig56 - rigi(1)*yy*zz*coeno(ij)
50  end do
    nbno = ii
    rig4 = rigi(4) - rig4
    rig5 = rigi(5) - rig5
    rig6 = rigi(6) - rig6
    write(ifr,1001) rig4,rig5,rig6
!
    ii = 0
    do 51 ij = 1, noemax
        if (parno(ij) .eq. 0) goto 51
        ii = ii + 1
        r1 = rigi(1)*coeno(ij)
        r2 = rigi(2)*coeno(ij)
        r3 = rigi(3)*coeno(ij)
        r4 = rig4*coeno(ij)
        r5 = rig5*coeno(ij)
        r6 = rig6*coeno(ij)
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
    AS_DEALLOCATE(vr=coegro)
    AS_DEALLOCATE(vk8=fongro)
    AS_DEALLOCATE(vr=coeno)
    AS_DEALLOCATE(vi=parno)
    AS_DEALLOCATE(vr=surmai)
!
    call jedema()
end subroutine
