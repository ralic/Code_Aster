subroutine masrep(noma, ioc, rigi, lvale, nbgr,&
                  ligrma, nbno, tabnoe, rignoe, rigto,&
                  ndim)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/compma.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
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
    character(len=8) :: noma, tabnoe(*)
    character(len=24) :: ligrma(nbgr)
    real(kind=8) :: rignoe(*), rigto(*)
    logical(kind=1) :: lvale
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8) :: nomnoe, typm
    character(len=8) :: nompar(3)
    character(len=24) :: manono, magrma, manoma, matyma
    real(kind=8) :: zero, x(9), y(9), z(9), rigi(6)
    real(kind=8) :: a(3), b(3), c(3), u(3)
    logical(kind=1) :: lfonc
    integer :: appui
!
!-----------------------------------------------------------------------
    integer :: i,    ii, iunite
    integer :: ij, im, in, inoe, iret
    integer :: ldgm, ldnm, ltyp, nb, nbma
    integer :: nfg, nm, nn, noemax, ntopo, numa


    real(kind=8) :: coef, hc, r1, r2, r3, r4, r5, r6
    real(kind=8) :: surf, surtot, xc
    real(kind=8) :: yc, z0
    real(kind=8) :: surtxx, surtxy, surtxz, surtyy, surtyz, surtzz
    real(kind=8), pointer :: coeno(:) => null()
    real(kind=8), pointer :: coenxx(:) => null()
    real(kind=8), pointer :: coenxy(:) => null()
    real(kind=8), pointer :: coenxz(:) => null()
    real(kind=8), pointer :: coenyy(:) => null()
    real(kind=8), pointer :: coenyz(:) => null()
    real(kind=8), pointer :: coenzz(:) => null()
    character(len=8), pointer :: fongro(:) => null()
    integer, pointer :: parno(:) => null()
    real(kind=8), pointer :: surma1(:) => null()
    real(kind=8), pointer :: surma2(:) => null()
    real(kind=8), pointer :: surma3(:) => null()
    real(kind=8), pointer :: surma4(:) => null()
    real(kind=8), pointer :: surma5(:) => null()
    real(kind=8), pointer :: surma6(:) => null()
    real(kind=8), pointer :: surmai(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    lfonc = .false.
    iunite = 6
!
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
!
!        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
    call compma(noma, nbgr, ligrma, nbma)
    manono = noma//'.NOMNOE'
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    matyma = noma//'.TYPMAIL'
!
    noemax = 0
!
!
!     --- DESCRIPTION NOEUDS STRUCTURE ---
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
!
!       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
!
!      CALL GETVR8('MASS_AJOU','Z0',IOC,IARG,1,Z0,NCG)
    z0 = zero
    call getvid('MASS_AJOU', 'FONC_GROUP', iocc=ioc, nbval=0, nbret=nfg)
    if (nfg .ne. 0) then
        AS_ALLOCATE(vk8=fongro, size=nbgr)
        lfonc = .true.
        call getvid('MASS_AJOU', 'FONC_GROUP', iocc=ioc, nbval=nbgr, vect=fongro,&
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
    AS_ALLOCATE(vr=coenxx, size=noemax)
    AS_ALLOCATE(vr=coenxy, size=noemax)
    AS_ALLOCATE(vr=coenxz, size=noemax)
    AS_ALLOCATE(vr=coenyy, size=noemax)
    AS_ALLOCATE(vr=coenyz, size=noemax)
    AS_ALLOCATE(vr=coenzz, size=noemax)
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    AS_ALLOCATE(vi=parno, size=noemax)
!
!
!     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
!
    AS_ALLOCATE(vr=surmai, size=nbma)
    AS_ALLOCATE(vr=surma1, size=nbma)
    AS_ALLOCATE(vr=surma2, size=nbma)
    AS_ALLOCATE(vr=surma3, size=nbma)
    AS_ALLOCATE(vr=surma4, size=nbma)
    AS_ALLOCATE(vr=surma5, size=nbma)
    AS_ALLOCATE(vr=surma6, size=nbma)
    im = 0
    surtot = zero
    surtxx = zero
    surtxy = zero
    surtxz = zero
    surtyy = zero
    surtyz = zero
    surtzz = zero
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
                x(nn) = vale(1+3*(inoe-1)+1-1)
                y(nn) = vale(1+3*(inoe-1)+2-1)
                z(nn) = vale(1+3*(inoe-1)+3-1)
                xc = xc + x(nn)
                yc = yc + y(nn)
                hc = hc + z(nn)
            end do
            xc = xc/nm
            yc = yc/nm
            hc = hc/nm
!
            if (appui .eq. 1) then
                ASSERT(.false.)
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
                c(1)=c(1)/sqrt(surf)
                c(2)=c(2)/sqrt(surf)
                c(3)=c(3)/sqrt(surf)
                surmai(im) = sqrt(surf)*0.5d0
            else
                ASSERT(.false.)
            endif
            if (.not.lvale) surtot = surtot + surmai(im)
            if (lfonc) then
                u(1) = xc
                u(2) = yc
                u(3) = hc
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'Z'
                call fointe('F ', fongro(i), 3, nompar, u,&
                            coef, iret)
                surmai(im) = surmai(im)*coef
            else
                surmai(im) = surmai(im)*1.0d3*(z0-hc)
            endif
            if (lvale) then
                surtot = surtot + surmai(im)
                surmai(im) = surmai(im)/nm
            else
                surtxx = surtxx + surmai(im)*c(1)*c(1)
                surtxy = surtxy + surmai(im)*c(1)*c(2)
                surtxz = surtxz + surmai(im)*c(1)*c(3)
                surtyy = surtyy + surmai(im)*c(2)*c(2)
                surtyz = surtyz + surmai(im)*c(2)*c(3)
                surtzz = surtzz + surmai(im)*c(3)*c(3)
                surma1(im) = surmai(im)*c(1)*c(1)/nm
                surma2(im) = surmai(im)*c(1)*c(2)/nm
                surma4(im) = surmai(im)*c(1)*c(3)/nm
                surma3(im) = surmai(im)*c(2)*c(2)/nm
                surma5(im) = surmai(im)*c(2)*c(3)/nm
                surma6(im) = surmai(im)*c(3)*c(3)/nm
            endif
        end do
    end do
!
    write(iunite,1010) surtxx,surtxy,surtxz,surtyy,surtyz,surtzz
!
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
                        if (lvale) then
                            coeno(ij)=coeno(ij)+surmai(1+&
                            im-1)/surtot
                        else
                            coenxx(ij) = coenxx(ij) + surma1(im)
                            coenxy(ij) = coenxy(ij) + surma2(im)
                            coenxz(ij) = coenxz(ij) + surma4(im)
                            coenyy(ij) = coenyy(ij) + surma3(im)
                            coenyz(ij) = coenyz(ij) + surma5(im)
                            coenzz(ij) = coenzz(ij) + surma6(im)
                        endif
                    endif
 37                 continue
                end do
            end do
        end do
    end do
    nbma = im
!
    ii = 0
    do ij = 1, noemax
        if (parno(ij) .eq. 0) goto 51
        ii = ii + 1
        if (lvale) then
            r1 = rigi(1)*coeno(ij)
            r2 = rigi(2)*coeno(ij)
            r3 = rigi(3)*coeno(ij)
        else
            r1 = coenxx(ij)
            r2 = coenxy(ij)
            r3 = coenyy(ij)
        endif
        if (ndim .eq. 3) then
            if (lvale) then
                r4 = rigi(4)*coeno(ij)
                r5 = rigi(5)*coeno(ij)
                r6 = rigi(6)*coeno(ij)
            else
                r4 = coenxz(ij)
                r5 = coenyz(ij)
                r6 = coenzz(ij)
            endif
        else
            r4 = zero
            r5 = zero
            r6 = zero
        endif
        call jenuno(jexnum(manono, ij), nomnoe)
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
        rignoe(6*(ii-1)+1) = r1
        rignoe(6*(ii-1)+2) = r2
        rignoe(6*(ii-1)+3) = r3
        rignoe(6*(ii-1)+4) = r4
        rignoe(6*(ii-1)+5) = r5
        rignoe(6*(ii-1)+6) = r6
        tabnoe(ii) = nomnoe
 51     continue
    end do
    nbno = ii
!
    AS_DEALLOCATE(vk8=fongro)
    AS_DEALLOCATE(vr=coeno)
    AS_DEALLOCATE(vr=coenxx)
    AS_DEALLOCATE(vr=coenxy)
    AS_DEALLOCATE(vr=coenyy)
    AS_DEALLOCATE(vr=coenxz)
    AS_DEALLOCATE(vr=coenyz)
    AS_DEALLOCATE(vr=coenzz)
    AS_DEALLOCATE(vi=parno)
    AS_DEALLOCATE(vr=surmai)
    AS_DEALLOCATE(vr=surma1)
    AS_DEALLOCATE(vr=surma2)
    AS_DEALLOCATE(vr=surma3)
    AS_DEALLOCATE(vr=surma4)
    AS_DEALLOCATE(vr=surma5)
    AS_DEALLOCATE(vr=surma6)
!
    call jedema()
    1010 format(' MXX= ',1pe12.5,' MXY= ',1pe12.5,' MXZ= ',1pe12.5/,&
     &       ' MYY= ',1pe12.5,' MYZ= ',1pe12.5,' MZZ= ',1pe12.5)
end subroutine
