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
#include "blas/ddot.h"
!
    integer :: ioc, nbgr, nbno, ndim
    character(len=8) :: noma, tabnoe(*)
    character(len=24) :: ligrma(nbgr)
    real(kind=8) :: rignoe(*), rigto(*)
    logical :: lvale
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
    logical :: lfonc
    integer :: appui
!
!-----------------------------------------------------------------------
    integer :: i, icoef, idno, ifongr, ii, iunite
    integer :: ij, im, in, inoe, iret, isurma, jcoor
    integer :: ldgm, ldnm, ltyp, nb, nbma
    integer :: nfg, nm, nn, noemax, ntopo, numa
    integer :: isurm1, isurm2, isurm3, isurm4, isurm5, isurm6
    integer :: icoexx, icoexy, icoexz, icoeyy, icoeyz, icoezz
    real(kind=8) :: coef, hc, r1, r2, r3, r4, r5, r6
    real(kind=8) :: surf, surtot, xc
    real(kind=8) :: yc, z0
    real(kind=8) :: surtxx, surtxy, surtxz, surtyy, surtyz, surtzz
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
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!
!       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
!
!      CALL GETVR8('MASS_AJOU','Z0',IOC,IARG,1,Z0,NCG)
    z0 = zero
    call getvid('MASS_AJOU', 'FONC_GROUP', iocc=ioc, nbval=0, nbret=nfg)
    if (nfg .ne. 0) then
        call wkvect('&&MASREP.FONGRO', 'V V K8', nbgr, ifongr)
        lfonc = .true.
        call getvid('MASS_AJOU', 'FONC_GROUP', iocc=ioc, nbval=nbgr, vect=zk8( ifongr),&
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
            do 24 nn = 1, nm
                inoe = zi(ldnm+nn-1)
                noemax = max(noemax,inoe)
 24         continue
 22     continue
 20 end do
    ASSERT(appui.ne.-1)
!
    call wkvect('&&MASREP.COENO', 'V V R', noemax, icoef)
    call wkvect('&&MASREP.COENXX', 'V V R', noemax, icoexx)
    call wkvect('&&MASREP.COENXY', 'V V R', noemax, icoexy)
    call wkvect('&&MASREP.COENXZ', 'V V R', noemax, icoexz)
    call wkvect('&&MASREP.COENYY', 'V V R', noemax, icoeyy)
    call wkvect('&&MASREP.COENYZ', 'V V R', noemax, icoeyz)
    call wkvect('&&MASREP.COENZZ', 'V V R', noemax, icoezz)
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    call wkvect('&&MASREP.PARNO', 'V V I', noemax, idno)
!
!
!     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
!
    call wkvect('&&MASREP.SURMAI', 'V V R', nbma, isurma)
    call wkvect('&&MASREP.SURMA1', 'V V R', nbma, isurm1)
    call wkvect('&&MASREP.SURMA2', 'V V R', nbma, isurm2)
    call wkvect('&&MASREP.SURMA3', 'V V R', nbma, isurm3)
    call wkvect('&&MASREP.SURMA4', 'V V R', nbma, isurm4)
    call wkvect('&&MASREP.SURMA5', 'V V R', nbma, isurm5)
    call wkvect('&&MASREP.SURMA6', 'V V R', nbma, isurm6)
    im = 0
    surtot = zero
    surtxx = zero
    surtxy = zero
    surtxz = zero
    surtyy = zero
    surtyz = zero
    surtzz = zero
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
 25         continue
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
                zr(isurma+im-1) = sqrt(surf)*0.5d0
            else
                ASSERT(.false.)
            endif
            if (.not.lvale) surtot = surtot + zr(isurma+im-1)
            if (lfonc) then
                u(1) = xc
                u(2) = yc
                u(3) = hc
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'Z'
                call fointe('F ', zk8(ifongr+i-1), 3, nompar, u,&
                            coef, iret)
                zr(isurma+im-1) = zr(isurma+im-1)*coef
            else
                zr(isurma+im-1) = zr(isurma+im-1)*1.0d3*(z0-hc)
            endif
            if (lvale) then
                surtot = surtot + zr(isurma+im-1)
                zr(isurma+im-1) = zr(isurma+im-1)/nm
            else
                surtxx = surtxx + zr(isurma+im-1)*c(1)*c(1)
                surtxy = surtxy + zr(isurma+im-1)*c(1)*c(2)
                surtxz = surtxz + zr(isurma+im-1)*c(1)*c(3)
                surtyy = surtyy + zr(isurma+im-1)*c(2)*c(2)
                surtyz = surtyz + zr(isurma+im-1)*c(2)*c(3)
                surtzz = surtzz + zr(isurma+im-1)*c(3)*c(3)
                zr(isurm1+im-1) = zr(isurma+im-1)*c(1)*c(1)/nm
                zr(isurm2+im-1) = zr(isurma+im-1)*c(1)*c(2)/nm
                zr(isurm4+im-1) = zr(isurma+im-1)*c(1)*c(3)/nm
                zr(isurm3+im-1) = zr(isurma+im-1)*c(2)*c(2)/nm
                zr(isurm5+im-1) = zr(isurma+im-1)*c(2)*c(3)/nm
                zr(isurm6+im-1) = zr(isurma+im-1)*c(3)*c(3)/nm
            endif
 23     continue
 21 end do
!
    write(iunite,1010) surtxx,surtxy,surtxz,surtyy,surtyz,surtzz
!
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
                        if (lvale) then
                            zr(icoef+ij-1)=zr(icoef+ij-1)+zr(isurma+&
                            im-1)/surtot
                        else
                            zr(icoexx+ij-1) = zr(icoexx+ij-1) + zr( isurm1+im-1)
                            zr(icoexy+ij-1) = zr(icoexy+ij-1) + zr( isurm2+im-1)
                            zr(icoexz+ij-1) = zr(icoexz+ij-1) + zr( isurm4+im-1)
                            zr(icoeyy+ij-1) = zr(icoeyy+ij-1) + zr( isurm3+im-1)
                            zr(icoeyz+ij-1) = zr(icoeyz+ij-1) + zr( isurm5+im-1)
                            zr(icoezz+ij-1) = zr(icoezz+ij-1) + zr( isurm6+im-1)
                        endif
                    endif
 37             continue
 35         continue
 33     continue
 31 end do
    nbma = im
!
    ii = 0
    do 51 ij = 1, noemax
        if (zi(idno+ij-1) .eq. 0) goto 51
        ii = ii + 1
        if (lvale) then
            r1 = rigi(1)*zr(icoef+ij-1)
            r2 = rigi(2)*zr(icoef+ij-1)
            r3 = rigi(3)*zr(icoef+ij-1)
        else
            r1 = zr(icoexx+ij-1)
            r2 = zr(icoexy+ij-1)
            r3 = zr(icoeyy+ij-1)
        endif
        if (ndim .eq. 3) then
            if (lvale) then
                r4 = rigi(4)*zr(icoef+ij-1)
                r5 = rigi(5)*zr(icoef+ij-1)
                r6 = rigi(6)*zr(icoef+ij-1)
            else
                r4 = zr(icoexz+ij-1)
                r5 = zr(icoeyz+ij-1)
                r6 = zr(icoezz+ij-1)
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
 51 end do
    nbno = ii
!
    call jedetr('&&MASREP.FONGRO')
    call jedetr('&&MASREP.COENO')
    call jedetr('&&MASREP.COENXX')
    call jedetr('&&MASREP.COENXY')
    call jedetr('&&MASREP.COENYY')
    call jedetr('&&MASREP.COENXZ')
    call jedetr('&&MASREP.COENYZ')
    call jedetr('&&MASREP.COENZZ')
    call jedetr('&&MASREP.PARNO')
    call jedetr('&&MASREP.SURMAI')
    call jedetr('&&MASREP.SURMA1')
    call jedetr('&&MASREP.SURMA2')
    call jedetr('&&MASREP.SURMA3')
    call jedetr('&&MASREP.SURMA4')
    call jedetr('&&MASREP.SURMA5')
    call jedetr('&&MASREP.SURMA6')
!
    call jedema()
    1010 format(' MXX= ',1pe12.5,' MXY= ',1pe12.5,' MXZ= ',1pe12.5/,&
     &       ' MYY= ',1pe12.5,' MYZ= ',1pe12.5,' MZZ= ',1pe12.5)
end subroutine
