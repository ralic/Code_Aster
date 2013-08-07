subroutine ssdmdm(mag)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8dgrd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ssdmge.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LE MOT CLEF "DEFI_SUPER_MAILLE"
!          DE LA COMMANDE DEFI_MAILLAGE.
!        - CREER LES OBJETS :
!            BASE GLOBALE : .DIME ,   .NOMACR, .SUPMAIL
!            BASE VOLATILE: .DIME_2 , .PARA_R, .COORDO_2
!
!     IN:
!        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
!
    character(len=8) :: kbi81, nomacr, nomail, kbid, ma
    real(kind=8) :: lisr8(9), dist, a1, a2, a3, dmin, dmax, r1
    integer :: iarg
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1noe, i1nol, iaconx, iacoo2, iacoor, iadesm
    integer :: iadim2, iadime, ialk81, ialk82, ianmcr, iaparr, iasupm
    integer :: ibid, idim, idimto, ierd, ino, inold, iocc
    integer :: iret, isma, itrou, j, jno, k, n1
    integer :: n2, n3, n4, n5, nbnoe, nbnoet, nbnol
    integer :: nbsma, nnnoe, nnnol, nocc
!-----------------------------------------------------------------------
    call jemarq()
    r1= r8dgrd()
!
!     -- ON COMPTE LES (SUPER)MAILLES :
!     ---------------------------------
    call getfac('DEFI_SUPER_MAILLE', nocc)
    nbsma=0
    do 1, iocc=1,nocc
    call getvid('DEFI_SUPER_MAILLE', 'MACR_ELEM', iocc, iarg, 0,&
                kbi81, n1)
    nbsma=nbsma-n1
    1 end do
!
!
!     -- ON ALLOUE .DIME , .NOMACR ,.PARA_R ET .SUPMAIL:
!     --------------------------------------------------
    call wkvect(mag//'.DIME', 'G V I', 6, iadime)
    zi(iadime-1+3)=0
    zi(iadime-1+4)=nbsma
    zi(iadime-1+5)=nbsma
!
    call wkvect(mag//'.NOMACR', 'G V K8', nbsma, ianmcr)
    call jecrec(mag//'.SUPMAIL', 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsma)
    call wkvect(mag//'.PARA_R', 'G V R', nbsma*14, iaparr)
!
!
!     -- ON ALLOUE DES OBJETS DE TRAVAIL :
!     ------------------------------------
    call wkvect('&&SSDMDM.LK81', 'V V K8', nbsma, ialk81)
    call wkvect('&&SSDMDM.LK82', 'V V K8', nbsma, ialk82)
    call wkvect(mag//'.DIME_2', 'V V I', nbsma*4, iadim2)
!
!
!     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
!     -----------------------------------------
    isma=0
    do 2, iocc=1,nocc
!
    call getvid('DEFI_SUPER_MAILLE', 'MACR_ELEM', iocc, iarg, nbsma,&
                zk8(ialk81), n1)
    call getvtx('DEFI_SUPER_MAILLE', 'SUPER_MAILLE', iocc, iarg, nbsma,&
                zk8(ialk82), n2)
    if (n2 .lt. 0) call u2mess('F', 'SOUSTRUC_50')
    if ((n2.gt.0) .and. (n2.ne.n1)) call u2mess('F', 'SOUSTRUC_51')
!
    do 3 ,k=1,9
    lisr8(k)=0.0d0
 3  continue
    call getvr8('DEFI_SUPER_MAILLE', 'TRAN', iocc, iarg, 3,&
                lisr8(1), n3)
    call getvr8('DEFI_SUPER_MAILLE', 'ANGL_NAUT', iocc, iarg, 3,&
                lisr8(4), n4)
    call getvr8('DEFI_SUPER_MAILLE', 'CENTRE', iocc, iarg, 3,&
                lisr8(7), n5)
    if (n3 .lt. 0) call u2mess('F', 'SOUSTRUC_52')
    if (n4 .lt. 0) call u2mess('F', 'SOUSTRUC_53')
    if (n5 .lt. 0) call u2mess('F', 'SOUSTRUC_54')
!
    do 4,i=1,n1
    isma=isma+1
    nomacr=zk8(ialk81-1+i)
    zk8(ianmcr-1+isma)=nomacr
!
    call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                ma, ierd)
    call dismoi('F', 'DIM_GEOM_B', ma, 'MAILLAGE', idim,&
                kbid, ierd)
    if (isma .eq. 1) then
        idimto=idim
        zi(iadime-1+6)=idimto
    else
        if (idim .ne. idimto) call u2mess('A', 'SOUSTRUC_55')
    endif
!
    nomail=nomacr
    if (n2 .gt. 0) nomail=zk8(ialk82-1+i)
!
    call jecroc(jexnom(mag//'.SUPMAIL', nomail))
    call jeexin(nomacr//'.DESM', iret)
    if (iret .eq. 0) call u2mesk('F', 'SOUSTRUC_56', 1, nomacr)
    call jeveuo(nomacr//'.DESM', 'L', iadesm)
    nbnoe=zi(iadesm-1+2)
    nbnol=zi(iadesm-1+8)+zi(iadesm-1+9)
    nbnoet =nbnoe+nbnol
    zi(iadim2-1+4*(isma-1)+1)= nbnoe
    zi(iadim2-1+4*(isma-1)+2)= nbnol
    call jeecra(jexnom(mag//'.SUPMAIL', nomail), 'LONMAX', nbnoet)
!
    do 5,k=1,9
    zr(iaparr-1+14*(isma-1)+k)=lisr8(k)
 5  continue
!         ON CHANGE LES DEGRES EN RADIANS:
    do 6,k=4,6
    zr(iaparr-1+14*(isma-1)+k)=zr(iaparr-1+14*(isma-1)+k)*&
                r1
 6  continue
 4  continue
!
    2 end do
!
!
!     -- MISE A JOUR DE .DIME_2 (3,4) ET .DIME :
!     ------------------------------------------
    do 10,isma=1,nbsma-1
    nbnoe=zi(iadim2-1+4*(isma-1)+1)
    nbnol=zi(iadim2-1+4*(isma-1)+2)
    zi(iadim2-1+4*(isma)+3)=zi(iadim2-1+4*(isma-1)+3)+nbnoe
    zi(iadim2-1+4*(isma)+4)=zi(iadim2-1+4*(isma-1)+4)+nbnol
    10 end do
    isma=nbsma
    nbnoe=zi(iadim2-1+4*(isma-1)+1)
    nbnol=zi(iadim2-1+4*(isma-1)+2)
    nnnoe=zi(iadim2-1+4*(isma-1)+3)+nbnoe
    nnnol=zi(iadim2-1+4*(isma-1)+4)+nbnol
    zi(iadime-1+1) =nnnoe
    zi(iadime-1+2) =nnnol
!
!
!     -- CREATION DE .COORDO_2 ET REMPLISSAGE DE .SUPMAIL:
!     ---------------------------------------------------
    call wkvect(mag//'.COORDO_2', 'V V R', 3*nnnoe, iacoo2)
    do 21, isma=1,nbsma
    nomacr=zk8(ianmcr-1+isma)
    call jeveuo(nomacr//'.CONX', 'L', iaconx)
    call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'E', iasupm)
    call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                ma, ierd)
    call jeveuo(ma//'.COORDO    .VALE', 'L', iacoor)
    nbnoe=zi(iadim2-1+4*(isma-1)+1)
    nbnol=zi(iadim2-1+4*(isma-1)+2)
    nbnoet=nbnoe+nbnol
    i1noe=zi(iadim2-1+4*(isma-1)+3)
    i1nol=zi(iadim2-1+4*(isma-1)+4)
!
    do 22, ino=1,nbnoet
!
!         -- SI C'EST UN NOEUD PHYSIQUE:
    if ((zi(iaconx-1+3*(ino-1)+1).eq.1) .and. (zi(iaconx-1+3*( ino-1)+3).eq.0)) then
        inold=zi(iaconx-1+3*(ino-1)+2)
        i1noe=i1noe+1
        zi(iasupm-1+ino)=i1noe
        call ssdmge(zr(iacoor+3*(inold-1)), zr(iacoo2+3*( i1noe-1)), zr(iaparr+14*(isma-1)),&
                    idim)
    else
!           -- SI C'EST UN NOEUD DE LAGRANGE:
        i1nol=i1nol+1
        zi(iasupm-1+ino)=nnnoe+i1nol
    endif
22  continue
    21 end do
!
!
!     -- REMPLISSAGE DE .PARA_R (13,14):
!     ----------------------------------
    do 31,isma=1,nbsma
    call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'L', iasupm)
    nbnoe=zi(iadim2-1+4*(isma-1)+1)
    nbnol=zi(iadim2-1+4*(isma-1)+2)
    nbnoet= nbnoe+nbnol
    dmin=0.0d0
    dmax=0.0d0
    itrou=0
    do 32, i=1,nbnoet
    ino=zi(iasupm-1+i)
    if (ino .gt. nnnoe) goto 32
    do 33, j=i+1,nbnoet
    jno=zi(iasupm-1+j)
    if (jno .gt. nnnoe) goto 33
    a1= zr(iacoo2-1+3*(ino-1)+1)-zr(iacoo2-1+3*(jno-1)+1)
    a2= zr(iacoo2-1+3*(ino-1)+2)-zr(iacoo2-1+3*(jno-1)+2)
    a3= zr(iacoo2-1+3*(ino-1)+3)-zr(iacoo2-1+3*(jno-1)+3)
    dist=sqrt(a1**2+a2**2+a3**2)
    if (itrou .eq. 0) then
        itrou=1
        dmin=dist
        dmax=dist
    else
        dmin=min(dmin,dist)
        dmax=max(dmax,dist)
    endif
33  continue
32  continue
    zr(iaparr-1+14*(isma-1)+13)=dmin
    zr(iaparr-1+14*(isma-1)+14)=dmax
    31 end do
!
!
! --- MENAGE
    call jedetr('&&SSDMDM.LK81')
    call jedetr('&&SSDMDM.LK82')
!
    call jedema()
end subroutine
