subroutine ssdmdm(mag)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/getfac.h"
#include "asterc/r8dgrd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
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
    character(len=8) :: nomacr, nomail, ma
    real(kind=8) :: lisr8(9), dist, a1, a2, a3, dmin, dmax, r1
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1noe, i1nol,  iacoo2, iacoor
    integer :: iadim2, iadime,   ianmcr, iaparr, iasupm
    integer :: idim, idimto, ino, inold, iocc
    integer :: iret, isma, itrou, j, jno, k, n1
    integer :: n2, n3, n4, n5, nbnoe, nbnoet, nbnol
    integer :: nbsma, nnnoe, nnnol, nocc
    character(len=8), pointer :: lk81(:) => null()
    character(len=8), pointer :: lk82(:) => null()
    integer, pointer :: desm(:) => null()
    integer, pointer :: conx(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    r1= r8dgrd()
!
!     -- ON COMPTE LES (SUPER)MAILLES :
!     ---------------------------------
    call getfac('DEFI_SUPER_MAILLE', nocc)
    nbsma=0
    do iocc = 1, nocc
        call getvid('DEFI_SUPER_MAILLE', 'MACR_ELEM', iocc=iocc, nbval=0, nbret=n1)
        nbsma=nbsma-n1
    end do
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
    AS_ALLOCATE(vk8=lk81, size=nbsma)
    AS_ALLOCATE(vk8=lk82, size=nbsma)
    call wkvect(mag//'.DIME_2', 'V V I', nbsma*4, iadim2)
!
!
!     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
!     -----------------------------------------
    isma=0
    do iocc = 1, nocc
!
        call getvid('DEFI_SUPER_MAILLE', 'MACR_ELEM', iocc=iocc, nbval=nbsma, vect=lk81,&
                    nbret=n1)
        call getvtx('DEFI_SUPER_MAILLE', 'SUPER_MAILLE', iocc=iocc, nbval=nbsma,&
                    vect=lk82, nbret=n2)
        if (n2 .lt. 0) then
            call utmess('F', 'SOUSTRUC_50')
        endif
        if ((n2.gt.0) .and. (n2.ne.n1)) then
            call utmess('F', 'SOUSTRUC_51')
        endif
!
        do 3 ,k=1,9
        lisr8(k)=0.0d0
  3     continue
        call getvr8('DEFI_SUPER_MAILLE', 'TRAN', iocc=iocc, nbval=3, vect=lisr8(1),&
                    nbret=n3)
        call getvr8('DEFI_SUPER_MAILLE', 'ANGL_NAUT', iocc=iocc, nbval=3, vect=lisr8(4),&
                    nbret=n4)
        call getvr8('DEFI_SUPER_MAILLE', 'CENTRE', iocc=iocc, nbval=3, vect=lisr8(7),&
                    nbret=n5)
        if (n3 .lt. 0) then
            call utmess('F', 'SOUSTRUC_52')
        endif
        if (n4 .lt. 0) then
            call utmess('F', 'SOUSTRUC_53')
        endif
        if (n5 .lt. 0) then
            call utmess('F', 'SOUSTRUC_54')
        endif
!
        do i = 1, n1
            isma=isma+1
            nomacr=lk81(i)
            zk8(ianmcr-1+isma)=nomacr
!
            call dismoi('NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', repk=ma)
            call dismoi('DIM_GEOM_B', ma, 'MAILLAGE', repi=idim)
            if (isma .eq. 1) then
                idimto=idim
                zi(iadime-1+6)=idimto
            else
                if (idim .ne. idimto) then
                    call utmess('A', 'SOUSTRUC_55')
                endif
            endif
!
            nomail=nomacr
            if (n2 .gt. 0) nomail=lk82(i)
!
            call jecroc(jexnom(mag//'.SUPMAIL', nomail))
            call jeexin(nomacr//'.DESM', iret)
            if (iret .eq. 0) then
                call utmess('F', 'SOUSTRUC_56', sk=nomacr)
            endif
            call jeveuo(nomacr//'.DESM', 'L', vi=desm)
            nbnoe=desm(2)
            nbnol=desm(8)+desm(9)
            nbnoet =nbnoe+nbnol
            zi(iadim2-1+4*(isma-1)+1)= nbnoe
            zi(iadim2-1+4*(isma-1)+2)= nbnol
            call jeecra(jexnom(mag//'.SUPMAIL', nomail), 'LONMAX', nbnoet)
!
            do k = 1, 9
                zr(iaparr-1+14*(isma-1)+k)=lisr8(k)
            end do
!         ON CHANGE LES DEGRES EN RADIANS:
            do k = 4, 6
                zr(iaparr-1+14*(isma-1)+k)=zr(iaparr-1+14*(isma-1)+k)*&
                r1
            end do
        end do
!
    end do
!
!
!     -- MISE A JOUR DE .DIME_2 (3,4) ET .DIME :
!     ------------------------------------------
    do isma = 1, nbsma-1
        nbnoe=zi(iadim2-1+4*(isma-1)+1)
        nbnol=zi(iadim2-1+4*(isma-1)+2)
        zi(iadim2-1+4*(isma)+3)=zi(iadim2-1+4*(isma-1)+3)+nbnoe
        zi(iadim2-1+4*(isma)+4)=zi(iadim2-1+4*(isma-1)+4)+nbnol
    end do
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
    do isma = 1, nbsma
        nomacr=zk8(ianmcr-1+isma)
        call jeveuo(nomacr//'.CONX', 'L', vi=conx)
        call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'E', iasupm)
        call dismoi('NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', repk=ma)
        call jeveuo(ma//'.COORDO    .VALE', 'L', iacoor)
        nbnoe=zi(iadim2-1+4*(isma-1)+1)
        nbnol=zi(iadim2-1+4*(isma-1)+2)
        nbnoet=nbnoe+nbnol
        i1noe=zi(iadim2-1+4*(isma-1)+3)
        i1nol=zi(iadim2-1+4*(isma-1)+4)
!
        do ino = 1, nbnoet
!
!         -- SI C'EST UN NOEUD PHYSIQUE:
            if ((conx(3*(ino-1)+1).eq.1) .and. (conx(3*( ino-1)+3).eq.0)) then
                inold=conx(3*(ino-1)+2)
                i1noe=i1noe+1
                zi(iasupm-1+ino)=i1noe
                call ssdmge(zr(iacoor+3*(inold-1)), zr(iacoo2+3*( i1noe-1)),&
                            zr(iaparr+14*(isma-1)), idim)
            else
!           -- SI C'EST UN NOEUD DE LAGRANGE:
                i1nol=i1nol+1
                zi(iasupm-1+ino)=nnnoe+i1nol
            endif
        end do
    end do
!
!
!     -- REMPLISSAGE DE .PARA_R (13,14):
!     ----------------------------------
    do isma = 1, nbsma
        call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'L', iasupm)
        nbnoe=zi(iadim2-1+4*(isma-1)+1)
        nbnol=zi(iadim2-1+4*(isma-1)+2)
        nbnoet= nbnoe+nbnol
        dmin=0.0d0
        dmax=0.0d0
        itrou=0
        do i = 1, nbnoet
            ino=zi(iasupm-1+i)
            if (ino .gt. nnnoe) goto 32
            do j = i+1, nbnoet
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
 33             continue
            end do
 32         continue
        end do
        zr(iaparr-1+14*(isma-1)+13)=dmin
        zr(iaparr-1+14*(isma-1)+14)=dmax
    end do
!
!
! --- MENAGE
    AS_DEALLOCATE(vk8=lk81)
    AS_DEALLOCATE(vk8=lk82)
!
    call jedema()
end subroutine
