subroutine extrs1(resu0, nbrang, nuordr, nbpara, nompar,&
                  nbarch, nuarch, nbexcl, chexcl, nbnosy)
    implicit none
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/extrs3.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsmena.h"
#include "asterfort/rsutch.h"
!
    integer :: nbrang, nuordr(*), nbarch, nbpara, nuarch(*), nbexcl, nbnosy
    character(len=16) :: nompar(*), chexcl(*)
    character(len=*) :: resu0
!     ------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!     EXTR_RESU / ARCHIVAGE + REUSE
! ----------------------------------------------------------------------
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: irang, i, j, k, jtach, iadin, iadou, ire1
    integer ::  iundf, iordr
    real(kind=8) :: rundf
    character(len=3) :: type
    character(len=16) :: nomsym
    character(len=16) :: nopara
    character(len=19) :: nomsdr
    character(len=19) :: chamin, nomch1, nomch2
    integer, pointer :: ordr(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    rundf=r8vide()
    iundf=isnnem()
!
    nomsdr=resu0
!
!
!     1. -- ON COMPACTE LES CHAMPS ARCHIVES :
!     -------------------------------------------------------
    do 50 i = 1, nbnosy
        call jenuno(jexnum(nomsdr//'.DESC', i), nomsym)
        call jeveuo(jexnum(nomsdr//'.TACH', i), 'E', jtach)
        do 20 j = 1, nbexcl
            if (chexcl(j) .eq. nomsym) then
                do 10 k = 1, nbrang
                    if (zk24(jtach+k-1)(1:1) .eq. ' ') goto 10
                    call rsexch('F', nomsdr, nomsym, nuordr(k), chamin,&
                                ire1)
                    call detrsd('CHAMP_GD', chamin)
                    zk24(jtach+k-1)=' '
10              continue
                goto 50
!
            endif
20      continue
!
        irang=0
        do 30 j = 1, nbrang
            if (zk24(jtach+j-1)(1:1) .eq. ' ') goto 30
            call rsexch('F', nomsdr, nomsym, nuordr(j), chamin,&
                        ire1)
            if (nuarch(j) .eq. 0) then
                call detrsd('CHAMP_GD', chamin)
            else
                irang=irang+1
                zk24(jtach+irang-1)=chamin
            endif
30      continue
!
        do 40 k = irang+1, nbrang
            zk24(jtach+k-1)=' '
40      continue
50  end do
!
!
!     2. -- ON COMPACTE LES PARAMETRES ARCHIVES :
!     -------------------------------------------
    irang=0
    do 70 i = 1, nbrang
        if (nuarch(i) .eq. 0) goto 70
        irang=irang+1
        do 60 j = 1, nbpara
            nopara=nompar(j)
            call rsadpa(nomsdr, 'L', 1, nopara, nuordr(i),&
                        1, sjv=iadin, styp=type, istop=0)
            call extrs3(nomsdr, nopara, irang, 'E', 1,&
                        type, iadou)
            if (type(1:1) .eq. 'I') then
                zi(iadou)=zi(iadin)
            else if (type(1:1).eq.'R') then
                zr(iadou)=zr(iadin)
            else if (type(1:1).eq.'C') then
                zc(iadou)=zc(iadin)
            else if (type(1:3).eq.'K80') then
                zk80(iadou)=zk80(iadin)
            else if (type(1:3).eq.'K32') then
                zk32(iadou)=zk32(iadin)
            else if (type(1:3).eq.'K24') then
                zk24(iadou)=zk24(iadin)
            else if (type(1:3).eq.'K16') then
                zk16(iadou)=zk16(iadin)
            else if (type(1:2).eq.'K8') then
                zk8(iadou)=zk8(iadin)
            endif
60      continue
70  end do
    ASSERT(irang.eq.nbarch)
!
!
!     3. -- ON COMPACTE LES NUME_ORDRE ARCHIVES :
!     -------------------------------------------
    call jeecra(nomsdr//'.ORDR', 'LONUTI', nbarch)
    call jeveuo(nomsdr//'.ORDR', 'E', vi=ordr)
    irang=0
    do 80 i = 1, nbrang
        if (nuarch(i) .eq. 0) goto 80
        irang=irang+1
        ordr(irang)=nuordr(i)
80  end do
    ASSERT(irang.eq.nbarch)
!
!
!     4. -- ON MET A "ZERO" LES IRANG INUTILISES :
!     -------------------------------------------------
    do 100 irang = nbarch+1, nbrang
        ordr(irang)=iundf
        do 90 j = 1, nbpara
            nopara=nompar(j)
            call extrs3(nomsdr, nopara, irang, 'E', 1,&
                        type, iadou)
            if (type(1:1) .eq. 'I') then
                zi(iadou)=iundf
            else if (type(1:1).eq.'R') then
                zr(iadou)=rundf
            else if (type(1:1).eq.'C') then
                zc(iadou)=dcmplx(rundf,rundf)
            else if (type(1:3).eq.'K80') then
                zk80(iadou)=' '
            else if (type(1:3).eq.'K32') then
                zk32(iadou)=' '
            else if (type(1:3).eq.'K24') then
                zk24(iadou)=' '
            else if (type(1:3).eq.'K16') then
                zk16(iadou)=' '
            else if (type(1:2).eq.'K8') then
                zk8(iadou)=' '
            endif
90      continue
100  end do
!
!
!     5. -- IL FAUT RENOMMER LES CHAMPS POUR QU'ILS RESPECTENT
!           LA REGLE DE NOMMAGE DE RSUTCH.F :
!     ---------------------------------------------------------
    do 51 i = 1, nbnosy
        call jenuno(jexnum(nomsdr//'.DESC', i), nomsym)
        call jeveuo(jexnum(nomsdr//'.TACH', i), 'E', jtach)
        do 41 j = 1, nbarch
            iordr=ordr(j)
            nomch1=zk24(jtach-1+j)
            if (nomch1 .eq. ' ') goto 41
            call rsutch(nomsdr, nomsym, iordr, nomch2, .false._1)
            if (nomch1 .ne. nomch2) then
                call copisd('CHAMP', 'G', nomch1, nomch2)
                call detrsd('CHAMP', nomch1)
                zk24(jtach-1+j)=nomch2
            endif
41      continue
51  end do
!
!
!     6. -- IL FAUT ENCORE DETRUIRE LES SCORIES INUTILES :
!     ----------------------------------------------------
    call rsmena(nomsdr)
!
    call jedema()
end subroutine
