subroutine pronua(method, nuag1, nuag2)
    implicit none
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
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nuadrf.h"
#include "asterfort/nuainr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: method, nuag1, nuag2
!
!  BUT : PROJETER LES VALEURS DU NUAGE NUAG1 SUR LES POINTS
!        DU NUAGE NUAG2 SELON LA METHODE METHOD
!
! IN  METHOD   : METHODE D'INTERPOLATION: 'NUAGE_DEG_0' OU 'NUAGE_DEG_1'
! IN  NUAG1 (JXIN)    : SD NUAGE A PROJETER
! IN  NUAG2 (JXVAR)   : SD NUAGE A EVALUER
!
! VARIABLES LOCALES :
    integer :: inuai1,   inuax2
    integer :: iret, inual1, inual2, ip2, ic2, ip1, ic1
    integer :: nx1, nx2, np1, np2, gd1, gd2, nc1, nc2
    integer :: i1, i2, ii2, i
    real(kind=8) :: val2r
    character(len=19) :: nua1, nua2
    character(len=24) :: valk(2)
    character(len=8) :: nogd
    character(len=3) :: tysca
    logical :: ldref
    integer, pointer :: corresp(:) => null()
    real(kind=8), pointer :: dref(:) => null()
    real(kind=8), pointer :: nuax1(:) => null()
    real(kind=8), pointer :: nuav1(:) => null()
    real(kind=8), pointer :: nuav2(:) => null()
    integer, pointer :: nuai2(:) => null()
!
! DEB-------------------------------------------------------------------
    call jemarq()
    nua1 = nuag1
    nua2 = nuag2
    call jeveuo(nua1//'.NUAI', 'L', inuai1)
    call jeveuo(nua2//'.NUAI', 'L', vi=nuai2)
    call jeveuo(nua1//'.NUAX', 'L', vr=nuax1)
    call jeveuo(nua1//'.NUAV', 'L', vr=nuav1)
    call jeveuo(nua2//'.NUAX', 'L', inuax2)
    call jeveuo(nua2//'.NUAV', 'E', vr=nuav2)
!
    nx1 = zi(inuai1-1+2)
    nx2 = nuai2(2)
    if (nx1 .ne. nx2) then
        valk(1) = nua1
        valk(2) = nua2
        call utmess('F', 'UTILITAI3_89', nk=2, valk=valk)
    endif
    np1 = zi(inuai1-1+1)
    np2 = nuai2(1)
    gd1 = zi(inuai1-1+4)
    gd2 = nuai2(4)
    if (gd1 .ne. gd2) then
        valk(1) = nua1
        valk(2) = nua2
        call utmess('F', 'UTILITAI3_90', nk=2, valk=valk)
    endif
    call jenuno(jexnum('&CATA.GD.NOMGD', gd1), nogd)
    call dismoi('TYPE_SCA', nogd, 'GRANDEUR', repk=tysca)
!
    nc1 = zi(inuai1-1+3)
    nc2 = nuai2(3)
!
!
!     -- L'OBJET '&&PRONUA.DREF' DONNE LA DISTANCE**2 DE REFERENCE
!        A UTILISER POUR CHAQUE POINT DE NUAG2 :
!        -----------------------------------------------------
    AS_ALLOCATE(vr=dref, size=np2)
!
!
!     -- L'OBJET '&&PRONUA.CORRESP' ETABLIT LA CORRESPONDANCE
!        ENTRE LES NUMEROS DE CMPS DE NUAG2 ET CEUX DE NUAG1 :
!        -----------------------------------------------------
    AS_ALLOCATE(vi=corresp, size=nc2)
    do i2 = 1, nc2
        ii2=nuai2(5+i2)
        i1=indiis(zi(inuai1-1+6),ii2,1,nc1)
        if (i1 .eq. 0) then
            call utmess('F', 'UTILITAI3_91', sk=nua1)
        else
            corresp(i2) = i1
        endif
    end do
!
!
!     SI LES OBJETS .NUAL N'EXISTENT PAS, ON LES CREE :
!     -------------------------------------------------
    call jeexin(nua1//'.NUAL', iret)
    if (iret .eq. 0) then
        call wkvect(nua1//'.NUAL', 'V V L', nc1*np1, inual1)
        do i = 1, nc1*np1
            zl(inual1-1+i)=.true.
        end do
    else
        call jeveuo(nua1//'.NUAL', 'L', inual1)
    endif
!
    call jeexin(nua2//'.NUAL', iret)
    if (iret .eq. 0) then
        call wkvect(nua2//'.NUAL', 'V V L', nc2*np2, inual2)
        do i = 1, nc2*np2
            zl(inual2-1+i)=.true.
        end do
    else
        call jeveuo(nua2//'.NUAL', 'L', inual2)
    endif
!
!
!     SI TOUS LES POINTS DE NUAG1 PORTENT LES MEMES CMPS
!     ON POURRA NE CALCULER L'OBJET .DREF QU'UNE SEULE FOIS
!     -------------------------------------------------
    ldref=.true.
    do ip1 = 2, np1
        do ic1 = 1, nc1
            if (zl(inual1-1+(ip1-1)*nc1+ic1) .neqv. zl(inual1-1+(ip1-2) *nc1+ic1)) then
                ldref=.false.
                goto 73
            endif
        end do
    end do
 73 continue
!
!     BOUCLE SUR LES CMPS DE NUAG2 :
!     ------------------------------
    do ic2 = 1, nc2
        ic1 = corresp(ic2)
!
!       CALCUL EVENTUEL DES DISTANCES DE REFERENCE :
!       --------------------------------------------
        if ((ic2.eq.1) .or. (.not.ldref)) call nuadrf(nua1, nua2, ic1, ic2,dref)
!
!       BOUCLE SUR LES POINTS DU NUAGE NUAG2 :
!       --------------------------------------
!
        if (tysca .eq. 'R') then
!       ----------------------
            do ip2 = 1, np2
                if (zl(inual2-1+ (ip2-1)*nc2+ic2)) then
                    call nuainr(method, np1, nx1, nc1, ic1,&
                                nuax1, zl(inual1), nuav1, zr(inuax2-1+ (ip2-1)*nx2+ 1),&
                                dref(ip2), val2r)
                    nuav2((ip2-1)*nc2+ic2) = val2r
                else
                    nuav2((ip2-1)*nc2+ic2) = 0.d0
                endif
            end do
!
        else
            call utmess('F', 'UTILITAI3_93')
        endif
!
    end do
!
!
!     MENAGE :
!     --------
    AS_DEALLOCATE(vi=corresp)
    AS_DEALLOCATE(vr=dref)
    call jedema()
end subroutine
