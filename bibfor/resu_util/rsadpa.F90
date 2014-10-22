subroutine rsadpa(nomsd, cel, npara, lpara, iordr,&
                  itype, tjv, ttyp, sjv, styp)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrs3.h"
#include "asterfort/iunifi.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeimpo.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsutrg.h"
#include "asterfort/utmess.h"
!
    integer, intent(in) :: npara, iordr, itype
    integer, intent(out), optional :: sjv, tjv(*)
    character(len=1), intent(in) :: cel
    character(len=*), intent(in) :: nomsd, lpara(*)
    character(len=*), intent(out), optional :: styp, ttyp(*)
! ----------------------------------------------------------------------
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
! person_in_charge: nicolas.sellenet at edf.fr
!
!      RECUPERATION DES ADRESSES JEVEUX DES PARAMETRES DE CALCUL
!      (OU DES VARIABLES D'ACCES)
!      D'UN RESULTAT-COMPOSE POUR LE NUMERO D'ORDRE : IORDR
!      ET POUR LA LISTE DE VARIABLES DE NOMS SYMBOLIQUES LPARA.
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT".
! IN  : CEL    : CONDITION D'ACCES AUX PARAMETRES :
!                    'L' : LECTURE, 'E' : ECRITURE.
! IN  : NPARA  : NOMBRE DE PARAMETRES CHERCHES.
! IN  : LPARA  : LISTE DES NOMS SYMBOLIQUES DES PARAMETRES.
! IN  : IORDR  : NUMERO D'ORDRE SOUHAITE.
! IN  : ITYPE  : CODE INDIQUANT QUE L'ON DESIRE LE TYPE
!                     = 0  PAS DE TYPE
!                    /= 0  ON FOURNIT LE TYPE
! OUT : LJEVEU : LISTE DES ADRESSES JEVEUX DANS ZI,ZR,...
! OUT : CTYPE  : CODE DU TYPE
!               R REAL,I INTEGER,C COMPLEXE,K8 K16 K24 K32 K80 CHARACTER
!-----------------------------------------------------------------------
! REMARQUE : CETTE ROUTINE NE FAIT PAS JEMARQ/JEDEMA POUR NE PAS
!            INVALIDER LJEVEU
!-----------------------------------------------------------------------
    integer :: nbordr, nrang, jordr, i, ipara, irang, ifr
    integer :: vali(2), ljeveu(npara)
    character(len=3) :: ctype(npara)
    character(len=24) :: valk(3)
    character(len=16) :: param, k16b
    character(len=19) :: noms2
! ----------------------------------------------------------------------
!
    noms2 = nomsd
!
!     --- RECUPERATION DU NUMERO DE RANGEMENT ---
    call rsutrg(nomsd, iordr, irang, nrang)
!
    if (cel .eq. 'L') then
        if (irang .eq. 0) then
            valk (1) = nomsd
            vali (1) = iordr
            call utmess('F', 'UTILITAI6_77', sk=valk(1), si=vali(1))
        endif
    else
        if (irang .eq. 0) then
            call jelira(noms2//'.ORDR', 'LONMAX', nbordr)
            nrang = nrang + 1
            if (nrang .gt. nbordr) then
                valk (1) = nomsd
                vali (1) = iordr
                vali (2) = nbordr
                call utmess('F', 'UTILITAI6_78', sk=valk(1), ni=2, vali=vali)
            endif
            call jeecra(noms2//'.ORDR', 'LONUTI', nrang)
            call jeveuo(noms2//'.ORDR', 'E', jordr)
            if (nrang .gt. 1) then
                ASSERT(zi(jordr+nrang-2).lt.iordr)
            endif
            zi(jordr+nrang-1) = iordr
            irang = nrang
        endif
    endif
!
    call jelira(jexnum(noms2//'.TACH', 1), 'LONMAX', nbordr)
    if (irang .gt. nbordr) then
        valk (1) = nomsd
        vali (1) = irang
        vali (2) = nbordr
        call utmess('F', 'UTILITAI6_79', sk=valk(1), ni=2, vali=vali)
    endif
!
    do i = 1, npara
        param = lpara(i)
        call jenonu(jexnom(noms2//'.NOVA', param), ipara)
        if (ipara .eq. 0) then
            ifr = iunifi('RESULTAT')
            call dismoi('TYPE_RESU', nomsd, 'RESULTAT', repk=k16b)
            call jeimpo(ifr, noms2//'.NOVA', ' ')
            valk (1) = nomsd
            valk (2) = param
            valk (3) = k16b
            call utmess('F', 'UTILITAI6_80', nk=3, valk=valk)
        endif
!
        call extrs3(noms2, param, irang, cel, itype,&
                    ctype(i), ljeveu(i))
!
    end do
!
    ASSERT(EXCLUS2(tjv,sjv))
    if (present(tjv)) then
        do i = 1, npara
            tjv(i)=ljeveu(i)
        end do
    else if (present(sjv)) then
        sjv=ljeveu(1)
    endif
    ASSERT(EXCLUS2(ttyp,styp))
    if (itype .gt. 0) then
        if (present(ttyp)) then
            do i = 1, npara
                ttyp(i)=ctype(i)
            end do
        else if (present(styp)) then
            styp=ctype(1)
        endif
    endif
!
end subroutine
