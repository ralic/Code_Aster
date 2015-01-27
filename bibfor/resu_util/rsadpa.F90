subroutine rsadpa(nomsd, cel, npara, lpara, iordr,&
                  itype, tjv, ttyp, sjv, styp, istop)
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
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
!
    integer, intent(in) :: npara, iordr, itype
    integer, intent(out), optional :: sjv, tjv(*)
    character(len=1), intent(in) :: cel
    character(len=*), intent(in) :: nomsd, lpara(*)
    character(len=*), intent(out), optional :: styp, ttyp(*)
    integer, intent(in), optional :: istop
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
! Recuperation des adresses jeveux des parametres d'une sd_resultat
! pour le numero d'ordre : iordr
! et pour la liste des noms de parametres lpara.
! ----------------------------------------------------------------------
! in  : nomsd  : nom de la structure "resultat".
! in  : cel    : condition d'acces aux parametres :
!                    'L' : lecture, 'E' : ecriture.
! in  : npara  : nombre de parametres cherches.
! in  : lpara  : liste des noms des parametres.
! in  : iordr  : numero d'ordre souhaite.
! in  : itype  : code indiquant que l'on desire le type
!                     = 0  pas de type
!                    /= 0  on retourne le type (dans styp ou ttyp)
! out : / tjv : liste des adresses jeveux dans zi,zr,...
!       / sjv : 1 adresse jeveux dans zi,zr,... (si npara=1)
! out : / ttyp  : liste des codes du type des parametres
!       / styp  : le code du type du parametre (si npara=1)
!        code :   R real,I integer,C complexe,K8 K16 K24 K32 K80 character
! in  : istop  : comportement souhaite en cas de lecture d'une valeur "undef"
!           / 1  on emet une erreur <F> (c'est le defaut)
!           / 0  on ne s'arrete pas et on retourne une adresse vers une valeur "undef"
!-----------------------------------------------------------------------
! remarque : cette routine ne fait pas jemarq/jedema pour ne pas
!            invalider ljeveu
!-----------------------------------------------------------------------
    integer :: nbordr, nrang, jordr, i, ipara, irang, ifr, iundef
    integer :: vali(2), ljeveu(npara), istop2
    real(kind=8) :: rundef
    complex(kind=8) :: cundef
    character(len=3) :: ctype(npara)
    character(len=24) :: valk(3)
    character(len=16) :: param, k16b
    character(len=19) :: noms2
! ----------------------------------------------------------------------

    noms2 = nomsd
    iundef = isnnem()
    rundef = r8vide()
    cundef = dcmplx(rundef,rundef)

    if (present(istop)) then
        istop2=istop
    else
        istop2=1
    endif


!   --- recuperation du numero de rangement ---
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

    call jelira(jexnum(noms2//'.TACH', 1), 'LONMAX', nbordr)
    if (irang .gt. nbordr) then
        valk (1) = nomsd
        vali (1) = irang
        vali (2) = nbordr
        call utmess('F', 'UTILITAI6_79', sk=valk(1), ni=2, vali=vali)
    endif

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

        call extrs3(noms2, param, irang, cel, 1,&
                    ctype(i), ljeveu(i))


!       -- on verifie que les parametres accedes en lecture n'ont pas
!          une valeur "undef" :
!       --------------------------------------------------------------
        if (cel .eq. 'L'.and. istop2.eq.1 ) then
           if (ctype(i).eq.'R') then
               ASSERT(zr(ljeveu(i)).ne.rundef)
           elseif (ctype(i).eq.'C') then
               ASSERT(zc(ljeveu(i)).ne.cundef)
           elseif (ctype(i).eq.'I') then
               ASSERT(zi(ljeveu(i)).ne.iundef)
           endif
        endif

    end do

    ASSERT(EXCLUS2(tjv,sjv))
    if (present(tjv)) then
        do i = 1, npara
            tjv(i)=ljeveu(i)
        end do
    else if (present(sjv)) then
        sjv=ljeveu(1)
    else
        ASSERT(.false.)
    endif

    ASSERT(EXCLUS2(ttyp,styp))
    if (itype .ne. 0) then
        if (present(ttyp)) then
            do i = 1, npara
                ttyp(i)=ctype(i)
            end do
        else if (present(styp)) then
            styp=ctype(1)
        else
            ASSERT(.false.)
        endif
    endif

end subroutine
