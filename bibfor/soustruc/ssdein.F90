subroutine ssdein(ul, ug, mail, nocas)
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
! ----------------------------------------------------------------------
!     BUT:
!      - CALCULER LE CHAMP DE DEPLACEMENT INTERNE A UNE SOUS-STRUCTURE
!        A PARTIR DU CHAMP DE DEPLACEMENT CONNU SUR SES NOEUDS EXTERNES
!
! IN_F,OU_J: UL : NOM DU CHAMP LOCAL A LA SOUS-STRUCTURE
! IN_F,IN_J: UG : NOM DU CHAMP GLOBAL (MODELE DE NIVEAU SUPERIEUR)
! IN_F     : MAIL : NOM DE LA (SUPER)MAILLE SUR LAQUELLE ON VEUT UL
! IN_F     : NOCAS: NOM DU CHARGEMENT CORRESPONDANT (EN PRINCIPE) A UG.
!                   (EVENTUELLEMENT : ' ')
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/ssrone.h"
#include "asterfort/ssvaro.h"
#include "asterfort/ssvau1.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: ul, ug, mail, nocas, mag, mal, nomgd, nomacr
    character(len=14) :: nul
    character(len=19) :: nug2, nul2
    real(kind=8) :: lambda(6, 6), angl(3), pgl(3, 3)
    aster_logical :: exil, exig
    character(len=8) :: rota, ch8(2)
    character(len=19) :: ug2, ul2
    character(len=24) :: valk(2)
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadesc, iadgg, iadgl, ialica
    integer :: ialich, iaphi0, iaphie
    integer :: iaprng, iaprnl, iarefe, iasupm, iavall, iavalp
    integer :: ibid, iblph, icmp, icog, icol
    integer :: ieqg, ieql, iiblph, ili, inoe, inog
    integer :: inol, iret, isma, j, lgblph, nblph
    integer :: nbnoet, ncmpmx, nddle, nddli, nddlt, nec, nlblph
    integer :: nueqg, nueql
    integer, pointer :: desm(:) => null()
    character(len=8), pointer :: vnomacr(:) => null()
    character(len=24), pointer :: refe(:) => null()
    real(kind=8), pointer :: para_r(:) => null()
    integer, pointer :: nueg(:) => null()
    integer, pointer :: nuel(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: conx(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ug2= ug
    ul2= ul
!
    call dismoi('NOM_MAILLA', ug, 'CHAM_NO', repk=mag)
    call jeveuo(ug2//'.REFE', 'L', vk24=refe)
    nug2=refe(2)(1:19)
    call dismoi('NOM_GD', ug, 'CHAM_NO', repk=nomgd)
    if (nomgd(1:6) .ne. 'DEPL_R') then
        call utmess('F', 'SOUSTRUC_43', sk=nomgd)
    endif
!
!
!     1- RECUPERATION DU NOM DU MACR_ELEM:
!     ------------------------------------
    call jeveuo(mag//'.NOMACR', 'L', vk8=vnomacr)
    call jenonu(jexnom(mag//'.SUPMAIL', mail), isma)
    if (isma .le. 0) then
        ch8(1)=mail
        ch8(2)=mag
        call utmess('F', 'SOUSTRUC_44', nk=2, valk=ch8)
    endif
    call jeveuo(jexnom(mag//'.SUPMAIL', mail), 'L', iasupm)
    nomacr= vnomacr(isma)
    nul= nomacr
    nul2=nul//'.NUME'
!
    call dismoi('NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', repk=mal)
    call jeveuo(nomacr//'.CONX', 'L', vi=conx)
    call jeveuo(nomacr//'.DESM', 'L', vi=desm)
    nbnoet= desm(2)+desm(8)+desm(9)
    nddle= desm(4)
    nddli= desm(5)
    nddlt= nddle+nddli
!                 '&&SSDEIN.VALP' EST UN VECTEUR DE TRAVAIL :
    call wkvect('&&SSDEIN.VALP', 'V V R', nddlt, iavalp)
!
    call jeveuo(ug2//'.VALE', 'L', vr=vale)
    call jeveuo(jexnum(nug2//'.PRNO', 1), 'L', iaprng)
    call jeveuo(nug2//'.NUEQ', 'L', vi=nueg)
    call jeveuo(nul2//'.NUEQ', 'L', vi=nuel)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
!
!
!     2- ALLOCATION DU CHAM_NO RESULTAT : UL
!     --------------------------------------
!     .DESC:
    call wkvect(ul2//'.DESC', 'G V I', 2, iadesc)
    call jeveuo(ug2//'.DESC', 'L', vi=desc)
    zi(iadesc-1+1)=desc(1)
    zi(iadesc-1+2)=1
    call jeecra(ul2//'.DESC', 'DOCU', ibid, 'CHNO')
!     .REFE:
    call wkvect(ul2//'.REFE', 'G V K24', 4, iarefe)
    zk24(iarefe-1+1)=mal
    zk24(iarefe-1+2)=nul//'.NUME'
!     .VALE:
    call wkvect(ul2//'.VALE', 'G V R', nddlt, iavall)
!
!
!     4- CALCUL DES VALEURS DE UL.VALE:
!     ---------------------------------
!
!     4-1- ON RECOPIE UG.VALE DANS Q_E:
!     ---------------------------------
    do inoe = 1, nbnoet
        inog=zi(iasupm-1+inoe)
        inol= conx(3*(inoe-1)+2)
        ili= conx(3*(inoe-1)+1)
!
        call jeveuo(jexnum(nul2//'.PRNO', ili), 'L', iaprnl)
!
        nueql = zi(iaprnl-1+ (inol-1)* (nec+2)+1)
        iadgl = iaprnl - 1 + (inol-1)* (nec+2)+3
        ieql=nuel(nueql)
        if (ieql .le. nddli) then
            call utmess('F', 'SOUSTRUC_45')
        endif
!
        nueqg = zi(iaprng-1+ (inog-1)* (nec+2)+1)
        iadgg = iaprng - 1 + (inog-1)* (nec+2)+3
!
        icol = 0
        icog = 0
        do 2 ,icmp = 1,ncmpmx
        exil= exisdg(zi(iadgl),icmp)
        exig= exisdg(zi(iadgg),icmp)
        if (exil) icol=icol+1
        if (exig) icog=icog+1
        if (exig .and. exil) then
            ieql= nuel(nueql-1+icol)
            ieqg= nueg(nueqg-1+icog)
            zr(iavall-1+ieql) = vale(ieqg)
        endif
  2     continue
    end do
!
!
!     4-2- ON CHANGE LE REPERE (ROTATION G->L ) : Q_E  --> Q_E :
!     ----------------------------------------------------------
!
    call ssrone(mag, isma, rota)
!
    if (rota(1:3) .eq. 'OUI') then
        call jeveuo(mag//'.PARA_R', 'L', vr=para_r)
        angl(1) = para_r(14*(isma-1)+4)
        angl(2) = para_r(14*(isma-1)+5)
        angl(3) = para_r(14*(isma-1)+6)
        call matrot(angl, pgl)
        do i = 1, 3
            do j = 1, 3
                lambda(i,j) = pgl(i,j)
                lambda(i,j+3) = 0.d0
                lambda(i+3,j) = 0.d0
                lambda(i+3,j+3) = pgl(i,j)
            end do
        end do
        call ssvaro(lambda, 'GL', .false._1, 'EXTE', nomacr,&
                    iavall, iavalp)
        do i = 1, nddle
            zr(iavall-1+nddli+i)= zr(iavalp-1+nddli+i)
        end do
        call jedetr('&&SSVARO.IINO')
    endif
!
!
!     4-3  Q_I= (K_II**-1)*F_I :
!     -------------------------
    if (nocas(1:1) .ne. ' ') then
        call jeexin(jexnom(nomacr//'.LICA', nocas), iret)
        if (iret .eq. 0) then
            valk(1) = nocas
            valk(2) = nomacr
            call utmess('A', 'SOUSTRUC_46', nk=2, valk=valk)
        else
            call jeveuo(jexnom(nomacr//'.LICA', nocas), 'L', ialica)
            call jeveuo(jexnom(nomacr//'.LICH', nocas), 'L', ialich)
!
            if (zk8(ialich-1+1)(1:3) .eq. 'NON') then
!
!           -- LE CHARGEMENT N'EST PAS "SUIVEUR" :
                if (rota(1:3) .eq. 'OUI') then
                    call ssvaro(lambda, 'GL', .false._1, 'TOUS', nomacr,&
                                ialica, iavalp)
                    call ssvau1(nomacr, iavalp, iavalp)
                    do i = 1, nddli
                        zr(iavall-1+i)=zr(iavalp-1+i)
                    end do
                else
                    do i = 1, nddli
                        zr(iavall-1+i)=zr(ialica-1+nddlt+i)
                    end do
                endif
!
            else if (zk8(ialich-1+1)(1:3).eq.'OUI') then
!
!           -- LE CHARGEMENT EST "SUIVEUR" :
                do i = 1, nddli
                    zr(iavall-1+i)=zr(ialica-1+nddlt+i)
                end do
            else
                call utmess('F', 'SOUSTRUC_47')
            endif
        endif
    endif
!
!
!
!     4-4  Q_I= Q_I + PHI_IE * Q_E :
!     ------------------------------
    call jelira(nomacr//'.PHI_IE', 'LONMAX', lgblph)
    call jelira(nomacr//'.PHI_IE', 'NMAXOC', nblph)
    nlblph=lgblph/nddli
!
    j=0
    do iblph = 1, nblph
        call jeveuo(jexnum(nomacr//'.PHI_IE', iblph), 'L', iaphi0)
        do iiblph = 1, nlblph
            j=j+1
            if (j .gt. nddle) goto 13
            iaphie=iaphi0+ (iiblph-1)*nddli
            do i = 1, nddli
                zr(iavall-1+i)=zr(iavall-1+i) - zr(iaphie-1+i)* zr(&
                iavall-1+nddli+j)
            end do
        end do
 13     continue
        call jelibe(jexnum(nomacr//'.PHI_IE', iblph))
    end do
!
    call jedetr('&&SSDEIN.VALP')
!
    call jedema()
end subroutine
