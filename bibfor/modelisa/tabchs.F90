subroutine tabchs(tabin, typchs, base, nomgd, ma,&
                  chs)
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
!     TRAITEMENT DE COMMANDE:   CREA_CHAMP / OPTION: 'EXTR' / TABLE
!
!     CREATION D'UN CHAMP SIMPLE A PARTIR D'UNE TABLE
!
!
!     IN : TABIN  : NOM DE LA TABLE
!     IN : TYPCHS : TYPE DU CHAMP SIMPLE (NOEU/ELEM/ELNO/ELGA)
!     IN : BASE   : BASE DE CREATION (G/V)
!     IN : NOMGD  : NOM DE LA GRANDEUR
!     IN : MA     : NOM DU MAILLAGE
!     IN/JXOUT : CHS: NOM DU CHAMP SIMPLE A CREER
!
    implicit none
!
!     ------------------------------------------------------------------
! 0.1. ==> ARGUMENT
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexve.h"
#include "asterfort/utmess.h"
#include "asterfort/verigd.h"
#include "asterfort/verima.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=1) :: base
    character(len=8) :: nomgd, ma
    character(len=16) :: typchs
    character(len=19) :: chs, tabin
!
!
! 0.2. ==> COMMUNS
!
!
!      ==> VARIABLES LOCALES
    integer ::   ncmp,  jcnsl,  i
    integer :: vali(2), nblig, isp
    integer :: nbval
    integer :: nuno, numa, nbma, jcesd, nbssp
    integer :: jcesl,  jcesc, iad
    integer :: nbcol, nbno, ksp, kpt, jcon1, jcon2
    integer :: jcolma, jcolno, jcolpt, jcolsp, ipt
    integer :: icmp, ili, iret,  jobj2, jobj3
    character(len=8) :: nono, tsca, noma
    character(len=24) :: objlg, objr, objtmp
    character(len=24) :: valk(3)
    logical(kind=1) :: lmail, lnoeu, lpoin, lspoin
    logical :: lcond
    character(len=24), pointer :: ncmp1(:) => null()
    integer, pointer :: ncmp2(:) => null()
    integer, pointer :: pg_tot(:) => null()
    integer, pointer :: sp_tot(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
    integer, pointer :: tbnp(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
! ---------------------------------------------------------------------
!
!
    call jemarq()
!
    call jeveuo(tabin//'.TBNP', 'L', vi=tbnp)
    nbcol=tbnp(1)
    nblig=tbnp(2)
    call jeveuo(tabin//'.TBLP', 'L', vk24=tblp)
!
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R')
    call jeveuo(ma//'.CONNEX', 'L', jcon1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jcon2)
!
!
!     1. VERIFICATION DES PARAMETRES DE LA TABLE :
!     --------------------------------------------
    call tbexip(tabin, 'MAILLE', lmail, tsca)
    call tbexip(tabin, 'NOEUD', lnoeu, tsca)
    call tbexip(tabin, 'POINT', lpoin, tsca)
    call tbexip(tabin, 'SOUS_POINT', lspoin, tsca)
!
    valk(1)=tabin(1:8)
    valk(2)=typchs
!
!     PRESENCE DU PARAMETRE MAILLE
    if (typchs .eq. 'EL' .and. .not.lmail) then
        call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS NOEU :
    if (typchs .eq. 'NOEU') then
        if (.not.lnoeu) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
        if (lmail .or. lpoin .or. lspoin) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELGA :
    if (typchs .eq. 'ELGA') then
        if (.not.lpoin .or. lnoeu) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELNO :
    if (typchs .eq. 'ELNO') then
        if (.not.lpoin .and. .not.lnoeu) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
        if (lpoin .and. lnoeu) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELEM :
    if (typchs .eq. 'ELEM') then
        if (lpoin .or. lnoeu) then
            call utmess('F', 'MODELISA9_1', nk=2, valk=valk)
        endif
    endif
!
!
!     2. RECUPERATION DES COLONNES DE LA TABLE :
!     -------------------------------------------
!     -- 2.1 COLONNE 'NOEUD' :
    if (lnoeu) then
!        ON VERIFIE QUE LES NOEUDS FOURNIS DANS LA TABLE
!        APPARTIENNENT AU MAILLAGE
        objtmp='&&TABCHS.NOEUD'
        call tbexve(tabin, 'NOEUD', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolno)
        call verima(ma, zk8(jcolno), nbval, 'NOEUD')
    endif
!
!     -- 2.2 COLONNE 'MAILLE' :
    if (lmail) then
!        ON VERIFIE QUE LES MAILLES FOURNIES DANS LA TABLE
!        APPARTIENNENT AU MAILLAGE
        objtmp='&&TABCHS.MAILLE'
        call tbexve(tabin, 'MAILLE', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolma)
        call verima(ma, zk8(jcolma), nbval, 'MAILLE')
    endif
!
!     -- 2.3 COLONNE 'POINT' :
    if (lpoin) then
        objtmp='&&TABCHS.POINT'
        call tbexve(tabin, 'POINT', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolpt)
    endif
!
!     -- 2.4 COLONNE 'SOUS_POINT' :
    if (lspoin) then
        objtmp='&&TABCHS.SPOINT'
        call tbexve(tabin, 'SOUS_POINT', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolsp)
    endif
!
!
!     -- ON REPERE LES COLONNES QUI CORRESPONDENT AUX CMPS.
!        CE SONT CELLES QUI NE SONT PAS : MAILLE, NOEUD, ...
    AS_ALLOCATE(vk24=ncmp1, size=nbcol)
    AS_ALLOCATE(vi=ncmp2, size=nbcol)
    ncmp=0
    do i = 1, nbcol
        if (tblp(1+4*(i-1)) .ne. 'MAILLE' .and. tblp(1+4*(i-1)) .ne. 'NOEUD' .and.&
            tblp(1+4*(i-1)) .ne. 'POINT' .and. tblp(1+4*(i-1)) .ne. 'SOUS_POINT') then
            ncmp=ncmp+1
            ncmp1(ncmp)=tblp(1+4*(i-1))
            ncmp2(ncmp)=i
        endif
    end do
!
!     ON VERIFIE QUE LE NOM ET LE TYPE DES COMPOSANTES
!        DE LA TABLE CORRESPONDENT A LA GRANDEUR LUE
    call verigd(nomgd, ncmp1, ncmp, iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA9_2', nk=ncmp, valk=ncmp1)
    endif
!
!
!
    if (typchs .eq. 'NOEU') then
!     ------------------------------------
!
! ---    CREATION DU CHAM_NO_S
        call cnscre(ma, nomgd, ncmp, ncmp1, base,&
                    chs)
!
! ---    REMPLISSAGE DU CHAM_S
        call jeveuo(chs//'.CNSV', 'E', vr=cnsv)
        call jeveuo(chs//'.CNSL', 'E', jcnsl)
!
        do icmp = 1, ncmp
            objlg=tblp(1+4*(ncmp2(icmp)-1)+3)
            call jeveuo(objlg, 'L', jobj2)
            objr=tblp(1+4*(ncmp2(icmp)-1)+2)
            call jeveuo(objr, 'L', jobj3)
            do ili = 1, nblig
                if (zi(jobj2+ili-1) .eq. 1) then
                    nono=zk8(jcolno+ili-1)
                    call jenonu(jexnom(ma//'.NOMNOE', nono), nuno)
                    ASSERT(nuno.gt.0)
                    cnsv(1+(nuno-1)*ncmp+icmp-1)=zr(jobj3+ili-1)
                    zl(jcnsl+(nuno-1)*ncmp+icmp-1)=.true.
                endif
            end do
        end do
!
!
!
    else if (typchs(1:2).eq.'EL') then
!     ------------------------------------
!
        if (typchs .eq. 'ELNO') then
!          POUR LES CHAMPS ELNO :
!           - SI LNOEU, ON CALCULE '&&TABCHS.POINT'
!           - ON VERIFIE QUE LE NUMERO DE POINT EST POSSIBLE
            if (lnoeu) then
                lcond=.not.lpoin
                ASSERT(lcond)
                call wkvect('&&TABCHS.POINT', 'V V I', nblig, jcolpt)
            endif
            do ili = 1, nblig
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                nbno=zi(jcon2-1+numa+1)-zi(jcon2-1+numa)
                if (lpoin) then
                    ipt=zi(jcolpt-1+ili)
                else
                    lcond=lnoeu
                    ASSERT(lcond)
                    nono=zk8(jcolno-1+ili)
                    call jenonu(jexnom(ma//'.NOMNOE', nono), nuno)
                    ipt=indiis(zi(jcon1-1+zi(jcon2-1+numa)),nuno,1,&
                    nbno)
                    zi(jcolpt-1+ili)=ipt
                endif
                if (ipt .eq. 0 .or. ipt .gt. nbno) then
                    valk(1)=tabin
                    valk(2)=noma
                    valk(3)=nono
                    vali(1)=ipt
                    vali(2)=nbno
                    call utmess('F', 'MODELISA9_5', nk=3, valk=valk, ni=2,&
                                vali=vali)
                endif
            end do
        endif
!
!
!       CALCUL DU NOMBRE DE SOUS_POINT PAR ELEMENT (&&TABCHS.SP_TOT):
!       CALCUL DE NBSSP : MAX DU NOMBRE DE SOUS_POINT
        if (lspoin) then
            AS_ALLOCATE(vi=sp_tot, size=nbma)
            nbssp=1
            do ili = 1, nblig
                ksp=zi(jcolsp+ili-1)
                ASSERT(ksp.gt.0)
                nbssp=max(nbssp,ksp)
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                ASSERT(numa.gt.0)
                sp_tot(numa)=max(sp_tot(numa),ksp)
            end do
        else
            nbssp=1
        endif
!
!
!       CALCUL DU NOMBRE DE POINTS DE GAUSS PAR ELEMENT
!       (&&TABCHS.PG_TOT):
        if (typchs .eq. 'ELGA') then
            AS_ALLOCATE(vi=pg_tot, size=nbma)
            do ili = 1, nblig
                kpt=zi(jcolpt+ili-1)
                ASSERT(kpt.gt.0)
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                pg_tot(numa)=max(pg_tot(numa),kpt)
            end do
        endif
!
!
!       CREATION DU CHAM_ELEM_S VIERGE :
        if (nbssp .eq. 1) then
            if (typchs .eq. 'ELNO' .or. typchs .eq. 'ELEM') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, ncmp1, [-1], [-1], [-ncmp])
            else if (typchs.eq.'ELGA') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, ncmp1, pg_tot, [-1], [-ncmp])
            endif
        else
            if (typchs .eq. 'ELNO' .or. typchs .eq. 'ELEM') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, ncmp1, [-1], sp_tot, [-ncmp])
            else if (typchs.eq.'ELGA') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, ncmp1, pg_tot, sp_tot, [-ncmp])
            endif
        endif
!
!
! ---   REMPLISSAGE DU CHAM_S
        call jeveuo(chs//'.CESD', 'L', jcesd)
        call jeveuo(chs//'.CESV', 'E', vr=cesv)
        call jeveuo(chs//'.CESL', 'E', jcesl)
        call jeveuo(chs//'.CESC', 'L', jcesc)
!
!
        do icmp = 1, ncmp
            objlg=tblp(1+4*(ncmp2(icmp)-1)+3)
            call jeveuo(objlg, 'L', jobj2)
            objr=tblp(1+4*(ncmp2(icmp)-1)+2)
            call jeveuo(objr, 'L', jobj3)
!
            do ili = 1, nblig
                if (zi(jobj2+ili-1) .eq. 0) goto 70
!
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
!
                ipt=1
                if (lpoin) ipt=zi(jcolpt+ili-1)
!
                isp=1
                if (lspoin) isp=zi(jcolsp+ili-1)
!
                nono=' '
                if (lnoeu) then
                    nono=zk8(jcolno+ili-1)
                    ipt=zi(jcolpt+ili-1)
                endif
!
                call cesexi('S', jcesd, jcesl, numa, ipt,&
                            isp, icmp, iad)
                ASSERT(iad.ne.0)
                if (iad .lt. 0) then
                    iad=-iad
                else
                    valk(1)=tabin(1:8)
                    valk(2)=noma
                    valk(3)=nono
                    vali(1)=ipt
                    vali(2)=isp
                    call utmess('F', 'MODELISA9_6', nk=3, valk=valk, ni=2,&
                                vali=vali)
                endif
                cesv(iad)=zr(jobj3+ili-1)
                zl(jcesl+iad-1)=.true.
 70             continue
            end do
        end do
    endif
!
    call jedetr('&&TABCHS.MAILLE')
    call jedetr('&&TABCHS.NOEUD')
    call jedetr('&&TABCHS.POINT')
    call jedetr('&&TABCHS.SPOINT')
    AS_DEALLOCATE(vk24=ncmp1)
    AS_DEALLOCATE(vi=ncmp2)
    AS_DEALLOCATE(vi=pg_tot)
    AS_DEALLOCATE(vi=sp_tot)
!
    call jedema()
end subroutine
