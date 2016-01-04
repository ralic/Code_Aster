subroutine exchml(imodat, iparg)

use calcul_module, only : ca_iachii_, ca_iachik_, ca_iachin_, ca_iachlo_,&
     ca_iamloc_, ca_iawlo2_, ca_igr_, ca_iichin_,&
     ca_ilchlo_, ca_ilmloc_, ca_nbelgr_, ca_nbgr_, ca_ncmpmx_, ca_nec_, ca_typegd_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chloet.h"
#include "asterfort/exisdg.h"
#include "asterfort/jacopo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"

    integer :: iparg, imodat
!----------------------------------------------------------------------
!     entrees:
!        imodat : mode local attendu
!        iparg  : numero du parametre dans l'option
!----------------------------------------------------------------------
    integer :: jceld, mode, debgr2, lggre2, iaux1
    integer :: itypl1, modlo1, nbpoi1, lgcata
    integer :: itypl2, modlo2, nbpoi2
    integer ::  iel
    integer :: ncmp1, ncmp2
    integer ::  iret, debugr, lggrel
    integer :: jec, ncmp, jad1, jad2, jel, ipt2, k, ipt1, jparal
    integer :: nbpoi, icmp1, icmp2, kcmp, ipt
    aster_logical :: etendu, lparal, lverec
    character(len=8) :: tych, cas
!-------------------------------------------------------------------

!   parallele or not ?
!   -------------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif

    tych=zk8(ca_iachik_-1+2*(ca_iichin_-1)+1)
    ASSERT(tych(1:4).eq.'CHML')

    jceld=zi(ca_iachii_-1+11*(ca_iichin_-1)+4)
    lggre2=zi(jceld-1+zi(jceld-1+4+ca_igr_)+4)
    debgr2=zi(jceld-1+zi(jceld-1+4+ca_igr_)+8)

    mode=zi(jceld-1+zi(jceld-1+4+ca_igr_)+2)

    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
    lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)


!   -- si mode=0 : il faut mettre champ_loc.exis a .false.
    if (mode .eq. 0) then
        do k = 1, lggrel
            zl(ca_ilchlo_-1+debugr-1+k)=.false.
        enddo
        goto 999
    endif


!   -- si le champ a le mode attendu : on recopie
!   ----------------------------------------------------
    if (mode .eq. imodat) then
        call jacopo(lggrel, ca_typegd_, ca_iachin_-1+debgr2, ca_iachlo_+debugr-1)
        goto 998
    endif


!   -- si le champ n'a pas le mode attendu ...
!   ----------------------------------------------------
    call chloet(iparg, etendu, jceld)
    if (etendu) then
        call utmess('F', 'CALCUL_8')
    endif


    modlo1=ca_iamloc_-1+zi(ca_ilmloc_-1+mode)
    modlo2=ca_iamloc_-1+zi(ca_ilmloc_-1+imodat)
    itypl1=zi(modlo1-1+1)
    itypl2=zi(modlo2-1+1)
    ASSERT(itypl1.le.3)
    ASSERT(itypl2.le.3)
    nbpoi1=zi(modlo1-1+4)
    nbpoi2=zi(modlo2-1+4)

    ncmp1=lggre2/(nbpoi1*ca_nbelgr_)
    ncmp2=lgcata/nbpoi2

!   -- on verifie que les points ne sont pas "diff__" :
    ASSERT(nbpoi1.lt.10000)
    ASSERT(nbpoi2.lt.10000)


!   -- Dans quel cas de figure se trouve-t-on ?
!   --------------------------------------------
    lverec=.true.
    if (nbpoi1 .eq. nbpoi2) then
        if (ncmp1 .eq. ncmp2) then
!            -- le cas "copie" est bizarre : il s'agit de 2 modes locaux
!               de meme contenu mais de noms differents.
!               faut-il l'interdire ?
            cas='COPIE'
            ncmp=ncmp1
!           -- quelques verifications :
            ASSERT(itypl1.eq.itypl2)
!           -- pour les champs elga, on verifie que c'est la meme famille
            if (itypl1 .eq. 3) then
                ASSERT(zi(modlo1+4+ca_nec_).eq.zi(modlo2+4+ca_nec_))
            endif
        else
            cas='TRICMP'
            lverec=.false.
        endif
    else
        ASSERT(ncmp1.eq.ncmp2)
        ncmp=ncmp1

        if (nbpoi1 .eq. 1) then
            cas='EXPAND'
        else if (nbpoi2.eq.1) then
            cas='MOYENN'
        else
            ASSERT(.false.)
        endif
    endif

    if (lverec) then
!       -- on verifie que les cmps sont les memes:
!          (sinon il faudrait trier ... => a faire (trigd) )
        do jec = 1, ca_nec_
            ASSERT(zi(modlo1-1+4+jec).eq.zi(modlo2-1+4+jec))
        enddo
    endif



!   -- cas "expand" ou "copie":
!   ---------------------------
    if (cas .eq. 'EXPAND' .or. cas .eq. 'COPIE') then
        do jel = 1, ca_nbelgr_
            if (lparal) then
                if (.not.zl(jparal-1+jel)) cycle
            endif
            if (cas .eq. 'EXPAND') then
                jad1=ca_iachin_-1+debgr2+(jel-1)*ncmp
                do ipt2 = 1, nbpoi2
                    jad2=ca_iachlo_+debugr-1+((jel-1)*nbpoi2+ipt2-1)*ncmp
                    call jacopo(ncmp, ca_typegd_, jad1, jad2)
                enddo
            else if (cas.eq.'COPIE') then
                ASSERT(nbpoi1.eq.nbpoi2)
                jad1=ca_iachin_-1+debgr2+(jel-1)*ncmp*nbpoi1
                jad2=ca_iachlo_-1+debugr+(jel-1)*ncmp*nbpoi1
                call jacopo(ncmp*nbpoi1, ca_typegd_, jad1, jad2)
            endif
        enddo


!   -- cas "tricmp":
!   ---------------------------
    else if (cas.eq.'TRICMP') then
        nbpoi=nbpoi1
        icmp1=0
        icmp2=0
        do kcmp = 1, ca_ncmpmx_
            if (exisdg(zi(modlo2-1+5),kcmp)) then
                icmp2=icmp2+1
                if (exisdg(zi(modlo1-1+5),kcmp)) then
                    icmp1=icmp1+1
                else
!                   -- a faire ... (gestion de zl)
                    ASSERT(.false.)
                endif
            else
                cycle
            endif
            ASSERT(icmp1.ge.1 .and. icmp1.le.ncmp1)
            ASSERT(icmp2.ge.1 .and. icmp2.le.ncmp2)
            do jel = 1, ca_nbelgr_
                if (lparal) then
                    if (.not.zl(jparal-1+jel)) cycle
                endif

                do ipt = 1, nbpoi
                    jad1=ca_iachin_+debgr2-1+((jel-1)*nbpoi+ipt-1)*ncmp1
                    jad2=ca_iachlo_+debugr-1+((jel-1)*nbpoi+ipt-1)*ncmp2
                    jad1=jad1-1+icmp1
                    jad2=jad2-1+icmp2
                    call jacopo(1, ca_typegd_, jad1, jad2)
                enddo
            enddo
        enddo


!   -- cas "moyenn" :
!   ------------------------
    else if (nbpoi2.eq.1) then

        if (ca_typegd_ .eq. 'R') then
            if (lparal) then
                do iel = 1, ca_nbelgr_
                    if (zl(jparal-1+iel)) then
                        iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
                        do k = 1, ncmp
                            zr(iaux1-1+k)=0.d0
                        enddo
                    endif
                enddo
            else
                do k = 1, ca_nbelgr_*ncmp
                    zr(ca_iachlo_+debugr-1-1+k)=0.d0
                enddo
            endif
        else if (ca_typegd_.eq.'C') then
            if (lparal) then
                do iel = 1, ca_nbelgr_
                    if (zl(jparal-1+iel)) then
                        iaux1=ca_iachlo_+debugr-1+(iel-1)*ncmp
                        do k = 1, ncmp
                            zc(iaux1-1+k)=(0.d0,0.d0)
                        enddo
                    endif
                enddo
            else
                do k = 1, ca_nbelgr_*ncmp
                    zc(ca_iachlo_+debugr-1-1+k)=(0.d0,0.d0)
                enddo
            endif
        else
            ASSERT(.false.)
        endif

        do jel = 1, ca_nbelgr_
            if (lparal) then
                if (.not.zl(jparal-1+jel)) cycle
            endif
            jad2=ca_iachlo_+debugr-1+(jel-1)*ncmp
            do ipt1 = 1, nbpoi1
                jad1=ca_iachin_-1+debgr2+((jel-1)*nbpoi1+ipt1-1)*ncmp
                do k = 0, ncmp-1
                    if (ca_typegd_ .eq. 'R') then
                        zr(jad2+k)=zr(jad2+k)+zr(jad1+k)/dble(nbpoi1)
                    else if (ca_typegd_.eq.'C') then
                        zc(jad2+k)=zc(jad2+k)+zc(jad1+k)/dble(nbpoi1)
                    endif
                enddo
            enddo
        enddo


!   -- autres cas pas encore programmes :
    else
        ASSERT(.false.)
    endif


998 continue
    do k = 1, lggrel
        zl(ca_ilchlo_-1+debugr-1+k)=.true.
    enddo

999 continue
end subroutine
