subroutine gcharf(ichar, fonc1, char1, fonc2, char2,&
                  charg)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gchfus.h"
#include "asterfort/gchs2f.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: char1, char2, charg
    integer :: ichar
    aster_logical :: fonc1, fonc2
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT : EFFECTUE LA FUSION DE 2 CHARGES DE MEME TYPE
!           (ROUTINE SPECIFIQUE A L'OPERATEUR CALC_G,
!            APPELEE PAR GCHARG)
!
!     IN :    FONC     :  = .TRUE. SI LE CHARGEMENT EST 'FONCTION'
!                         = .FALSE. SI LE CHARGEMENT EST 'SCALAIRE'
!             CHAR1    :  CHARGE 1 (ASSOCIEE A FONC1)
!             CHAR2    :  CHARGE 2(ASSOCIEE A FONC2)
!             ICHAR    :  OCCURENCE DU CHARGEMENT DANS GCHARG
!     IN/OUT  CHARG    :  CHARGE 1  +  CHARGE 2
! ======================================================================
! ----------------------------------------------------------------------
!
    integer :: jzcar1, nbma, p1, p2, nmazo, jdes
    integer :: nbzo1, ima, izo, numa, nbzo2, jzcar2, izo1, izo2, ii, nbzo, nuzo1
    integer :: nuzo2, jzcar, jma, jval, ncmpmx, jk24, ilim, jmazo, jnumz, nuzo
    integer :: k, jval1, icmp, jval2, kk
    character(len=8) :: ma, k8b, val1, val2, nomfct, noms2f
    character(len=19) :: charg1, charg2
    character(len=40) :: acces
    aster_logical :: s2f
    integer, pointer :: des1(:) => null()
    integer, pointer :: des2(:) => null()
!
    call jemarq()
!
    nomfct='&&FF0000'
    noms2f='&&SF0000'
    call codent(ichar, 'D0', nomfct(5:6))
    call codent(ichar, 'D0', noms2f(5:6))
!
!
! --- 1.PREPARATION (ALLOCATION DE TABLEAUX DE TRAVAIL ...)
!     -----------------------------------------------------
!
    call jeveuo(char1//'.NOMA', 'L', jma)
    ma=zk8(jma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
!
! --  1.1 LORS D'UNE COMBINAISON DE CHARGEMENT 'SCALAIRE'/'FONCTION'
!         LE CHARGEMENT 'SCALAIRE' EST TRANSFORME EN CHARGEMENT
!         'FONCTION' (CONSTANT)
    s2f=.false.
    if (fonc1 .and. .not.fonc2) then
        charg1=char1
        charg2=noms2f
        call gchs2f(char2, char1, charg2)
        fonc2=.true.
        s2f=.true.
    else if (.not.fonc1 .and. fonc2) then
        charg1=noms2f
        charg2=char2
        call gchs2f(char1, char2, charg1)
        fonc1=.true.
        s2f=.true.
    else
        charg1=char1
        charg2=char2
    endif
!
    call jeveuo(charg1//'.DESC', 'L', vi=des1)
    call jeveuo(charg2//'.DESC', 'L', vi=des2)
    ASSERT(des1(1).eq.des2(1))
!
! --  1.2 TABLEAUX : MAILLES -> NUM_ZONE D'AFFECTATION (CARTE_1)
!                    MAILLES -> NUM_ZONE D'AFFECTATION (CARTE_2)
    call wkvect('&&GCHARF_ZONE.CART1', 'V V I', nbma, jzcar1)
    call wkvect('&&GCHARF_ZONE.CART2', 'V V I', nbma, jzcar2)
!
    do ima = 1, nbma
        zi(jzcar1+ima-1)=0
        zi(jzcar2+ima-1)=0
    end do
!
    call jeveuo(jexatr(charg1//'.LIMA', 'LONCUM'), 'L', p2)
    call jeveuo(charg1//'.LIMA', 'L', p1)
    call jelira(charg1//'.LIMA', 'ACCES', cval=acces)
    ASSERT(acces(1:2).eq.'NU')
!
    nbzo1=des1(3)
    do izo = 1, nbzo1
        nmazo=zi(p2+izo)-zi(p2+izo-1)
        do ima = 1, nmazo
            numa=zi(p1+zi(p2+izo-1)-1+ima-1)
            zi(jzcar1+numa-1)=izo
            call jenuno(jexnum(ma//'.NOMMAI', numa), k8b)
        end do
    end do
!
    call jeveuo(jexatr(charg2//'.LIMA', 'LONCUM'), 'L', p2)
    call jeveuo(charg2//'.LIMA', 'L', p1)
    call jelira(charg2//'.LIMA', 'ACCES', cval=acces)
    ASSERT(acces(1:2).eq.'NU')
!
    nbzo2=des2(3)
    do izo = 1, nbzo2
        nmazo=zi(p2+izo)-zi(p2+izo-1)
        do ima = 1, nmazo
            numa=zi(p1+zi(p2+izo-1)-1+ima-1)
            zi(jzcar2+numa-1)=izo
            call jenuno(jexnum(ma//'.NOMMAI', numa), k8b)
        end do
    end do
!
! --  1.3 TABLEAUX : ZONE_CARTE_1 x ZONE_CARTE_2 -> NB MAILLES AFFECTEES
!                    ZONE_CARTE_1 x ZONE_CARTE_2 -> NUM DE LA ZONE
    call wkvect('&&GCHARF_NB_MAILLE_ZONE', 'V V I', nbzo1*nbzo2, jmazo)
    call wkvect('&&GCHARF_NUM_ZONE_CARTE', 'V V I', nbzo1*nbzo2, jnumz)
!
    do izo2 = 1, nbzo2
        do izo1 = 1, nbzo1
            zi(jmazo+nbzo1*(izo2-1)+izo1-1)=0
        end do
    end do
!
    do ima = 1, nbma
        nuzo1=zi(jzcar1+ima-1)
        nuzo2=zi(jzcar2+ima-1)
        ii=nbzo1*(nuzo2-1)+nuzo1
        zi(jmazo+ii-1)=zi(jmazo+ii-1)+1
    end do
!
!     NOMBRE DE ZONES AFFECTEES POUR LA NOUVELLE CARTE:
    nbzo=0
    do izo = 1, nbzo1*nbzo2
        zi(jnumz+izo-1)=0
        if (zi(jmazo+izo-1) .ne. 0) then
            nbzo=nbzo+1
            zi(jnumz+izo-1)=nbzo
        endif
    end do
!
! --  1.4 TABLEAU: MAILLES -> NUM_ZONE D'AFFECTATION (NOUVELLE CARTE)
    call wkvect('&&GCHARF_ZONE.CARTE', 'V V I', nbma, jzcar)
!
    do ima = 1, nbma
        nuzo1=zi(jzcar1+ima-1)
        nuzo2=zi(jzcar2+ima-1)
        ii=nbzo1*(nuzo2-1)+nuzo1
        zi(jzcar+ima-1)=zi(jnumz+ii-1)
    end do
!
!
! --- 2. ALLOCATION DE LA NOUVELLE CARTE
!     ----------------------------------
!
!     STOCKAGE DE MA:
    call wkvect(charg//'.NOMA', 'V V K8', 1, jma)
    zk8(jma-1+1) = ma
!
!     ALLOCATION DE NOLI
    call wkvect(charg//'.NOLI', 'V V K24', nbzo, jk24)
!
!     ALLOCATION DE DESC:
    call wkvect(charg//'.DESC', 'V V I', 3+nbzo*3, jdes)
    call jeecra(charg//'.DESC', 'DOCU', cval='CART')
    zi(jdes-1+1) = des1(1)
    zi(jdes-1+2) = nbzo
    zi(jdes-1+3) = nbzo
    do izo = 1, nbzo
        zi(jdes-1+3+2*izo-1)= 3
        zi(jdes-1+3+2*izo) = izo
        zi(jdes-1+3+2*nbzo+izo)= des1(3+2*nbzo1+1)
    end do
!
!     ALLOCATION DE VALE:
    call jelira(jexnum('&CATA.GD.NOMCMP', des1(1)), 'LONMAX', ncmpmx)
    if (.not.fonc1 .and. .not.fonc2) then
        call wkvect(charg//'.VALE', 'V V R', nbzo*ncmpmx, jval)
    else if (fonc1) then
        call wkvect(charg//'.VALE', 'V V K8', nbzo*ncmpmx, jval)
    else if (fonc2) then
        call jelira(jexnum('&CATA.GD.NOMCMP', des2(1)), 'LONMAX', ncmpmx)
        call wkvect(charg//'.VALE', 'V V K8', nbzo*ncmpmx, jval)
    endif
!
!     ALLOCATION DE LIMA :
    call jecrec(charg//'.LIMA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbzo)
    call jeecra(charg//'.LIMA', 'LONT', nbma, ' ')
!
!
! --- 3. REMPLISSAGE DE LIMA ET VALE
!     ------------------------------
!
! --  3.1 LIMA
!
    do izo = 1, nbzo1*nbzo2
        if (zi(jmazo+izo-1) .ne. 0) then
            nuzo=zi(jnumz+izo-1)
            call jecroc(jexnum(charg//'.LIMA', nuzo))
            call jeecra(jexnum(charg//'.LIMA', nuzo), 'LONMAX', zi( jmazo+izo-1))
            call jeveuo(jexnum(charg//'.LIMA', nuzo), 'E', ilim)
            k=0
            do ima = 1, nbma
                if (zi(jzcar+ima-1) .eq. nuzo) then
                    k=k+1
                    zi(ilim+k-1)=ima
                endif
            end do
        endif
    end do
!
!
! --  3.2 VALE
!
!     3.2.1 CHARGEMENTS 'SCALAIRES'
    if (.not.fonc1 .and. .not.fonc2) then
!
        call jeveuo(charg1//'.VALE', 'L', jval1)
        call jeveuo(charg2//'.VALE', 'L', jval2)
        k=0
        do izo2 = 1, nbzo2
            do izo1 = 1, nbzo1
                ii=nbzo1*(izo2-1)+izo1
                if (zi(jmazo+ii-1) .ne. 0) then
                    do icmp = 1, ncmpmx
                        k=k+1
                        zr(jval+k-1)= zr(jval1+ncmpmx*(izo1-1)+icmp-1)&
                        + zr(jval2+ncmpmx*(izo2-1)+icmp-1)
                    end do
                endif
            end do
        end do
!
!     3.2.1 CHARGEMENTS 'FONCTION'
    else if (fonc1 .and. fonc2) then
        call jeveuo(charg1//'.VALE', 'L', jval1)
        call jeveuo(charg2//'.VALE', 'L', jval2)
        k=0
        kk=0
        do izo2 = 1, nbzo2
            do izo1 = 1, nbzo1
                ii=nbzo1*(izo2-1)+izo1
                if (zi(jmazo+ii-1) .ne. 0) then
                    do icmp = 1, ncmpmx
                        k=k+1
                        val1=zk8(jval1+ncmpmx*(izo1-1)+icmp-1)
                        val2=zk8(jval2+ncmpmx*(izo2-1)+icmp-1)
                        if (val1(1:7) .eq. '&FOZERO' .and. val2(1:7) .eq. '&FOZERO') then
                            zk8(jval+k-1)='&FOZERO'
                            elseif( val1(1:6).eq.'GLOBAL' .or. val2(1:6)&
                        .eq.'GLOBAL' )then
                            zk8(jval+k-1)='GLOBAL'
                        else if (val1(1:7).eq.'&FOZERO') then
                            zk8(jval+k-1)=val2
                        else if (val2(1:7).eq.'&FOZERO') then
                            zk8(jval+k-1)=val1
                            elseif( val1(1:1).eq.' ' .or.&
     &              val2(1:1).eq.' '  )then
                            zk8(jval+k-1)='        '
                        else
                            kk=kk+1
                            call codent(kk, 'D0', nomfct(7:8))
                            call gchfus(val1, val2, nomfct)
                            zk8(jval+k-1)=nomfct
                        endif
                    end do
                endif
            end do
        end do
!
    endif
!
    if (s2f) call detrsd('CARTE', noms2f)
    call jedetr('&&GCHARF_NUM_ZONE_CARTE')
    call jedetr('&&GCHARF_NB_MAILLE_ZONE')
    call jedetr('&&GCHARF_ZONE.CART1')
    call jedetr('&&GCHARF_ZONE.CART2')
    call jedetr('&&GCHARF_ZONE.CARTE')
!
!
    call jedema()
!
end subroutine
