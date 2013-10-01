subroutine w039c1(carte, ifi, form, ligrel, titre)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/exisd.h"
#include "asterfort/imprsd.h"
#include "asterfort/irceme.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/w039c2.h"
#include "asterfort/w039c4.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel
    character(len=*) :: carte, titre, form
    integer :: ifi
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     BUT:
!       IMPRIMER UNE "CARTE" D'1 CONCEPT CHAM_MATER, CARA_ELE, ...
! ----------------------------------------------------------------------
!
!
!
!
    integer :: ibid, iret, jptma, ierd, ima, nbma, izone, nuzone
    integer :: jcesv, jcesd, jcesl, iad, dec1, dec2, ifm, ifr, nncp, iexi
    integer :: jdesc, jvale, ngedit, nugd, ncmpmx, kgedit, jzones, kzone, kcmp
    character(len=19) :: cart1, cel2, ces2
    character(len=64) :: nommed
    character(len=8) :: kbid, ma, tsca, nomgd, modele, typech, sdcarm
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     -- SI LA CARTE N'EXISTE PAS, IL N'Y A RIEN A FAIRE :
!     -----------------------------------------------------
    call exisd('CARTE', carte, iexi)
    if (iexi .eq. 0) goto 999
!
!
    ifm=6
    ifr=8
    cart1=carte
!
!     -- POUR QUE LE CHAM_ELEM QUE L'ON VA IMPRIMER AIT UN NOM "PROCHE"
!        DE CELUI DE LA VRAIE CARTE
    cel2=cart1
    cel2(9:9)='_'
!
!
!     -- QUELQUES INFOS SUR LA CARTE :
    call jeveuo(cart1//'.DESC', 'L', jdesc)
    call jeveuo(cart1//'.VALE', 'L', jvale)
    ngedit=zi(jdesc-1+3)
    nugd=zi(jdesc-1+1)
    call jenuno(jexnum('&CATA.GD.NOMGD', nugd), nomgd)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx)
    write (ifm,*)' '
    write (ifr,*)' '
    write (ifm,'(A,A)')'IMPRESSION D''UN CHAMP DE CONCEPT : ',titre
    write (ifr,'(A,A)')'IMPRESSION D''UN CHAMP DE CONCEPT : ',titre
    write (ifm,'(A,A)')'NOM DU CHAMP : ',cel2
    write (ifr,'(A,A)')'NOM DU CHAMP : ',cel2
!
!
!     -- SI LA CARTE A DES VALEURS REELLES ET QUE LE FORMAT EST 'MED'
!        ON L'IMPRIME AVEC SES VALEURS REELLES. C'EST PLUS JOLI !
    if (form .eq. 'MED' .and. tsca .eq. 'R') then
        call w039c4(carte, ifi, form)
        goto 999
    endif
!
!
!
    write (ifm,'(A)')'CORRESPONDANCE VALEUR <-> CONTENU :'
    write (ifr,'(A)')'CORRESPONDANCE VALEUR <-> CONTENU :'
!
!     -- PARFOIS LA CARTE CONTIENT DES ZONES AYANT LES MEMES VALEURS :
!     ----------------------------------------------------------------
    call wkvect('&&W039C1.ZONES', 'V V I', ngedit, jzones)
    nuzone=0
    do kgedit = 1, ngedit
        izone=kgedit
!       -- ON REGARDE SI LES VALEURS DE IZONE N'ONT PAS DEJA ETE VUES
!          POUR KZONE < IZONE :
        do kzone = 1, izone-1
            do kcmp = 1, ncmpmx
                dec1=ncmpmx*(kzone-1)+kcmp
                dec2=ncmpmx*(izone-1)+kcmp
                if (tsca .eq. 'K8') then
                    if (zk8(jvale-1+dec1) .ne. zk8(jvale-1+dec2)) goto 20
                else if (tsca.eq.'K16') then
                    if (zk16(jvale-1+dec1) .ne. zk16(jvale-1+dec2)) goto 20
                else if (tsca.eq.'K24') then
                    if (zk24(jvale-1+dec1) .ne. zk24(jvale-1+dec2)) goto 20
                else if (tsca.eq.'I') then
                    if (zi(jvale-1+dec1) .ne. zi(jvale-1+dec2)) goto 20
                else if (tsca.eq.'R') then
                    if (zr(jvale-1+dec1) .ne. zr(jvale-1+dec2)) goto 20
                else if (tsca.eq.'C') then
                    if (zc(jvale-1+dec1) .ne. zc(jvale-1+dec2)) goto 20
                else
                    ASSERT(.false.)
                endif
            end do
!         -- IZONE == KZONE :
            zi(jzones-1+izone)=zi(jzones-1+kzone)
            goto 30
!
 20         continue
        end do
        nuzone=nuzone+1
        zi(jzones-1+izone)=nuzone
        call w039c2(nuzone, jvale, jdesc, nomgd, ifm,&
                    ifr)
 30     continue
    end do
!
!
!
!     -- ON TRANSFORME LA CARTE EN UN CHAM_ELEM_S DE NEUT_R :
!     ------------------------------------------------------
    call jelira(cart1//'.DESC', 'DOCU', cval=kbid)
    ASSERT(kbid.eq.'CART')
    call dismoi('F', 'NOM_MAILLA', cart1, 'CARTE', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ierd)
!
    call etenca(cart1, ligrel, iret)
    ASSERT(iret.eq.0)
    call jeveuo(cart1//'.PTMA', 'L', jptma)
!
    ces2='&&W039C1.CES2'
    call cescre('V', ces2, 'ELEM', ma, 'NEUT_R',&
                1, 'X1', [-1], [-1], [-1])
    call jeveuo(ces2//'.CESD', 'L', jcesd)
    call jeveuo(ces2//'.CESV', 'E', jcesv)
    call jeveuo(ces2//'.CESL', 'E', jcesl)
    do ima = 1, nbma
        izone=zi(jptma-1+ima)
        if (izone .gt. 0) then
            nuzone=zi(jzones-1+izone)
            ASSERT(nuzone.gt.0)
            call cesexi('C', jcesd, jcesl, ima, 1,&
                        1, 1, iad)
            ASSERT(iad.le.0)
            zl(jcesl-1-iad)=.true.
            zr(jcesv-1-iad)=dble(nuzone)
        endif
    end do
!
!
!     -- TRANSFORMATION DE CES2 EN CEL2 (CHAM_ELEM/ELEM) :
!     ----------------------------------------------------
    call cescel(ces2, ligrel, 'TOU_INI_ELEM', 'PNEU1_R', 'OUI',&
                nncp, 'V', cel2, 'F', iret)
    ASSERT(iret.eq.0)
    call detrsd('CHAM_ELEM_S', ces2)
!
!
!     -- IMPRESSION DE CEL2 :
!     -----------------------
!
    if (form .eq. 'MED') then
!     -------------------------
        nommed=cel2
        typech='ELEM'
        modele=' '
        sdcarm=' '
        call irceme(ifi, nommed, cel2, typech, modele,&
                    0, ' ', ' ', ' ', 0,&
                    0.d0, 0, 0, [0], sdcarm,&
                    iret)
        ASSERT(iret.eq.0)
!
!
    else if (form.eq.'RESULTAT') then
!     ---------------------------
        call imprsd('CHAMP', cel2, ifi, titre)
!
!
    else
        ASSERT(.false.)
    endif
    call detrsd('CHAM_ELEM', cel2)
    call jedetr('&&W039C1.ZONES')
!
!
999 continue
    call jedema()
end subroutine
