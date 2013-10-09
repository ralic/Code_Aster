subroutine cesred(ces1z, nbma, lima, nbcmp, licmp,&
                  base, ces2z)
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/knindi.h"
#include "asterfort/wkvect.h"
    integer :: nbma, nbcmp
    integer :: lima(nbma)
    character(len=*) :: ces1z, ces2z, base
    character(len=*) :: licmp(*)
!
! ------------------------------------------------------------------
! BUT: REDUIRE UN CHAM_ELEM_S SUR UN ENSEMBLE DE MAILLES
!      ET/OU DE COMPOSANTES
! ------------------------------------------------------------------
!     ARGUMENTS:
! CES1Z   IN/JXIN  K19  : SD CHAM_ELEM_S A REDUIRE
! CES2Z   IN/JXOUT K19  : SD CHAM_ELEM_S REDUITE
! BASE    IN       K1   : BASE DE CREATION POUR CES2Z : G/V/L
! NBMA    IN       I    : NOMBRE DE MAILLES DE LIMA
!                         SI NBMA=0 : ON NE REDUIT PAS SUR LES MAILLES
! LIMA    IN       L_I  : LISTE DES NUMEROS DE MAILLES SUR LESQUELLES
!                         ON VEUT REDUIRE LE CHAM_ELEM_S
!                         LES MAILLES TARDIVES (<0) SONT IGNOREES.
! NBCMP   IN       I    : NOMBRE DE CMPS DE LICMP
!                         SI NBCMP=0 : ON NE REDUIT PAS SUR LES CMPS
!                         SI NBCMP<0 : ON ENLEVE DES CMPS A CES1Z
! LICMP   IN       L_K8 : LISTE DES NOMS DES CMPS SUR LESQUELLES
!                         ON VEUT REDUIRE LE CHAM_ELEM_S
!                         LISTE DES COMPOSANTES A OTER DE CES1Z
!
! SI NBCMP > 0 : LES CMPS DU CHAM_ELEM_S REDUIT SONT DANS L'ORDRE
!                DE LICMP.
! SI NBCMP = 0 : LES CMPS DU CHAM_ELEM_S REDUIT SONT DANS L'ORDRE DE
!                CES1Z, TOUTES LES COMPOSANTES SONT PRESENTES.
! SI NBCMP < 0 : LES CMPS DU CHAM_ELEM_S REDUIT SONT DANS L'ORDRE
!                DE CES1Z EN OTANT LES COMPOSANTES PRESENTES DANS LICMP.
!
! REMARQUES :
!  - POUR REDUIRE SUR LES MAILLES (ET GARDER TOUTES LES CMPS)
!        NBCMP=0   LICMP= KBID          NBMA=N   LIMA= LISTE(1..N)
!  - POUR REDUIRE SUR LES CMPS (ET GARDER TOUTES LES MAILLES)
!        NBCMP=+N  LICMP= LISTE(1..N)   NBMA=0   LIMA= IBID
!  - POUR OTER DES CMPS (ET GARDER TOUTES LES MAILLES)
!        NBCMP=-N  LICMP= LISTE(1..N)   NBMA=0   LIMA= IBID
!
!  - SI NBCMP=0 ET NBMA=0 : CES2Z=CES1Z
!
!  - ON PEUT APPELER CETTE ROUTINE AVEC CES2Z=CES1Z
!    LA SD INITIALE (CES1Z) EST ALORS PERDUE.
!
!-----------------------------------------------------------------------
    logical :: loter
    integer :: jce1k, jce1d, jce1v, jce1l, jce1c, nbmam, ncmp2, jexma
    integer :: jce2d, jce2v, jce2l, jce2c, jnbpt, jnbsp, jnbcmp, nbpt
    integer :: kma, isp, iad1, iad2, jce3c, nbsp, ipt
    integer :: ncmpmx, ncmp1, icmp1, icmp2, icmp3
    integer :: ima
    character(len=8) :: ma, nomgd, nocmp, typces
    character(len=3) :: tsca
    character(len=19) :: ces1, ces2
!     ------------------------------------------------------------------
    call jemarq()
!
    ces1 = ces1z
    ces2 = ces2z
!
!     -- POUR NE PAS ECRASER LA SOURCE SI CES2=CES1 :
    if (ces2 .eq. ces1) then
        call copisd('CHAM_ELEM_S', 'V', ces1, '&&CESRED.CES1')
        ces1 = '&&CESRED.CES1'
    endif
!
!
!     1- RECUPERATION D'INFORMATIONS DANS CES1 :
!     ------------------------------------------
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
    call jeveuo(ces1//'.CESC', 'L', jce1c)
    call jeveuo(ces1//'.CESV', 'L', jce1v)
    call jeveuo(ces1//'.CESL', 'L', jce1l)
!
    ma = zk8(jce1k-1+1)
    nomgd = zk8(jce1k-1+2)
    typces = zk8(jce1k-1+3)
    nbmam = zi(jce1d-1+1)
    ncmp1 = zi(jce1d-1+2)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
!
    jce3c = jce1c
    if (nbcmp .gt. 0) then
!        CHAM_ELEM_S CES2 AVEC CMPS IMPOSEES DANS L'ORDRE DE LICMP
        ncmp2 = nbcmp
    else if (nbcmp.eq.0) then
!        CHAM_ELEM_S CES2 AVEC MEME CMPS ET MEME ORDRE QUE CES1
        ncmp2 = ncmp1
    else
!        CHAM_ELEM_S CES2 AVEC CMPS DANS LE MEME ORDRE QUE CES1
!        AUXQUELLES SONT ENLEVEES LES COMPOSANTES PRESENTES DANS LICMP
        call wkvect('&&CESRED.CMP2', 'V V K8', ncmp1, jce3c)
        ncmp2 = 0
        do icmp1 = 1, ncmp1
            loter = .false.
            do icmp3 = 1, -nbcmp
                if (zk8(jce1c-1+icmp1) .eq. licmp(icmp3)) loter = .true.
            end do
            if (.not.loter) then
                ncmp2 = ncmp2 +1
                zk8(jce3c-1+ncmp2) = zk8(jce1c-1+icmp1)
            endif
        end do
        ASSERT(ncmp2 .lt. ncmp1)
    endif
    ASSERT(ncmp2 .ne. 0)
!
!     2- CREATION DE 3 OBJETS CONTENANT LES NOMBRES DE POINTS,
!         SOUS-POINTS ET CMPS POUR CHAQUE MAILLE :
!     -----------------------------------------------------------
    call wkvect('&&CESRED.NBPT', 'V V I', nbmam, jnbpt)
    call wkvect('&&CESRED.NBSP', 'V V I', nbmam, jnbsp)
    call wkvect('&&CESRED.NBCMP', 'V V I', nbmam, jnbcmp)
!
    if (nbma .ne. 0) then
        do kma = 1, nbma
            ASSERT(lima(kma).le.nbmam)
            if (lima(kma) .le. 0) goto 10
            zi(jnbpt-1+lima(kma))=zi(jce1d-1+5+4*(lima(kma)-1)+1)
            zi(jnbsp-1+lima(kma))=zi(jce1d-1+5+4*(lima(kma)-1)+2)
            zi(jnbcmp-1+lima(kma))=min(zi(jce1d-1+5+4*(lima(kma)-1)+3)&
            , ncmp2)
 10         continue
        end do
!
    else
!
        do ima = 1, nbmam
            zi(jnbpt-1+ima)=zi(jce1d-1+5+4*(ima-1)+1)
            zi(jnbsp-1+ima)=zi(jce1d-1+5+4*(ima-1)+2)
            zi(jnbcmp-1+ima)=min(zi(jce1d-1+5+4*(ima-1)+3),ncmp2)
        end do
    endif
!
!     3- CREATION DE CES2 :
!     ---------------------------------------
    if (nbcmp .gt. 0) then
!        ON DEMANDE EXPLICITEMENT UNE LISTE DE COMPOSANTES SUR LES
!        MAILLES. TOUTES LES MAILLES SONT DONC AFFECTEES ET SE
!        RETROUVE AVEC LE MEME NOMBRE DE COMPOSANTES. IL SE PEUT
!        EGALEMENT QUE LE NOMBRE DE COMPOSANTES SUR CERTAINE MAILLES
!        SOIT SUPÉRIEUR A CE QU'IL ETAIT PRECEDEMMENT, ON NE PEUT DONC
!        PAS UTILISER ZI(JNBCMP) QUI EST LE MIN ==> -NBCMP
        call cescre(base, ces2, typces, ma, nomgd,&
                    ncmp2, licmp, zi(jnbpt), zi(jnbsp), [-nbcmp])
    else
!        CHAM_ELEM_S AVEC COMPOSANTES DANS LE MEME ORDRE QUE CES1
        call cescre(base, ces2, typces, ma, nomgd,&
                    ncmp2, zk8(jce3c), zi(jnbpt), zi(jnbsp), zi(jnbcmp))
    endif
!
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
!
!     4- ON TRANSFORME LIMA EN LISTE DE BOOLEENS:
!     ------------------------------------------
    call wkvect('&&CESRED.EXIMA', 'V V L', nbmam, jexma)
    do kma = 1, nbmam
        zl(jexma-1+kma) = .false.
    end do
!
    ASSERT(nbma.ge.0)
    if (nbma .eq. 0) then
        do kma = 1, nbmam
            zl(jexma-1+kma) = .true.
        end do
!
    else
        do kma = 1, nbma
            if (lima(kma) .le. 0) goto 40
            zl(jexma-1+lima(kma)) = .true.
 40         continue
        end do
    endif
!
!     5- REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
    do icmp2 = 1, ncmp2
        nocmp = zk8(jce2c-1+icmp2)
        icmp1 = knindi(8,nocmp,zk8(jce1c),ncmp1)
        if (icmp1 .eq. 0) goto 80
!
        do ima = 1, nbmam
            if (zl(jexma-1+ima)) then
                nbpt = zi(jce2d-1+5+4* (ima-1)+1)
                nbsp = zi(jce2d-1+5+4* (ima-1)+2)
                do ipt = 1, nbpt
                    do isp = 1, nbsp
                        call cesexi('C', jce1d, jce1l, ima, ipt,&
                                    isp, icmp1, iad1)
                        call cesexi('C', jce2d, jce2l, ima, ipt,&
                                    isp, icmp2, iad2)
                        ASSERT(iad2.le.0)
                        if ((iad1.le.0) .or. (iad2.eq.0)) goto 50
!               -- RECOPIE DE LA VALEUR:
                        zl(jce2l-1-iad2) = .true.
                        if (tsca .eq. 'R') then
                            zr(jce2v-1-iad2) = zr(jce1v-1+iad1)
                        else if (tsca.eq.'C') then
                            zc(jce2v-1-iad2) = zc(jce1v-1+iad1)
                        else if (tsca.eq.'I') then
                            zi(jce2v-1-iad2) = zi(jce1v-1+iad1)
                        else if (tsca.eq.'L') then
                            zl(jce2v-1-iad2) = zl(jce1v-1+iad1)
                        else if (tsca.eq.'K8') then
                            zk8(jce2v-1-iad2) = zk8(jce1v-1+iad1)
                        else if (tsca.eq.'K16') then
                            zk16(jce2v-1-iad2) = zk16(jce1v-1+iad1)
                        else if (tsca.eq.'K24') then
                            zk24(jce2v-1-iad2) = zk24(jce1v-1+iad1)
                        else if (tsca.eq.'K32') then
                            zk32(jce2v-1-iad2) = zk32(jce1v-1+iad1)
                        else if (tsca.eq.'K80') then
                            zk80(jce2v-1-iad2) = zk80(jce1v-1+iad1)
                        else
                            ASSERT(.false.)
                        endif
!
 50                     continue
                    end do
                end do
            endif
!
        end do
 80     continue
    end do
!
!     6- MENAGE :
!     -----------
    call jedetr('&&CESRED.NBPT')
    call jedetr('&&CESRED.NBSP')
    call jedetr('&&CESRED.NBCMP')
    if (nbcmp .lt. 0) call jedetr('&&CESRED.CMP2')
    call jedetr('&&CESRED.EXIMA')
    if (ces1 .eq. '&&CESRED.CES1') call detrsd('CHAM_ELEM_S', ces1)
!
    call jedema()
end subroutine
