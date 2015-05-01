subroutine carces(cartz, typces, cesmoz, base, cesz,&
                  kstop, iret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cestas.h"
#include "asterfort/cmpcha.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenc2.h"
#include "asterfort/exisd.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: cartz, cesz, base, cesmoz, typces
    character(len=1) :: kstop
! ------------------------------------------------------------------
! BUT: TRANSFORMER UNE CARTE EN CHAM_ELEM_S
! ------------------------------------------------------------------
!     ARGUMENTS:
! CARTZ  IN/JXIN  K19 : SD CARTE A TRANSFORMER
! TYPCES IN       K4  : TYPE VOULU POUR LE CHAM_ELEM_S
!                      /'ELEM' /'ELGA' /'ELNO'
! CESMOZ IN/JXIN  K19 :  SD CHAM_ELEM_S "MODELE" POUR CESZ
!       SI TYPCES = 'ELEM' : CESMOZ N'EST PAS UTILISE
!       SI TYPCES  ='ELGA' ON SE SERT DE CESMOZ POUR DETERMINER
!          LE NOMBRE DE POINTS ET DE SOUS-POINTS  DU CHAM_ELEM_S
!       SI TYPCES  ='ELNO' ON SE SERT DE CESMOZ POUR DETERMINER
!          LE NOMBRE DE SOUS-POINTS  DU CHAM_ELEM_S. SI CESMOZ
!          EST ABSENT, NBSP=1
!
! CESZ   IN/JXOUT K19 : SD CHAM_ELEM_S RESULTAT
! BASE    IN      K1  : BASE DE CREATION POUR CESZ : G/V/L
! KSTOP   IN      K1  : COMPORTEMENT EN CAS DE PROBLEME :
!               / 'A' : ON EMET UNE ALARME ET ON REND IRET > 0
!               / ' ' : ON N'EMET PAS DE MESSAGE
! IRET   OUT      I   : CODE RETOUR :
!                       0 : R.A.S.
!                       1 : LA CARTE CONCERNAIT AUSSI DES MAILLES
!                           TARDIVES QUI N'ONT PAS ETE TRAITEES.
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ima, iret, nec, nb_cmp_mx, jdesc, jvale, ngrmx, nb_cmp
    integer ::  jcesd, jcesc, jcesv, jcesl, nbma, ient, debgd, deb1, ico
    integer :: cmp, ieq, iad, cmp2,    nbpt, ipt
    integer ::   jconx2, isp, nbsp,  kcmp, iret2
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=19) :: cart, ces, cesmod
    integer, pointer :: cemd(:) => null()
    integer, pointer :: ptma(:) => null()
    integer, pointer :: vnbpt(:) => null()
    integer, pointer :: vnbsp(:) => null()
    integer, pointer :: cata_to_field(:) => null()
    integer, pointer :: field_to_cata(:) => null()
    character(len=8), pointer :: cmp_name(:) => null()

!     ------------------------------------------------------------------
    call jemarq()
!
!
    cart = cartz
    ces = cesz
    cesmod = cesmoz
!
    call dismoi('NOM_MAILLA', cart, 'CARTE', repk=ma)
    call dismoi('NOM_GD', cart, 'CARTE', repk=nomgd)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!
!     1-CALCUL DES OBJETS  '&&CARCES.NBPT','&CARCES.NBSP'
!     -----------------------------------------------------------------
    AS_ALLOCATE(vi=vnbpt, size=nbma)
    AS_ALLOCATE(vi=vnbsp, size=nbma)
!
!
    call exisd('CHAM_ELEM_S', cesmod, iret2)
    if (iret2 .gt. 0) then
        call jeveuo(cesmod//'.CESD', 'L', vi=cemd)
        do ima = 1, nbma
            vnbpt(ima) = cemd(5+4* (ima-1)+1)
            vnbsp(ima) = cemd(5+4* (ima-1)+2)
        end do
    else
        do ima = 1, nbma
            vnbpt(ima) = 1
            vnbsp(ima) = 1
        end do
    endif
!
!
    if (typces .eq. 'ELEM') then
        do ima = 1, nbma
            vnbpt(ima) = 1
            vnbsp(ima) = 1
        end do
    else if (typces.eq.'ELNO') then
        do ima = 1, nbma
            vnbpt(ima) = zi(jconx2+ima) - zi(jconx2+ima-1)
        end do
    endif
!
!
!
!     2- RECUPERATION D'INFORMATIONS DANS CART :
!     ------------------------------------------
    call jeveuo(cart//'.DESC', 'L', jdesc)
    call jeveuo(cart//'.VALE', 'L', jvale)
    ngrmx = zi(jdesc-1+2)
!
!
!     3- ON ETEND LA CARTE POUR CREER L'OBJET .PTMA :
!     -----------------------------------------------------------
    call etenc2(cart, iret)
    if (iret .eq. 1 .and. kstop .eq. 'A') then
        call utmess('A', 'CALCULEL_38')
    endif
    call jeveuo(cart//'.PTMA', 'L', vi=ptma)
!
! - Create objects for global components (catalog) <=> local components (field)
!
    call cmpcha(cart     , cmp_name, cata_to_field, field_to_cata, nb_cmp,&
                nb_cmp_mx)
!
!
!     5- CREATION DE CES :
!     ---------------------------------------
    call cescre(base, ces, typces, ma, nomgd,&
                nb_cmp, cmp_name, vnbpt, vnbsp, [-nb_cmp])
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', jcesc)
    call jeveuo(ces//'.CESV', 'E', jcesv)
    call jeveuo(ces//'.CESL', 'E', jcesl)
!
!
!
!     6- REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
    do ima = 1, nbma
        ient = ptma(ima)
        if (ient .eq. 0) goto 120
!
        deb1 = (ient-1)*nb_cmp_mx + 1
        debgd = 3 + 2*ngrmx + (ient-1)*nec + 1
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
!
        ico = 0
        do kcmp = 1, nb_cmp
            cmp = field_to_cata(kcmp)
            if (.not. (exisdg(zi(jdesc-1+debgd),cmp))) goto 110
            ico = ico + 1
            ieq = deb1 - 1 + ico
!
            cmp2 = cata_to_field(cmp)
            ASSERT(cmp2.gt.0)
            ASSERT(cmp2.le.nb_cmp)
!
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, cmp2, iad)
                    ASSERT(iad.le.0)
                    if (iad .eq. 0) goto 110
!
!
!         -- RECOPIE DE LA VALEUR:
                    zl(jcesl-1-iad) = .true.
                    if (tsca .eq. 'R') then
                        zr(jcesv-1-iad) = zr(jvale-1+ieq)
                    else if (tsca.eq.'C') then
                        zc(jcesv-1-iad) = zc(jvale-1+ieq)
                    else if (tsca.eq.'I') then
                        zi(jcesv-1-iad) = zi(jvale-1+ieq)
                    else if (tsca.eq.'L') then
                        zl(jcesv-1-iad) = zl(jvale-1+ieq)
                    else if (tsca.eq.'K8') then
                        zk8(jcesv-1-iad) = zk8(jvale-1+ieq)
                    else if (tsca.eq.'K16') then
                        zk16(jcesv-1-iad) = zk16(jvale-1+ieq)
                    else if (tsca.eq.'K24') then
                        zk24(jcesv-1-iad) = zk24(jvale-1+ieq)
                    else if (tsca.eq.'K32') then
                        zk32(jcesv-1-iad) = zk32(jvale-1+ieq)
                    else if (tsca.eq.'K80') then
                        zk80(jcesv-1-iad) = zk80(jvale-1+ieq)
                    else
                        ASSERT(.false.)
                    endif
                end do
            end do
110         continue
        end do
!
120     continue
    end do
!
!
!     7- RETASSAGE DE CES :
!     ---------------------
    call cestas(ces)
!
!
!     8- MENAGE :
!     -----------
    call jedetr(cart//'.PTMA')
    AS_DEALLOCATE(vi=vnbpt)
    AS_DEALLOCATE(vi=vnbsp)
    AS_DEALLOCATE(vi = cata_to_field)
    AS_DEALLOCATE(vi = field_to_cata)
    AS_DEALLOCATE(vk8 = cmp_name)
!
    call jedema()
end subroutine
