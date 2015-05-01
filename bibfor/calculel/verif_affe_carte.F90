subroutine verif_affe_carte(ligrmo,carte,comment)
    implicit none
!
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
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/knindi.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/imprsd.h"
#include "asterfort/list_grma.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/jxveri.h"
!
    character(len=19), intent(in) :: ligrmo
    character(len=19), intent(in) :: carte
    character(len=*), intent(in) :: comment
!
!-----------------------------------------------------------------------
!   But :
!     Emettre des alarmes concernant les affectations douteuses d'une carte
!
!   Entrees:
!     ligrmo     :  ligrel du modele
!     carte      :  sd_carte
!     comment    :  commentaire de description pour la sd_carte
!
!-----------------------------------------------------------------------
    character(len=3) :: tsca
    character(len=8) :: nomgd,nocmp,mailla,nommai
    character(len=24) :: lgrma(4)
    character(len=16) :: nomte
    character(len=19) :: ces
    character(len=80) :: valk(5)
    integer :: nbgrel,igrel,kcmp,kcmp1,nbcmp,nbcmp1,nbop,nbte,k1
    integer :: jnocmp,numgd,joptte,jligrmo,n1,kop,ioptte,joptmod
    integer :: jmodeloc,nbin,kin,moloc,nbma,ima,iad1,iret,te,nbmapb,nbgrma
    integer :: jcesd,jcesc,jcesv,jcesl,nucalc,k
    integer, pointer :: a_un_sens(:) => null()
    integer, pointer :: num_grel(:) => null()
    integer :: list_ma_pb(5)
    aster_logical :: lnul,lpb

!-----------------------------------------------------------------------
!
    call jemarq()
!   call imprsd('CHAMP', carte, 6, 'AJACOT CARTE:')

    call dismoi('NOM_GD', carte, 'CARTE', repk=nomgd)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=nbcmp)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=numgd)
    call jeveuo(jexnom('&CATA.GD.NOMCMP',nomgd),'L',jnocmp)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_GREL', ligrmo, 'LIGREL', repi=nbgrel)
    call dismoi('NOM_MAILLA', ligrmo, 'LIGREL', repk=mailla)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbma)

    ces='&&verif_affe_ca.ces'
    call carces(carte, 'ELEM', ' ', 'V', ces, ' ', iret)
    if (iret.ne.0) goto 999


!   -- 1. recuperation des objets des catalogues EF:
!   -------------------------------------------------
    call jelira('&CATA.OP.NOMOPT','NOMMAX',nbop)
    call jelira('&CATA.TE.NOMTE','NOMMAX',nbte)
    call jeveuo('&CATA.TE.OPTTE','L',joptte)




!   -- 2. On calcule 2 tableaux :
!         A_UN_SENS(igrel,kcmp)  -> 0 : non , 1 : oui
!         NUM_GREL(2*(ima-1)+1)  : igrel associe a la maille ima
!         NUM_GREL(2*(ima-1)+2)  : te    associe a la maille ima
!   --------------------------------------------------------------------
    AS_ALLOCATE(vi=a_un_sens, size=nbgrel*nbcmp)
    a_un_sens=0
    AS_ALLOCATE(vi=num_grel, size=2*nbma)
    num_grel=0

    do igrel=1,nbgrel
       call jeveuo(jexnum(ligrmo//'.LIEL', igrel), 'L', jligrmo)
       call jelira(jexnum(ligrmo//'.LIEL', igrel), 'LONMAX', n1)
       te=zi(jligrmo-1+n1)
       do k=1,n1-1
           ima=zi(jligrmo-1+k)
           if (ima.gt.0) then
               num_grel(2*(ima-1)+1)=igrel
               num_grel(2*(ima-1)+2)=te
           endif
       enddo
       do kop=1,nbop
           ioptte=zi(joptte-1+(te-1)*nbop + kop)
           if (ioptte.gt.0) then
               call jeveuo(jexnum('&CATA.TE.OPTMOD', ioptte), 'L', joptmod)
               nucalc=zi(joptmod-1+1)
               if (nucalc.lt.0) goto 1

               nbin=zi(joptmod-1+2)
               do kin=1,nbin
                   moloc=zi(joptmod-1+3+kin)
                   call jeveuo(jexnum('&CATA.TE.MODELOC', moloc), 'L', jmodeloc)
                   if (zi(jmodeloc-1+2).eq.numgd) then
                       ASSERT(zi(jmodeloc-1+4).lt.10000)
                       ASSERT(zi(jmodeloc-1+4).gt.0)
                       do kcmp = 1, nbcmp
                           if (exisdg(zi(jmodeloc-1+5),kcmp)) then
                                   a_un_sens((igrel-1)*nbcmp+kcmp)=1
                           endif
                       enddo
                   endif
               enddo
1              continue
           endif
       enddo
    enddo



!   -- 3. On parcourt les valeurs de la carte :
!   -------------------------------------------
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', jcesc)
    call jeveuo(ces//'.CESV', 'L', jcesv)
    call jeveuo(ces//'.CESL', 'L', jcesl)

    ASSERT(nbma.eq.zi(jcesd-1+1))
    nbcmp1=zi(jcesd-1+2)
    do kcmp1=1,nbcmp1
        nocmp=zk8(jcesc-1+kcmp1)


!       -- Exceptions :
!       ----------------------------------------------------------------
!       E1) PESA_R / ROTA_R sont en general utilises sans preciser les mailles
        if (nomgd.eq.'PESA_R') goto 2
        if (nomgd.eq.'ROTA_R') goto 2

!       E2) Valeurs fournies par le code d'AFFE_CHAR_MECA
        if (nomgd(1:5).eq.'FORC_' .and. nocmp.eq.'REP') goto 2
        if (nomgd(1:5).eq.'FORC_' .and. nocmp.eq.'PLAN') goto 2
        if (nomgd.eq.'VENTCX_F' .and. nocmp.eq.'FCXP') goto 2

!       E3) Valeurs fournies en loucede par le code d'AFFE_CARA_ELEM
        if (nomgd.eq.'CAMASS' .and. nocmp.eq.'C') goto 2
        if (nomgd.eq.'CACOQU' .and. nocmp.eq.'KAPPA') goto 2
        if (nomgd.eq.'CACOQU' .and. nocmp.eq.'CTOR') goto 2
        if (nomgd.eq.'CAORIE' .and. nocmp.eq.'ALPHA') goto 2

        if (nomgd.eq.'CINFDI' .and. nocmp(1:3).eq.'REP') goto 2
        if (nomgd.eq.'CINFDI' .and. nocmp(1:3).eq.'SYM') goto 2

        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'TSEC') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HY1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HZ1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HY2') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HZ2') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPY1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPY2') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPZ1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPZ2') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EP1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EP2') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'R1') goto 2
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'R2') goto 2

        if (nomgd.eq.'CAGNPO') goto 2


        kcmp = knindi(8,nocmp,zk8(jnocmp),nbcmp)
        ASSERT(kcmp.gt.0)
        nbmapb=0
        do ima=1,nbma
            lpb=.false.
            igrel=num_grel(2*(ima-1)+1)
!           -- on ne verifie pas les mailles qui ne sont pas affectees dans le modele
!              (on ne saurait pas remplir le champ nomte du message)
            if (igrel.eq.0) goto 3

            te=num_grel(2*(ima-1)+2)
            call cesexi('C', jcesd, jcesl, ima, 1, 1, kcmp1, iad1)
            if (iad1.gt.0) then
                lnul=.false.
                if (tsca .eq. 'R') then
                    if (zr(jcesv-1+iad1).eq.0.d0) lnul=.true.
                else if (tsca.eq.'C') then
                    if (abs(zc(jcesv-1+iad1)).eq.0.d0) lnul=.true.
                else if (tsca.eq.'I') then
                    if (zi(jcesv-1+iad1).eq.0) lnul=.true.
                else if (tsca.eq.'L') then
                    if (.not.zl(jcesv-1+iad1)) lnul=.true.
                else if (tsca(1:2).eq.'K8') then
                    if (zk8(jcesv-1+iad1).eq.' ') lnul=.true.
                    if (zk8(jcesv-1+iad1).eq.'&FOZERO') lnul=.true.
                else if (tsca(1:3).eq.'K16') then
                    if (zk16(jcesv-1+iad1).eq.' ') lnul=.true.
                    if (zk16(jcesv-1+iad1).eq.'&FOZERO') lnul=.true.
                else if (tsca(1:3).eq.'K24') then
                    if (zk24(jcesv-1+iad1).eq.' ') lnul=.true.
                    if (zk24(jcesv-1+iad1).eq.'&FOZERO') lnul=.true.
                else
                    ASSERT(.false.)
                endif

                if (.not.lnul) then
                    if (a_un_sens((igrel-1)*nbcmp+kcmp).eq.0) lpb=.true.
                endif
            endif
            if (lpb) then
               nbmapb=nbmapb+1
               if (nbmapb.eq.1) call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
               if (nbmapb.le.5) list_ma_pb(nbmapb)=ima
            endif
3           continue
        enddo

!       -- message d'alarme en cas de probleme :
!       -----------------------------------------
        if (nbmapb.gt.0) then
            valk(1)=carte
            valk(2)=comment
            valk(3)=nomgd
            valk(4)=nocmp
            valk(5)=nomte
            call utmess('A','CALCULEL_40',nk=5,valk=valk,si=nbmapb)
            do k=1,min(5,nbmapb)
               valk=' '
               ima=list_ma_pb(k)
               call jenuno(jexnum(mailla//'.NOMMAI', ima), nommai)
               valk(1)=nommai

               call list_grma(mailla,ima,4,lgrma,nbgrma)
               do k1=1,min(nbgrma,3)
                  valk(1+k1)=lgrma(k1)
               enddo
               if (nbgrma.gt.3) valk(5)='...'
               call utmess('I','CALCULEL_41',nk=5,valk=valk)
            enddo
        endif
2       continue
    enddo

    call detrsd('CHAM_ELEM_S', ces)
    AS_DEALLOCATE(vi=a_un_sens)
    AS_DEALLOCATE(vi=num_grel)

999 continue
    call jedema()
end subroutine
