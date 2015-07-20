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
#include "asterfort/etenca.h"
#include "asterfort/jeexin.h"
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
    character(len=80) :: valk(5)
    integer :: nbgrel,igrel,kcmp,nbcmp,nbop,nbte,k1,iexi,ient,deb1,iad1
    integer :: jnocmp,numgd,joptte,jligrmo,n1,kop,ioptte,joptmod,jvale
    integer :: jmodeloc,nbin,kin,moloc,nbma,ima,iret,te,nbmapb,nbgrma
    integer :: nucalc,k,kma,nec,nbma_verif,nbgdmx,code,decal,ico,kcmp2
    integer, pointer :: a_un_sens(:) => null()
    integer, pointer :: num_grel(:) => null()
    integer, pointer :: numa_verif(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: ptma(:) => null()
    integer, pointer :: dg(:) => null()
    integer :: list_ma_pb(5)

!-----------------------------------------------------------------------
!
    call jemarq()

    call dismoi('NOM_GD', carte, 'CARTE', repk=nomgd)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=nbcmp)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=numgd)
    call jeveuo(jexnom('&CATA.GD.NOMCMP',nomgd),'L',jnocmp)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('NB_GREL', ligrmo, 'LIGREL', repi=nbgrel)
    call dismoi('NOM_MAILLA', ligrmo, 'LIGREL', repk=mailla)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbma)



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



!   -- 3. On parcourt les CMPS affectees volontairement dans la carte :
!         (hors : TOUT='OUI')
!   -------------------------------------------------------------------


!   3.1 : on repere les mailles a verifier (numa_verif(*)) :
!   --------------------------------------------------------
    call jeveuo(carte//'.DESC','L',vi=desc)
    call jeveuo(carte//'.VALE', 'L', jvale)
    nbgdmx=desc(2)
!   -- si la carte est constante (TOUT='OUI'), on ne verifie pas
    if (nbgdmx.eq.1.and.desc(3+1).eq.1) goto 999

    call etenca(carte, ligrmo, iret)
    if (iret .gt. 0) goto 999
    call jeexin(carte//'.PTMA', iexi)
    if (iexi .eq. 0) goto 999

    nbma_verif=0
    AS_ALLOCATE(vi=numa_verif, size=nbma)
    call jeveuo(carte//'.PTMA','L',vi=ptma)
    do ima=1,nbma
        ient = ptma(ima)

!       -- si la maille n'est pas affectee :
        if (ient.eq.0) cycle

!       -- si la maille est affectee par TOUT='OUI' :
        code=desc(3+2*(ient-1)+1)
        if (code.eq.1) cycle

!       -- on ne verifie pas les mailles qui ne sont pas affectees dans le modele
!          (on ne saurait pas remplir le champ nomte du message)
        igrel=num_grel(2*(ima-1)+1)
        if (igrel.eq.0) cycle

        nbma_verif=nbma_verif+1
        numa_verif(nbma_verif)=ima
    enddo


!   3.2 : on verifie les mailles a verifier (cmp par cmp) :
!   -------------------------------------------------------
    do kcmp=1,nbcmp
        nocmp=zk8(jnocmp-1+kcmp)


!       -- Exceptions :
!       ----------------------------------------------------------------
!       E1) PESA_R / ROTA_R sont en general utilises sans preciser les mailles
        if (nomgd.eq.'PESA_R') cycle
        if (nomgd.eq.'ROTA_R') cycle
        if (nomgd.eq.'CAGNPO') cycle

!       E2) Valeurs fournies par le code d'AFFE_CHAR_MECA
        if (nomgd(1:5).eq.'FORC_' .and. nocmp.eq.'REP') cycle
        if (nomgd(1:5).eq.'FORC_' .and. nocmp.eq.'PLAN') cycle
        if (nomgd.eq.'VENTCX_F' .and. nocmp.eq.'FCXP') cycle

!       E3) Valeurs fournies en loucede par le code d'AFFE_CARA_ELEM
        if (nomgd.eq.'CAMASS' .and. nocmp.eq.'C') cycle
        if (nomgd.eq.'CACOQU' .and. nocmp.eq.'KAPPA') cycle
        if (nomgd.eq.'CACOQU' .and. nocmp.eq.'CTOR') cycle
        if (nomgd.eq.'CAORIE' .and. nocmp.eq.'ALPHA') cycle

        if (nomgd.eq.'CINFDI' .and. nocmp(1:3).eq.'REP') cycle
        if (nomgd.eq.'CINFDI' .and. nocmp(1:3).eq.'SYM') cycle

        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'TSEC') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HY1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HZ1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HY2') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'HZ2') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPY1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPY2') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPZ1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EPZ2') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EP1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'EP2') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'R1') cycle
        if (nomgd.eq.'CAGEPO' .and. nocmp.eq.'R2') cycle


        nbmapb=0
        do kma=1,nbma_verif
            ima=numa_verif(kma)
            igrel=num_grel(2*(ima-1)+1)
            if (a_un_sens((igrel-1)*nbcmp+kcmp).eq.1) cycle

            ient=ptma(ima)
            decal=3+2*nbgdmx+nec*(ient-1)
            dg=>desc(decal+1:decal+nec)
            if (.not.exisdg(dg, kcmp)) cycle


!           -- si la cmp est nulle, on n'alarme pas :
!           ------------------------------------------

!           -- on compte les cmps presentes pour pouvoir acceder a la valeur
            ico = 0
            do kcmp2 = 1, kcmp
                if (.not. (exisdg(dg,kcmp2))) cycle
                ico = ico + 1
            enddo
            deb1 = (ient-1)*nbcmp + 1
            iad1 = deb1 - 1 + ico

            if (tsca .eq. 'R') then
                if (zr(jvale-1+iad1).eq.0.d0) cycle
            else if (tsca.eq.'C') then
                if (abs(zc(jvale-1+iad1)).eq.0.d0) cycle
            else if (tsca.eq.'I') then
                if (zi(jvale-1+iad1).eq.0) cycle
            else if (tsca.eq.'L') then
                if (.not.zl(jvale-1+iad1)) cycle
            else if (tsca(1:2).eq.'K8') then
                if (zk8(jvale-1+iad1).eq.' ') cycle
                if (zk8(jvale-1+iad1).eq.'&FOZERO') cycle
            else if (tsca(1:3).eq.'K16') then
                if (zk16(jvale-1+iad1).eq.' ') cycle
                if (zk16(jvale-1+iad1).eq.'&FOZERO') cycle
            else if (tsca(1:3).eq.'K24') then
                if (zk24(jvale-1+iad1).eq.' ') cycle
                if (zk24(jvale-1+iad1).eq.'&FOZERO') cycle
            else
                ASSERT(.false.)
            endif

!           -- si il y a un probleme :
            nbmapb=nbmapb+1
            te=num_grel(2*(ima-1)+2)
            if (nbmapb.eq.1) call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
            if (nbmapb.le.5) list_ma_pb(nbmapb)=ima
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
    enddo
    AS_DEALLOCATE(vi=numa_verif)

999 continue
    AS_DEALLOCATE(vi=a_un_sens)
    AS_DEALLOCATE(vi=num_grel)

    call jedema()
end subroutine
