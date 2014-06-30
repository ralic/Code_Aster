subroutine cteltb(nbma, mesmai, noma, nbval, nkcha,&
                  nkcmp, toucmp, nbcmp, typac, ndim,&
                  nrval, resu, nomtb, nsymb, chpgs,&
                  tych, nival, niord)
    implicit   none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbcmp, ndim, nbval, nbma
    character(len=4) :: tych
    character(len=8) :: typac, noma, resu, nomtb
    character(len=16) :: nsymb
    character(len=19) :: chpgs
    character(len=24) :: nkcha, nkcmp, mesmai, nival, nrval, niord
    logical(kind=1) :: toucmp
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : REMPLISSAGE DE LA TABLE POUR UN CHAM_ELEM OU UNE CARTE
!
!        IN     : NKCHA (K24)  : OBJET DES NOMS DE CHAMP
!                 RESU  (K8)   : NOM DU RESULTAT (SI RESULTAT,SINON ' ')
!                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
!                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
!                 NBCMP (I)    : NOMBRE DE COMPOSANTES LORSQUE
!                                NOM_CMP EST RENSEIGNE, 0 SINON
!                 TYPAC (K8)   : ACCES (ORDRE,MODE,FREQ,INST)
!                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
!                 NOMA   (K8)  : NOM DU MAILLAGE
!                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
!                 NRVAL (K16)  : OBJET DES VALEURS D'ACCES (REELS)
!                 NIVAL (K16)  : OBJET DES VALEURS D'ACCES (ENTIERS)
!                 NIORD (K16)  : NOM D'OBJET DES NUMEROS D'ORDRE
!                 NSYMB (K16)  : NOM SYMBOLIQUE DU CHAMP
!                 TYCH  (K4)   : TYPE DE CHAMP (ELNO,ELEM,ELGA,CART)
!                 CHPGS (K19)  : CHAMP DES COORD DES POINTS DE GAUSS
!                 NBMA  (I)    : NOMBRE DE MAILLES UTILISATEUR
!
!        IN/OUT : NOMTB (K24)  : OBJET TABLE
!
! ----------------------------------------------------------------------
!
    integer :: jcmp, jkcha, jlma, jrval, jival, jniord,   jconx2
    integer ::  jcpgl, jcpgd, i, j,  jcesl, jcesd,  nbmax
    integer :: nbcmpx
    integer :: n,   ima, ipt, ispt, icmp, indma, nbpt, kk
    integer :: nbcmpt, nbspt, inot, kcp, indcmp, iad, ni, nk, nr
    integer :: nbpara,  iret
    character(len=8) :: kma, kno
    complex(kind=8) :: cbid
    character(len=19) :: chames
    character(len=8), pointer :: nom_cmp(:) => null()
    character(len=16), pointer :: table_parak(:) => null()
    integer, pointer :: table_vali(:) => null()
    character(len=16), pointer :: table_valk(:) => null()
    real(kind=8), pointer :: table_valr(:) => null()
    real(kind=8), pointer :: val_cmp(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: cpgv(:) => null()
    integer, pointer :: connex(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
!
!
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- 0. INITIALISATIONS
!      -----------------
    cbid=(0.d0,0.d0)
    chames = '&&CTELTB.CES       '
    call jeveuo(nkcmp, 'L', jcmp)
    call jeveuo(nkcha, 'L', jkcha)
    call jeveuo(mesmai, 'L', jlma)
    call jeveuo(nrval, 'L', jrval)
    call jeveuo(nival, 'L', jival)
    call jeveuo(niord, 'L', jniord)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    if (tych .eq. 'ELGA') then
        call jeveuo(chpgs//'.CESV', 'L', vr=cpgv)
        call jeveuo(chpgs//'.CESL', 'L', jcpgl)
        call jeveuo(chpgs//'.CESD', 'L', jcpgd)
    endif
!     TABLEAU D'ENTIERS DE LA TABLE: ZI(JI)
!     TABLEAU DE REELS DE LA TABLE: ZR(JR)
!     TABLEAU DE CARACTERES DE LA TABLE: ZK16(JK)
!     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
!     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
    AS_ALLOCATE(vr=table_valr, size=250)
    AS_ALLOCATE(vi=table_vali, size=250)
    AS_ALLOCATE(vk16=table_valk, size=250)
!
! --- 1. LECTURE DES CHAMPS ET REMPLISSAGE DE LA TABLE
!      -----------------------------------------------
!
    do 100 i = 1, nbval
!     -- JE NE COMPRENDS PAS LA BOUCLE I=1,NBVAL (J. PELLET)
!
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
!
!
!            -- PASSAGE CHAMP => CHAM_ELEM_S
            if (tych(1:2) .eq. 'EL') then
                call celces(zk24(jkcha+i-1), 'V', chames)
            else if (tych.eq.'CART') then
                call carces(zk24(jkcha+i-1), 'ELEM', ' ', 'V', chames,&
                            ' ', iret)
                ASSERT(iret.eq.0)
            else
                ASSERT(.false.)
            endif
            call jeveuo(chames//'.CESV', 'L', vr=cesv)
            call jeveuo(chames//'.CESL', 'L', jcesl)
            call jeveuo(chames//'.CESD', 'L', jcesd)
            call jeveuo(chames//'.CESC', 'L', vk8=cesc)
!
!             NOMBRE DE MAILLES MAX DU CHAMP : NBMAX
            nbmax=zi(jcesd)
!
!             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
            nbcmpx=zi(jcesd+1)
!
!             NOMBRE DE COMPOSANTES DESIREES : N
            if (toucmp) then
                n=nbcmpx
            else
                n=nbcmp
            endif
!
!             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
            AS_ALLOCATE(vr=val_cmp, size=n)
!
!             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
            AS_ALLOCATE(vk8=nom_cmp, size=n)
!
!            -- ON PARCOURT LES MAILLES
            do 210 ima = 1, nbmax
!
!               - SI LA MAILLE FAIT PARTIE DES MAILLES DESIREES,
!               ON POURSUIT, SINON ON VA A LA MAILLE SUIVANTE:
                indma=indiis(zi(jlma),ima,1,nbma)
                if (indma .eq. 0) goto 210
!
!               NOMBRE DE POINTS DE LA MAILLE IMA : NBPT
                nbpt=zi(jcesd+5+4*(ima-1))
!
!               NOMBRE DE COMPOSANTES PORTEES PAR LES POINTS
!               DE LA MAILLE IMA
                nbcmpt=zi(jcesd+5+4*(ima-1)+2)
!
!               NOMBRE DE SOUS-POINTS PORTES PAR LES POINTS
                nbspt=zi(jcesd+5+4*(ima-1)+1)
!
!               -- ON PARCOURT LES POINTS DE LA MAILLE IMA
                do 220 ipt = 1, nbpt
!
!                 NUMERO DU POINT (DU MAILLAGE GLOBAL): INOT
                    inot = connex(zi(jconx2-1+ima)+ipt-1)
!
!                 -- ON PARCOURT LES SOUS-POINTS DE LA MAILLE IMA
                    do 225 ispt = 1, nbspt
                        kcp=0
!
!                   -- ON PARCOURT LES COMPOSANTES PORTEES
!                   PAR LE POINT IPT
                        do 230 icmp = 1, nbcmpt
!
                            if (.not.toucmp) then
!                        -SI LA COMPOSANTE FAIT PARTIE DES
!                         COMPOSANTES DESIREES, ON POURSUIT,
!                         SINON ON VA A LA COMPOSANTE SUIVANTE
                                indcmp=indik8(zk8(jcmp),cesc(1+&
                                icmp-1), 1,nbcmp)
                                if (indcmp .eq. 0) goto 230
                            endif
!
!                      VALEUR DE LA COMPOSANTE ICMP AU POINT IPT DE
!                      LA MAILLE IMA: ZR(JCESV+IAD-1)
                            call cesexi('C', jcesd, jcesl, ima, ipt,&
                                        ispt, icmp, iad)
                            if (iad .gt. 0) then
                                kcp=kcp+1
                                val_cmp(kcp)=cesv(iad)
                                nom_cmp(kcp)=cesc(icmp)
                            endif
!
230                      continue
                        if (kcp .eq. 0) goto 225
!                   -- POUR NE PAS DEBORDER DES OBJETS (L=250):
                        ASSERT(kcp.le.200)
!
!                   SOIT NI LE NOMBRE DE ENTIERS DE LA TABLE
!                   SOIT NR LE NOMBRE DE REELS DE LA TABLE
!                   SOIT NK LE NOMBRE DE CARACTERES DE LA TABLE
                        ni=1
                        nk=3
                        nr=kcp
                        if (tych .eq. 'ELNO' .or. tych .eq. 'ELGA') nr=nr+ ndim
!
                        if (resu .ne. ' ') then
                            if (typac .eq. 'FREQ' .or. typac .eq. 'INST') then
                                nr=nr+1
                            else if (typac.eq.'MODE') then
                                ni=ni+1
                            endif
                        else
                            ni=0
                            nk=2
                        endif
!
                        if (tych .eq. 'ELNO') then
!                      -- noeud + sous_point
                            nk=nk+1
                            ni=ni+1
                        else if (tych.eq.'ELGA') then
!                      -- point + sous_point
                            ni=ni+2
                        else if (tych.eq.'ELEM') then
!                      -- sous_point
                            ni=ni+1
                        endif
!
!                   ON REMPLIT LES TABLEAUX ZI(JI),ZR(JR),ZK16(JK)
                        kk=0
                        if (typac .eq. 'FREQ' .or. typac .eq. 'INST') then
                            table_valr(kk+1)=zr(jrval+i-1)
                            kk=kk+1
                        endif
                        if (tych .eq. 'ELNO') then
                            do 240 j = 1, ndim
                                table_valr(kk+1)=vale(1+3*(inot-1)+j-1)
                                kk=kk+1
240                          continue
                        else if (tych.eq.'ELGA') then
                            do 241 j = 1, ndim
                                call cesexi('C', jcpgd, jcpgl, ima, ipt,&
                                            ispt, j, iad)
                                if (iad .gt. 0) then
                                    table_valr(kk+1)=cpgv(iad)
                                    kk=kk+1
                                endif
241                          continue
                        endif
                        do 250 j = 1, kcp
                            table_valr(kk+1)=val_cmp(j)
                            kk=kk+1
250                      continue
                        ASSERT(kk .eq. nr)
!
                        kk=0
                        if (resu .ne. ' ') then
                            table_vali(kk+1)=zi(jniord+i-1)
                            kk=kk+1
                        endif
                        if (typac .eq. 'MODE') then
                            table_vali(kk+1)=zi(jival+i-1)
                            kk=kk+1
                        endif
                        if (tych .eq. 'ELGA') then
                            table_vali(kk+1)=ipt
                            kk=kk+1
                        endif
                        if (tych(1:2) .eq. 'EL') then
                            table_vali(kk+1)=ispt
                            kk=kk+1
                        endif
                        ASSERT(kk .eq. ni)
!
                        kk=0
                        if (resu .eq. ' ') then
                            table_valk(kk+1)=zk24(jkcha+i-1)(1:16)
                            kk=kk+1
                        else
                            table_valk(kk+1)=resu
                            kk=kk+1
                            table_valk(kk+1)=nsymb
                            kk=kk+1
                        endif
                        call jenuno(jexnum(noma//'.NOMMAI', ima), kma)
                        table_valk(kk+1)=kma
                        kk=kk+1
                        if (tych .eq. 'ELNO') then
                            call jenuno(jexnum(noma//'.NOMNOE', inot), kno)
                            table_valk(kk+1)=kno
                            kk=kk+1
                        endif
                        ASSERT(kk .eq. nk)
!
!                   TABLEAU DES NOMS DE PARAMETRES DE LA TABLE
                        nbpara=nr+ni+nk
                        AS_ALLOCATE(vk16=table_parak, size=nbpara)
!
                        kk=0
                        if (resu .eq. ' ') then
                            table_parak(kk+1)='CHAM_GD'
                            kk=kk+1
                        else
                            table_parak(kk+1)='RESULTAT'
                            kk=kk+1
                            table_parak(kk+1)='NOM_CHAM'
                            kk=kk+1
                        endif
!
                        if (resu .ne. ' ') then
                            if (typac .ne. 'ORDRE') then
                                table_parak(kk+1)=typac
                                kk=kk+1
                            endif
                            table_parak(kk+1)='NUME_ORDRE'
                            kk=kk+1
                        endif
                        table_parak(kk+1)='MAILLE'
                        kk=kk+1
                        if (tych .eq. 'ELNO') then
                            table_parak(kk+1)='NOEUD'
                            kk=kk+1
                        else if (tych.eq.'ELGA') then
                            table_parak(kk+1)='POINT'
                            kk=kk+1
                        endif
                        if (tych(1:2) .eq. 'EL') then
                            table_parak(kk+1)='SOUS_POINT'
                            kk=kk+1
                        endif
!
!                   -- COORDONNEES :
                        if (tych .eq. 'ELNO' .or. tych .eq. 'ELGA') then
                            table_parak(kk+1)='COOR_X'
                            kk=kk+1
                            if (ndim .ge. 2) then
                                table_parak(kk+1)='COOR_Y'
                                kk=kk+1
                            endif
                            if (ndim .eq. 3) then
                                table_parak(kk+1)='COOR_Z'
                                kk=kk+1
                            endif
                        endif
!
!                   -- COMPOSANTES :
                        do 260 j = 1, kcp
                            table_parak(kk+1)=nom_cmp(j)
                            kk=kk+1
260                      continue
!
                        ASSERT(kk .le. nbpara)
!                       ON AJOUTE LA LIGNE A LA TABLE
                        call tbajli(nomtb, nbpara, table_parak, table_vali, table_valr,&
                                    [cbid], table_valk, 0)
!
                        AS_DEALLOCATE(vk16=table_parak)
225                  continue
!
220              continue
!
210         continue
            AS_DEALLOCATE(vk8=nom_cmp)
            AS_DEALLOCATE(vr=val_cmp)
!
        endif
!
100  end do
!
    AS_DEALLOCATE(vr=table_valr)
    AS_DEALLOCATE(vi=table_vali)
    AS_DEALLOCATE(vk16=table_valk)
!
    call jedema()
!
end subroutine
