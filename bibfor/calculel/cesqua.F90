subroutine cesqua(nbchs, lichs, lcumul, base, ces3z)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
!
    integer :: nbchs
    character(len=*) :: lichs(nbchs), ces3z, base
    logical :: lcumul(nbchs)
! ---------------------------------------------------------------------
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
! BUT: COPIE DE CESFUS MAIS ON SOMME QUADRATIQUEMENT
! ---------------------------------------------------------------------
!     ARGUMENTS:
! NBCHS   IN       I      : NOMBRE DE CHAM_ELEM_S A FUSIONNER
! LICHS   IN/JXIN  V(K19) : LISTE DES SD CHAM_ELEM_S A FUSIONNER
! LCUMUL  IN       V(L)   : V(I) =.TRUE. => ON ADDITIONNE LE CHAMP I
!                         : V(I) =.FALSE. => ON SURCHARGE LE CHAMP I
! CES3Z   IN/JXOUT K19 : SD CHAM_ELEM_S RESULTAT
! BASE    IN       K1  : BASE DE CREATION POUR CES3Z : G/V/L
!
! REMARQUES :
!
!- LES CHAM_ELEM_S DE LICHS DOIVENT ETRE DE LA MEME GRANDEUR,S'APPUYER
!  SUR LE MEME MAILLAGE ET ETRE DE MEME TYPE (CART/ELGA/ELNO).
!  DANS TOUS LES CHAM_ELEM_S, CHAQUE MAILLE DOIT AVOIR LE MEME
!  NOMBRE DE POINTS (NOEUD OU GAUSS) ET LE MEME NOMBRE DE SOUS-POINTS.
!
!- L'ORDRE DES CHAM_ELEM_S DANS LICHS EST IMPORTANT :
!  LES CHAM_ELEM_S SE SURCHARGENT LES UNS LES AUTRES
!
!- ON PEUT APPELER CETTE ROUTINE MEME SI CES3Z APPARTIENT
!  A LA LISTE LICHS (CHAM_ELEM_S IN/OUT)
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jce1k, jce1d, jce1v, jce1l, jce1c, nbma, n1, k
    integer :: jce3d, jce3v, jce3l, jce3c
    integer ::  jcmpgd, jlicmp, ichs, icmp, icmp3, ncmp3
    integer :: ncmpmx, ncmp1, icmp1, jnucmp, jnbpt, jnbsp, jnbcmp, jcrcmp
    integer :: ima, ipt, isp, nbpt, nbsp, iad1, iad3, ncmp
    character(len=8) :: ma, nomgd, nocmp, typces, nomcmp
    character(len=3) :: tsca
    character(len=19) :: ces1, ces3
    logical :: cumul
!     ------------------------------------------------------------------
    call jemarq()
!
!
!     -- POUR NE PAS RISQUER D'ECRASER UN CHAM_ELEM_S "IN",
!        ON CREE CES3 SOUS UN NOM TEMPORAIRE :
    ces3 = '&&CESQUA.CES3'
    ASSERT(nbchs.gt.0)
!
    ces1 = lichs(1)
!
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
!
    ma = zk8(jce1k-1+1)
    nomgd = zk8(jce1k-1+2)
    typces = zk8(jce1k-1+3)
!
    nbma = zi(jce1d-1+1)
    ncmp1 = zi(jce1d-1+2)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!     -- RECUPERATION DE LA LISTE DES CMPS DU CATALOGUE :
!        POUR LA GRANDEUR VARI_* , IL FAUT CONSTITUER :(V1,V2,...,VN)
!     ---------------------------------------------------------------
    if (nomgd(1:5) .ne. 'VARI_') then
        call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
    else
        ncmpmx = 0
        do 20 ichs = 1, nbchs
            ces1 = lichs(ichs)
            call jeveuo(ces1//'.CESC', 'L', jce1c)
            call jelira(ces1//'.CESC', 'LONMAX', n1)
            do 10 k = 1, n1
                read (zk8(jce1c-1+k) (2:),'(I7)') icmp
                ncmpmx = max(ncmpmx,icmp)
 10         continue
 20     continue
!
        call wkvect('&&CESQUA.LISVARI', 'V V K8', ncmpmx, jcmpgd)
        do 30 k = 1, ncmpmx
            nomcmp = 'V'
            call codent(k, 'G', nomcmp(2:8))
            zk8(jcmpgd-1+k) = nomcmp
 30     continue
!
    endif
!
!     1- QUELQUES VERIFICATIONS SUR LES CHAMPS "IN"
!        + CALCUL DES OBJETS CONTENANT LES NOMBRES DE POINTS
!          ET DE SOUS-POINTS PAR MAILLE
!     --------------------------------------------------------
    call wkvect('&&CESQUA.NBPT', 'V V I', nbma, jnbpt)
    call wkvect('&&CESQUA.NBSP', 'V V I', nbma, jnbsp)
    do 60 ichs = 1, nbchs
        ces1 = lichs(ichs)
        call jeveuo(ces1//'.CESK', 'L', jce1k)
        call jeveuo(ces1//'.CESD', 'L', jce1d)
!
!       TEST SUR IDENTITE DES 2 MAILLAGES
        ASSERT(ma.eq.zk8(jce1k-1+1))
!       TEST SUR IDENTITE DES 2 GRANDEURS
        ASSERT(nomgd.eq.zk8(jce1k-1+2))
!       TEST SUR IDENTITE DES 2 TYPES (CART/ELNO/ELGA)
        ASSERT(typces.eq.zk8(jce1k-1+3))
!
        if (ichs .eq. 1) then
            do 40 ima = 1, nbma
                zi(jnbpt-1+ima) = zi(jce1d-1+5+4* (ima-1)+1)
                zi(jnbsp-1+ima) = zi(jce1d-1+5+4* (ima-1)+2)
 40         continue
        else
            do 50 ima = 1, nbma
                nbpt = zi(jce1d-1+5+4* (ima-1)+1)
                nbsp = zi(jce1d-1+5+4* (ima-1)+2)
                ncmp = zi(jce1d-1+5+4* (ima-1)+3)
                if (nbpt*nbsp*ncmp .eq. 0) goto 50
!           TEST SUR IDENTITE DU NOMBRE DE POINTS
                ASSERT(zi(jnbpt-1+ima).eq.nbpt)
!           TEST SUR IDENTITE DU NOMBRE DE SOUS-POINTS
                ASSERT(zi(jnbsp-1+ima).eq.nbsp)
 50         continue
        endif
        call jelibe(ces1//'.CESK')
        call jelibe(ces1//'.CESD')
 60 end do
!
!
!     2- CALCUL DE LA LISTE DES CMPS DE CES3
!     -------------------------------------------
!
!     -- ON "COCHE" LES CMPS PRESENTES DANS LES CES DE LICHS:
    call wkvect('&&CESQUA.LICMP', 'V V K8', ncmpmx, jlicmp)
    call wkvect('&&CESQUA.NUCMP', 'V V I', ncmpmx, jnucmp)
    do 80 ichs = 1, nbchs
        ces1 = lichs(ichs)
        call jeveuo(ces1//'.CESK', 'L', jce1k)
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
!
        ncmp1 = zi(jce1d-1+2)
        do 70 icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
!
            icmp = indik8(zk8(jcmpgd),nocmp,1,ncmpmx)
            zi(jnucmp-1+icmp) = 1
 70     continue
        call jelibe(ces1//'.CESK')
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
 80 end do
!
    icmp3 = 0
    do 90 icmp = 1, ncmpmx
        if (zi(jnucmp-1+icmp) .eq. 1) then
            icmp3 = icmp3 + 1
            zk8(jlicmp-1+icmp3) = zk8(jcmpgd-1+icmp)
        endif
 90 end do
    ncmp3 = icmp3
!
!     3- CALCUL DE L'OBJET CONTENANT LE NOMBRE DE CMPS PAR MAILLE
!     -----------------------------------------------------------
    call wkvect('&&CESQUA.NBCMP', 'V V I', nbma, jnbcmp)
    call wkvect('&&CESQUA.CORR_CMP', 'V V I', ncmpmx, jcrcmp)
!
    do 120 ichs = 1, nbchs
        ces1 = lichs(ichs)
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
!
        ncmp1 = zi(jce1d-1+2)
        do 100 icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
            icmp3 = indik8(zk8(jlicmp),nocmp,1,ncmp3)
            zi(jcrcmp-1+icmp1) = icmp3
100     continue
!
        do 110 ima = 1, nbma
            icmp1 = zi(jce1d-1+5+4* (ima-1)+3)
            if (icmp1 .eq. 0) goto 110
            ASSERT(icmp1.gt.0)
            icmp3 = zi(jcrcmp-1+icmp1)
            zi(jnbcmp-1+ima) = max(icmp3,zi(jnbcmp-1+ima))
110     continue
!
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
120 end do
!
!     4- ALLOCATION DE CES3 :
!     --------------------------
    call cescre(base, ces3, typces, ma, nomgd,&
                ncmp3, zk8(jlicmp), zi(jnbpt), zi(jnbsp), zi(jnbcmp))
    call jeveuo(ces3//'.CESD', 'L', jce3d)
    call jeveuo(ces3//'.CESC', 'L', jce3c)
    call jeveuo(ces3//'.CESV', 'E', jce3v)
    call jeveuo(ces3//'.CESL', 'E', jce3l)
!
!     5- RECOPIE DE CES1 DANS CES3 :
!     ------------------------------------------
    do 170 ichs = 1, nbchs
        ces1 = lichs(ichs)
!
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
        call jeveuo(ces1//'.CESV', 'L', jce1v)
        call jeveuo(ces1//'.CESL', 'L', jce1l)
        ncmp1 = zi(jce1d-1+2)
!
        cumul = lcumul(ichs)
!
        do 160 icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
            icmp3 = indik8(zk8(jce3c),nocmp,1,ncmp3)
            do 150 ima = 1, nbma
                nbpt = zi(jce3d-1+5+4* (ima-1)+1)
                nbsp = zi(jce3d-1+5+4* (ima-1)+2)
                do 140 ipt = 1, nbpt
                    do 130 isp = 1, nbsp
                        call cesexi('C', jce1d, jce1l, ima, ipt,&
                                    isp, icmp1, iad1)
                        call cesexi('C', jce3d, jce3l, ima, ipt,&
                                    isp, icmp3, iad3)
                        if (iad1 .le. 0) goto 130
!
                        ASSERT(iad3.ne.0)
!
!               -- SI AFFECTATION :
                        if ((.not.cumul) .or. (iad3.lt.0)) then
                            iad3 = abs(iad3)
                            zl(jce3l-1+iad3) = .true.
!
                            if (tsca .eq. 'R') then
                                zr(jce3v-1+iad3) = zr(jce1v-1+iad1)** 2
                            else
                                ASSERT(.false.)
                            endif
!
!               -- SI CUMUL DANS UNE VALEUR DEJA AFFECTEE :
                        else
!
                            if (tsca .eq. 'R') then
                                zr(jce3v-1+iad3) = zr(jce3v-1+iad3) + zr(jce1v-1+iad1)**2
                            else
                                ASSERT(.false.)
                            endif
                        endif
!
!
130                 continue
140             continue
150         continue
160     continue
!
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
        call jelibe(ces1//'.CESV')
        call jelibe(ces1//'.CESL')
!
170 end do
!
!
    do 200 icmp3 = 1, ncmp3
        do 210 ima = 1, nbma
            nbpt = zi(jce3d-1+5+4*(ima-1)+1)
            nbsp = zi(jce3d-1+5+4*(ima-1)+2)
            do 220 ipt = 1, nbpt
                do 230 isp = 1, nbsp
                    call cesexi('C', jce3d, jce3l, ima, ipt,&
                                isp, icmp3, iad3)
                    if (iad3 .eq. 0) goto 230
!
                    if (tsca .eq. 'R') then
                        zr(jce3v-1+iad3) = sqrt( zr(jce3v-1+iad3) )
                    endif
230             continue
220         continue
210     continue
200 end do
!
!     6- RECOPIE DE LA SD TEMPORAIRE DANS LE RESULTAT :
!     -------------------------------------------------
    call copisd('CHAM_ELEM_S', base, ces3, ces3z)
!
!
!     7- MENAGE :
!     -----------
    call detrsd('CHAM_ELEM_S', ces3)
    call jedetr('&&CESQUA.LISVARI')
    call jedetr('&&CESQUA.NBPT')
    call jedetr('&&CESQUA.NBSP')
    call jedetr('&&CESQUA.LICMP')
    call jedetr('&&CESQUA.NUCMP')
    call jedetr('&&CESQUA.NBCMP')
    call jedetr('&&CESQUA.CORR_CMP')
!
    call jedema()
end subroutine
