subroutine cesqua(nbchs, lichs, lcumul, base, ces3z)
    implicit none
#include "asterf_types.h"
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbchs
    character(len=*) :: lichs(nbchs), ces3z, base
    aster_logical :: lcumul(nbchs)
! ---------------------------------------------------------------------
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
    integer :: jce1k, jce1d, jce1l, jce1c, nbma, n1, k
    integer :: jce3d, jce3l
    integer :: jcmpgd, ichs, icmp, icmp3, ncmp3
    integer :: ncmpmx, ncmp1, icmp1
    integer :: ima, ipt, isp, nbpt, nbsp, iad1, iad3, ncmp
    character(len=8) :: ma, nomgd, nocmp, typces, nomcmp
    character(len=3) :: tsca
    character(len=19) :: ces1, ces3
    aster_logical :: cumul
    integer, pointer :: corr_cmp(:) => null()
    character(len=8), pointer :: licmp(:) => null()
    integer, pointer :: nbcmp(:) => null()
    integer, pointer :: vnbpt(:) => null()
    integer, pointer :: vnbsp(:) => null()
    integer, pointer :: nucmp(:) => null()
    real(kind=8), pointer :: ce1v(:) => null()
    real(kind=8), pointer :: ce3v(:) => null()
    character(len=8), pointer :: ce3c(:) => null()
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
        do ichs = 1, nbchs
            ces1 = lichs(ichs)
            call jeveuo(ces1//'.CESC', 'L', jce1c)
            call jelira(ces1//'.CESC', 'LONMAX', n1)
            do k = 1, n1
                read (zk8(jce1c-1+k) (2:),'(I7)') icmp
                ncmpmx = max(ncmpmx,icmp)
            end do
        end do
!
        call wkvect('&&CESQUA.LISVARI', 'V V K8', ncmpmx, jcmpgd)
        do k = 1, ncmpmx
            nomcmp = 'V'
            call codent(k, 'G', nomcmp(2:8))
            zk8(jcmpgd-1+k) = nomcmp
        end do
!
    endif
!
!     1- QUELQUES VERIFICATIONS SUR LES CHAMPS "IN"
!        + CALCUL DES OBJETS CONTENANT LES NOMBRES DE POINTS
!          ET DE SOUS-POINTS PAR MAILLE
!     --------------------------------------------------------
    AS_ALLOCATE(vi=vnbpt, size=nbma)
    AS_ALLOCATE(vi=vnbsp, size=nbma)
    do ichs = 1, nbchs
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
            do ima = 1, nbma
                vnbpt(ima) = zi(jce1d-1+5+4* (ima-1)+1)
                vnbsp(ima) = zi(jce1d-1+5+4* (ima-1)+2)
            end do
        else
            do ima = 1, nbma
                nbpt = zi(jce1d-1+5+4* (ima-1)+1)
                nbsp = zi(jce1d-1+5+4* (ima-1)+2)
                ncmp = zi(jce1d-1+5+4* (ima-1)+3)
                if (nbpt*nbsp*ncmp .eq. 0) goto 50
!           TEST SUR IDENTITE DU NOMBRE DE POINTS
                ASSERT(vnbpt(ima).eq.nbpt)
!           TEST SUR IDENTITE DU NOMBRE DE SOUS-POINTS
                ASSERT(vnbsp(ima).eq.nbsp)
 50             continue
            end do
        endif
        call jelibe(ces1//'.CESK')
        call jelibe(ces1//'.CESD')
    end do
!
!
!     2- CALCUL DE LA LISTE DES CMPS DE CES3
!     -------------------------------------------
!
!     -- ON "COCHE" LES CMPS PRESENTES DANS LES CES DE LICHS:
    AS_ALLOCATE(vk8=licmp, size=ncmpmx)
    AS_ALLOCATE(vi=nucmp, size=ncmpmx)
    do ichs = 1, nbchs
        ces1 = lichs(ichs)
        call jeveuo(ces1//'.CESK', 'L', jce1k)
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
!
        ncmp1 = zi(jce1d-1+2)
        do icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
!
            icmp = indik8(zk8(jcmpgd),nocmp,1,ncmpmx)
            nucmp(icmp) = 1
        end do
        call jelibe(ces1//'.CESK')
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
    end do
!
    icmp3 = 0
    do icmp = 1, ncmpmx
        if (nucmp(icmp) .eq. 1) then
            icmp3 = icmp3 + 1
            licmp(icmp3) = zk8(jcmpgd-1+icmp)
        endif
    end do
    ncmp3 = icmp3
!
!     3- CALCUL DE L'OBJET CONTENANT LE NOMBRE DE CMPS PAR MAILLE
!     -----------------------------------------------------------
    AS_ALLOCATE(vi=nbcmp, size=nbma)
    AS_ALLOCATE(vi=corr_cmp, size=ncmpmx)
!
    do ichs = 1, nbchs
        ces1 = lichs(ichs)
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
!
        ncmp1 = zi(jce1d-1+2)
        do icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
            icmp3 = indik8(licmp,nocmp,1,ncmp3)
            corr_cmp(icmp1) = icmp3
        end do
!
        do ima = 1, nbma
            icmp1 = zi(jce1d-1+5+4* (ima-1)+3)
            if (icmp1 .eq. 0) goto 110
            ASSERT(icmp1.gt.0)
            icmp3 = corr_cmp(icmp1)
            nbcmp(ima) = max(icmp3,nbcmp(ima))
110         continue
        end do
!
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
    end do
!
!     4- ALLOCATION DE CES3 :
!     --------------------------
    call cescre(base, ces3, typces, ma, nomgd,&
                ncmp3, licmp, vnbpt, vnbsp, nbcmp)
    call jeveuo(ces3//'.CESD', 'L', jce3d)
    call jeveuo(ces3//'.CESC', 'L', vk8=ce3c)
    call jeveuo(ces3//'.CESV', 'E', vr=ce3v)
    call jeveuo(ces3//'.CESL', 'E', jce3l)
!
!     5- RECOPIE DE CES1 DANS CES3 :
!     ------------------------------------------
    do ichs = 1, nbchs
        ces1 = lichs(ichs)
!
        call jeveuo(ces1//'.CESD', 'L', jce1d)
        call jeveuo(ces1//'.CESC', 'L', jce1c)
        call jeveuo(ces1//'.CESV', 'L', vr=ce1v)
        call jeveuo(ces1//'.CESL', 'L', jce1l)
        ncmp1 = zi(jce1d-1+2)
!
        cumul = lcumul(ichs)
!
        do icmp1 = 1, ncmp1
            nocmp = zk8(jce1c-1+icmp1)
            icmp3 = indik8(ce3c,nocmp,1,ncmp3)
            do ima = 1, nbma
                nbpt = zi(jce3d-1+5+4* (ima-1)+1)
                nbsp = zi(jce3d-1+5+4* (ima-1)+2)
                do ipt = 1, nbpt
                    do isp = 1, nbsp
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
                                ce3v(iad3) = ce1v(iad1)** 2
                            else
                                ASSERT(.false.)
                            endif
!
!               -- SI CUMUL DANS UNE VALEUR DEJA AFFECTEE :
                        else
!
                            if (tsca .eq. 'R') then
                                ce3v(iad3) = ce3v(iad3) + ce1v(iad1)**2
                            else
                                ASSERT(.false.)
                            endif
                        endif
!
!
130                     continue
                    end do
                end do
            end do
        end do
!
        call jelibe(ces1//'.CESD')
        call jelibe(ces1//'.CESC')
        call jelibe(ces1//'.CESV')
        call jelibe(ces1//'.CESL')
!
    end do
!
!
    do icmp3 = 1, ncmp3
        do ima = 1, nbma
            nbpt = zi(jce3d-1+5+4*(ima-1)+1)
            nbsp = zi(jce3d-1+5+4*(ima-1)+2)
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    call cesexi('C', jce3d, jce3l, ima, ipt,&
                                isp, icmp3, iad3)
                    if (iad3 .eq. 0) goto 230
!
                    if (tsca .eq. 'R') then
                        ce3v(iad3) = sqrt( ce3v(iad3) )
                    endif
230                 continue
                end do
            end do
        end do
    end do
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
    AS_DEALLOCATE(vi=vnbpt)
    AS_DEALLOCATE(vi=vnbsp)
    AS_DEALLOCATE(vk8=licmp)
    AS_DEALLOCATE(vi=nucmp)
    AS_DEALLOCATE(vi=nbcmp)
    AS_DEALLOCATE(vi=corr_cmp)
!
    call jedema()
end subroutine
