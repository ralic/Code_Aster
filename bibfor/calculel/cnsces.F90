subroutine cnsces(cnsz, typces, cesmoz, mnogaz, base,&
                  cesz)
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
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: cnsz, cesz, base, cesmoz, typces, mnogaz
! ------------------------------------------------------------------
! BUT: TRANSFORMER UN CHAM_NO_S EN CHAM_ELEM_S
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNSZ  IN/JXIN  K19 : SD CHAM_NO_S A TRANSFORMER
! TYPCES IN       K4  : TYPE VOULU POUR LE CHAM_ELEM_S
!                      /'ELEM' /'ELGA' /'ELNO'
! CESMOZ IN/JXIN  K19 :  SD CHAM_ELEM_S "MODELE" POUR CESZ
!       SI TYPCES = 'ELEM' : CESMOZ N'EST PAS UTILISE
!       SI TYPCES  ='ELGA' ON SE SERT DE CESMOZ POUR DETERMINER
!          LE NOMBRE DE POINTS ET DE SOUS-POINTS  DU CHAM_ELEM _S
!       SI TYPCES  ='ELNO' ON SE SERT DE CESMOZ POUR DETERMINER
!          LE NOMBRE DE SOUS-POINTS  DU CHAM_ELEM_S
!
! MNOGAZ IN/JXIN  K19 : SD CHAM_ELEM_S CONTENANT LES MATRICES
!                       DE PASSAGE NOEUD -> GAUSS.
!                       MNOGAZ N'EST UTILISE QUE SI ELNO->ELGA
! ATTENTION :  MNOGAZ EST UN CHAM_ELEM_S AVEC UNE CONVENTION
!              TRES PARTICULIERE  (MAILLE DE REFERENCE)
!              (VOIR ROUTINE MANOPG.F)
!
! CESZ   IN/JXOUT K19 : SD CHAM_ELEM_S RESULTAT
! BASE    IN      K1  : BASE DE CREATION POUR CESZ : G/V/L
!-----------------------------------------------------------------------
!
!  PRINCIPES RETENUS POUR LA CONVERSION :
!
!  1) ON NE TRAITE QUE LES CHAM_NO_S REELS (R8)
!  2)
!    SI  TYPCES='ELEM'
!       ON AFFECTE A LA MAILLE LA MOYENNE ARITHMETIQUE DES NOEUDS
!    SI  TYPCES='ELNO'
!       ON RECOPIE LA VALEUR DU NOEUD GLOBAL SUR LE NOEUD LOCAL
!    SI  TYPCES='ELGA'
!       ON UTILISE LA MATRICE DE PASSAGE NOEUD -> GAUSS.
!
!  3) LES EVENTUELS SOUS-POINTS PORTENT TOUS LES MEMES VALEURS
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ima, ncmp, icmp,  jcnsl
    integer :: jcesd,  jcesl, nbma, iret, nbsp, nbno, ico
    integer :: iad,  nbpt, ipt, ino, nuno, isp, nbpg2, nbno2, iad1
    integer ::   ilcnx1,  nbpg, ipg, imaref
    integer :: mnogal, mnogad
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=19) :: ces, cesmod, cns, mnoga
    real(kind=8) :: v, v1
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: nmnogav(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
    integer, pointer :: connex(:) => null()
    character(len=8), pointer :: cnsk(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    integer, pointer :: cemd(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
    integer, pointer :: vnbpt(:) => null()
    integer, pointer :: vnbsp(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
!
    ces = cesz
    cesmod = cesmoz
    cns = cnsz
!
!
!     1- RECUPERATION D'INFORMATIONS DANS CNS :
!        MA    : NOM DU MAILLAGE
!        NOMGD : NOM DE LA GRANDEUR
!        NCMP  : NOMBRE DE CMPS DANS CNS
!     ------------------------------------------
    call jeveuo(cns//'.CNSK', 'L', vk8=cnsk)
    call jeveuo(cns//'.CNSC', 'L', vk8=cnsc)
    call jeveuo(cns//'.CNSV', 'L', vr=cnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma = cnsk(1)
    nomgd = cnsk(2)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R')
    call jeveuo(ma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jelira(cns//'.CNSC', 'LONMAX', ncmp)
!
!
!     2. CALCUL DES OBJETS  '&&CNSCES.NBPT','&CNSCES.NBSP'
!     -----------------------------------------------------------------
    AS_ALLOCATE(vi=vnbpt, size=nbma)
    AS_ALLOCATE(vi=vnbsp, size=nbma)
!
!
!     -- PAR DEFAUT : NBSP=1
    do ima = 1, nbma
        vnbsp(ima) = 1
    end do
!
    call exisd('CHAM_ELEM_S', cesmod, iret)
    ASSERT((typces.ne.'ELGA') .or. (iret.gt.0))
!
    if (iret .gt. 0) then
        call jeveuo(cesmod//'.CESD', 'L', vi=cemd)
        do ima = 1, nbma
            vnbpt(ima) = cemd(5+4* (ima-1)+1)
            vnbsp(ima) = cemd(5+4* (ima-1)+2)
        end do
    endif
!
    if (typces .eq. 'ELEM') then
        do ima = 1, nbma
            vnbpt(ima) = 1
        end do
    else if (typces.eq.'ELNO') then
        do ima = 1, nbma
            vnbpt(ima) = zi(ilcnx1+ima) - zi(ilcnx1+ima-1)
        end do
    else if (typces.eq.'ELGA') then
!       DEJA FAIT GRACE A CESMOD
    else
        ASSERT(.false.)
    endif
!
!
!     5- CREATION DE CES :
!     ---------------------------------------
    call cescre(base, ces, typces, ma, nomgd,&
                ncmp, cnsc, vnbpt, vnbsp, [-ncmp])
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESV', 'E', vr=cesv)
    call jeveuo(ces//'.CESL', 'E', jcesl)
!
!
!
!     6- REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
!
!
    if (typces .eq. 'ELEM') then
!     --------------------------
        do ima = 1, nbma
            nbpt = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
            do icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
                ico = 0
                do ino = 1, nbno
                    nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                    if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
                end do
                if (ico .ne. nbno) goto 90
!
!         -- CALCUL DE LA MOYENNE ARITHMETIQUE :
                v = 0.d0
                do ino = 1, nbno
                    nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                    if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) then
                        v = v + cnsv((nuno-1)*ncmp+icmp)
                    endif
                end do
                v = v/nbno
!
!
                do ipt = 1, nbpt
                    do isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    isp, icmp, iad)
                        ASSERT(iad.lt.0)
                        zl(jcesl-1-iad) = .true.
                        cesv(1-1-iad) = v
                    end do
                end do
 90             continue
            end do
        end do
!
!
    else if (typces.eq.'ELNO') then
!     --------------------------
        do ima = 1, nbma
            nbpt = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
            ASSERT(nbno.eq.nbpt)
!
            do icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
                ico = 0
                do ino = 1, nbno
                    nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                    if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
                end do
                if (ico .ne. nbno) goto 140
!
                do ino = 1, nbno
                    nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                    if (.not.zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) goto 130
                    v = cnsv((nuno-1)*ncmp+icmp)
                    do isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ino,&
                                    isp, icmp, iad)
                        ASSERT(iad.lt.0)
                        zl(jcesl-1-iad) = .true.
                        cesv(1-1-iad) = v
                    end do
130                 continue
                end do
140             continue
            end do
        end do
!
!
    else if (typces.eq.'ELGA') then
!     --------------------------
        mnoga = mnogaz
        call jeveuo(mnoga//'.CESK', 'L', vk8=cesk)
        call jeveuo(mnoga//'.CESD', 'L', mnogad)
        call jeveuo(mnoga//'.CESL', 'L', mnogal)
        call jeveuo(mnoga//'.CESV', 'L', vr=nmnogav)
        ASSERT(cesk(1).eq.ma)
!
        do ima = 1, nbma
            call cesexi('C', mnogad, mnogal, ima, 1,&
                        1, 1, iad)
            if (iad .le. 0) goto 210
            if (nint(nmnogav(iad)) .gt. 0) then
                imaref=ima
            else
                imaref=-nint(nmnogav(iad))
            endif
            call cesexi('C', mnogad, mnogal, imaref, 1,&
                        1, 1, iad)
            if (iad .le. 0) goto 210
!
            nbno2 = nint(nmnogav(iad))
            nbpg2 = nint(nmnogav(iad+1))
!
            nbpg = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
            if (nbno .ne. nbno2 .and. cnsz .eq. '&&VRCIN1.CNS1') nbno = nbno2
            ASSERT(nbno.eq.nbno2)
            ASSERT(nbpg.eq.nbpg2)
!
            do icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
                ico = 0
                do ino = 1, nbno
                    nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                    if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
                end do
                if (ico .ne. nbno) goto 200
!
                do ipg = 1, nbpg
                    v = 0.d0
                    do ino = 1, nbno
                        nuno = connex(1+zi(ilcnx1-1+ima)-2+ino)
                        v1 = cnsv((nuno-1)*ncmp+icmp)
                        v = v + v1*nmnogav(iad+1+nbno* (ipg-1)+ ino)
                    end do
!
                    do isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ipg,&
                                    isp, icmp, iad1)
                        ASSERT(iad1.lt.0)
                        zl(jcesl-1-iad1) = .true.
                        cesv(1-1-iad1) = v
                    end do
                end do
!
200             continue
            end do
210         continue
        end do
!
    endif
!
!
!     7- MENAGE :
!     -----------
    AS_DEALLOCATE(vi=vnbpt)
    AS_DEALLOCATE(vi=vnbsp)
!
    call jedema()
end subroutine
