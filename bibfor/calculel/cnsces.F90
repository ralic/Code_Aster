subroutine cnsces(cnsz, typces, cesmoz, mnogaz, base,&
                  cesz)
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
!
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
    integer :: ima, ibid, ncmp, icmp, jcnsk, jcnsl, jcnsc, jcnsv
    integer :: jcesd, jcesv, jcesl, nbma, iret, nbsp, nbno, ico
    integer :: iad, jnbpt, nbpt, ipt, ino, nuno, isp, nbpg2, nbno2, iad1
    integer :: jcemd, jnbsp, ilcnx1, iacnx1, nbpg, ipg, imaref
    integer :: mnogal, mnogad, mnogav, mnogak
    character(len=1) :: kbid
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=19) :: ces, cesmod, cns, mnoga
    real(kind=8) :: v, v1
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
    call jeveuo(cns//'.CNSK', 'L', jcnsk)
    call jeveuo(cns//'.CNSC', 'L', jcnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma = zk8(jcnsk-1+1)
    nomgd = zk8(jcnsk-1+2)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call assert(tsca.eq.'R')
    call jeveuo(ma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jelira(cns//'.CNSC', 'LONMAX', ncmp, kbid)
!
!
!     2. CALCUL DES OBJETS  '&&CNSCES.NBPT','&CNSCES.NBSP'
!     -----------------------------------------------------------------
    call wkvect('&&CNSCES.NBPT', 'V V I', nbma, jnbpt)
    call wkvect('&&CNSCES.NBSP', 'V V I', nbma, jnbsp)
!
!
!     -- PAR DEFAUT : NBSP=1
    do 10,ima = 1,nbma
    zi(jnbsp-1+ima) = 1
    10 end do
!
    call exisd('CHAM_ELEM_S', cesmod, iret)
    call assert((typces.ne.'ELGA') .or. (iret.gt.0))
!
    if (iret .gt. 0) then
        call jeveuo(cesmod//'.CESD', 'L', jcemd)
        do 20,ima = 1,nbma
        zi(jnbpt-1+ima) = zi(jcemd-1+5+4* (ima-1)+1)
        zi(jnbsp-1+ima) = zi(jcemd-1+5+4* (ima-1)+2)
20      continue
    endif
!
    if (typces .eq. 'ELEM') then
        do 30,ima = 1,nbma
        zi(jnbpt-1+ima) = 1
30      continue
    else if (typces.eq.'ELNO') then
        do 40,ima = 1,nbma
        zi(jnbpt-1+ima) = zi(ilcnx1+ima) - zi(ilcnx1+ima-1)
40      continue
    else if (typces.eq.'ELGA') then
!       DEJA FAIT GRACE A CESMOD
    else
        call assert(.false.)
    endif
!
!
!     5- CREATION DE CES :
!     ---------------------------------------
    call cescre(base, ces, typces, ma, nomgd,&
                ncmp, zk8(jcnsc), zi(jnbpt), zi(jnbsp), -ncmp)
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESV', 'E', jcesv)
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
        do 100,ima = 1,nbma
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
        do 90 icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
            ico = 0
            do 50,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
50          continue
            if (ico .ne. nbno) goto 90
!
!         -- CALCUL DE LA MOYENNE ARITHMETIQUE :
            v = 0.d0
            do 60,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) then
                v = v + zr(jcnsv-1+ (nuno-1)*ncmp+icmp)
            endif
60          continue
            v = v/nbno
!
!
            do 80,ipt = 1,nbpt
            do 70,isp = 1,nbsp
            call cesexi('C', jcesd, jcesl, ima, ipt,&
                        isp, icmp, iad)
            call assert(iad.lt.0)
            zl(jcesl-1-iad) = .true.
            zr(jcesv-1-iad) = v
70          continue
80          continue
90      continue
100      continue
!
!
    else if (typces.eq.'ELNO') then
!     --------------------------
        do 150,ima = 1,nbma
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
        call assert(nbno.eq.nbpt)
!
        do 140 icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
            ico = 0
            do 110,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
110          continue
            if (ico .ne. nbno) goto 140
!
            do 130,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            if (.not.zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) goto 130
            v = zr(jcnsv-1+ (nuno-1)*ncmp+icmp)
            do 120,isp = 1,nbsp
            call cesexi('C', jcesd, jcesl, ima, ino,&
                        isp, icmp, iad)
            call assert(iad.lt.0)
            zl(jcesl-1-iad) = .true.
            zr(jcesv-1-iad) = v
120          continue
130          continue
140      continue
150      continue
!
!
    else if (typces.eq.'ELGA') then
!     --------------------------
        mnoga = mnogaz
        call jeveuo(mnoga//'.CESK', 'L', mnogak)
        call jeveuo(mnoga//'.CESD', 'L', mnogad)
        call jeveuo(mnoga//'.CESL', 'L', mnogal)
        call jeveuo(mnoga//'.CESV', 'L', mnogav)
        call assert(zk8(mnogak).eq.ma)
!
        do 210,ima = 1,nbma
        call cesexi('C', mnogad, mnogal, ima, 1,&
                    1, 1, iad)
        if (iad .le. 0) goto 210
        if (nint(zr(mnogav-1+iad)) .gt. 0) then
            imaref=ima
        else
            imaref=-nint(zr(mnogav-1+iad))
        endif
        call cesexi('C', mnogad, mnogal, imaref, 1,&
                    1, 1, iad)
        if (iad .le. 0) goto 210
!
        nbno2 = nint(zr(mnogav-1+iad))
        nbpg2 = nint(zr(mnogav-1+iad+1))
!
        nbpg = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
        if (nbno .ne. nbno2 .and. cnsz .eq. '&&VRCIN1.CNS1') nbno = nbno2
        call assert(nbno.eq.nbno2)
        call assert(nbpg.eq.nbpg2)
!
        do 200 icmp = 1, ncmp
!
!           - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
            ico = 0
            do 160,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            if (zl(jcnsl-1+ (nuno-1)*ncmp+icmp)) ico = ico + 1
160          continue
            if (ico .ne. nbno) goto 200
!
            do 190,ipg = 1,nbpg
            v = 0.d0
            do 170,ino = 1,nbno
            nuno = zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            v1 = zr(jcnsv-1+ (nuno-1)*ncmp+icmp)
            v = v + v1*zr(mnogav-1+iad+1+nbno* (ipg-1)+ ino)
170          continue
!
            do 180,isp = 1,nbsp
            call cesexi('C', jcesd, jcesl, ima, ipg,&
                        isp, icmp, iad1)
            call assert(iad1.lt.0)
            zl(jcesl-1-iad1) = .true.
            zr(jcesv-1-iad1) = v
180          continue
190          continue
!
200      continue
210      continue
!
    endif
!
!
!     7- MENAGE :
!     -----------
    call jedetr('&&CNSCES.NBPT')
    call jedetr('&&CNSCES.NBSP')
!
    call jedema()
end subroutine
