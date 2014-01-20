subroutine cesces(cesa, typces, cesmoz, mnogaz, celfpz,&
                  base, cesb)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesgno.h"
#include "asterfort/copisd.h"
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
    character(len=*) :: cesa, cesb, base, cesmoz, typces, mnogaz, celfpz
! ------------------------------------------------------------------
! BUT: TRANSFORMER UN CHAM_ELEM_S EN CHAM_ELEM_S D'UN AUTRE TYPE
!      ELNO -> ELGA , ELEM -> ELNO , ...
! ------------------------------------------------------------------
!     ARGUMENTS:
! CESA  IN/JXIN  K19 : SD CHAM_ELEM_S A TRANSFORMER
!
! TYPCES IN       K4  : TYPE VOULU POUR LE NOUVEAU CHAM_ELEM_S
!                      /'ELEM' /'ELGA' /'ELNO'
!
! CESMOZ IN/JXIN  K19 :  SD CHAM_ELEM_S "MODELE" POUR CESZ
!       SI TYPCES  ='ELGA' ON SE SERT DE CESMOZ POUR DETERMINER
!          LE NOMBRE DE POINTS DE GAUSS DE CESB
!
! MNOGAZ IN/JXIN  K19 :
!    SD CHAM_ELEM_S CONTENANT LES MATRICES DE PASSAGE NOEUD -> GAUSS.
!    CET OBJET N'EST UTILISE QUE SI ELNO -> ELGA
!    CET OBJET EST OBTENU PAR LA ROUTINE MANOPG.F
! ATTENTION :  MNOGAZ EST UN CHAM_ELEM_S AVEC UNE CONVENTION
!              TRES PARTICULIERE  (MAILLE DE REFERENCE)
!              (VOIR ROUTINE MANOPG.F)
!
! CELFPZ IN/JXIN  K24 :
!    NOM DE L'OBJET DECRIVANT LES FAMILLES DE P.G. DE CESA (OU ' ')
!    CET OBJET N'EST UTILISE QUE SI ELGA -> ELNO
!    CET OBJET EST OBTENU PAR LA ROUTINE CELFPG.F
!
!
! BASE    IN      K1  : BASE DE CREATION POUR CESB : G/V
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ima, ncmp, icmp
    integer :: jcesd, jcesv, jcesl, nbma, iret, nbsp, nbno, ico
    integer :: iad,  ino, isp, nbpg2, nbno2, iad1
    integer ::   ilcnx1, iacnx1, nbpg, ipg
    integer :: mnogal, mnogad,   nbno1, imaref
    integer ::  jces1d, jces1l, jces1v
    integer :: nbpt, nbpt1, nbsp1, ipt, ipt1, nbv
    character(len=4) :: typce1
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=19) :: ces1, cesmod, ces2, mnoga
    character(len=24) :: celfpg
    real(kind=8) :: vr, v1r
    complex(kind=8) :: vc, v1c
    integer, pointer :: cemd(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    character(len=8), pointer :: bref(:) => null()
    character(len=8), pointer :: ces1k(:) => null()
    character(len=8), pointer :: nmnogak(:) => null()
    real(kind=8), pointer :: nmnogav(:) => null()
    integer, pointer :: nbcmp(:) => null()
    integer, pointer :: vnbpt(:) => null()
    integer, pointer :: vnbsp(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    ces1 = cesa
    ces2 = cesb
    cesmod = cesmoz
    mnoga = mnogaz
    celfpg=celfpz
!
!
!     1. RECUPERATION DE :
!        MA     : NOM DU MAILLAGE
!        NOMGD  : NOM DE LA GRANDEUR
!        TYPCE1 : TYPE DE CES1 : ELGA/ELNO/ELEM
!        NCMP   : NOMBRE DE CMPS DE CES1
!        TSCA   : TYPE SCALAIRE DE LA GRANDEUR : R/C/I ...
!        NBMA   : NOMBRE DE MAILLES DU MAILLAGE
!        ILCNX1,IACNX1   : ADRESSES DE LA CONNECTIVITE DU MAILLAGE
!     --------------------------------------------------------------
    call exisd('CHAM_ELEM_S', ces1, iret)
    ASSERT(iret.gt.0)
    call jeveuo(ces1//'.CESK', 'L', vk8=ces1k)
    call jeveuo(ces1//'.CESC', 'L', vk8=cesc)
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESV', 'L', jces1v)
    call jeveuo(ces1//'.CESL', 'L', jces1l)
    ma = ces1k(1)
    nomgd = ces1k(2)
    typce1 = ces1k(3)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call jeveuo(ma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jelira(ces1//'.CESC', 'LONMAX', ncmp)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!
!     2. SI C'EST FACILE, ON LE FAIT ... :
!     ------------------------------------------------
    if (typce1 .eq. typces) then
        call copisd('CHAM_ELEM_S', base, ces1, ces2)
        goto 180
    endif
    ASSERT(tsca.eq.'R'.or.tsca.eq.'C')
!
!
!
!     3. VERIFICATIONS :
!     ---------------------------
    if ((typce1.eq.'ELNO') .and. (typces.eq.'ELGA')) then
        call exisd('CHAM_ELEM_S', mnoga, iret)
        ASSERT(iret.gt.0)
        call jeveuo(mnoga//'.CESK', 'L', vk8=bref)
        ASSERT(ma.eq.bref(1))
    endif
!
!
!     4. CALCUL DES OBJETS  '.NBPT','.NBSP' ET '.NBCMP'
!     -----------------------------------------------------------------
    AS_ALLOCATE(vi=vnbpt, size=nbma)
    AS_ALLOCATE(vi=vnbsp, size=nbma)
    AS_ALLOCATE(vi=nbcmp, size=nbma)
!
!
    if (typces .eq. 'ELEM') then
        do ima = 1, nbma
            vnbpt(ima) = 1
            vnbsp(ima) = 1
            nbcmp(ima) = zi(jces1d-1+5+4* (ima-1)+3)
        end do
!
    else if (typces.eq.'ELNO') then
        do ima = 1, nbma
            vnbpt(ima) = zi(ilcnx1+ima) - zi(ilcnx1+ima-1)
            vnbsp(ima) = zi(jces1d-1+5+4* (ima-1)+2)
            nbcmp(ima) = zi(jces1d-1+5+4* (ima-1)+3)
        end do
!
    else if (typces.eq.'ELGA') then
        call exisd('CHAM_ELEM_S', cesmod, iret)
!       TEST ARGUMENT CESMOD OBLIGATOIRE
        ASSERT(iret.gt.0)
        call jeveuo(cesmod//'.CESD', 'L', vi=cemd)
        do ima = 1, nbma
            vnbpt(ima) = cemd(5+4* (ima-1)+1)
            vnbsp(ima) = zi(jces1d-1+5+4* (ima-1)+2)
            nbcmp(ima) = zi(jces1d-1+5+4* (ima-1)+3)
        end do
!
    else
        ASSERT(.false.)
    endif
!
!
!     5. CREATION DE CES2 :
!     ---------------------------------------
    call cescre(base, ces2, typces, ma, nomgd,&
                ncmp, cesc, vnbpt, vnbsp,nbcmp)
!
    call jeveuo(ces2//'.CESD', 'L', jcesd)
    call jeveuo(ces2//'.CESV', 'E', jcesv)
    call jeveuo(ces2//'.CESL', 'E', jcesl)
!
!
!
!     6- REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
!
!
    if ((typce1.eq.'ELNO') .and. (typces.eq.'ELGA')) then
!     ------------------------------------------------------
        mnoga = mnogaz
        call jeveuo(mnoga//'.CESK', 'L', vk8=nmnogak)
        call jeveuo(mnoga//'.CESD', 'L', mnogad)
        call jeveuo(mnoga//'.CESL', 'L', mnogal)
        call jeveuo(mnoga//'.CESV', 'L', vr=nmnogav)
        ASSERT(nmnogak(1).eq.ma)
!
        do ima = 1, nbma
            call cesexi('C', mnogad, mnogal, ima, 1,&
                        1, 1, iad)
            if (iad .le. 0) goto 90
            if (nint(nmnogav(iad)) .gt. 0) then
                imaref=ima
            else
                imaref=-nint(nmnogav(iad))
            endif
            call cesexi('C', mnogad, mnogal, imaref, 1,&
                        1, 1, iad)
            if (iad .le. 0) goto 90
!
            nbno2 = nint(nmnogav(iad))
            nbpg2 = nint(nmnogav(iad+1))
!
            nbpg = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            nbno = zi(ilcnx1+ima) - zi(ilcnx1-1+ima)
!
            nbno1 = zi(jces1d-1+5+4* (ima-1)+1)
!
            ASSERT(nbno.eq.nbno1)
            ASSERT(nbno.eq.nbno2)
            ASSERT(nbpg.eq.nbpg2)
!
            do icmp = 1, ncmp
                do isp = 1, nbsp
!
!             - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
                    ico = 0
                    do ino = 1, nbno
                        call cesexi('C', jces1d, jces1l, ima, ino,&
                                    isp, icmp, iad1)
                        if (iad1 .gt. 0) ico = ico + 1
                    end do
                    if (ico .ne. nbno) goto 70
!
                    if (tsca .eq. 'R') then
                        do ipg = 1, nbpg
                            vr = 0.d0
                            do ino = 1, nbno
                                call cesexi('C', jces1d, jces1l, ima, ino,&
                                            isp, icmp, iad1)
                                v1r = zr(jces1v-1+iad1)
                                vr = vr + v1r*nmnogav(iad+1+nbno* (ipg-1)+ino)
                            end do
!
                            call cesexi('C', jcesd, jcesl, ima, ipg,&
                                        isp, icmp, iad1)
                            ASSERT(iad1.lt.0)
                            zl(jcesl-1-iad1) = .true.
                            zr(jcesv-1-iad1) = vr
                        end do
!
                    else if (tsca.eq.'C') then
                        do ipg = 1, nbpg
                            vc = dcmplx(0.d0,0.d0)
                            do ino = 1, nbno
                                call cesexi('C', jces1d, jces1l, ima, ino,&
                                            isp, icmp, iad1)
                                v1c = zc(jces1v-1+iad1)
                                vc = vc + v1c*nmnogav(iad+1+nbno* (ipg-1)+ino)
                            end do
!
                            call cesexi('C', jcesd, jcesl, ima, ipg,&
                                        isp, icmp, iad1)
                            ASSERT(iad1.lt.0)
                            zl(jcesl-1-iad1) = .true.
                            zc(jcesv-1-iad1) = vc
                        end do
                    endif
 70                 continue
                end do
!
            end do
 90         continue
        end do
!
!
    else if ((typce1.eq.'ELEM') .and. (typces(1:2).eq.'EL')) then
!     ------------------------------------------------------
        do ima = 1, nbma
            nbpt = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp1 = zi(jces1d-1+5+4* (ima-1)+2)
!
            do icmp = 1, ncmp
                do isp = 1, nbsp1
                    call cesexi('C', jces1d, jces1l, ima, 1,&
                                isp, icmp, iad1)
                    if (iad1 .le. 0) goto 110
!
                    do ipt = 1, nbpt
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    isp, icmp, iad)
                        ASSERT(iad.lt.0)
                        zl(jcesl-1-iad) = .true.
                        if (tsca .eq. 'R') then
                            zr(jcesv-1-iad) = zr(jces1v-1+iad1)
                        else if (tsca.eq.'C') then
                            zc(jcesv-1-iad) = zc(jces1v-1+iad1)
                        endif
                    end do
110                 continue
                end do
!
            end do
        end do
!
!
    else if ((typce1(1:2).eq.'EL') .and. (typces.eq.'ELEM')) then
!     ------------------------------------------------------
        do ima = 1, nbma
            nbpt1 = zi(jces1d-1+5+4* (ima-1)+1)
            nbsp1 = zi(jces1d-1+5+4* (ima-1)+2)
            if ((nbpt1*nbsp1) .eq. 0) goto 170
!
            do icmp = 1, ncmp
                do isp = 1, nbsp1
                    if (tsca .eq. 'R') then
                        vr = 0.d0
                    else if (tsca.eq.'C') then
                        vc = dcmplx(0.d0,0.d0)
                    endif
                    nbv = 0
                    do ipt1 = 1, nbpt1
                        call cesexi('C', jces1d, jces1l, ima, ipt1,&
                                    isp, icmp, iad1)
                        if (iad1 .le. 0) goto 140
                        nbv = nbv + 1
                        if (tsca .eq. 'R') then
                            vr = vr + zr(jces1v-1+iad1)
                        else if (tsca.eq.'C') then
                            vc = vc + zc(jces1v-1+iad1)
                        endif
140                     continue
                    end do
!
!             -- ON N'AFFECTE DE VALEUR A LA MAILLE QUE SI TOUS
!                LES POINTS ONT CONTRIBUE :
                    if (nbv .eq. nbpt1) then
                        call cesexi('C', jcesd, jcesl, ima, 1,&
                                    isp, icmp, iad)
                        ASSERT(iad.lt.0)
                        zl(jcesl-1-iad) = .true.
                        if (tsca .eq. 'R') then
                            zr(jcesv-1-iad) = vr/dble(nbv)
                        else if (tsca.eq.'C') then
                            zc(jcesv-1-iad) = vc/dble(nbv)
                        endif
                    endif
                end do
            end do
170         continue
        end do
!
!
    else if ((typce1.eq.'ELGA') .and. (typces.eq.'ELNO')) then
!     ------------------------------------------------------
        call cesgno(ces1, celfpg, ces2)
!
!
    else
!       CAS NON ENCORE PROGRAMME ...
        ASSERT(.false.)
    endif
!
!
!     7- MENAGE :
!     -----------
    AS_DEALLOCATE(vi=vnbpt)
    AS_DEALLOCATE(vi=vnbsp)
    AS_DEALLOCATE(vi=nbcmp)
!
180 continue
!
    call jedema()
end subroutine
