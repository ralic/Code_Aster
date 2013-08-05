subroutine xpoco2(malini, dirno, nbno, dirma, nbma,&
                  cns1, cns2, ces1, ces2, cesvi1,&
                  cesvi2, resuco, comps1, comps2)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
!
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/xismec.h"
    character(len=8) :: malini, resuco
    character(len=19) :: cns1, cns2, ces1, ces2, cesvi1, cesvi2
    character(len=19) :: comps1, comps2
    integer :: nbno, dirno(nbno), nbma, dirma(nbma)
!
!   COPIE DES DEPLACMENTS DES NOEUDS DU MAILLAGE MA1
!   CONTENUS DANS LES TABLEAUX D'INDIRECTION DIRMA ET DIRNO
!
!   IN
!       MALINI : NOM DU MAILLAGE SAIN
!       DIRNO : TABLEAU DE CORRESPONDANCE DES NUMEROS DE NOEUDS
!       NBNO  : LONGUEUR DE DIRNO
!       DIRMA : TABLEAU DE CORRESPONDANCE DES NUMEROS DE MAILLES
!       NBMA  : LONGUEUR DE DIRMA
!       CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
!       CES1   : CHAMP_ELEM_S DE CONTRAINTES EN ENTREE
!       RESUCO : NOM DU CONCEPT RESULTAT D'ORIGINE
!       COMPS1 : CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
!   OUT
!       CNS2   : CHAMP_NO_S DU DEPLACEMENT EN SORTIE
!       CES2   : CHAMP_ELEM_S DE CONTRAINTES EN SORTIE
!       COMPS2 : CHAM_ELEM_S DU COMPORTEMENT EN SORTIE
!
!
!
    integer :: ier, i, j, ndim, nbcmp, jcnsd1, jcnsc1, jcnsv1, jcnsv2, jcnsl2
    integer :: jcesv1, jcesd1, jcesl1, jcesv2, jcesd2, jcesl2, iad1, iad2
    integer :: jcviv1, jcvid1, jcvil1, jcviv2, jcvid2, jcvil2
    integer :: ima, npg1, ncmp1, npg2, ncmp2, ipg, icmp, ima2, npgv1, npgv2
    integer :: ncmv1, ncmv2, ndimc, idecv2, idecl2
    logical :: lmeca
    character(len=8) :: k8b
    character(len=16) :: tysd
    integer :: iviex, iret
!
    integer :: jresd1, jresv1, jresl1, iadr1
    integer :: jresd2, jresv2, jresl2, iadr2
!
!
    call jemarq()
    call dismoi('F', 'DIM_GEOM', malini, 'MAILLAGE', ndim,&
                k8b, ier)
!
!     ------------------------------------------------------------------
!                    1.     DEPLACEMENT
!     ------------------------------------------------------------------
    call jeveuo(cns1//'.CNSV', 'L', jcnsv1)
    call jeveuo(cns1//'.CNSD', 'L', jcnsd1)
    call jeveuo(cns1//'.CNSC', 'L', jcnsc1)
    call jeveuo(cns2//'.CNSV', 'E', jcnsv2)
    call jeveuo(cns2//'.CNSL', 'E', jcnsl2)
!
!     NBCMP : NBRE DE CMP MAX PAR NOEUDS DU CHAM_NO_S CNS1
    nbcmp = zi(jcnsd1-1+2)
!
!     VERIF QUE LES 2 PREMIERES COMPOSANTES DU CHAMP DEP1 SONT DX DY
!     OU QUE LA PREMIERE COMPOSANTES DE CE CHAMP EST TEMP
    ASSERT((zk8(jcnsc1-1+1).eq.'DX'.and.zk8(jcnsc1-1+2).eq.'DY') .or. (zk8(jcnsc1-1+1).eq.'TEMP'))
!
    lmeca = xismec()
!     RQ : "NDIMC" CORRESPOND AU NOMBRE DE COMPOSANTE VECTORIELLE DU
!     CHAMP PRIMAL (DEPL EN MECA -> NDIM CMP / TEMP EN THERMIQUE
!     -> 1 CMP)
    if (lmeca) then
        ndimc = ndim
    else
        ndimc = 1
    endif
!
    do 100 i = 1, nbno
        if (dirno(i) .ne. 0) then
            if (lmeca) then
                idecv2 = jcnsv2-1+2*ndimc*(dirno(i)-1)
                idecl2 = jcnsl2-1+2*ndimc*(dirno(i)-1)
            else
                idecv2 = jcnsv2-1+ndimc*(dirno(i)-1)
                idecl2 = jcnsl2-1+ndimc*(dirno(i)-1)
            endif
            do 110 j = 1, ndimc
                zr(idecv2+j)= zr(jcnsv1-1+nbcmp*(i-1)+j)
                zl(idecl2+j)=.true.
110          continue
        endif
100  end do
!
    call gettco(resuco, tysd)
!
    if (tysd(1:9) .ne. 'MODE_MECA' .and. tysd(1:9) .ne. 'EVOL_THER') then
!     ------------------------------------------------------------------
!                    2.      CONTRAINTES
!     ------------------------------------------------------------------
        call jeveuo(ces1//'.CESV', 'L', jcesv1)
        call jeveuo(ces1//'.CESD', 'L', jcesd1)
        call jeveuo(ces1//'.CESL', 'L', jcesl1)
        call jeveuo(ces2//'.CESV', 'E', jcesv2)
        call jeveuo(ces2//'.CESD', 'L', jcesd2)
        call jeveuo(ces2//'.CESL', 'E', jcesl2)
!
        call jeexin(cesvi1//'.CESV', iret)
        if (iret .ne. 0) then
            call jeveuo(cesvi1//'.CESV', 'L', jcviv1)
            call jeveuo(cesvi1//'.CESD', 'L', jcvid1)
            call jeveuo(cesvi1//'.CESL', 'L', jcvil1)
        endif
        iviex = iret
!
        call jeexin(cesvi2//'.CESV', iret)
        if (iret .ne. 0) then
            call jeveuo(cesvi2//'.CESV', 'E', jcviv2)
            call jeveuo(cesvi2//'.CESD', 'L', jcvid2)
            call jeveuo(cesvi2//'.CESL', 'E', jcvil2)
        endif
        iviex = iviex*iret
!
!
        do 10 ima = 1, nbma
            ima2 = dirma(ima)
!
            if (ima2 .eq. 0) goto 10
            npg1 = zi(jcesd1-1+5+4* (ima-1)+1)
            ncmp1 = zi(jcesd1-1+5+4* (ima-1)+3)
!
            npg2 = zi(jcesd2-1+5+4* (ima2-1)+1)
            ncmp2 = zi(jcesd2-1+5+4* (ima2-1)+3)
            ASSERT(npg1.eq.npg2)
            ASSERT(ncmp1.eq.ncmp2)
!
            if (iviex .ne. 0) then
                npgv1 = zi(jcvid1-1+5+4* (ima-1)+1)
                ncmv1 = zi(jcvid1-1+5+4* (ima-1)+3)
                npgv2 = zi(jcvid2-1+5+4* (ima2-1)+1)
                ncmv2 = zi(jcvid2-1+5+4* (ima2-1)+3)
                ASSERT(npg2.eq.npgv2)
                ASSERT(npgv1.eq.npg2)
                ASSERT(ncmv1.le.ncmv2)
            endif
!
            do 20 ipg = 1, npg1
                do 30 icmp = 1, ncmp1
                    call cesexi('C', jcesd1, jcesl1, ima, ipg,&
                                1, icmp, iad1)
                    ASSERT(iad1.gt.0)
                    call cesexi('C', jcesd2, jcesl2, dirma(ima), ipg,&
                                1, icmp, iad2)
                    ASSERT(iad2.gt.0)
                    zl(jcesl2-1+iad2) = .true.
                    zr(jcesv2-1+iad2) = zr(jcesv1-1+iad1)
30              continue
                if (iviex .ne. 0) then
                    do 40 icmp = 1, ncmv1
                        call cesexi('C', jcvid1, jcvil1, ima, ipg,&
                                    1, icmp, iad1)
                        ASSERT(iad1.gt.0)
                        call cesexi('C', jcvid2, jcvil2, dirma(ima), ipg,&
                                    1, icmp, iad2)
                        ASSERT(iad2.lt.0)
                        iad2 = -iad2
                        zl(jcvil2-1+iad2) = .true.
                        zr(jcviv2-1+iad2) = zr(jcviv1-1+iad1)
40                  continue
                endif
20          continue
10      continue
    endif
!
!
!     ------------------------------------------------------------------
!                      3.  COMPORTEMENT
!     ------------------------------------------------------------------
!
!     RECUPERATION DU CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
    call exisd('CHAM_ELEM_S', comps1, iret)
!
!     SI CE CHAMP N'EXISTE PAS ON N'A RIEN A FAIRE
    if (iret .ne. 0) then
!
!       RECUP DES INFOS SUR LE CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
        call jeveuo(comps1//'.CESD', 'L', jresd1)
        call jeveuo(comps1//'.CESV', 'L', jresv1)
        call jeveuo(comps1//'.CESL', 'L', jresl1)
!
!       NB CMP
        nbcmp = zi(jresd1-1+2)
!
!       VERIF QUE LE CHAMP DE SORTIE A BIEN ETE CREE
        call exisd('CHAM_ELEM_S', comps2, iret)
        ASSERT(iret.ne.0)
!
!       RECUP DES INFOS SUR LE CHAM_ELEM_S DU COMPORTEMENT EN SORTIE
        call jeveuo(comps2//'.CESD', 'L', jresd2)
        call jeveuo(comps2//'.CESV', 'E', jresv2)
        call jeveuo(comps2//'.CESL', 'E', jresl2)
!
        do 300 ima = 1, nbma
!
            ima2 = dirma(ima)
!
!         ON ZAPPE LES MAILLES NON CLASSIQUES
            if (ima2 .eq. 0) goto 300
!
            do 310 icmp = 1, nbcmp
!
                call cesexi('C', jresd1, jresl1, ima, 1,&
                            1, icmp, iadr1)
                call cesexi('C', jresd2, jresl2, ima2, 1,&
                            1, icmp, iadr2)
!
                if (iadr1 .gt. 0) then
                    ASSERT(iadr2.lt.0)
                    zk16(jresv2-1-iadr2) = zk16(jresv1-1+iadr1)
                    zl(jresl2-1-iadr2) = .true.
                endif
!
310          continue
300      continue
!
    endif
!
!     ------------------------------------------------------------------
!
    call jedema()
end subroutine
