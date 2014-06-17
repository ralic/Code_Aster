subroutine xpoco2(malini, dirno, nbno, dirma, nbma,&
                  cns1, cns2, ces1, ces2, cesvi1,&
                  cesvi2, resuco, comps1, comps2, pre1)
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
!
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
    integer :: i, j, ndim, nbcmp,    jcnsv2, jcnsl2
    integer ::  jcesd1, jcesl1,  jcesd2, jcesl2, iad1, iad2
    integer ::  jcvid1, jcvil1,  jcvid2, jcvil2
    integer :: ima, npg1, ncmp1, npg2, ncmp2, ipg, icmp, ima2, npgv1, npgv2
    integer :: ncmv1, ncmv2, ndimc, idecv2, idecl2
    logical :: lmeca, pre1
    character(len=16) :: tysd
    integer :: iviex, iret
!
    integer :: jresd1,  jresl1, iadr1
    integer :: jresd2,  jresl2, iadr2
    integer :: jcnsl1
    logical :: exist(nbno, 7)
    real(kind=8), pointer :: cnsv1(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
    real(kind=8), pointer :: cesv1(:) => null()
    real(kind=8), pointer :: cesv2(:) => null()
    real(kind=8), pointer :: cviv1(:) => null()
    real(kind=8), pointer :: cviv2(:) => null()
    character(len=16), pointer :: resv1(:) => null()
    character(len=16), pointer :: resv2(:) => null()
    integer, pointer :: cnsd(:) => null()
!
!
    call jemarq()
    call dismoi('DIM_GEOM', malini, 'MAILLAGE', repi=ndim)
!
!     ------------------------------------------------------------------
!                    1.     DEPLACEMENT
!     ------------------------------------------------------------------
    call jeveuo(cns1//'.CNSV', 'L', vr=cnsv1)
    call jeveuo(cns1//'.CNSD', 'L', vi=cnsd)
    call jeveuo(cns1//'.CNSC', 'L', vk8=cnsc)
    call jeveuo(cns2//'.CNSV', 'E', jcnsv2)
    call jeveuo(cns2//'.CNSL', 'E', jcnsl2)
!
    call jeveuo(cns1//'.CNSL', 'L', jcnsl1)
!
!     NBCMP : NBRE DE CMP MAX PAR NOEUDS DU CHAM_NO_S CNS1
    nbcmp = cnsd(2)
!
!     VERIF QUE LES 2 PREMIERES COMPOSANTES DU CHAMP DEP1 ou DEP4
!     SONT DX DY OU QUE LA PREMIERE COMPOSANTES DE CE CHAMP EST TEMP
      ASSERT((cnsc(1).eq.'DX'.and.cnsc(2).eq.'DY') .or. (cnsc(1).eq.'TEMP'))
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
    if (pre1) then
!       INDICATEUR QUI NOUS SERT POUR RECUPERER LA PRESSION SUR
!       LES NOEUDS SOMMETS UNIQUEMENT
        do i = 1, nbno
            do icmp = 1, nbcmp
                exist(i,icmp)= zl(jcnsl1-1+(i-1)*nbcmp + icmp)
            end do
        end do
!
        do i = 1, nbno
            if (dirno(i) .ne. 0) then
                idecv2 = jcnsv2-1+(ndimc+1)*(dirno(i)-1)
                idecl2 = jcnsl2-1+(ndimc+1)*(dirno(i)-1)
                do j = 1, ndimc
                    zr(idecv2+j)= cnsv1(nbcmp*(i-1)+j)
                    zl(idecl2+j)=.true.
                end do
                if (ndim .eq. 2) then
                  if (exist(i,3)) then
                    zr(idecv2+3)= cnsv1(nbcmp*(i-1)+3)
                    zl(idecl2+3)=.true.
                  endif
                else if (ndim .eq. 3) then
                  if (exist(i,4)) then
                    zr(idecv2+4)= cnsv1(nbcmp*(i-1)+4)
                    zl(idecl2+4)=.true.
                  endif
                endif
            endif
        end do
    else
        do i = 1, nbno
            if (dirno(i) .ne. 0) then
                if (lmeca) then
                    idecv2 = jcnsv2-1+2*ndimc*(dirno(i)-1)
                    idecl2 = jcnsl2-1+2*ndimc*(dirno(i)-1)
                else
                    idecv2 = jcnsv2-1+ndimc*(dirno(i)-1)
                    idecl2 = jcnsl2-1+ndimc*(dirno(i)-1)
                endif
                do j = 1, ndimc
                    zr(idecv2+j)= cnsv1(nbcmp*(i-1)+j)
                    zl(idecl2+j)=.true.
                end do
            endif
        end do
    endif
!
    call gettco(resuco, tysd)
!
    if (tysd(1:9) .ne. 'MODE_MECA' .and. tysd(1:9) .ne. 'EVOL_THER') then
!     ------------------------------------------------------------------
!                    2.      CONTRAINTES
!     ------------------------------------------------------------------
        call jeveuo(ces1//'.CESV', 'L', vr=cesv1)
        call jeveuo(ces1//'.CESD', 'L', jcesd1)
        call jeveuo(ces1//'.CESL', 'L', jcesl1)
        call jeveuo(ces2//'.CESV', 'E', vr=cesv2)
        call jeveuo(ces2//'.CESD', 'L', jcesd2)
        call jeveuo(ces2//'.CESL', 'E', jcesl2)
!
        call jeexin(cesvi1//'.CESV', iret)
        if (iret .ne. 0) then
            call jeveuo(cesvi1//'.CESV', 'L', vr=cviv1)
            call jeveuo(cesvi1//'.CESD', 'L', jcvid1)
            call jeveuo(cesvi1//'.CESL', 'L', jcvil1)
        endif
        iviex = iret
!
        call jeexin(cesvi2//'.CESV', iret)
        if (iret .ne. 0) then
            call jeveuo(cesvi2//'.CESV', 'E', vr=cviv2)
            call jeveuo(cesvi2//'.CESD', 'L', jcvid2)
            call jeveuo(cesvi2//'.CESL', 'E', jcvil2)
        endif
        iviex = iviex*iret
!
!
        do ima = 1, nbma
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
            do ipg = 1, npg1
                do icmp = 1, ncmp1
                    call cesexi('C', jcesd1, jcesl1, ima, ipg,&
                                1, icmp, iad1)
                    ASSERT(iad1.gt.0)
                    call cesexi('C', jcesd2, jcesl2, dirma(ima), ipg,&
                                1, icmp, iad2)
                    ASSERT(iad2.gt.0)
                    zl(jcesl2-1+iad2) = .true.
                    cesv2(iad2) = cesv1(iad1)
                end do
                if (iviex .ne. 0) then
                    do icmp = 1, ncmv1
                        call cesexi('C', jcvid1, jcvil1, ima, ipg,&
                                    1, icmp, iad1)
                        ASSERT(iad1.gt.0)
                        call cesexi('C', jcvid2, jcvil2, dirma(ima), ipg,&
                                    1, icmp, iad2)
                        ASSERT(iad2.lt.0)
                        iad2 = -iad2
                        zl(jcvil2-1+iad2) = .true.
                        cviv2(iad2) = cviv1(iad1)
                    end do
                endif
            end do
 10         continue
        end do
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
        call jeveuo(comps1//'.CESV', 'L', vk16=resv1)
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
        call jeveuo(comps2//'.CESV', 'E', vk16=resv2)
        call jeveuo(comps2//'.CESL', 'E', jresl2)
!
        do ima = 1, nbma
!
            ima2 = dirma(ima)
!
!         ON ZAPPE LES MAILLES NON CLASSIQUES
            if (ima2 .eq. 0) goto 300
!
            do icmp = 1, nbcmp
!
                call cesexi('C', jresd1, jresl1, ima, 1,&
                            1, icmp, iadr1)
                call cesexi('C', jresd2, jresl2, ima2, 1,&
                            1, icmp, iadr2)
!
                if (iadr1 .gt. 0) then
                    ASSERT(iadr2.lt.0)
                    resv2(1-1-iadr2) = resv1(iadr1)
                    zl(jresl2-1-iadr2) = .true.
                endif
!
            end do
300         continue
        end do
!
    endif
!
!     ------------------------------------------------------------------
!
    call jedema()
end subroutine
