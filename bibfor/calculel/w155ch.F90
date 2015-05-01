subroutine w155ch(chin, carele, ligrel, chextr, motfac,&
                  nucou, nicou, nangl, nufib)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/liglma.h"
#include "asterfort/utmess.h"
#include "asterfort/w155ma.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=8) :: carele
    character(len=3) :: nicou
    character(len=16) :: motfac
    character(len=19) :: chin, chextr, ligrel
    integer :: nucou, nangl, nufib
!
! ----------------------------------------------------------------------
! BUT : EXTRACTION DU CHAM_ELEM CORRESPONDANT A UN SOUS-POINT
!
! IN/JXIN  CHIN  : CHAM_ELEM (PLUSIEURS SOUS-POINTS) DANS LEQUEL
!                  ON DOIT EXTRAIRE CHEXTR
! IN/JXIN  CARELE  : CARA_ELEM ASSOCIE A CHIN
! IN/JXIN  LIGREL  : LIGREL SUR LEQUEL CREER CHEXTR
! IN/JXOUT CHEXTR  : CHAM_ELEM (1 SEUL SOUS-POINT) A CREER
! IN       MOTFAC  : EXTR_COQUE / EXTR_TUYAU / EXTR_PMF
! IN       NUCOU   : NUMERO DE LA COUCHE
! IN       NICOU   : NIVEAU DE LA COUCHE (MOY/INF/SUP)
! IN       NANGL   : VALEUR (ENTIER) DE L'ANGLE (EN DEGRES)
! IN       NUFIB   : NUMERO DE LA FIBRE
! ----------------------------------------------------------------------
    character(len=24) :: linuma, linute
    character(len=19) :: ces1, ces2, ces5, dcel, ligre1
    character(len=16) :: option
    character(len=8) :: licmp(4), ma, nomgd, tsca
    character(len=8) :: nompar
    integer :: iret, nbellg, nbma, numa, jcesd, jcesl
    integer :: nbpt, ksp1, ksp2, kcmp, iad
    integer :: jlima, ncmp, ncdyn1, ncdyn2, ncdyn, ncmp1
    integer :: jce2l, jce2d, jce2v, jce5l, jce5d, jce5v, nbspmx
    integer :: jcelv1, jcelv2, igr1, igr2, nbgr, nbgr1, nbgr2, debugr1, debugr2
    integer :: lgcata, jmolo, iel1, iel2, nbsp1, nbsp2, adiel1, adiel2, ipt
    integer :: ieq2, ieq11, ieq12, nbel1, nbel2, imolo
    real(kind=8) :: c1, c2
    integer, pointer :: celd1(:) => null()
    integer, pointer :: celd2(:) => null()
    integer, pointer :: liel1(:) => null()
    integer, pointer :: liel2(:) => null()
    integer, pointer :: lliel1(:) => null()
    integer, pointer :: lliel2(:) => null()
    integer, pointer :: igriel1(:) => null()
    aster_logical :: same_ligrel
    integer, pointer :: cesv(:) => null()
!
#define numail2(igr,iel) liel2(lliel2(igr)+iel-1)
#define numail1(igr,iel) liel1(lliel1(igr)+iel-1)
! ----------------------------------------------------------------------
    call jemarq()
    call dismoi('NOM_MAILLA', chin, 'CHAM_ELEM', repk=ma)
    call dismoi('NOM_GD', chin, 'CHAM_ELEM', repk=nomgd)
    call dismoi('TYPE_SCA', chin, 'CHAM_ELEM', repk=tsca)
    call dismoi('MXNBSP', chin, 'CHAM_ELEM', repi=nbspmx)
    call dismoi('NOM_LIGREL', chin, 'CHAM_ELEM', repk=ligre1)
    same_ligrel=ligre1.eq.ligrel
    call jeveuo(ligrel//'.LIEL', 'L', vi=liel2)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', vi=lliel2)
    call jeveuo(ligre1//'.LIEL', 'L', vi=liel1)
    call jeveuo(jexatr(ligre1//'.LIEL', 'LONCUM'), 'L', vi=lliel1)
    if (nbspmx .le. 1) then
        call utmess('F', 'CALCULEL2_15')
    endif
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call jeveuo(chin//'.CELD', 'L', vi=celd1)
    call jeveuo(chin//'.CELV', 'L', jcelv1)
    nbgr1 = celd1(2)
!
    ces1='&&W155CH.CES1'
    ces2='&&W155CH.CES2'
    ces5='&&W155CH.CES5'
!
!   1.  liste des mailles a traiter :
!   ---------------------------------
    linuma='&&W155CH.LIMA'
    linute='&&W155CH.LITE'
    call liglma(ligrel, nbellg, linuma, linute)
    ASSERT(nbellg.gt.0)
    call jeveuo(linuma, 'L', jlima)
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgr)
    if (.not.same_ligrel) then
!       -- il faut pouvoir faire la correspondance (igr2,iel2 <-> igr1, iel1)
        AS_ALLOCATE(vi=igriel1, size=2*nbma)
        do igr1 = 1, nbgr1
            debugr1 = celd1(4+igr1)
            nbel1 = celd1(debugr1+1)
            imolo = celd1(debugr1+2)
            if (imolo .eq. 0) goto 172
            do iel1 = 1, nbel1
                numa = numail1(igr1,iel1)
                if (numa .lt. 0) goto 142
                igriel1(2*(numa-1)+1)=igr1
                igriel1(2*(numa-1)+2)=iel1
142             continue
            enddo
172         continue
        enddo
    endif
!
!
!   2.  nombre de couches, secteurs et fibres  des elements :
!   -----------------------------------------------------------
    call celces(carele//'.CANBSP', 'V', ces1)
!
!     -- l'ordre des cmps est important (utilise dans w155ma)
    licmp(1)='COQ_NCOU'
    licmp(2)='TUY_NCOU'
    licmp(3)='TUY_NSEC'
    licmp(4)='NBFIBR'
    call cesred(ces1, nbellg, zi(jlima), 4, licmp,&
                'V', ces2)
    call detrsd('CHAM_ELEM_S', ces1)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESV', 'L', jce2v)
    call jeveuo(ces2//'.CESL', 'L', jce2l)
!
!
!   2-bis  valeur de omega (angzzk) pour les tuyaux :
!   -----------------------------------------------------------
    if (motfac .eq. 'EXTR_TUYAU') then
        call carces(carele//'.CARORIEN', 'ELEM', ' ', 'V', ces1,&
                    'A', iret)
        ASSERT(iret.eq.0)
        licmp(1)='ANGZZK'
        call cesred(ces1, nbellg, zi(jlima), 1, licmp,&
                    'V', ces5)
        call detrsd('CHAM_ELEM_S', ces1)
        call jeveuo(ces5//'.CESD', 'L', jce5d)
        call jeveuo(ces5//'.CESV', 'L', jce5v)
        call jeveuo(ces5//'.CESL', 'L', jce5l)
    else
        jce5d=0
        jce5v=0
        jce5l=0
    endif
!
!
!   3. Allocation de chextr :
!   --------------------------
!
    if (nomgd .eq. 'VARI_R') then
!       -- il faut recuperer le nombre de variables internes dans chin
        dcel='&&W155CH.DCEL'
!
        licmp(1) = 'NPG_DYN'
        licmp(2) = 'NCMP_DYN'
        call cescre('V', dcel, 'ELEM', ma, 'DCEL_I',&
                    2, licmp, [-1], [-1], [-2])
!
        call jeveuo(dcel//'.CESD', 'E', jcesd)
        call jeveuo(dcel//'.CESL', 'E', jcesl)
        call jeveuo(dcel//'.CESV', 'E', vi=cesv)
!
        do igr1 = 1, nbgr1
            debugr1 = celd1(4+igr1)
            nbel1 = celd1(debugr1+1)
            imolo = celd1(debugr1+2)
            if (imolo .eq. 0) goto 171
!
            do iel1 = 1, nbel1
                numa = numail1(igr1,iel1)
                if (numa .lt. 0) goto 141
!
                ncdyn = celd1(debugr1+4+4* (iel1-1)+2)
                call cesexi('C', jcesd, jcesl, numa, 1,&
                            1, 1, iad)
                cesv(1-1-iad)=0
                zl(jcesl-1-iad)=.true.
                call cesexi('C', jcesd, jcesl, numa, 1,&
                            1, 2, iad)
                cesv(1-1-iad)=ncdyn
                zl(jcesl-1-iad)=.true.
141             continue
            end do
171         continue
        end do
    else
        dcel=' '
    endif
!
    call dismoi('NOM_OPTION', chin, 'CHAM_ELEM', repk=option)
    call dismoi('NOM_PARAM', chin, 'CHAM_ELEM', repk=nompar)
    call alchml(ligrel, option, nompar, 'G', chextr,&
                iret, dcel)
    ASSERT(iret.eq.0)
!
!
!   4. Recopie des valeurs de chin vers chextr :
!   ---------------------------------------------
!   -- une difficulte : ligrel peut etre different de ligre1
    call jeveuo(chextr//'.CELD', 'L', vi=celd2)
    call jeveuo(chextr//'.CELV', 'L', jcelv2)
    nbgr2=celd2(2)
    ASSERT(nbgr2.eq.nbgr)
!
    do igr2 = 1, nbgr2
        debugr2 = celd2(4+igr2)
        nbel2 = celd2(debugr2+1)
        imolo = celd2(debugr2+2)
        if (imolo .eq. 0) goto 170
!
        lgcata = celd2(debugr2+3)
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        nbpt = zi(jmolo-1+4)
        ncmp=lgcata/nbpt
        ASSERT(nbpt*ncmp.eq.lgcata)
!
        do iel2 = 1, nbel2
            numa = numail2(igr2,iel2)
            if (numa .lt. 0) goto 140
!
            if (same_ligrel) then
                igr1=igr2
                iel1=iel2
            else
                igr1=igriel1(2*(numa-1)+1)
                iel1=igriel1(2*(numa-1)+2)
                if (iel1*igr1 .eq. 0) goto 140
                ASSERT(iel1*igr1.gt.0)
            endif
            debugr1 = celd1(4+igr1)
!
            nbsp1 = celd1(debugr1+4+4* (iel1-1)+1)
            nbsp2 = celd2(debugr2+4+4* (iel2-1)+1)
            ASSERT(nbsp2.le.1)
            ncdyn1 = max(celd1(debugr1+4+4* (iel1-1)+2),1)
            ncdyn2 = max(celd2(debugr2+4+4* (iel2-1)+2),1)
            ASSERT(ncdyn1.eq.ncdyn2)
            ncdyn=ncdyn1
            ASSERT(celd1(debugr1+4+4* (iel1-1)+3).eq.lgcata*nbsp1*ncdyn)
            ASSERT(celd2(debugr2+4+4* (iel2-1)+3).eq.lgcata*ncdyn)
!
            adiel1 = celd1(debugr1+4+4* (iel1-1)+4)
            adiel2 = celd2(debugr2+4+4* (iel2-1)+4)
!
            call w155ma(numa, nucou, nicou, nangl, nufib,&
                        motfac, jce2d, jce2l, jce2v, jce5d,&
                        jce5l, jce5v, ksp1, ksp2, c1,&
                        c2, iret)
            if (iret .eq. 1) goto 140
!
            do ipt = 1, nbpt
                ncmp1=ncmp*ncdyn
                do kcmp = 1, ncmp1
                    ieq2=adiel2-1+(ipt-1)*ncmp1+kcmp
                    ieq11=adiel1-1+(ipt-1)*nbsp1*ncmp1 + (ksp1-1)*ncmp1 + kcmp
                    ieq12=adiel1-1+(ipt-1)*nbsp1*ncmp1 + (ksp2-1)*ncmp1 + kcmp
!
                    if (tsca .eq. 'R') then
                        zr(jcelv2-1+ieq2) = c1*zr(jcelv1-1+ieq11)+c2*zr(jcelv1-1+ieq12)
                    else if (tsca.eq.'C') then
                        zc(jcelv2-1+ieq2) = c1*zc(jcelv1-1+ieq11)+c2*zc(jcelv1-1+ieq12)
                    else
                        ASSERT(.false.)
                    endif
                end do
            end do
140         continue
        end do
170     continue
    end do
!
!
!   5. menage :
!   ------------
    call detrsd('CHAM_ELEM_S', ces2)
    call detrsd('CHAM_ELEM_S', ces5)
    call detrsd('CHAM_ELEM_S', dcel)
    call jedetr(linuma)
    call jedetr(linute)
    AS_DEALLOCATE(vi=igriel1)
!
    call jedema()
end subroutine
