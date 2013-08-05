subroutine w155ch(chin, carele, ligrel, chextr, motfac,&
                  nucou, nicou, nangl, nufib)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
    implicit none
#include "jeveux.h"
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
#include "asterfort/liglma.h"
#include "asterfort/u2mess.h"
#include "asterfort/w155ma.h"
#include "asterfort/wkvect.h"
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
    character(len=19) :: ces1, ces2, ces3, ces4, ces5
    character(len=16) :: option
    character(len=8) :: kbid, licmp(4), ma, nomgd, tsca, typces
    character(len=8) :: nompar
    integer :: iret, nbma, ibid, nbmat, numa, jnbpt, kma
    integer :: nbpt, ksp1, ksp2, kpt, kcmp, nncp
    integer :: iad1, iad2, iad4, jlima, ncmp
    integer :: jce2l, jce2d, jce2v, jce3k, jce3l, jce3d, jce3v, jce3c
    integer :: jce4l, jce4d, jce4v, jce5l, jce5d, jce5v, nbspmx
    real(kind=8) :: c1, c2
!
! ----------------------------------------------------------------------
    call jemarq()
    call dismoi('F', 'NOM_MAILLA', chin, 'CHAM_ELEM', ibid,&
                ma, iret)
    call dismoi('F', 'NOM_GD', chin, 'CHAM_ELEM', ibid,&
                nomgd, iret)
    call dismoi('F', 'TYPE_SCA', chin, 'CHAM_ELEM', ibid,&
                tsca, iret)
    call dismoi('F', 'MXNBSP', chin, 'CHAM_ELEM', nbspmx,&
                kbid, iret)
    if (nbspmx .le. 1) call u2mess('F', 'CALCULEL2_15')
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbmat,&
                kbid, iret)
!
    ces1='&&W155CH.CES1'
    ces2='&&W155CH.CES2'
    ces3='&&W155CH.CES3'
    ces4='&&W155CH.CES4'
    ces5='&&W155CH.CES5'
!
!     1.  LISTE DES MAILLES A TRAITER :
!     ---------------------------------
    linuma='&&W155CH.LIMA'
    linute='&&W155CH.LITE'
    call liglma(ligrel, nbma, linuma, linute)
    ASSERT(nbma.gt.0)
    call jeveuo(linuma, 'L', jlima)
!
!
!     2.  NOMBRE DE COUCHES, SECTEURS ET FIBRES  DES ELEMENTS :
!     -----------------------------------------------------------
    call celces(carele//'.CANBSP', 'V', ces1)
!
!     -- L'ORDRE DES CMPS EST IPORTANT (UTILISE DANS W155MA)
    licmp(1)='COQ_NCOU'
    licmp(2)='TUY_NCOU'
    licmp(3)='TUY_NSEC'
    licmp(4)='NBFIBR'
    call cesred(ces1, nbma, zi(jlima), 4, licmp,&
                'V', ces2)
    call detrsd('CHAM_ELEM_S', ces1)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESV', 'L', jce2v)
    call jeveuo(ces2//'.CESL', 'L', jce2l)
!
!
!     2-BIS  VALEUR DE OMEGA (ANGZZK) POUR LES TUYAUX :
!     -----------------------------------------------------------
    if (motfac .eq. 'EXTR_TUYAU') then
        call carces(carele//'.CARORIEN', 'ELEM', ' ', 'V', ces1,&
                    'A', iret)
        ASSERT(iret.eq.0)
        licmp(1)='ANGZZK'
        call cesred(ces1, nbma, zi(jlima), 1, licmp,&
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
!     3. CHIN -> CES3 :
!     ------------------
    call celces(chin, 'V', ces3)
    call jeveuo(ces3//'.CESK', 'L', jce3k)
    call jeveuo(ces3//'.CESD', 'L', jce3d)
    call jeveuo(ces3//'.CESC', 'L', jce3c)
    call jeveuo(ces3//'.CESL', 'L', jce3l)
    call jeveuo(ces3//'.CESV', 'L', jce3v)
    call jelira(ces3//'.CESC', 'LONMAX', ncmp, kbid)
    typces=zk8(jce3k-1+3)
    call wkvect('&&W155CH.NBPT', 'V V I', nbmat, jnbpt)
    do 10,kma=1,nbma
    numa=zi(jlima-1+kma)
    if (numa .le. 0) goto 10
    nbpt=zi(jce3d-1+5+4*(numa-1)+1)
    zi(jnbpt-1+numa)=nbpt
    10 end do
!
!
!     4. ALLOCATION ET CALCUL DE CHEXTR :
!     ------------------------------------
    call cescre('V', ces4, typces, ma, nomgd,&
                ncmp, zk8(jce3c), zi(jnbpt), -1, -ncmp)
    call jeveuo(ces4//'.CESD', 'L', jce4d)
    call jeveuo(ces4//'.CESV', 'L', jce4v)
    call jeveuo(ces4//'.CESL', 'L', jce4l)
    do 40,kma=1,nbma
    numa=zi(jlima-1+kma)
    if (numa .le. 0) goto 40
    ASSERT(numa.le.nbmat)
    nbpt=zi(jnbpt-1+numa)
    call w155ma(numa, nucou, nicou, nangl, nufib,&
                motfac, jce2d, jce2l, jce2v, jce5d,&
                jce5l, jce5v, ksp1, ksp2, c1,&
                c2, iret)
    if (iret .eq. 1) goto 40
    do 30,kpt=1,nbpt
    do 20,kcmp=1,ncmp
    call cesexi('C', jce3d, jce3l, numa, kpt,&
                ksp1, kcmp, iad1)
    call cesexi('C', jce3d, jce3l, numa, kpt,&
                ksp2, kcmp, iad2)
    if (iad1 .gt. 0) then
        ASSERT(iad2.gt.0)
        call cesexi('C', jce4d, jce4l, numa, kpt,&
                    1, kcmp, iad4)
        ASSERT(iad4.lt.0)
        iad4=-iad4
        if (tsca .eq. 'R') then
            zr(jce4v-1+iad4)=c1*zr(jce3v-1+iad1)+c2*zr(&
                        jce3v-1+iad2)
        else if (tsca.eq.'C') then
            zc(jce4v-1+iad4)=c1*zc(jce3v-1+iad1)+c2*zc(&
                        jce3v-1+iad2)
        else
            ASSERT(.false.)
        endif
        zl(jce4l-1+iad4)=.true.
    endif
20  continue
30  continue
    40 end do
!
!     4.5 CES4 -> CHEXTR :
!     ------------------------------------
!
    call dismoi('F', 'NOM_OPTION', chin, 'CHAM_ELEM', ibid,&
                option, ibid)
    call dismoi('F', 'NOM_PARAM', chin, 'CHAM_ELEM', ibid,&
                nompar, ibid)
    call cescel(ces4, ligrel, option, nompar, 'OUI',&
                nncp, 'G', chextr, 'F', iret)
    ASSERT(nncp.eq.0)
!
!
!     6. MENAGE :
!     ------------
    call detrsd('CHAM_ELEM_S', ces2)
    call detrsd('CHAM_ELEM_S', ces3)
    call detrsd('CHAM_ELEM_S', ces4)
    call detrsd('CHAM_ELEM_S', ces5)
    call jedetr('&&W155CH.NBPT')
    call jedetr(linuma)
    call jedetr(linute)
!
    call jedema()
end subroutine
