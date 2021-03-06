subroutine pj1dtr(cortr3, corres, nutm1d, elrf1d)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elraca.h"
#include "asterfort/elrfvf.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: corres, cortr3
    character(len=8) :: elrf1d(3)
    integer :: nutm1d(3)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     BUT :
!       TRANSFORMER CORTR3 EN CORRES EN UTILISANT LES FONC. DE FORME
!       DES MAILLES DU MAILLAGE1 (EN 1D ISOPARAMETRIQUE)


!  IN/JXIN   CORTR3   K16 : NOM DU CORRESP_2_MAILLA FAIT AVEC LES SEG2
!  IN/JXOUT  CORRES   K16 : NOM DU CORRESP_2_MAILLA FINAL
!  IN        NUTM1D(3) I  : NUMEROS DES 3 TYPES DE MAILLES 1D (SEGX)
!  IN        ELRF1D(5) K8 : NOMS DES 3 TYPES DE MAILLES 1D  (SEGX)
! ----------------------------------------------------------------------

    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    character(len=8) :: m1, m2, elrefa, fapg(nbfamx)
    integer :: nbpg(nbfamx), i1conb, i1conu,   nno1, nno2
    integer :: nma1
    integer :: nma2, ialim1, ialin1,   ilcnx1,  j2xxk1
    integer :: i2conb, i2com1, ideca2, ino2, itr, ima1, nbno, i2conu, i2cocf
    integer :: ideca1, itypm, nutm, ndim, nno, nnos, nbfpg, kk, ino
    integer :: nuno,  ialin2, i2coco
    real(kind=8) :: crrefe(3*nbnomx), ksi, x(1), ff(nbnomx), vol, x1
    integer, pointer :: seg2(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: pjef_tr(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    integer, pointer :: typmail(:) => null()
    character(len=24), pointer :: pjxx_k1(:) => null()
! --- DEB --------------------------------------------------------------

    call jemarq()


!     1. RECUPERATION DES INFORMATIONS GENERALES :
!     -----------------------------------------------
    call jeveuo(cortr3//'.PJXX_K1', 'L', vk24=pjxx_k1)
    call jeveuo(cortr3//'.PJEF_NB', 'L', i1conb)
    call jeveuo(cortr3//'.PJEF_NU', 'L', i1conu)
    call jeveuo(cortr3//'.PJEF_CF', 'L', vr=pjef_cf)
    call jeveuo(cortr3//'.PJEF_TR', 'L', vi=pjef_tr)

    m1=pjxx_k1(1)
    m2=pjxx_k1(2)
    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)

    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)
    call jeveuo('&&PJXXCO.SEG2', 'L', vi=seg2)

    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)


!     2. ALLOCATION DE CORRES :
!     -----------------------------------------------
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, j2xxk1)
    zk24(j2xxk1-1+1)=m1
    zk24(j2xxk1-1+2)=m2
    zk24(j2xxk1-1+3)='COLLOCATION'


!     2.1 REMPLISSAGE DE .PJEF_NB ET .PJEF_M1:
!     ----------------------------------------
    call wkvect(corres//'.PJEF_NB', 'V V I', nno2, i2conb)
    call wkvect(corres//'.PJEF_M1', 'V V I', nno2, i2com1)
    ideca2=0
    do ino2 = 1, nno2
!       ITR : SEG2 ASSOCIE A INO2
        itr=pjef_tr(ino2)
        if (itr .eq. 0) goto 10
!       IMA1 : MAILLE DE M1 ASSOCIEE AU SEG2 ITR
        ima1=seg2(1+3*(itr-1)+3)
        nbno=zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
        zi(i2conb-1+ino2)=nbno
        zi(i2com1-1+ino2)=ima1
        ideca2=ideca2+nbno
 10     continue
    end do
    if (ideca2 .eq. 0) then
        call utmess('F', 'CALCULEL3_97')
    endif

!     2.2 ALLOCATION DE .PJEF_NU .PJEF_CF .PJEF_CO:
!         (ET REMPLISSAGE DE CES 3 OBJETS)
!     ------------------------------------------------------
    call wkvect(corres//'.PJEF_NU', 'V V I', ideca2, i2conu)
    call wkvect(corres//'.PJEF_CF', 'V V R', ideca2, i2cocf)
    call wkvect(corres//'.PJEF_CO', 'V V R', 3*nno2, i2coco)
    ideca1=0
    ideca2=0
    do ino2 = 1, nno2
!       ITR : SEG2 ASSOCIE A INO2
        itr = pjef_tr(ino2)
        if (itr .eq. 0) goto 20
!       IMA1 : MAILLE DE M1 ASSOCIE AU SEG2 ITR
        ima1= seg2(1+3*(itr-1)+3)
!       ITYPM : TYPE DE LA MAILLE IMA1
        itypm = typmail(ima1)
        nutm = indiis(nutm1d,itypm,1,3)
        elrefa = elrf1d(nutm)
        nbno = zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)

        call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                    fapg, nbpg, crrefe, vol)
        ASSERT(nbno.eq.nno)


!       2.2.1 DETERMINATION DES COORDONEES DE INO2 DANS L'ELEMENT
!             DE REFERENCE : KSI
!     -----------------------------------------------------------
        ksi=0.d0
        do kk = 1, 2
            x1 = crrefe(ndim*(kk-1)+1)
            ksi = ksi + pjef_cf(ideca1+kk)*x1
        end do
        x(1) = ksi
        zr(i2coco-1+3*(ino2-1)+1)=x(1)


!       2.2.2 :
!       CALCUL DES F. DE FORME AUX NOEUDS POUR LE POINT KSI
!       -------------------------------------------------------
        call elrfvf(elrefa, x, 27, ff, nno)
        do ino = 1, nbno
            nuno = connex(1+ zi(ilcnx1-1+ima1)-2+ino)
            zi(i2conu-1+ideca2+ino) = nuno
            zr(i2cocf-1+ideca2+ino) = ff(ino)
        end do

        ideca1=ideca1+2
        ideca2=ideca2+nbno

 20     continue
    end do

    call jedema()
end subroutine
