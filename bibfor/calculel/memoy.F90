subroutine memoy(champa, ncpa, champb, ncpb, vr,&
                 nbmail, numail)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/scalai.h"
#include "asterfort/utmess.h"
    character(len=*) :: champa, champb
    integer :: ncpa, ncpb, nbmail, numail(*)
    real(kind=8) :: vr(2)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     BUT :  FAIRE LA "MOYENNE" DE LA COMPOSANTE NCPA D'UN CHAM_ELEM
!            EN PONDERANT PAR LA COMPOSANTE NCPB D'UN AUTRE CHAM_ELEM
!            LA SEULE CONTRAINTE EST QUE TOUS LES TYPE_ELEMENT DU LIGREL
!            CONNAISSENT LA GRANDEUR AVEC LA MEME LONGUEUR CUMULEE :
!
! IN  : CHAMPA :  NOM DU CHAMP A MOYENNER
! IN  : CHAMPB :  NOM DU CHAMP DE PONDERATION
! IN  : NCPA   :  NUMERO DE COMPOSANTE DU CHAMP A
! IN  : NCPB   :  NUMERO DE COMPOSANTE DU CHAMP B
! IN  : NBMAIL :  = 0   , CALCUL SUR TOUT LE CHAM_ELEM
!                 SINON , CALCUL SUR UNE PARTIE DU CHAM_ELEM
! IN  : NUMAIL :  NUMERO DES MAILLES
! OUT : VR     :  VECTEUR RESULTAT
!
!       LE RESULTAT EST DONNE SOUS LA FORME DE DEUX COMPOSANTES
!          (1) VALEUR DE LA MOYENNE
!          (2) SOMME DES VALEURS DU CHAMP DE PONDERATION
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: longt1, longt2, ncmpel, mode, j, igd1, igd2
    real(kind=8) :: rzero
    character(len=8) :: scal1, scal2
    character(len=19) :: champ1, champ2, ligrel, ligre1, ligre2
    aster_logical :: first
!
!-----------------------------------------------------------------------
    integer :: i, iacelk, ibid, icoef, idecg1
    integer :: idecg2, iel, im, inum
    integer :: k, mode1, mode2, nbgr, nel
    integer, pointer :: liel(:) => null()
    integer, pointer :: celd1(:) => null()
    integer, pointer :: celd2(:) => null()
    real(kind=8), pointer :: val1(:) => null()
    real(kind=8), pointer :: val2(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    champ1 = champa
    champ2 = champb
    rzero = 0.0d0
    if ((ncpa.le.0) .or. (ncpb.le.0)) then
        call utmess('F', 'CALCULEL3_57')
    endif
!
!     -- ON RETROUVE LE NOM DU LIGREL:
!     --------------------------------
    call jeveuo(champ1//'.CELK', 'L', iacelk)
    ligre1 = zk24(iacelk-1+1)(1:19)
!
    call jeveuo(champ2//'.CELK', 'L', iacelk)
    ligre2 = zk24(iacelk-1+1)(1:19)
!
    if (ligre1 .ne. ligre2) then
        call utmess('F', 'CALCULEL3_58')
    endif
    ligrel = ligre1
!
    call jeexin(champ1//'.CELD', ibid)
    if (ibid .eq. 0) then
        call utmess('F', 'CALCULEL3_59', sk=champ1)
    endif
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ1, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ1, 'NBSPT_1', 'STOP', ibid)
!
    call jeveuo(champ1//'.CELD', 'L', vi=celd1)
    igd1 = celd1(1)
    scal1 = scalai(igd1)
    if (scal1(1:1) .ne. 'R') then
        call utmess('F', 'CALCULEL3_53')
    endif
!
    call jeexin(champ2//'.CELD', ibid)
    if (ibid .eq. 0) then
        call utmess('F', 'CALCULEL3_59', sk=champ2)
    endif
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ2, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ2, 'NBSPT_1', 'STOP', ibid)
!
    call jeveuo(champ2//'.CELD', 'L', vi=celd2)
    igd2 = celd2(1)
    scal2 = scalai(igd2)
    if (scal2(1:1) .ne. 'R') then
        call utmess('F', 'CALCULEL3_53')
    endif
!
!     -- ON VERIFIE LES LONGUEURS DE CHAQUE CHAMP:
!     --------------------------------------------
    first = .true.
    nbgr = nbgrel(ligrel)
    do 1 ,j = 1,nbgr
    mode=celd1(celd1(4+j) +2)
    if (mode .eq. 0) goto 1
    ncmpel = digdel(mode)
    icoef=max(1,celd1(4))
    ncmpel = ncmpel * icoef
    if (first) then
        longt1 = ncmpel
    else
        if (longt1 .ne. ncmpel) then
            call utmess('F', 'CALCULEL3_60')
        endif
    endif
    first = .false.
    1 end do
!
    first = .true.
    nbgr = nbgrel(ligrel)
    do 100 ,j = 1,nbgr
    mode=celd2(celd2(4+j) +2)
    if (mode .eq. 0) goto 100
    ncmpel = digdel(mode)
    icoef=max(1,celd2(4))
    ncmpel = ncmpel * icoef
    if (first) then
        longt2 = ncmpel
    else
        if (longt2 .ne. ncmpel) then
            call utmess('F', 'CALCULEL3_61')
        endif
    endif
    first = .false.
    100 end do
!
    if ((ncpa.gt.longt1) .or. (ncpb.gt.longt2)) then
        call utmess('F', 'CALCULEL3_62')
    endif
!
!     -- ON MET A ZERO LE VECTEUR "VSCAL":
!     ------------------------------------
    do 10 i = 1, 2
        vr(i) = rzero
 10 end do
!
! --- ON MOYENNE :
!     ------------
!
    call jeveuo(champ1//'.CELV', 'L', vr=val1)
    call jeveuo(champ2//'.CELV', 'L', vr=val2)
    if (nbmail .le. 0) then
        do 2 ,j = 1,nbgr
        mode1 = celd1(celd1(4+j) +2)
        mode2 = celd2(celd2(4+j) +2)
        if ((mode1.eq.0 ) .or. (mode2.eq.0)) goto 2
        nel = nbelem(ligrel,j)
        idecg1 = celd1(celd1(4+j)+8)
        idecg2 = celd2(celd2(4+j)+8)
        do 3 , k = 1,nel
        vr(1) = vr(1) + val1(idecg1+(k-1)*longt1+ncpa- 1) *val2(idecg2+(k-1)*longt2+ncpb-1)
        vr(2) = vr(2) + val2(idecg2+(k-1)*longt2+ncpb- 1)
  3     continue
  2     continue
        vr(1) = vr(1)/vr(2)
    else
        call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
        do 30 im = 1, nbmail
            inum = 0
            do 20 j = 1, nbgr
                mode1 = celd1(celd1(4+j) +2)
                mode2 = celd2(celd2(4+j) +2)
                nel = nbelem(ligrel,j)
                if ((mode1.eq.0 ) .or. (mode2.eq.0)) then
                    inum = inum + nel + 1
                    goto 20
                endif
                idecg1 = celd1(celd1(4+j)+8)
                idecg2 = celd2(celd2(4+j)+8)
                do 22 k = 1, nel
                    iel = liel(1+inum+k-1)
                    if (iel .ne. numail(im)) goto 22
                    vr(1) =vr(1) + val1(idecg1+(k-1)*longt1+&
                    ncpa-1) *val2(idecg2+(k-1)*longt2+ncpb-1)
                    vr(2)= vr(2) + val2(idecg2+(k-1)*longt2+&
                    ncpb-1)
                    goto 30
 22             continue
                inum = inum + nel + 1
 20         continue
 30     continue
        vr(1) = vr(1)/vr(2)
    endif
!
    call jedema()
end subroutine
