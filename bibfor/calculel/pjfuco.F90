subroutine pjfuco(c1, c2, base, c3)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: c1, c2, c3
    character(len=1) :: base
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
!     BUT :
!       FUSIONNER 2 SD CORRESP_2_MAILLA C1 ET C2 POUR FABRIQUER C3
!
!  IN/JXIN   C1  : SD CORRESP_2_MAILLA
!  IN/JXIN   C2  : SD CORRESP_2_MAILLA
!  IN/JXOUT  C3  : SD CORRESP_2_MAILLA RESULTAT DE LA FUSION
!  IN        BASE: NOM DE LA BASE POUR CREER C3
!
!  REMARQUE :  C2 "SURCHARGE" C1 :
!     SI UN NOEUD INO2 APPARTIENT A C1 ET C2,
!     IL AURA DANS C3 LA MEME DESCRIPTION QUE DANS C2
!----------------------------------------------------------------------
!
!
!
    character(len=8) :: ma1, ma2
    character(len=24) :: valk(2)
    integer :: i1, i2, i3, j1, j2, j3, k
    integer :: ino2, iatmp1, deca1, deca2, deca3, nbno, lont, nbno2
    integer :: i1nb, i2nb, i3nb, i1nu, i2nu, i3nu, i1cf, i2cf, i3cf, i1ou2
!
!
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
!
!     1- OBJET '.PJXX_K1' :
!     ----------------------
    call jeveuo(c1//'.PJXX_K1', 'L', i1)
    call jeveuo(c2//'.PJXX_K1', 'L', i2)
    do 10,k = 1,2
    ma1 = zk24(i1-1+k)
    ma2 = zk24(i2-1+k)
    if (ma1 .ne. ma2) then
        valk(1) = ma1
        valk(2) = ma2
        call utmess('F', 'CALCULEL4_65', nk=2, valk=valk)
    endif
    10 end do
    call jedupo(c1//'.PJXX_K1', base, c3//'.PJXX_K1', .false.)
!
!
!     2- OBJETS  .PJEF_NB et .PJEF_M1 :
!     ---------------------------------
    call jelira(c1//'.PJEF_NB', 'LONMAX', nbno2)
    call jeveuo(c1//'.PJEF_NB', 'L', i1)
    call jeveuo(c2//'.PJEF_NB', 'L', i2)
    call jeveuo(c1//'.PJEF_M1', 'L', j1)
    call jeveuo(c2//'.PJEF_M1', 'L', j2)
    call wkvect(c3//'.PJEF_NB', base//' V I', nbno2, i3)
    call wkvect(c3//'.PJEF_M1', base//' V I', nbno2, j3)
!
!     2.1 CREATION D'UN OBJET DE TRAVAIL '&&PJFUCO.TMP1'
!        QUI DIRA DANS QUEL CORRESP_2_MAILLA A ETE PRIS LE NOEUD INO2
!     ----------------------------------------------------------------
    call wkvect('&&PJFUCO.TMP1', 'V V I', nbno2, iatmp1)
!
    do 20,ino2 = 1,nbno2
    if (zi(i1-1+ino2) .ne. 0) then
        zi(iatmp1-1+ino2) = 1
        zi(i3-1+ino2) = zi(i1-1+ino2)
        zi(j3-1+ino2) = zi(j1-1+ino2)
    endif
    if (zi(i2-1+ino2) .ne. 0) then
        zi(iatmp1-1+ino2) = 2
        zi(i3-1+ino2) = zi(i2-1+ino2)
        zi(j3-1+ino2) = zi(j2-1+ino2)
    endif
    20 end do
!
!
!     -- OBJETS '.PJEF_NU' ET '.PJEF_CF' :
!     -------------------------------------
    call jeveuo(c1//'.PJEF_NB', 'L', i1nb)
    call jeveuo(c2//'.PJEF_NB', 'L', i2nb)
    call jeveuo(c3//'.PJEF_NB', 'L', i3nb)
    call jeveuo(c1//'.PJEF_NU', 'L', i1nu)
    call jeveuo(c2//'.PJEF_NU', 'L', i2nu)
    call jeveuo(c1//'.PJEF_CF', 'L', i1cf)
    call jeveuo(c2//'.PJEF_CF', 'L', i2cf)
    lont = 0
    do 30,k = 1,nbno2
    lont = lont + zi(i3nb-1+k)
    30 end do
    call wkvect(c3//'.PJEF_NU', base//' V I', lont, i3nu)
    call wkvect(c3//'.PJEF_CF', base//' V R', lont, i3cf)
!
    deca1 = 0
    deca2 = 0
    deca3 = 0
    do 60,ino2 = 1,nbno2
    i1ou2 = zi(iatmp1-1+ino2)
    if (i1ou2 .eq. 1) then
        nbno = zi(i3nb-1+ino2)
        do 40,k = 1,nbno
        zi(i3nu-1+deca3+k) = zi(i1nu-1+deca1+k)
        zr(i3cf-1+deca3+k) = zr(i1cf-1+deca1+k)
40      continue
    else if (i1ou2.eq.2) then
        nbno = zi(i3nb-1+ino2)
        do 50,k = 1,nbno
        zi(i3nu-1+deca3+k) = zi(i2nu-1+deca2+k)
        zr(i3cf-1+deca3+k) = zr(i2cf-1+deca2+k)
50      continue
    endif
    deca1 = deca1 + zi(i1nb-1+ino2)
    deca2 = deca2 + zi(i2nb-1+ino2)
    deca3 = deca3 + zi(i3nb-1+ino2)
    60 end do
!
!
!
    call jedetr('&&PJFUCO.TMP1')
    call jedema()
end subroutine
