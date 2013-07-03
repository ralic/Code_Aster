subroutine tbexve(nomta, para, nomobj, basobj, nbval,&
                  typval)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbval
    character(len=*) :: nomta, para, nomobj, basobj, typval
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
!      LECTURE DE TOUTES LES VALEURS D'UNE COLONNE D'UNE TABLE
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : PARA   : PARAMETRE DESIGNANT LA COLONNE A EXTRAIRE
! IN  : NOMOBJ : NOM DE L'OBJET JEVEUX CONTENANT LES VALEURS
! IN  : BASOBJ : BASE SUR LAQUELLE ON CREE LE VECTEUR
! OUT : NBVAL  : NOMBRE DE VALEURS EXTRAITES
! OUT : TYPVAL : TYPE JEVEUX DES VALEURS EXTRAITES
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, jtblp, ipar
    integer :: i, iv, jvale, jvall, kvale
    character(len=1) :: base
    character(len=4) :: type
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar
    character(len=24) :: valk
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = nomta
    base = basobj(1:1)
    inpar = para
!
!     --- VERIFICATION DE LA BASE ---
!
    call assert(base.eq.'V' .or. base.eq.'G')
!
!     --- VERIFICATION DE LA TABLE ---
!
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call u2mess('F', 'UTILITAI4_76')
    endif
!
!     --- VERIFICATION QUE LE PARAMETRE EXISTE DANS LA TABLE ---
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    do 10 ipar = 1, nbpara
        jnpar = zk24(jtblp+4*(ipar-1))
        if (inpar .eq. jnpar) goto 12
10  continue
    valk = inpar
    call u2mesg('F', 'UTILITAI6_89', 1, valk, 0,&
                0, 0, 0.d0)
12  continue
!
    type = zk24(jtblp+4*(ipar-1)+1)
    nomjv = zk24(jtblp+4*(ipar-1)+2)
    nomjvl = zk24(jtblp+4*(ipar-1)+3)
!
    call jeveuo(nomjv, 'L', jvale)
    call jeveuo(nomjvl, 'L', jvall)
    nbval = 0
    do 20 i = 1, nblign
        if (zi(jvall+i-1) .eq. 1) nbval = nbval + 1
20  end do
!
    iv = 0
    if (type(1:1) .eq. 'I') then
        call wkvect(nomobj, base//' V I', nbval, kvale)
        do 100 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zi(kvale+iv-1) = zi(jvale+i-1)
            endif
100      continue
!
    else if (type(1:1) .eq. 'R') then
        call wkvect(nomobj, base//' V R', nbval, kvale)
        do 200 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zr(kvale+iv-1) = zr(jvale+i-1)
            endif
200      continue
!
    else if (type(1:1) .eq. 'C') then
        call wkvect(nomobj, base//' V C', nbval, kvale)
        do 300 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zc(kvale+iv-1) = zc(jvale+i-1)
            endif
300      continue
!
    else if (type(1:3) .eq. 'K80') then
        call wkvect(nomobj, base//' V K80', nbval, kvale)
        do 400 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zk80(kvale+iv-1) = zk80(jvale+i-1)
            endif
400      continue
!
    else if (type(1:3) .eq. 'K32') then
        call wkvect(nomobj, base//' V K32', nbval, kvale)
        do 500 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zk32(kvale+iv-1) = zk32(jvale+i-1)
            endif
500      continue
!
    else if (type(1:3) .eq. 'K24') then
        call wkvect(nomobj, base//' V K24', nbval, kvale)
        do 600 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zk24(kvale+iv-1) = zk24(jvale+i-1)
            endif
600      continue
!
    else if (type(1:3) .eq. 'K16') then
        call wkvect(nomobj, base//' V K16', nbval, kvale)
        do 700 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zk16(kvale+iv-1) = zk16(jvale+i-1)
            endif
700      continue
!
    else if (type(1:2) .eq. 'K8') then
        call wkvect(nomobj, base//' V K8', nbval, kvale)
        do 800 i = 1, nblign
            if (zi(jvall+i-1) .eq. 1) then
                iv = iv + 1
                zk8(kvale+iv-1) = zk8(jvale+i-1)
            endif
800      continue
    endif
!
    typval = type
    nbval = iv
    call jeecra(nomobj, 'LONUTI', nbval, ' ')
!
    call jedema()
end subroutine
