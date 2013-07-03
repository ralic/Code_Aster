subroutine juveca(nom, long)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nom
    integer :: long
!     ------------------------------------------------------------------
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
!     REDIMENSIONNEMENT D'UN OBJET SIMPLE JEVEUX DEJA EXISTANT
!     ------------------------------------------------------------------
! IN  NOM  : K24 : NOM DE L'OBJET A REDIMENSIONNER
! IN  LONG : I   : NOUVELLE LONGUEUR DU VECTEUR
!     ------------------------------------------------------------------
!     REMARQUE: LES VALEURS SONT RECOPIEES
!      SI LA NOUVELLE LONGUEUR EST INFERIEURE A L'ANCIENNE, DES VALEURS
!      SONT PERDUES
!     ------------------------------------------------------------------
!
!
    character(len=8) :: base, type, cbid
    character(len=32) :: valk(2)
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, ldec, ll, lonma2, lonmax, lonuti
    integer :: ltamp, ltyp
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(nom, 'L', ldec)
!
!     --- TYPE, LONGUEUR ET BASE DE L'OBJET A REDIMENSIONNER
    call jelira(nom, 'TYPE  ', ibid, type)
    call jelira(nom, 'LONMAX', lonmax, cbid)
    call jelira(nom, 'LONUTI', lonuti, cbid)
    call jelira(nom, 'CLAS', ibid, base)
!
!     -- LONMA2 : LONGUEUR DE RECOPIE :
    call assert(lonmax.gt.0)
    call assert(long.gt.0)
    lonma2=min(long,lonmax)
!
!     --- ALLOCATION D'UN TAMPON ---
    if (type(1:1) .ne. 'K') then
        call wkvect('&&JUVECA.TAMPON', 'V V '//type, lonma2, ltamp)
    else
        call jelira(nom, 'LTYP', ltyp, cbid)
        call codent(ltyp, 'G', type(2:))
        call wkvect('&&JUVECA.TAMPON', 'V V '//type, lonma2, ltamp)
    endif
!
!     --- RECOPIE L'OBJET DANS LE TAMPON ---
    if (type .eq. 'I') then
        do 10 i = 1, lonma2
            zi(ltamp+i-1) = zi(ldec+i-1)
10      continue
    else if (type .eq. 'R') then
        do 20 i = 1, lonma2
            zr(ltamp+i-1) = zr(ldec+i-1)
20      continue
    else if (type .eq. 'C') then
        do 30 i = 1, lonma2
            zc(ltamp+i-1) = zc(ldec+i-1)
30      continue
    else if (type .eq. 'L') then
        do 40 i = 1, lonma2
            zl(ltamp+i-1) = zl(ldec+i-1)
40      continue
    else if (type(1:1) .eq. 'K') then
        if (ltyp .eq. 8) then
            do 50 i = 1, lonma2
                zk8(ltamp+i-1) = zk8(ldec+i-1)
50          continue
        else if (ltyp .eq. 16) then
            do 51 i = 1, lonma2
                zk16(ltamp+i-1) = zk16(ldec+i-1)
51          continue
        else if (ltyp .eq. 24) then
            do 52 i = 1, lonma2
                zk24(ltamp+i-1) = zk24(ldec+i-1)
52          continue
        else if (ltyp .eq. 32) then
            do 53 i = 1, lonma2
                zk32(ltamp+i-1) = zk32(ldec+i-1)
53          continue
        else if (ltyp .eq. 80) then
            do 54 i = 1, lonma2
                zk80(ltamp+i-1) = zk80(ldec+i-1)
54          continue
        else
            valk(1)=nom
            valk(2)=type
            call u2mesk('F', 'JEVEUX_31', 2, valk)
        endif
    else
        valk(1)=nom
        valk(2)=type
        call u2mesk('F', 'JEVEUX_31', 2, valk)
    endif
!
!     --- DESTRUCTION DU VIEUX ET CREATION DU NEUF ---
    call jedetr(nom)
    call wkvect(nom, base//' V '//type, long, ldec)
!
!     --- RECOPIE DU TAMPON DANS L'OBJET DEFINITIF ---
    if (type .eq. 'I') then
        do 110 i = 1, lonma2
            zi(ldec+i-1) = zi(ltamp+i-1)
110      continue
    else if (type .eq. 'R') then
        do 120 i = 1, lonma2
            zr(ldec+i-1) = zr(ltamp+i-1)
120      continue
    else if (type .eq. 'C') then
        do 130 i = 1, lonma2
            zc(ldec+i-1) = zc(ltamp+i-1)
130      continue
    else if (type .eq. 'L') then
        do 140 i = 1, lonma2
            zl(ldec+i-1) = zl(ltamp+i-1)
140      continue
        do 142 i = lonma2+1, long
            zl(ldec+i-1) = .false.
142      continue
    else if (type(1:1) .eq. 'K') then
        if (ltyp .eq. 8) then
            do 150 i = 1, lonma2
                zk8(ldec+i-1) = zk8(ltamp+i-1)
150          continue
        else if (ltyp .eq. 16) then
            do 151 i = 1, lonma2
                zk16(ldec+i-1) = zk16(ltamp+i-1)
151          continue
        else if (ltyp .eq. 24) then
            do 152 i = 1, lonma2
                zk24(ldec+i-1) = zk24(ltamp+i-1)
152          continue
        else if (ltyp .eq. 32) then
            do 153 i = 1, lonma2
                zk32(ldec+i-1) = zk32(ltamp+i-1)
153          continue
        else if (ltyp .eq. 80) then
            do 154 i = 1, lonma2
                zk80(ldec+i-1) = zk80(ltamp+i-1)
154          continue
        endif
    endif
    ll = min(lonuti,long)
    if (lonuti .gt. 0) call jeecra(nom, 'LONUTI', ll, cbid)
!
!     --- DESTRUCTION DU TAMPON ---
    call jedetr('&&JUVECA.TAMPON')
    call jedema()
end subroutine
