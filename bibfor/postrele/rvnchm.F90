subroutine rvnchm(mailla, tm, nbm, numnd, nomnd)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=24) :: numnd, nomnd
    character(len=8) :: mailla
    integer :: tm(*), nbm
!
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
!     ------------------------------------------------------------------
!     ORIENTATION D' UN CHEMIN DE MAILLE 1D
!     ------------------------------------------------------------------
! IN  MAILLA : K : NOM DU MAILLAGE
! IN  TM     : I : TABLE DES MAILLES DU CHEMIN
! IN  NBM    : I : NBR DE MAILLES DU CHEMIN
! OUT NUMND  : K :NOM DE LA TABLE DES NUM DE NOEUDS (ORDRE DE PARCOURS)
! OUT NOMND  : K :NOM DE LA TABLE DES NOM DE NOEUDS (ORDRE DE PARCOURS)
!     ------------------------------------------------------------------
!
!
    character(len=24) :: nlnbnm
    character(len=15) :: connex, nrepnd
    character(len=16) :: typmai
    character(len=8) :: ktypm
    integer :: aconec, im, m, n1, n2, nder, nbpt, nbn
    integer :: anbnm, pt, anumnd, anomnd, atypm, iatyma
!
!====================== CORPS DE LA ROUTINE ===========================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    connex = mailla//'.CONNEX'
    nrepnd = mailla//'.NOMNOE'
    typmai = mailla//'.TYPMAIL'
    nlnbnm = '&&RVNCHM.NB.NOEUD.MAILLE'
    call wkvect(nlnbnm, 'V V I', nbm, anbnm)
    nbpt = 1
    pt = 1
    do 200, im = 1, nbm, 1
    call jeveuo(typmai, 'L', iatyma)
    atypm=iatyma-1+tm(im)
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), ktypm)
    if (ktypm .eq. 'SEG2') then
        nbn = 2
    else
        nbn = 3
    endif
    nbpt = nbpt + nbn -1
    zi(anbnm + im-1) = nbn
    200 end do
    call wkvect(numnd, 'V V I', nbpt, anumnd)
    call wkvect(nomnd, 'V V K8', nbpt, anomnd)
    call jeveuo(jexnum(connex, tm(1)), 'L', aconec)
    if (nbm .le. 0) then
        call u2mess('F', 'POSTRELE_24')
    else if (nbm .eq. 1) then
        nder = zi(aconec + 1-1)
    else
        n1 = zi(aconec + 1-1)
        n2 = zi(aconec + 2-1)
        call jeveuo(jexnum(connex, tm(2)), 'L', aconec)
        if ((n1 .eq. zi(aconec)) .or. (n1 .eq. zi(aconec+1))) then
            nder = n2
        else if ((n2.eq.zi(aconec)).or.(n2.eq.zi(aconec+1))) then
            nder = n1
        else
            call u2mess('F', 'POSTRELE_25')
        endif
    endif
    zi(anumnd + pt-1) = nder
    call jenuno(jexnum(nrepnd, nder), zk8(anomnd + pt-1))
    pt = pt + 1
    do 100, im = 1, nbm, 1
    m = tm(im)
    call jeveuo(jexnum(connex, m), 'L', aconec)
    n1 = zi(aconec + 1-1)
    n2 = zi(aconec + 2-1)
    if (zi(anbnm + im-1) .eq. 3) then
        zi(anumnd + pt-1) = zi(aconec + 3-1)
        call jenuno(jexnum(nrepnd, zi(aconec+3-1)), zk8(anomnd+pt-1))
        pt = pt + 1
    endif
    if (n1 .eq. nder) then
        nder = n2
    else
        nder = n1
    endif
    zi(anumnd + pt-1) = nder
    call jenuno(jexnum(nrepnd, nder), zk8(anomnd + pt-1))
    pt = pt + 1
    100 end do
    call jedetr(nlnbnm)
    call jedema()
end subroutine
