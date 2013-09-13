subroutine rvchl3(vale, padr, pnsp, pnbn, ma,&
                  nbma, itypm, nbco, nbsp, nbpt,&
                  nbcp, face, cref, nbndf, clocf,&
                  conec, vlccnc, val, ptadr, tabaux)
    implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/i3nloc.h"
#include "asterfort/utmess.h"
    integer :: padr(*), pnbn(*), pnsp(*)
    integer :: ma(*), face(*), conec(*), vlccnc(*), nbndf(6, *)
    integer :: nbco, nbsp, nbpt, nbma, nbcp, ptadr, itypm, clocf(6, 4, *)
    real(kind=8) :: vale(*), cref(2, *), val(*), tabaux(4, *)
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
!
!     ------------------------------------------------------------------
!     CALCUL DU VALE D' UN SS_CHAM_ELEM SUR 1 MAILLE DE NBPT
!     A NBCO COUCHE, NBSP SOUS-POINT POUR NBCP
!     RQ : LE CAS NBPT = 1 CORRESPOND A UN SS_CHAM_NO
!     ------------------------------------------------------------------
! IN  VALE   : R :  VALE DU SSCH19
! IN  PADR   : I :  PADR DU SSCH19
! IN  PNBN   : I :  PNBN DU SSCH19
! IN  PNSP   : I :  PNSP DU SSCH19
! IN  MA     : I :  TABLE  DE MAILLE 3D CONTRIBUANT
! IN  NBMA   : I :  NOMBRE DE MAILLE 3D CONTRIBUANT
! IN  NBCO   : I :  NBCO DU SS_CHAM_ELEM
! IN  NBSP   : I :  NBSP DU SS_CHAM_ELEM
! IN  NBCP   : I :  NBCP DU SS_CHAM_ELEM
! IN  NBPT   : I :  NOMBRE DE POINT DE LA MAILLE DU SS_CHAM_ELEM
! IN  FACE   : I :  TABLE DES FACE (POUR MA(1))CONTENANT LES POINT
! IN  CREF   : R :  TABLE(1..2,1..NBPT) DES C_REF DES PTS DANS LEUR FACE
! IN  NBNDF  : I :  TABLE(1..6,1..3)     --+
! IN  CLOCF  : I :  TABLE(1..6,1..4,1..3)--+ --> DESC_TYPE_MAILLE_3D
! IN  CONEC  : I :  CONNECTIVITE DU MAILLAGE
! IN  VLCCNC : I :  LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE
! OUT VAL    : R :  VALE DU SS_CHAM_ELEM (ET DE LA SD_EVAL SOUS-JACENTE)
! VAR PTADR  : I :  POINTEUR SUR LA PARTIE DE VALE NON AFFECTEE
!     ------------------------------------------------------------------
!     NBPT > 1 => LES POINT SONT SUR  UNE MEME FACE OU ARETE DE MA(1)
!                 MA(I), I>1,ONT EN COMMUN CETTE FACE OU ARETE
!     ------------------------------------------------------------------
    integer :: m, f, ipt, ico, i, j, k, ndglo1(4), ndloc(4), nloc
    integer :: nspm, nbnm, dec1, dec2, dec3, nbnf
    real(kind=8) :: c1, c2, c3, c4, c5, r, s, unsur4
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    unsur4 = 0.25d0
!
    do 300, ipt = 1, nbpt, 1
    m = ma(1)
    f = face(ipt)
    nbnf = nbndf(f,itypm)
    nbnm = pnbn(m)
    nspm = pnsp(m)
    do 10, i = 1, nbnf, 1
    nloc = clocf(f,i,itypm)
    ndglo1(i) = conec(vlccnc(m) + nloc-1)
    ndloc (i) = nloc
    dec1 = nspm*nbcp*(nloc-1)
    do 11, ico = 1, nbco, 1
    dec2 = nbcp*nbsp*(ico-1)
    dec3 = nbcp*nspm*nbnm*(ico-1)
    do 12, j = 1, nbsp*nbcp, 1
    tabaux(i,dec2+j) = vale(padr(m)+dec1+dec3+j-1)
12  continue
11  continue
10  continue
    do 100, k = 2, nbma, 1
    m = ma(k)
    nbnm = pnbn(m)
    nspm = pnsp(m)
    call i3nloc(ndglo1, conec(vlccnc(m)), nbnf, nbnm, ndloc)
    dec1 = 0
    do 101, i = 1, nbnf, 1
    dec1 = dec1 + max(0,min(1,ndloc(i)))
101  continue
    if (dec1 .lt. 2) then
        call utmess('F', 'POSTRELE_16')
    endif
    do 110, i = 1, nbnf, 1
    nloc = ndloc(i)
    if (nloc .gt. 0) then
        dec1 = nspm*nbcp*(nloc-1)
        do 111, ico = 1, nbco, 1
        dec2 = nbcp*nbsp*(ico-1)
        dec3 = nbcp*nspm*nbnm*(ico-1)
        do 112, j = 1, nbsp*nbcp, 1
        if (tabaux(i,dec2+j) .ne. r8vide()) then
            tabaux(i,dec2+j) = vale(padr(m)+dec1+ dec3+j-1) + tabaux(i,dec2+j)
        endif
112      continue
111      continue
    endif
110  continue
100  continue
!
    do 200, i = 1, nbnf, 1
    do 210, j = 1, nbco*nbsp*nbcp, 1
    if (tabaux(i,j) .ne. r8vide()) then
        tabaux(i,j) = tabaux(i,j)/nbma
    endif
210  continue
200  continue
!
    r = cref(1,ipt)
    s = cref(2,ipt)
    dec1 = nbcp*nbsp*(ipt-1)
    do 310, ico = 1, nbco, 1
    dec2 = nbcp*nbpt*nbsp*(ico-1)
    do 320, i = 1, nbcp*nbsp, 1
    if (tabaux(1,i) .eq. r8vide()) then
        c5 = r8vide()
    else
        c1 = tabaux(1,i)
        c2 = tabaux(2,i)
        c3 = tabaux(3,i)
        if (nbnf .eq. 3) then
            c5 = c1 + r*(c2-c1) + s*(c3-c1)
        else
            c4 = tabaux(4,i)
            c5 = r*((c2+c3-c1-c4) + s*(c1+c3-c2-c4))+c1+ c2+c3+c4
            c5 = unsur4*(c5 + s*(c3+c4-c1-c2))
        endif
    endif
    val(ptadr+dec1+dec2+i-1) = c5
320  continue
310  continue
!
    300 end do
!
    ptadr = ptadr + nbpt*nbcp*nbsp*nbco
!
end subroutine
