subroutine rvchn3(vale, padr, ma, itypm, nbpt,&
                  nbcp, face, cref, nbndf, clocf,&
                  conec, vlccnc, val, ptadr, tabaux)
    implicit none
!
    integer :: padr(*), ma(*), face(*), conec(*), vlccnc(*), nbndf(6, *)
    integer :: nbpt, nbcp, ptadr, itypm, clocf(6, 4, *)
    real(kind=8) :: vale(*), cref(2, *), val(*), tabaux(4, *)
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CALCUL DU VALE D' UN SS_CHAM_NO EN UN NBPT POINT
!     ------------------------------------------------------------------
! IN  VALE   : R :  VALE DU SSCH19
! IN  PADR   : I :  PADR DU SSCH19
! IN  MA     : I :  TABLE  DE MAILLE 3D CONTRIBUANT
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
    integer :: m, f, ipt, i, j, nloc, nglo, nbnf
    real(kind=8) :: c1, c2, c3, c4, c5, r, s, unsur4
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    unsur4 = 0.25d0
    m = ma(1)
    f = face(1)
    nbnf = nbndf(f,itypm)
    do 10, i = 1, nbnf, 1
    nloc = clocf(f,i,itypm)
    nglo = conec(vlccnc(m) + nloc-1)
    do 12, j = 1, nbcp, 1
    tabaux(i,j) = vale(padr(nglo)+j-1)
12  continue
    10 end do
    do 300, ipt = 1, nbpt, 1
    r = cref(1,ipt)
    s = cref(2,ipt)
    do 320, i = 1, nbcp, 1
    c1 = tabaux(1,i)
    c2 = tabaux(2,i)
    c3 = tabaux(3,i)
    if (nbnf .eq. 3) then
        c5 = c1 + r*(c2-c1) + s*(c3-c1)
    else
        c4 = tabaux(4,i)
        c5 = r*((c2+c3-c1-c4) + s*(c1+c3-c2-c4))+c1+c2+c3+c4
        c5 = unsur4*(c5 + s*(c3+c4-c1-c2))
    endif
    val(ptadr+nbcp*(ipt-1)+i-1) = c5
320  continue
    300 end do
    ptadr = ptadr + nbpt*nbcp
end subroutine
