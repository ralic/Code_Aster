subroutine i3lchs(nil, tete, queue, succ, prec,&
                  pt, nbpt)
    implicit none
!
    integer :: nil, tete, queue, succ(*), prec(*), pt(*), nbpt
!
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     SUPPRESSION DE PT DANS LE TYPE LISTE CHAINEE DE MAILLE
!     ------------------------------------------------------------------
! IN  NIL    : I : POINTEUR NIL
! IN  PT     : I : TABLE DES POINTEUR A SUPPRIMER
! IN  NBPT   : I : NBR DE POINTEUR A SUPPRIMER
! VAR TETE   : I : POINTEUR DE TETE
! VAR QUEUE  : I : POINTEUR DE QUEUE
! VAR SUCC   : I : POINTEUR SUR LA LISTE DES SUCCESEURS
! VAR PREC   : I : POINTEUR SUR LA LISTE DES PREDECESSEURS
!     ------------------------------------------------------------------
!     LA LISTE EST SUPPOSEE NON VIDE
!     ------------------------------------------------------------------
!
    integer :: pts, ptp, ptc, i
!
!======================================================================
!
    do 10, i = 1, nbpt, 1
    ptc = pt(i)
    if (ptc .eq. tete) then
        tete = succ(tete)
        if (tete .ne. nil) then
            prec(tete) = nil
        endif
    else if (ptc .eq. queue) then
        queue = prec(queue)
        if (queue .ne. nil) then
            succ(queue) = nil
        endif
    else
        ptp = prec(ptc)
        pts = succ(ptc)
        prec(pts) = ptp
        succ(ptp) = pts
    endif
    10 end do
end subroutine
