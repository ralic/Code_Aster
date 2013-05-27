subroutine i3lchi(nil, tete, queue, pt, info,&
                  desc, succ, prec)
    implicit none
!
    integer :: nil, tete, queue, pt, info, desc(*), succ(*), prec(*)
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
!     INSERTION EN TETE DANS LE TYPE LISTE CHAINEE DE MAILLE
!     ------------------------------------------------------------------
! IN  NIL    : I : POINTEUR NIL
! IN  PT     : I : POINTEUR A INSERER
! IN  INFO   : I : VALEUR DU CHAMP INFO POINTE
! VAR TETE   : I : POINTEUR DE TETE
! VAR QUEUE  : I : POINTEUR DE QUEUE
! VAR DESC   : I : TABLE IMPLEMENTANT LA LISTE
! VAR SUCC   : I : POINTEUR SUR LA LISTE DES SUCCESEURS
! VAR PREC   : I : POINTEUR SUR LA LISTE DES PREDECESSEURS
!     ------------------------------------------------------------------
!
!======================================================================
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (tete .eq. nil) then
        tete = pt
        queue = pt
        succ(queue) = nil
        prec(tete ) = nil
    else
        succ(pt) = tete
        prec(tete) = pt
        tete = pt
        prec(tete) = nil
    endif
    desc(tete ) = info
end subroutine
