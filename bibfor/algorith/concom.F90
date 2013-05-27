subroutine concom(macor, nblir, macoc, nblic, nbnoco,&
                  nococ)
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
!
!  ROUTINE CONCOM
!    RECHERCHE DES NOEUDS COMMUNS
!  DECLARATIONS
!    INC    : INDICE SUR LES NOEUD          POUR UNE MAILLE FISSURE
!    INR    : INDICE SUR LES NOEUD          POUR UNE MAILLE REFERENCE
!    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE FISSURE
!    MACOR  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE REFERENCE
!    NBLIC  : NOMBRE DE NOEUD TESTES        POUR UNE MAILLE FISSURE
!    NBLIR  : NOMBRE DE NOEUD TESTES        POUR UNE MAILLE REFERENCE
!    NBNOCO : NOMBRE DE NOEUD COMMUNS
!    NOCOC  : TABLEAU DE RANG DES NOEUDS    POUR UNE MAILLE FISSURE
!
!  MOT_CLEF : ORIE_FISSURE
!
    implicit none
!
!     ------------------------------------------------------------------
!
    integer :: nbnoco, nblir, inr, nblic, inc, nococ(*)
!
    character(len=8) :: macor(nblir+2), macoc(nblic+2)
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nbnoco = 0
    do 20 inc = 1, nblic
        do 10 inr = 1, nblir
            if (macor(inr+2) .eq. macoc(inc+2)) then
                nbnoco = nbnoco + 1
                nococ(nbnoco) = inc
            endif
10      continue
20  end do
!
end subroutine
