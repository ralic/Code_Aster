subroutine premla(neq, diag, col, lt, nrl,&
                  rl, deb, vois, suit, ier)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/calajt.h'
    include 'asterfort/infniv.h'
    integer :: neq, diag(0:neq), col(*), deb(neq)
    integer :: vois(*), suit(*), lt, nrl, rl(4, nrl), ier
!     VARIABLES LOCALES
    integer :: i, j, j1, j2, k, illist, ifm, niv, lbd2
!****************************************************************
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
    ier=0
    if (nrl .ne. 0) then
!
!     AVEC RELATION LINEAIRE
!     CALCUL DES LISTES DE NOEUDS A AJOUTER
        do 190 i = 1, neq
            deb(i) =0
190      continue
        illist = 0
        do 200 k = 1, nrl
            lbd2 = rl(2,k)
            j1 = diag(lbd2-1) + 2
            j2 = diag(lbd2) - 1
            do 210 j = j2, j1, -1
!     ON AJOUTE COL(J1),..., COL(J-1) AUX VOISINS DE COL(J)
                call calajt(j1, j, diag, col, neq,&
                            illist, deb, vois, suit, lt,&
                            ier)
                if (ier .gt. 0) goto 999
210          continue
!
200      continue
    endif
999  continue
!
!
end subroutine
