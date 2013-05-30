subroutine utacli(x, liste, indmax, tole, indice)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: indmax, indice
    real(kind=8) :: x, liste(0:indmax-1), tole
!
! ----------------------------------------------------------------------
!               RECHERCHE DANS UNE LISTE DE REELS MONOTONE
! ----------------------------------------------------------------------
! IN  X      : REEL RECHERCHE
! IN  LISTE  : LISTE (0:INDMAX-1) DE TAILLE INDMAX
! IN  INDMAX : INDICE MAXIMUM DANS LA LISTE
! IN  TOLE   : TOLERANCE ABSOLUE AUTORISEE
! OUT INDICE : INDICE DU REEL LE PLUS PROCHE DANS LE SEUIL DE TOLERANCE
!               INDICE = -1 : REEL NON TROUVE
! ----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: delta, delmin
! ----------------------------------------------------------------------
!
!
!    RECHERCHE DU PREMIER INDICE CORRESPONDANT
    do 10 i = 0, indmax-1
        delta = abs(x-liste(i))
        if (delta .le. tole) goto 100
10  end do
!
!    AUCUN INDICE TROUVE
    indice = -1
    goto 9999
!
!    RECHERCHE DE L'INDICE OPTIMAL (CARACTERE MONOTONE DE LA LISTE)
100  continue
    indice = i
    delmin = delta
    do 110 i = indice+1, indmax-1
        delta = abs(x-liste(i))
        if (delta .gt. delmin) goto 9999
        delmin = delta
        indice = i
110  end do
!
!
9999  continue
end subroutine
