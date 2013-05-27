subroutine connec(nomte, nse, nnop2, c)
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
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/teattr.h'
    include 'asterfort/tecael.h'
    character(len=16) :: nomte
    integer :: nsemax, nnomax
!-----------------------------------------------------------------------
    integer :: ibid
!-----------------------------------------------------------------------
    parameter (nsemax = 6)
    parameter (nnomax = 9)
    integer :: nse, nnop2, c(nsemax, nnomax)
!
! ......................................................................
!    - FONCTION REALISEE:  INITIALISATION DES ELEMENTS ISO-P2
!
!    - ARGUMENTS:
!        DONNEES:    NOMTE         -->  NOM DU TYPE ELEMENT
!        SORTIES:    NSE           <--  NOMBRE DE SOUS-ELEMENTS P1
!                    NNOP2         <--  NOMBRE DE NOEUD DE L'ELEMENT P2
!                    C (NSE*NNO)   <--  CONNECTIVITE DES SOUS-ELEMENTS
! ......................................................................
!
!
    integer :: nno, i, j, iadzi, iazk24
    character(len=8) :: alias8
!
    call tecael(iadzi, iazk24)
    nno = zi(iadzi-1+2)
!
! INITIALISATION DU TABLEAU COMPLET
!
    nse = 1
    nnop2 = nno
    do 20 i = 1, nsemax
        do 10 j = 1, nnomax
            c(i,j) = j
10      continue
20  end do
!
! CONNECTIVITE DES SOUS ELEMENTS (ELEMENTS ISO_P2)
!
    call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
!
    if (lteatt(' ','LUMPE','OUI') .and. (alias8(6:8).eq.'SE3')) then
        nnop2 = 3
        nse = 2
        c(1,1) = 1
        c(1,2) = 3
        c(2,1) = c(1,2)
        c(2,2) = 2
!
        else if (lteatt(' ','LUMPE','OUI').and. (alias8(6:8).eq.'TR6'))&
    then
        nnop2 = 6
        nse = 4
        c(1,1) = 1
        c(1,2) = 4
        c(1,3) = 6
        c(2,1) = c(1,2)
        c(2,2) = 2
        c(2,3) = 5
        c(3,1) = c(1,3)
        c(3,2) = c(2,3)
        c(3,3) = 3
        c(4,1) = c(1,2)
        c(4,2) = c(2,3)
        c(4,3) = c(1,3)
        else if (lteatt(' ','LUMPE','OUI').and. (alias8(6:8).eq.'QU9'))&
    then
        nnop2 = 9
        nse = 4
        c(1,1) = 1
        c(1,2) = 5
        c(1,3) = 9
        c(1,4) = 8
        c(2,1) = c(1,2)
        c(2,2) = 2
        c(2,3) = 6
        c(2,4) = c(1,3)
        c(3,1) = c(1,3)
        c(3,2) = c(2,3)
        c(3,3) = 3
        c(3,4) = 7
        c(4,1) = c(1,4)
        c(4,2) = c(1,3)
        c(4,3) = c(3,4)
        c(4,4) = 4
    endif
!
end subroutine
