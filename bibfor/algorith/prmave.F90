subroutine prmave(ipr, amat, na, na1, na2,&
                  bvec, nb1, cvec, nc1, ier)
!
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
    integer :: na
    integer :: na1, na2, nb1, nc1, ipr, ier
    real(kind=8) :: amat(na, *), bvec(*), cvec(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE
!
! PRODUIT  MATRICE VECTEUR : C = A * B
!
!
! ----------------------------------------------------------------------
!
!
! IN  IPR      : =  0 => C = A * B
!                <> 0 => C = C + A * B
! IN  AMAT     : MATRICE A
! IN  NA       : NOMBRE DE LIGNES DE AMAT
! IN  NA1      : NOMBRE DE LIGNES UTILISEES DANS AMAT
!                POUR LA MULTIPLICATION
! IN  NA2      : NOMBRE DE COLONNES UTILISEES DANS AMAT
!                POUR LA MULTIPLICATION
! IN  BVEC     : VECTEUR B
! IN  NB1      : NOMBRE DE LIGNES UTILISEES DANS B
!                POUR LA MULTIPLICATION
! OUT CVEC     : VECTEUR RESULTAT C
! IN  NC1      : NOMBRE DE LIGNES DE C
! IN  IER      : CODE RETOUR : = 0 => TOUT S'EST BIEN PASSE
!                              > 0 => CA S'EST MAL PASSE
!
!
! ----------------------------------------------------------------------
!
    integer :: i, j
!
! ----------------------------------------------------------------------
!
    ier=0
!
    if (na2 .ne. nb1) then
        ier = ier + 1
    endif
!
    if (nc1 .ne. na1) then
        ier = ier + 1
    endif
!
    if (ier .eq. 0) then
!
        if (ipr .eq. 0) then
!
            do 10 i = 1, na1
                cvec(i)=0.0d0
10          continue
!
        endif
!
        do 20 i = 1, na1
            do 30 j = 1, na2
                cvec(i)=cvec(i)+amat(i,j)*bvec(j)
30          continue
20      continue
!
    endif
!
end subroutine
