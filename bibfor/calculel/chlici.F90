subroutine chlici(chaine, long)
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
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=
    implicit none
    include 'asterfort/assert.h'
    character(len=*) :: chaine
    integer :: long
! ----------------------------------------------------------------------
! BUT : VERIFIER QU'UNE CHAINE DE CARACTERES NE CONTIENT QUE
!       DES CARACTERES AUTORISES PAR JEVEUX : (A-Z)(a-z)(0-9)
!       PLUS : PERLUETTE, BLANC, BLANC SOULIGNE ET POINT
!       ON VERIFIE LA CHAINE SUR UNE LONGUEUR : LONG
!       SI CARACTERE ILLICITE : ERREUR FATALE <F>
!
! IN  CHAINE    : CHAINE A VERIFIER
! IN  LONG      : LA CHAINE EST VERIFIEE DE (1:LONG)
! ----------------------------------------------------------------------
    integer :: i, k
!-----------------------------------------------------------------------
!
    do 10,i = 1,long
    k = ichar(chaine(i:i))
    call assert((&
                (k.eq.32) .or. (k.eq.46) .or. (k.eq.38)&
                .or. (k.eq.95) .or. ((k.ge.48).and. (k.le.57)) .or.&
                ((k.ge.65).and. ( k.le.90)) .or. ((k.ge.97).and. (k.le.122))&
                ))
    10 end do
end subroutine
