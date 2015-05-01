subroutine cuacat(indic, nbliac, ajliai, spliai, lmat,&
                  indfac, deficu, resocu, solveu, cncine,&
                  xjvmax)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "asterfort/cuaca1.h"
#include "asterfort/cuaca2.h"
    integer :: indic
    integer :: nbliac
    integer :: ajliai
    integer :: spliai
    integer :: indfac
    integer :: lmat
    real(kind=8) :: xjvmax
    character(len=24) :: deficu, resocu
    character(len=19) :: solveu, cncine
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (RESOLUTION)
!
! ROUTINE MERE POUR LE CALCUL DE A.C-1.AT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICU : SD DE DEFINITION
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  CNCINE : CHAM_NO CINEMATIQUE
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O XJVMAX : VALEUR DU PIVOT MAX
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
! I/O INDIC  : +1 ON A RAJOUTE UNE LIAISON
!              -1 ON A ENLEVE UNE LIAISON
!
! ----------------------------------------------------------------------
!
    if (indic .ne. -1) then
        call cuaca1(deficu, resocu, solveu, lmat, cncine,&
                    nbliac, ajliai)
    endif
    call cuaca2(deficu, resocu, nbliac, spliai, indfac,&
                lmat, xjvmax)
!
    indic = 1
!
end subroutine
