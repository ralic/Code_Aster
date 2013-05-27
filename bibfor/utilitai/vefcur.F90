subroutine vefcur(vec1, nbn, knom, vec2, nbvale,&
                  nomnoe)
    implicit none
! ----------------------------------------------------------------------
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
!   VERIFICATION DE LA DEFINITION DE LA FONCTION : EXISTENCE DES NOEUDS
!    ET ORDRE D'APPARITION DANS LA LISTE. LECTURE DU NOMBRE DE VALEURS
!                POUR LE DIMENSIONNEMENT DU .VALE
! ----------------------------------------------------------------------
!  IN : VEC1    : I  LISTE DES NUMEROS DE NOEUDS (ABS_CURV)
!  IN : NBN     :    DIMENSION DE VEC1
!  IN : KNOM    : K8 NOM DES NOEUDS
!  OUT: VEC2    : I  POINTEURS D INDICE DE NOEUDS
!  IN : NBVALE  :    DIMENSION DES VECTEURS KNOM ET VEC2
! ----------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mess.h'
    integer :: nbn, vec1(nbn), nbvale, vec2(nbvale)
    character(len=8) :: knom(nbvale), nomnd
    character(len=24) :: nomnoe
    integer :: i, it, ji, jj, jp, numn
!     ------------------------------------------------------------------
!
!
    do 10 i = 1, nbvale
        nomnd = knom(i)
        call jenonu(jexnom(nomnoe, nomnd), numn)
!
        do 20 jj = 1, nbn
            if (vec1(jj) .eq. numn) then
                vec2(i) = jj
                it = 1
            endif
20      continue
        if (it .ne. 1) then
            call u2mess('F', 'UTILITAI5_59')
        endif
        it = 0
10  end do
    do 30 i = 1, nbvale
        jp = vec2(i)
        ji = i
        do 40 jj = i, nbvale
            if (vec2(jj) .lt. jp) then
                ji = jj
                jp = vec2(jj)
            endif
40      continue
        vec2(ji) = vec2(i)
        vec2(i) = jp
30  end do
end subroutine
