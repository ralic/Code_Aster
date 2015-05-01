subroutine intimp(iuni, vect1, vect2, nmatr, nfcod)
    implicit   none
    integer :: iuni, nmatr, nfcod
    real(kind=8) :: vect1(*)
    character(len=24) :: vect2(*)
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
!     IMPRESSION DES RESULTATS OBTENUS PAR L'OPERATEUR CALC_INTE_SPEC
!
!     ------------------------------------------------------------------
!     IN  : IMP   : NIVEAU D'IMPRESSION
!     IN  : VECT1 : VALEURS DES INTERSPECTRES ET DES AUTOSPECTRES
!     IN  : VECT2 : VECTEURS DES NOMS DE FONCTIONS
!     IN  : NMATR : NOMBRE DE TIRAGES REALISES
!     ------------------------------------------------------------------
    integer :: it1, it2, if, ib, l1
!     ------------------------------------------------------------------
!
    write (iuni,101)
    it1 = 1
    it2 = 0
    do 10 if = 1, nfcod
        if (if .eq. it1) then
            write (iuni,102) vect2(if)
            it2 = it2 + 1
            it1 = it1 + it2 + 1
        else
            write (iuni,100) vect2(if)
        endif
        do 20 ib = 1, nmatr
            l1 = (if-1)*nmatr + ib
            write (iuni,200) vect1(l1)
20      continue
10  end do
!
    101 format ( 'CONVERGENCE DE LA MATRICE INTERSPECTRALE ',&
     &         'EN FONCTION DU NOMBRE DE TIRAGES ALEATOIRES')
    100 format ( 'NOM DE L INTERSPECTRE :',a19 )
    102 format ( 'NOM DE L AUTOSPECTRE :',a19 )
    200 format ( 4 (e11.4,2x) )
!
end subroutine
