subroutine mmmtdb(valr, typmat, ii, jj)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    real(kind=8) :: valr
    character(len=2) :: typmat
    integer :: ii, jj
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! ROUTINE DE DEBUGGAGE POUR LES TE
!
! ----------------------------------------------------------------------
!
!
! IN  VALR   : VALEUR DE LA COMPOSANTE DANS LA MATRICE
! IN  II     : LIGNE DE LA MATRICE
! IN  JJ     : COLONNE DE LA MATRICE
! IN  TYPMAT : TYPE DE MATRICE PONCTUELLE
!                'XY' AVEC
!                E  - ESCLAVE
!                M  - MAITRE
!                C  - CONTACT
!                F  - FROTTEMENT
!                IJ - MATR_ELEM
!
! ----------------------------------------------------------------------
!
    if (valr .ne. 0.d0) then
        write(6,*) 'MATR-ELEM-'//typmat,'(',ii,',',jj,'): ',valr
    endif
!
end subroutine
