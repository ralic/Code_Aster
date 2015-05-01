subroutine xfem_calc_inv(tab_mat, jadr, nm, scal, info)
!-----------------------------------------------------------------------
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
!
!-----------------------------------------------------------------------
! BUT : CALCUL DU PRE CONDITIONNEUR POUR LES NOEUDS XFEM
!           - CALCUL DE LA FACTORISEE DE CHOLESKY :: DPORTF (LAPACK)
!           - CALCUL DE L INVERSE DE CHOLESKY     :: DTRTRI (LAPACK)
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
!  REMARQUE SUR LE STOCKAGE DES MATRICES LOCALES:
!  * EN ENTREE : LES MATRICES LOCALES SONT STOCKEES A PLAT PAR LIGNE DOMINANTE
!                SEULE LA PARTIE SUPERIEURE EST PRISE EN COMPTE (COMME LA MATRICE 
!                LOCALE EST A PRIORI SYMETRIQUE)
!                M(I,J)=TAB_LOC(DECA*(INO-1)+I*NM+J) AVEC I=<J
!  * EN SORTIE : LES MATRICES LOCALES SONT STOCKEES A PLAT ET PAR LIGNE DOMINANTE
!                M(I,J)=TAB_LOC(DECA*(INO-1)+I*NM+J)
#include "jeveux.h"
#include "asterfort/assert.h"
#include "blas/dtrtri.h"
#include "asterfort/matini.h"
!
    integer :: nm, jadr
    real(kind=8) :: tab_mat(*), scal
     
!-----------------------------------------------------------------------
    real(kind=8) :: ab(nm,nm)
    integer :: j, i, info 
!-----------------------------------------------------------------------
!
    call matini(nm, nm, 0.d0, ab)
    do j=1,nm
       do i=1,j
         ab(i,j)=tab_mat(jadr+nm*(i-1)+j)
       enddo
    enddo
    info=0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INVERSION DE LA MATRICE TRIANGULAIRE SUPERIEURE :
!   L INVERSE ETANT AUSSI UNE MATRICE TRIANGULAIRE SUPERIEURE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call dtrtri('U', 'N', nm, ab, nm, info)
!
    if (info .ne. 0) then
      if (info .lt. 0)  write(6,*) '<INV> argument illegal n°',info
      if (info .gt. 0)  &
         write(6,*) '<INV> sous-matrice non inversible à la ligne n°',info
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! RECOPIE FINALE DANS TAB_MAT : FORMAT PAR LIGNE DOMINANTE
!!! ON MULTIPLIE PAR SCAL : UN COEFFICIENT DE MISE A L ECHELLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( info .eq. 0) then
       do j=1,nm
          do i=1,j
            tab_mat(jadr+nm*(j-1)+i)=0.d0
            tab_mat(jadr+nm*(i-1)+j)=scal*ab(i,j)
          enddo
       enddo    
    endif
!
end subroutine
