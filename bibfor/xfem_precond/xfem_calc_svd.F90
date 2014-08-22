subroutine xfem_calc_svd(tab_mat, jadr, nm, scal, info)
!-----------------------------------------------------------------------
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
!
!-----------------------------------------------------------------------
! BUT : CALCUL DU PRE CONDITIONNEUR POUR LES NOEUDS XFEM
!           - CALCUL D UNE SVD  :: DGESVD (LAPACK)
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
#include "blas/dgesvd.h"
#include "asterfort/matini.h"
!
    integer :: nm, jadr, info
    real(kind=8) :: tab_mat(*), scal
!-----------------------------------------------------------------------
    real(kind=8), allocatable :: ab(:,:), s(:), u(:,:), vt(:,:), work(:)
    real(kind=8) :: work0(1), coef_j, lambda_j
    integer :: j, i, lwork
    blas_int :: iret
    real(kind=8) :: seuil
    parameter (seuil=1.E-12)
!-----------------------------------------------------------------------
!
    allocate(ab(nm,nm))
    allocate(s(nm))
    allocate(u(nm,nm))
    allocate(vt(nm,nm))
    call matini(nm, nm, 0.d0, u)
    call matini(nm, nm, 0.d0, vt)
    do j=1,nm
       do i=1,nm
         ab(i,j)=tab_mat(jadr+nm*(i-1)+j)
       enddo
    enddo
    info=0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALCUL DE LA SVD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call dgesvd('S', 'S', nm, nm, ab, nm, s, u, nm, vt, nm,&
                work0, -1, iret)
!
    lwork=int(work0(1))
    allocate(work(lwork))
!
    call dgesvd('S', 'S', nm, nm, ab, nm, s, u, nm, vt, nm,&
                work, lwork, iret)
    info=iret
!
    if (info .ne. 0) then
      if (info .lt. 0)  write(6,*) '<SVD> argument illegal n°',info
      if (info .gt. 0)  &
         write(6,*) '<SVD> sous-matrice non inversible à la ligne n°',info
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! RECOPIE FINALE DANS TAB_MAT : FORMAT PAR LIGNE DOMINANTE
!    ON MULTIPLIE PAR SCAL : UN COEFFICIENT DE MISE A L ECHELLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( info .eq. 0) then
!       ASSERT(s(1) .gt. 0.d0)
       do j=1,nm
          ASSERT(s(j) .ge. 0.d0)         
          lambda_j=s(j)/s(1)
          if (lambda_j .lt. seuil) then
!             write(6,*) '<SVD> sous-matrice non inversible à la ligne n°',j             
             coef_j=dsqrt(s(1))*1.d16
          else
             coef_j=dsqrt(s(j)) 
          endif
          do i=1,nm
             tab_mat(jadr+nm*(i-1)+j)=(1/scal)*u(i,j)/coef_j
          enddo
       enddo  
    endif
!
    deallocate(ab)
    deallocate(s)
    deallocate(u)
    deallocate(vt)
    deallocate(work)
!
end subroutine
