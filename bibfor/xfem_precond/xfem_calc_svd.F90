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
#include "asterc/r8prem.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "blas/dgesvd.h"
#include "asterfort/matini.h"
#include "asterf_types.h"
!
    integer :: nm, jadr, info
    real(kind=8) :: tab_mat(*), scal
!-----------------------------------------------------------------------
    real(kind=8), allocatable :: ab(:,:), s(:), u(:,:), vt(:,:), work(:), diag(:)
    real(kind=8) :: work0(1), coef_j, lambda_j, mini, maxi, dii, ech
    integer :: j, i, lwork
    blas_int :: iret
    real(kind=8) :: seuil_svd, seuil_mloc
!-----------------------------------------------------------------------
!
    allocate(ab(nm,nm))
    allocate(diag(nm))
    allocate(s(nm))
    allocate(u(nm,nm))
    allocate(vt(nm,nm))
    call matini(nm, nm, 0.d0, u)
    call matini(nm, nm, 0.d0, vt)
!   MISE A L ECHELLE
    mini=abs(tab_mat(jadr+1))
    maxi=abs(tab_mat(jadr+1))
    do i=1,nm
       dii=abs(tab_mat(jadr+nm*(i-1)+i))
       diag(i)=sqrt(dii)
       mini=min(dii,mini)
       maxi=max(dii,maxi)
    enddo
    ech=(maxi+mini)/2.d0
!
    do j=1,nm
       do i=1,nm
         ab(i,j)=tab_mat(jadr+nm*(i-1)+j)/(diag(i)*diag(j))
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
    seuil_svd=sqrt(r8prem())
    seuil_mloc=sqrt(ech*5.d-17)
    do i=1,nm
       if (diag(i) .lt. seuil_mloc) then
         info=-1
         goto 99
         diag(i)=sqrt(ech)*r8gaem()
       endif
    enddo
    if ( info .eq. 0) then
       do j=1,nm
          if(s(j) .lt. 0.d0)  then
             info=-1
             goto 99
          endif
          lambda_j=s(j)/s(1)
          if (lambda_j .lt. seuil_svd) then
             coef_j=sqrt(s(1))*r8gaem()
          else
             coef_j=sqrt(s(j)) 
          endif
!     ELIMINATION DES DDLS TROP PETITS
          do i=1,nm
            tab_mat(jadr+nm*(i-1)+j)=(1.d0/scal)*(1.d0/diag(i))*(u(i,j)/coef_j)
          enddo
       enddo  
    endif
!
99  continue
!
    deallocate(diag)
    deallocate(ab)
    deallocate(s)
    deallocate(u)
    deallocate(vt)
    deallocate(work)
!
end subroutine
