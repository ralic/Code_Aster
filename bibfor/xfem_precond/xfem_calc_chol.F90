subroutine xfem_calc_chol(tab_mat, jadr, nm, scal, info, methode)
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
! BUT : CALCUL DE LA RACINE CARREE D UNE PETITE MATRICE SYTMTRIQUE POSITIVE :
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!! M=S*S^T  !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!           - PAR LA FACTORISEE DE CHOLESKY :: DPORTF (LAPACK) 
!           - PAR UNE SVD  :: DGESVD (LAPACK)
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!  - TAB_MAT (IN/OUT) : MATRICE DE TAILLE NMxNM STOCKEE A PLAT (CF. REMARQUE STOCKAGE)
!  - JADR (IN)        : ADRESSE DE LA MATRICE DANS LA PILE DE REELS (CF. REMARQUE STOCKAGE)
!  - NM (IN)          : TAILLE DE LA MATRICE 
!  - SCAL (IN)        : COEFFICIENT DE MISE A L ECHELLE
!  - INFO (OUT)       : INFORMATION SUR LE DEROULEMENT DU CALCUL
!                * INFO = 0  : OK
!                * INFO <> 0 : IL Y A EU UN PROBLEME
!  - METHODE (OUT)    : LA METHODE CHOISIE POUR LE CALCUL DE LA RACINE CARREE DE LA MATRICE
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
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "blas/dpotrf.h"
#include "asterfort/infniv.h"
#include "asterfort/matini.h"
#include "asterfort/xfem_calc_svd.h"
#include "asterfort/utmess.h"
!
    integer :: nm, jadr
    real(kind=8) :: tab_mat(*), scal
    character(len=8) :: methode   
!-----------------------------------------------------------------------
    integer :: j, i, info, niv, ifm
    real(kind=8) :: prod, dete, trace, seuil, ab(nm,nm)
    parameter  (seuil=1.E-9)
!-----------------------------------------------------------------------
!
    methode='CHOLESKY'
    call matini(nm, nm, 0.d0, ab)
    do j=1,nm
       do i=1,j
         ab(i,j)=tab_mat(jadr+nm*(i-1)+j)
       enddo
    enddo
    info=0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!FACTORISATION DE CHOLESKY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call dpotrf('U', nm, ab, nm, info)
!   VERIFICATIONS
    if (info .ne. 0) then
      call infniv(ifm, niv)
      if(niv .ge. 2) then
        if (info .lt. 0)  write(6,*) '<CHOL> argument illegal n°',info
        if (info .gt. 0)  &
           write(6,*) '<CHOL> sous-matrice non factorisable à la ligne n°',info 
        write(6,*) '<CHOL> Remplacement de la méthode cholesky par le méthode svd'
      endif
      methode='SVD'
      call xfem_calc_svd(tab_mat, jadr, nm, scal, info)
    else
!    VERIFICATION MATRICE MATRICE LOCALE INVERSIBLE
!      SI DET(G)>SEUIL ALORS DET(MLOC)>SEUIL^2
      prod=1.d0
      trace=0.d0
      do i=1,nm
        prod=prod*ab(i,i)
        trace=trace+ab(i,i)
      enddo
      dete=abs(prod/(trace/nm)**nm)
      if (dete .lt. seuil) then
         methode='SVD'
         call xfem_calc_svd(tab_mat, jadr, nm, scal, info)
         if (info .ne. 0) then
            call utmess('A', 'XFEMPRECOND_5')
         endif
      else
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! RECOPIE FINALE DANS TAB_MAT : FORMAT PAR LIGNE DOMINANTE
!!! ON MULTIPLIE PAR SCAL : UN COEFFICIENT DE MISE A L ECHELLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       do j=1,nm
          do i=1,j
            tab_mat(jadr+nm*(j-1)+i)=0.d0
            tab_mat(jadr+nm*(i-1)+j)=scal*ab(i,j)
          enddo
       enddo   
      endif
    endif
!
end subroutine
