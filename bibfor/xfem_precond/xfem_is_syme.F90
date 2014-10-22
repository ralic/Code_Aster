subroutine xfem_is_syme(tab_mat, jadr, nm, iret)
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
! BUT : VERIFIER LA SYMETRIE D UNE MATRICE
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
!  * EN SORTIE : IRET=0 , SI LA MATRICE EST PRESQUE SYMETRIQUE
!                    LA MATRICE VERIFIE ALORS LE CRITERE : 
!                        |TAB_MAT-TAB_MAT^T|<SEUIL*|TAB_MAT|
!                IRET <> 0, SINON
!                 
#include "jeveux.h"
!
    integer :: nm, jadr, iret
    real(kind=8) :: tab_mat(*)
!-----------------------------------------------------------------------
    real(kind=8) :: seuil, abs_raw(nm)
    integer :: j, i
    parameter  (seuil=1.d-9)
!-----------------------------------------------------------------------
!
    iret=0
!
    abs_raw(1:nm)=0.d0
    do i=1,nm
      do j=1,nm
        abs_raw(i)=abs_raw(i)+abs(tab_mat(jadr+nm*(i-1)+j))
      enddo
    enddo
!
    do i=1,nm
      do j=1,(i-1)
        if (abs(tab_mat(jadr+nm*(i-1)+j)-tab_mat(jadr+nm*(j-1)+i)) .gt. seuil*&
             (abs_raw(i)+ abs_raw(j))/2.d0) then
          iret=-1
        endif
      enddo
    enddo
!
!
end subroutine
