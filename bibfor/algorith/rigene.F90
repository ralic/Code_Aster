function rigene(i, j, riggen, nbmodes, typbas, method)
    implicit none
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
!-----------------------------------------------------------------------
! DYNA_VIBRA // TRAN/GENE
! Extract the term (i,j) from the rigidity matrix as stocked inside the
! riggen variable in mdtr74/ssdt74, the routines which prepare for a 
! transient calculation over a general basis.
! ======================================================================
! The extraction rules are as follows :
! ======================================================================
! 1) In the substructure type calculations (typbas = 'MODELE_GENE') the
!    values available are stictly diagonal.
! 2) In the case of simple projection on a given set of normal modes, 
!    the rigidity matrix is diagonal and thus only diagonal terms are
!    present. (typbas = 'MODE_MECA' or 'MODE_GENE')
! 3) If the projection basis is composed (RITZ or other) :
!    3.1) If DEVOGE integration method is used, then only diagonal values
!         can be retrieved.
!    3.2) A full matrix is available in other cases(typbas = 'BASE_MODA')
!-----------------------------------------------------------------------
    real(kind=8) :: rigene
    integer :: i, j, nbmodes
    real(kind=8) :: riggen(*)
    character(len=*) :: typbas, method
!-----------------------------------------------------------------------
    if ((typbas(1:4).eq.'MODE').or.(method(1:6).eq.'DEVOGE')) then
        if (i .ne. j) then 
            rigene = 0.d0
        else 
            rigene = riggen(i)
        endif
    else
        rigene = riggen((i-1)*nbmodes+j)
    endif
end function
