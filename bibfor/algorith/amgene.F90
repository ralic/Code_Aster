function amgene(i, j, amogen, nbmodes, typbas, method, lamor)
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
! Extract the term (i,j) from the damping matrix as stocked inside the
! amogen variable in mdtr74/ssdt74, the routines which prepare for a 
! transient calculation over a general basis.
! ======================================================================
! The extraction rules are as follows :
! ======================================================================
! 1) In the substructure type calculations (typbas = 'MODE_GENE') the
!    values available are stictly diagonal.
! 2) In the other cases :
!    2.1) If only modal damping coefficients are given (lamor is .TRUE.),
!         then only diagonal terms can be found.
!    2.2) If the DEVOGE 4th order integration scheme is used, then only
!         diagonal terms can be retrieved.
!    2.3) For all other cases, a full matrix is consultable.
!-----------------------------------------------------------------------
    real(kind=8) :: amgene
    integer :: i, j, nbmodes
    real(kind=8) :: amogen(*)
    character(len=*) :: typbas, method
    logical(kind=1) :: lamor
!-----------------------------------------------------------------------
    if ((typbas(1:11) .eq. 'MODELE_GENE') .or. &
        (lamor) .or. (method(1:6).eq.'DEVOGE')) then
        if (i .ne. j) then 
            amgene = 0.d0
        else 
            amgene = amogen(i)
        endif
    else 
        amgene = amogen((i-1)*nbmodes+j)
    endif
end function
