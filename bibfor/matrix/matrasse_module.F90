! 
! Sparse Real Matrix stored in Aster format
! This module provides some tools for aster "matr_asse"
! matrices 
! 
module matrasse_module
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! aslint: disable=C1310
!
implicit none
private
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h" 
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
  !
  integer, private :: ierr
  integer, parameter, public :: lagrange1_dof=0, physical_dof=1, lagrange2_dof =2 
  !
  public :: get_indices_of_dofs
  !
  contains 
    ! 
    ! This function allocates, fills and returns an integer array with 
    ! the (Fortran) indices of some selected dofs : one may select 
    ! physical_dof or lagrange1_dof or lagrange2_dof
    ! 
    function get_indices_of_dofs(type_dof, matass) result( idof )
    !
    ! Dummy arguments 
    integer, intent(in)                     :: type_dof
    character(len=19), intent(in)           :: matass
    integer(kind=4),dimension(:), pointer   :: idof
    ! Local variables 
    character(len=14) :: nonu
    integer, dimension(:), pointer :: delg => null()
    integer :: nbeq, nlag1, nlag2, nphys, ndof, iret
    integer :: i 
    mpi_int :: mpicomm
    !
    call jemarq() 
    !
    ASSERT((type_dof==physical_dof).or.(type_dof == lagrange1_dof).or.(type_dof == lagrange2_dof)) 
    !
    idof => null()
    ! Communicateur MPI 
    call asmpi_comm('GET', mpicomm)
    !
    ! La matrice existe-t-elle ? 
    call jeexin(matass//'.REFA', iret)
    ASSERT(iret > 0)
    ! Quelle est sa taille ? 
    call dismoi('NB_EQUA', matass, 'MATR_ASSE', repi=nbeq)
    ! Le tableau delg permet de distinguer ddls physiques/lagrange
    call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=nonu)
    call jeveuo(nonu//'.NUME.DELG', 'L', vi=delg)
    !
    ! Construction du vecteur d'indices
    !
    ! Nombre de ddls physiques 
    nphys=count( delg(1:nbeq) == 0)
    ! Nombre de Lagrange 1
    nlag1=count( delg(1:nbeq) == -1 )
    ! Nombre de Lagrange 2
    nlag2=count( delg(1:nbeq) == -2 )
    ! Vérification de la cohérence des dimensions trouvées 
    ASSERT(nbeq.eq.nphys+nlag1+nlag2)
    ! Nombre de ddls sélectionnés (soit lagrange1, soit physiques)
    select case( type_dof ) 
    case( physical_dof )
       ndof = nphys
    case( lagrange1_dof )
       ndof = nlag1
    case( lagrange2_dof ) 
       ndof = nlag2 
    end select 
    !
    ! Allocation du vecteur d'indices (Fortran)
    allocate( idof( ndof ), stat = ierr )
    ASSERT( ierr == 0 )
    idof(:) = 0
    !
    ndof = 0 
    do i = 1, nbeq
      if (delg(i) .eq. 0) then
        if ( type_dof == physical_dof ) then 
          ndof = ndof + 1
          idof(ndof)= i
        endif 
      endif
      if (delg(i) .eq. -1) then
        nlag1 = nlag1 + 1
        if ( type_dof == lagrange1_dof ) then
            ndof = ndof + 1 
            idof(ndof) = i
        endif
      endif
      if (delg(i) .eq. -2) then
        nlag2 = nlag2 + 1
        if ( type_dof == lagrange2_dof ) then
            ndof = ndof + 1 
            idof(ndof) = i
        endif
      endif
   end do
   !
   call jedema()
   end function get_indices_of_dofs
   !
  end module matrasse_module
