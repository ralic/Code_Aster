module petsc_data_module
!
! aslint:disable=C1308
implicit none
#include "asterf.h"
private 
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!-------------------------------------------------------------------- 
!
! COPYRIGHT (C) 2016 -     EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!----------------------------------------------------------------
!
! person_in_charge natacha.bereux at edf.fr 
!
! Variables globales PETSc pour la définition d'un système linéaire 
! au format PETSc 
!
! On ne peut pas creer directement un tableau de pointer,
! il faut passer par un artifice (type derive) :
 
 type p_int4
 sequence
    integer(kind=4), pointer :: pi4(:)
 end type

 integer, parameter, public :: nmxins=5
 character(len=19), public  :: nomats(nmxins), nosols(nmxins), nomat_courant
 character(len=14), public  :: nonus(nmxins),nonu_courant
 Mat, public :: ap(nmxins)
 KSP, public :: kp(nmxins)
 Vec, public :: b, x

! Les variables suivantes sont utilisees par les preconditionneurs multigrille
 integer(kind=4), public :: tblocs(nmxins),fictifs(nmxins)
 type(p_int4), target, public :: new_ieqs(nmxins), old_ieqs(nmxins)
!
!----------------------------------------------------------------
! Variables globales pour la définition d'un preconditionneur 
! simple precision ldlt_sp
 character(len=19), public :: spsomu, spmat, spsolv
 Vec, public :: xlocal, xglobal
 VecScatter, public :: xscatt
!----------------------------------------------------------------
!
public :: get_mat_id, mat_record
!
contains 
!
! Cette fonction renvoie l'identifiant (kptsc) permettant d'accéder 
! à la matrice au format PETSc créée à partir de la matr_asse de nom
! matas. 
! Si on n'a pas créé de matrice PETSc, la fonction renvoie 0.
!
function get_mat_id( matas ) result ( kptsc ) 
  !
  ! Dummy arguments 
  character(len=19), intent(in) :: matas
  integer :: kptsc
  ! Local variables 
  character(len=14) :: nu
  integer :: jnequ, nglo, k
  PetscInt :: m, n 
  PetscErrorCode :: ierr 
  !
  call jemarq()
!
!   On cherche si la matrice est deja enregistree :
!   -------------------------------------------------
!   On teste le nom de la matrice, celui du nume_ddl,
!   et la taille des matrices aster et petsc
    call dismoi('NOM_NUME_DDL', matas, 'MATR_ASSE', repk=nu)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    nglo = zi(jnequ)
! Valeur par défaut de kptsc 
    kptsc=0
!
    do k = 1, nmxins
        if ((nomats(k).eq.matas) .and. (nonus (k).eq.nu )) then
! si de plus le clone PETSc a ete cree, on verifie que les dimensions
! des matrices aster et petsc sont coherentes
          if ( ap(k) .ne. 0 ) then
             call MatGetSize(ap(k), m, n, ierr)
             ASSERT(ierr.eq.0)
             ASSERT(m.eq.n)
             if (fictifs(k).eq.1) then
               ASSERT(n.ge.nglo)
               ASSERT(size(new_ieqs(k)%pi4).eq.nglo)
             else
               ASSERT(nglo.eq.n)
             endif
          endif 
! la verification a ete effectuee avec succes, on renvoie k   
          kptsc = k
        endif
    enddo
   call jedema()
  !
end function get_mat_id 
!
!
! Retourne un identifiant libre pour stocker une nouvelle
! matrice. Si on ne trouve pas d'identifiant, on renvoie 0 
function get_new_mat_id() result (kptsc)
  !
  ! Dummy arguments 
  integer :: kptsc 
  ! Local variables 
  integer :: k
!
!   Y-a-t-il encore une place libre ? Calcul de kptsc :
!   ---------------------------------------------------
    kptsc = 0
    do k = 1, nmxins
        if (nomats(k) .eq. ' ') then
            kptsc = k
            exit
        endif
    end do
end function get_new_mat_id 
!
! La routine mat_record enregistre la matrice matas 
! i.e. determine son identifiant kptsc 
! et note son nom dans les tableaux  
subroutine mat_record ( matas, solveu, kptsc )
  ! Dummy arguments
  character(len=19), intent(in) :: matas, solveu
  integer, intent(out)          :: kptsc
  ! Local variables
  character(len=19) :: nu
  !
  call dismoi('NOM_NUME_DDL', matas, 'MATR_ASSE', repk=nu)
  !
  ! Verification : est-ce que la matrice est deja enregistree ? 
  kptsc = get_mat_id( matas ) 
  if ( kptsc ==  0 ) then   
    kptsc = get_new_mat_id()
    if ( kptsc == 0 ) then 
      call utmess('F', 'PETSC_3')
    endif 
  !
    ASSERT(nomats(kptsc).eq.' ')
    ASSERT(nosols(kptsc).eq.' ')
    ASSERT(nonus(kptsc).eq.' ')
  !
    nomats(kptsc) = matas
    nonus(kptsc) = nu
    nosols(kptsc) = solveu
!  Initialisation par défaut 
!  la veritable initialisation sera faite en appelant apbloc
    tblocs(kptsc) = -1
    fictifs(kptsc) = -1
    new_ieqs(kptsc)%pi4 => null()
    old_ieqs(kptsc)%pi4 => null()
  else
    !  La matrice est deja enregistree, on ne fait rien
  endif 
!
end subroutine mat_record
#endif
end module petsc_data_module 
