subroutine pcmump(matasz, solvez, iretz)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/amumph.h"
#include "asterfort/assert.h"
#include "asterfort/crsmsp.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtmchc.h"
    character(len=*) :: matasz, solvez
    integer :: iretz
!-----------------------------------------------------------------------
!
!     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT (PETSC, GCPC)
!     PAR FACTORISATION SIMPLE PRECISION PAR MUMPS
!
!-----------------------------------------------------------------------
! IN  K*  MATASZ    : NOM DE LA MATR_ASSE A PRECONDITIONNER
! IN  K*  SOLVEZ    : NOM DE LA SD SOLVEUR
! IN  I   IRETZ     : CODE RETOUR (!=0 SI ERREUR)
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer ::   iterpr, reacpr, pcpiv,  iret
    complex(kind=8) :: cbid
    character(len=19) :: solveu, matass
    character(len=24) :: precon, solvbd
    integer, pointer :: slvi(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: slvk(:) => null()
!----------------------------------------------------------------------
    call jemarq()
!
    matass = matasz
    solveu = solvez
    cbid=dcmplx(0.d0,0.d0)
!
! --  PARAMETRES DU PRECONDITIONNEUR
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    call jeveuo(solveu//'.SLVI', 'L', vi=slvi)
    precon=slvk(2)
    iterpr=slvi(5)
    reacpr=slvi(6)
    pcpiv =slvi(7)
!
    ASSERT(precon.eq.'LDLT_SP')
!
! --  PRISE EN COMPTE DES CHARGEMENTS CINEMATIQUES
! --  SAUF DANS LE CAS OU LE SOLVEUR EST PETSC
! --  CAR DEJA FAIT DANS APETSC
    if (slvk(1) .ne. 'PETSC') then
        call jeveuo(matass//'.REFA', 'L', vk24=refa)
 !       ASSERT(refa(3).ne.'ELIMF')
        if (refa(3) .eq. 'ELIML') call mtmchc(matass, 'ELIMF')
        ASSERT(refa(3).ne.'ELIML')
    endif
!
! --  CREATION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION
! --  (A DETRUIRE A LA SORTIE)
    solvbd=slvk(3)
    call crsmsp(solvbd, matass, pcpiv)
!
! --  APPEL AU PRECONDITIONNEUR
    iret = 0
    if (iterpr .gt. reacpr .or. iterpr .eq. 0) then
        call amumph('DETR_MAT', solvbd, matass, [0.d0], [cbid],&
                    ' ', 0, iret, .true._1)
        call amumph('PRERES', solvbd, matass, [0.d0], [cbid],&
                    ' ', 0, iret, .true._1)
    endif
!
! --  DESTRUCTION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION
    call detrsd('SOLVEUR', solvbd)
!
! --  CODE RETOUR
    iretz = iret
!
    call jedema()
end subroutine
