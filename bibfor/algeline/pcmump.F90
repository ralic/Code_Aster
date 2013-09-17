subroutine pcmump(matasz, solvez, iretz)
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
!     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT DU GCPC
!     PAR FACTORISATION SIMPLE PRECISION PAR MUMPS
!
!-----------------------------------------------------------------------
! IN  K*  MATASZ    : NOM DE LA MATR_ASSE A PRECONDITIONNER
! IN  K*  SOLVEZ    : NOM DE LA SD SOLVEUR
! IN  I   IRETZ     : CODE RETOUR (!=0 SI ERREUR)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer :: jslvk, jslvi, iterpr, reacpr, pcpiv, jrefa, iret
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=19) :: solveu, matass
    character(len=24) :: precon, solvbd
!----------------------------------------------------------------------
    call jemarq()
!
    matass = matasz
    solveu = solvez
!
! --  PARAMETRES DU PRECONDITIONNEUR
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    call jeveuo(solveu//'.SLVI', 'L', jslvi)
    precon=zk24(jslvk-1+2)
    iterpr=zi(jslvi-1+5)
    reacpr=zi(jslvi-1+6)
    pcpiv =zi(jslvi-1+7)
!
    ASSERT(precon.eq.'LDLT_SP')
!
! --  PRISE EN COMPTE DES CHARGEMENTS CINEMATIQUES
! --  SAUF DANS LE CAS OU LE SOLVEUR EST PETSC
! --  CAR DEJA FAIT DANS APETSC
    if (zk24(jslvk) .ne. 'PETSC') then
        call jeveuo(matass//'.REFA', 'L', jrefa)
        ASSERT(zk24(jrefa-1+3).ne.'ELIMF')
        if (zk24(jrefa-1+3) .eq. 'ELIML') call mtmchc(matass, 'ELIMF')
        ASSERT(zk24(jrefa-1+3).ne.'ELIML')
    endif
!
! --  CREATION DE LA SD SOLVEUR MUMPS SIMPLE PRECISION
! --  (A DETRUIRE A LA SORTIE)
    solvbd=zk24(jslvk-1+3)
    call crsmsp(solvbd, matass, pcpiv)
!
! --  APPEL AU PRECONDITIONNEUR
    iret = 0
    if (iterpr .gt. reacpr .or. iterpr .eq. 0) then
        call amumph('DETR_MAT', solvbd, matass, [rbid], [cbid],&
                    ' ', 0, iret, .true.)
        call amumph('PRERES', solvbd, matass, [rbid], [cbid],&
                    ' ', 0, iret, .true.)
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
