subroutine parti0(nbmat, tlimat, partit)
    implicit none
!
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
! person_in_charge: jacques.pellet at edf.fr
!
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
    character(len=*) :: tlimat(*), partit
    integer :: nbmat
!----------------------------------------------------------------------
! but : determiner le nom de la partition attachee a la liste de
!       matr_elem (ou de vect_elem)  tlimat
!
! out k8 partit  : nom de la partition
!        - si partit=' ' : il n'y a pas de partition. Tous les resuelem de
!          tous les matr_elem ete completement calcules
!          sur tous les procs.
! Remarque : si les resuelem des matr_elem n'ont pas ete calcules avec la
!            meme partition : erreur <F>
!----------------------------------------------------------------------
    character(len=8) :: part1
    character(len=19) :: matel
    character(len=24) :: valk(5)
    integer :: i
!----------------------------------------------------------------------

    partit=' '
    do i = 1, nbmat
        matel = tlimat(i)
        call dismoi('PARTITION', matel, 'MATR_ELEM', repk=part1)
        if (partit .eq. ' ' .and. part1 .ne. ' ') partit=part1
        if (partit .ne. ' ' .and. part1 .eq. ' ') goto 10
        if (partit .ne. ' ' .and. partit .ne. part1) then
            valk(1)=partit
            valk(2)=part1
            call utmess('F', 'CALCULEL_10', nk=2, valk=valk)
        endif
 10     continue
    end do
end subroutine
