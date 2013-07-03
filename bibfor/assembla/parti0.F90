subroutine parti0(nbvec, tlivec, partit)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/u2mesk.h"
    character(len=*) :: tlivec(*), partit
    integer :: nbvec
! ----------------------------------------------------------------------
! BUT : DETERMINER LE NOM DE LA PARTITION ATTACHEE A LA LISTE DE
!       VECT_ELEM (OU DE MATR_ELEM)  TLIVEC
! OUT K8 PARTIT  : NOM DE LA PARTITION
!        - SI PARTIT=' ' : IL N'Y A PAS DE PARTITION. LES RESUELEM ONT
!                          ETE COMPLETEMENT CALCULES SUR TOUS LES PROCS.
! ----------------------------------------------------------------------
    character(len=8) :: part1
    character(len=19) :: vecel
    character(len=24) :: valk(5)
    integer :: i, ibid
!
!
    partit=' '
    do 10 i = 1, nbvec
        vecel = tlivec(i)
        call dismoi('F', 'PARTITION', vecel, 'VECT_ELEM', ibid,&
                    part1, ibid)
        if (partit .eq. ' ' .and. part1 .ne. ' ') partit=part1
        if (partit .ne. ' ' .and. part1 .eq. ' ') goto 10
        if (partit .ne. ' ' .and. partit .ne. part1) then
            valk(1)=partit
            valk(2)=part1
            call u2mesk('F', 'CALCULEL_10', 2, valk)
        endif
10  end do
end subroutine
