subroutine sdmpic(typesd, nomsd)
    implicit none
#include "jeveux.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
    character(len=*) :: nomsd, typesd
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!  BUT : "COMPLETER" LE CALCUL D'UNE STRUCTURE DE DONNEES INCOMPLETEMENT
!        CALCULEE  DU FAIT DE L'UTILISATION DE CALCULS PARALLELES (MPI)
!
!  LA ROUTINE ECHANGE LES MORCEAUX CALCULES PARTIELLEMENT SUR LES
!  DIFFERENTS PROCESSEURS (MPI_ALLREDUCE)
!
! ----------------------------------------------------------------------
! IN TYPESD (K*) :  TYPE DE LA SD A COMPLETER
! IN NOMSD  (K*) :  NOM DE LA SD A COMPLETER
! ----------------------------------------------------------------------
    character(len=24) :: noms2, types2
    character(len=19) :: k19
    character(len=8) :: kmpic
    integer :: ifm, niv,  iexi
    character(len=24), pointer :: noli(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: celk(:) => null()
    character(len=16), pointer :: valk(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    noms2 = nomsd
    types2 = typesd
!
!
    k19=noms2(1:19)
    if (types2 .eq. 'CHAM_ELEM') then
!     ----------------------------------
        call dismoi('MPI_COMPLET', k19, 'CHAM_ELEM', repk=kmpic)
        if (kmpic .eq. 'OUI') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.CELV')
        call jeveuo(k19//'.CELK', 'E', vk24=celk)
        celk(7)='MPI_COMPLET'
!
!
    else if (types2.eq.'RESUELEM') then
!     ----------------------------------
        call dismoi('MPI_COMPLET', k19, 'RESUELEM', repk=kmpic)
        if (kmpic .eq. 'OUI') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.RESL')
        call jeveuo(k19//'.NOLI', 'E', vk24=noli)
        noli(3)='MPI_COMPLET'
!
!
    else if (types2.eq.'MATR_ASSE') then
!     ----------------------------------
        call dismoi('MPI_COMPLET', k19, 'MATR_ASSE', repk=kmpic)
        if (kmpic .eq. 'OUI') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.VALM')
!
        call jeexin(k19//'.CCVA', iexi)
        if (iexi .gt. 0) call asmpi_comm_jev('MPI_SUM', k19//'.CCVA')
!
        call jeveuo(k19//'.REFA', 'E', vk24=refa)
        refa(11)='MPI_COMPLET'
!
    else if (types2 .eq. 'SD_APPA') then
!     ----------------------------------
        call jeveuo(k19//'.MPIA', 'E', vk16=valk)
        if (valk(1) .eq. 'MPI_COMPLET') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.APPA')
        call asmpi_comm_jev('MPI_SUM', k19//'.DIST')
        call asmpi_comm_jev('MPI_SUM', k19//'.TAU1')
        call asmpi_comm_jev('MPI_SUM', k19//'.TAU2')
        call asmpi_comm_jev('MPI_SUM', k19//'.PROJ')     
        valk(1)='MPI_COMPLET'
!
    else if (types2 .eq. 'SD_APPA_TGEL') then
!     ----------------------------------
        call jeveuo(k19//'.MPIB', 'E', vk16=valk)
        if (valk(1) .eq. 'MPI_COMPLET') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.TGEL')  
        valk(1)='MPI_COMPLET'
!
    else if (types2 .eq. 'SD_APPA_TGNO') then
!     ----------------------------------
        call jeveuo(k19//'.MPIC', 'E', vk16=valk)
        if (valk(1) .eq. 'MPI_COMPLET') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.TGNO')  
        valk(1)='MPI_COMPLET'
!
    else if (types2 .eq. 'SD_APPA_LAC1') then
!     ----------------------------------
        !call jeveuo(k19//'.MPIA', 'E', vk16=valk)
        !if (valk(1) .eq. 'MPI_COMPLET') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.GAPI')
        call asmpi_comm_jev('MPI_SUM', k19//'.PWT ')
        call asmpi_comm_jev('MPI_SUM', k19//'.PWC ')
        call asmpi_comm_jev('MPI_SUM', k19//'.NAPP')   
        !valk(1)='MPI_COMPLET'       
!
!
!
    else if (types2 .eq. 'SD_APPA_LAC2') then
!     ----------------------------------
        !call jeveuo(k19//'.MPIA', 'E', vk16=valk)
        !if (valk(1) .eq. 'MPI_COMPLET') goto 999
        call asmpi_comm_jev('MPI_SUM', k19//'.AUX ')  
        !valk(1)='MPI_COMPLET'       
!
!
    else
        ASSERT(.false.)
    endif
!
999 continue
    call jedema()
end subroutine
