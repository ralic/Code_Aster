subroutine ledome(option, nomo, materi, mate, carele)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/u2mess.h"
    character(len=8) :: nomo
    character(len=24) :: mate, carele
    character(len=8) :: materi
    character(len=2) :: option
!
! ----------------------------------------------------------------------
!
! LECTURE DONNEES MECANIQUES
!
! ----------------------------------------------------------------------
!
! IN  OPTION : PRECISE SI MATERIAU/CARA_ELEM OBLIGATOIRES
!              ALARME L'UTILISATUER  EN CAS D'ABSENCE
! OUT NOMO   : MODELE
! OUT MATERI : CHAMP DE MATERIAU (NON CODE)
! OUT MATE   : MATERIAU CODE
! OUT CARELE : CARACTERISTIQUES ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
    integer :: iarg, n, ibid, iret
    character(len=8) :: repons
!
! ----------------------------------------------------------------------
!
    nomo = ' '
    mate = ' '
    carele = ' '
!
! --- RECUPERER LE MODELE
!
    call getvid(' ', 'MODELE', scal=nomo, nbret=n)
!
! --- RECUPERER LE MATERIAU
!
    call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n)
    if (nomo .ne. ' ') then
        call dismoi('F', 'BESOIN_MATER', nomo, 'MODELE', ibid,&
                    repons, iret)
        if ((n.eq.0) .and. (repons(1:3).eq.'OUI') .and. (option(1:1) .eq.'O')) then
            call u2mess('A', 'CALCULEL3_40')
        endif
    endif
!
! --- CREATION DE LA CARTE DU MATERIAU CODE
!
    if (n .ne. 0) then
        call rcmfmc(materi, mate)
    else
        mate = ' '
    endif
!
! --- RECUPERER LES CARACTERISTIQUES ELEMENTAIRES
!
    call getvid(' ', 'CARA_ELEM', scal=carele, nbret=n)
    if (nomo .ne. ' ') then
        call dismoi('F', 'EXI_RDM', nomo, 'MODELE', ibid,&
                    repons, iret)
        if ((n.eq.0) .and. (repons(1:3).eq.'OUI') .and. (option(1:1) .eq.'O')) then
            call u2mess('A', 'CALCULEL3_39')
        endif
    endif
!
end subroutine
