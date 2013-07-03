subroutine nmdocn(parcri, parcon)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8nnem.h"
#include "asterc/r8vide.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: parcri(*), parcon(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (LECTURE)
!
! LECTURE DES CRITERES DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! OUT PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE
!                1 : ITER_GLOB_MAXI
!                2 : RESI_GLOB_RELA
!                3 : RESI_GLOB_MAXI
!                4 : ARRET (0=OUI, 1=NON)
!                5 : ITER_GLOB_ELAS
!                6 : RESI_REFE_RELA
!                7 : RESI_COMP_RELA
! OUT PARCON : PARAMETRES DU CRITERE DE CONVERGENCE EN CONTRAINTE
!                   SI PARCRI(6)=RESI_REFE_RELA != R8VIDE()
!                1 : SIGM_REFE
!                2 : EPSI_REFE
!                3 : FLUX_THER_REFE
!                4 : FLUX_HYD1_REFE
!                5 : FLUX_HYD2_REFE
!                6 : VARI_REFE
!                7 : EFFORT
!                8 : MOMENT
!                9 : DEPL_REFE
!               10 : LAGR_REFE
!
! ----------------------------------------------------------------------
!
    integer :: iterat, iret, ire1, ire2, ire3, ire4
    character(len=8) :: rep
    integer :: ifm, niv
    logical :: lretcv
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE CRITERES CONVERGENCE'
    endif
!
! --- INITIALISATIONS
!
    parcri(2) = r8vide()
    parcri(3) = r8vide()
    parcri(6) = r8vide()
    parcri(7) = r8vide()
!
! --- RECUPERATION DES CRITERES DE CONVERGENCE GLOBAUX
!
    call getvis('CONVERGENCE', 'ITER_GLOB_MAXI', 1, iarg, 1,&
                iterat, iret)
    parcri(1) = iterat
    call getvis('CONVERGENCE', 'ITER_GLOB_ELAS', 1, iarg, 1,&
                iterat, iret)
    parcri(5) = iterat
    call getvr8('CONVERGENCE', 'RESI_GLOB_RELA', 1, iarg, 1,&
                parcri(2), ire1)
    if (ire1 .le. 0) parcri(2) = r8vide()
    call getvr8('CONVERGENCE', 'RESI_GLOB_MAXI', 1, iarg, 1,&
                parcri(3), ire2)
    if (ire2 .le. 0) parcri(3) = r8vide()
    call getvr8('CONVERGENCE', 'RESI_REFE_RELA', 1, iarg, 1,&
                parcri(6), ire3)
    if (ire3 .le. 0) then
        parcri(6) = r8vide()
    else
        call getvr8('CONVERGENCE', 'SIGM_REFE', 1, iarg, 1,&
                    parcon(1), iret)
        if (iret .le. 0) parcon(1)=r8nnem()
        call getvr8('CONVERGENCE', 'EPSI_REFE', 1, iarg, 1,&
                    parcon(2), iret)
        if (iret .le. 0) parcon(2)=r8nnem()
        call getvr8('CONVERGENCE', 'FLUX_THER_REFE', 1, iarg, 1,&
                    parcon(3), iret)
        if (iret .le. 0) parcon(3)=r8nnem()
        call getvr8('CONVERGENCE', 'FLUX_HYD1_REFE', 1, iarg, 1,&
                    parcon(4), iret)
        if (iret .le. 0) parcon(4)=r8nnem()
        call getvr8('CONVERGENCE', 'FLUX_HYD2_REFE', 1, iarg, 1,&
                    parcon(5), iret)
        if (iret .le. 0) parcon(5)=r8nnem()
        call getvr8('CONVERGENCE', 'VARI_REFE', 1, iarg, 1,&
                    parcon(6), iret)
        if (iret .le. 0) parcon(6)=r8nnem()
        call getvr8('CONVERGENCE', 'FORC_REFE', 1, iarg, 2,&
                    parcon(7), iret)
        if (iret .le. 0) then
            parcon(7) = r8nnem()
            parcon(8) = r8nnem()
        endif
        call getvr8('CONVERGENCE', 'DEPL_REFE', 1, iarg, 1,&
                    parcon(9), iret)
        if (iret .le. 0) parcon(9)=r8nnem()
        call getvr8('CONVERGENCE', 'LAGR_REFE', 1, iarg, 1,&
                    parcon(10), iret)
        if (iret .le. 0) parcon(10)=r8nnem()
    endif
    call getvr8('CONVERGENCE', 'RESI_COMP_RELA', 1, iarg, 1,&
                parcri(7), ire4)
    if (ire4 .le. 0) parcri(7) = r8vide()
!
! --- VALEURS PAR DEFAUT DES RESI_*
!
    lretcv=(ire1.le.0 .and. ire2.le.0 .and. ire3.le.0 .and. ire4.le.0)
    if (lretcv) then
        parcri(2) = 1.d-6
    endif
!
    call getvtx('CONVERGENCE', 'ARRET', 1, iarg, 1,&
                rep, iret)
    parcri(4) = 0
    if (iret .gt. 0) then
        if (rep .eq. 'NON') parcri(4) = 1
    endif
!
! --- ALARMES RELATIVES A LA QUALITE DE LA CONVERGENCE
!
    if (parcri(2) .ne. r8vide() .and. parcri(2) .gt. 1.0001d-4) then
        call u2mess('A', 'MECANONLINE5_21')
    endif
!
    call jedema()
end subroutine
