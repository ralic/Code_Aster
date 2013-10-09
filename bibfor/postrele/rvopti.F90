subroutine rvopti(mcf, iocc, nch19, nomgd, typegd,&
                  option)
    implicit none
!
#include "asterc/getexm.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
    character(len=*) :: mcf
    character(len=19) :: nch19
    character(len=16) :: option
    character(len=8) :: nomgd
    character(len=4) :: typegd
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     RECUPERATION DE L' OPTION DE CALCUL DU CHAMP_19 DE NOM NCH19
!     ------------------------------------------------------------------
! IN  NCH19  : K : NOM DU CHAMP_19
! IN  NOMGD  : K : NOM DE LA GRANDEUR
! IN  TYPEGD : K : VAUT 'CHNO' OU 'CHLM'
! OUT OPTION : K : NOM OPTION POUR CHLM OU ADAPTATION CHNO
!     ------------------------------------------------------------------
!
    integer :: iocc, nc
    integer :: lnch
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    option = '                '
!
    if (typegd .eq. 'CHML') then
!
        call dismoi('NOM_OPTION', nch19, 'CHAMP', repk=option)
!
    else if (typegd .eq. 'CHNO') then
!
! ------ POUR LES OPTIONS XXXX_NOEU_XXXX, ON RECUPERE L'OPTION
!        PAR LE MOT CLE "NOM_CHAM"
!
        lnch = getexm ( mcf, 'NOM_CHAM' )
        if (lnch .eq. 1) then
            call getvtx(mcf, 'NOM_CHAM', iocc=iocc, scal=option, nbret=nc)
            if (option(6:9) .eq. 'NOEU') goto 9999
        endif
!
        if (nomgd .eq. 'SIEF_R') then
            option = 'SIEF_NOEU_DEPL  '
        else if (nomgd .eq. 'EPSI_R') then
            option = 'EPSI_NOEU  '
        else if (nomgd .eq. 'FLUX_R') then
            option = 'FLUX_NOEU  '
        else if (nomgd .eq. 'DEPL_R') then
            option = 'DEPL_NOEU_DEPL  '
        else if (nomgd .eq. 'FORC_R') then
            option = 'FORC_NOEU_FORC  '
        endif
    endif
9999 continue
end subroutine
