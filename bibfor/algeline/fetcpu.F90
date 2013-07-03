subroutine fetcpu(option, temps, infofe, rang, ifm)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  PROFILING ALGO FETI POUR SOULAGER ALFETI.F
!     ------------------------------------------------------------------
!     IN  OPTION : IN   : OPTION DE LA ROUTINE
!     IN  TEMPS  : R8   : VECTEUR DE TIMING POUR UTTCPU
!     IN  INFOFE : CH19 : CHAINE DE CHARACTERES POUR MONITORING FETI
!     IN  RANG   : IN   : RANG DU PROCESSEUR
!     IN  IFM    : IN   : UNITE LOGIQUE D'IMPRESSION STANDARD
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterfort/assert.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
    integer :: option, rang, ifm
    real(kind=8) :: temps(6)
    character(len=24) :: infofe
!
    if (infofe(9:9) .eq. 'T') then
        if (option .eq. 1) then
            call uttcpu('CPU.FETCPU.37', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.37', 'DEBUT', ' ')
        else if (option.eq.2) then
            call uttcpu('CPU.FETCPU.37', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.37', 6, temps)
            write(ifm,*)'PROC ',rang,' INIT 1 CPU/SYS: ',temps(5),&
            temps(6)
            call uttcpu('CPU.FETCPU.38', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.38', 'DEBUT', ' ')
        else if (option.eq.3) then
            call uttcpu('CPU.FETCPU.38', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.38', 6, temps)
            write(ifm,*)'PROC ',rang,' INIT 2 CPU/SYS: ',temps(5),&
            temps(6)
            call uttcpu('CPU.FETCPU.39', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.39', 'DEBUT', ' ')
        else if (option.eq.31) then
            call uttcpu('CPU.FETCPU.39', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.39', 'DEBUT', ' ')
        else if (option.eq.4) then
            call uttcpu('CPU.FETCPU.39', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.39', 6, temps)
            write(ifm,*)'PROC ',rang,' INIT 3 CPU/SYS: ',temps(5),&
            temps(6)
        else if (option.eq.5) then
            call uttcpu('CPU.FETCPU.40', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.40', 'DEBUT', ' ')
        else if (option.eq.6) then
            call uttcpu('CPU.FETCPU.40', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.40', 6, temps)
            write(ifm,*)'PROC ',rang,' FETFIV CPU/SYS: ',temps(5),&
            temps(6)
            call uttcpu('CPU.FETCPU.41', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.41', 'DEBUT', ' ')
        else if (option.eq.7) then
            call uttcpu('CPU.FETCPU.41', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.41', 6, temps)
            write(ifm,*)'PROC ',rang,' DDOT/DAXPY/FETPRJ CPU/SYS: ',&
            temps(5),temps(6)
            call uttcpu('CPU.FETCPU.43', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.43', 'DEBUT', ' ')
        else if (option.eq.9) then
            call uttcpu('CPU.FETCPU.43', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.43', 6, temps)
            write(ifm,*)'PROC ',rang,' TEST CV CPU/SYS: ',temps(5),&
            temps(6)
        else if (option.eq.10) then
            call uttcpu('CPU.FETCPU.44', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.44', 'DEBUT', ' ')
        else if (option.eq.11) then
            call uttcpu('CPU.FETCPU.44', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.44', 6, temps)
            write(ifm,*)'PROC ',rang,' FETPRC+SCA+PRJ CPU/SYS: ',&
            temps(5), temps(6)
            call uttcpu('CPU.FETCPU.45', 'INIT', ' ')
            call uttcpu('CPU.FETCPU.45', 'DEBUT', ' ')
        else if (option.eq.12) then
            call uttcpu('CPU.FETCPU.45', 'FIN', ' ')
            call uttcpr('CPU.FETCPU.45', 6, temps)
            write(ifm,*)'PROC ',rang,' FETREO CPU/SYS: ',temps(5),&
            temps(6)
        else
            call assert(.false.)
        endif
    endif
end subroutine
