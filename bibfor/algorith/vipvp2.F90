subroutine vipvp2(nbvari, vintm, vintp, advico, vicpvp,&
                  pvp0, pvp1, p2, dp2, t,&
                  dt, kh, mamolv, r, rho11m,&
                  yate, pvp, pvpm, retcom)
    implicit      none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/iunifi.h"
#include "asterfort/tecael.h"
    integer :: nbvari, advico, vicpvp, yate, retcom
    real(kind=8) :: vintm(nbvari), vintp(nbvari), pvp0, pvp1, p2, dp2, t, dt
    real(kind=8) :: mamolv, r, rho11m, pvp, pvpm, kh
! ======================================================================
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
! ======================================================================
! --- CALCUL ET STOCKAGE DE LA PRESSION DE VAPEUR DANS LE CAS ----------
! --- AVEC AIR DISSOUS -------------------------------------------------
! ======================================================================
    integer :: iadzi, iazk24, umess
    real(kind=8) :: varbio
    character(len=8) :: nomail
! ======================================================================
! ======================================================================
    varbio = (rho11m*kh/pvp1)-mamolv*(1+r*log(t/(t-dt)))
! ======================================================================
! --- VERIFICATION DES COHERENCES --------------------------------------
! ======================================================================
    if (abs(varbio) .lt. r8prem()) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'VIPVP2','DIVISION PAR ZERO A LA MAILLE',&
        nomail
        retcom = 1
        goto 30
    endif
    pvpm = vintm(advico+vicpvp) + pvp0
    pvp = (rho11m*kh-mamolv*(pvpm+(p2-dp2)*r*log(t/(t-dt))))/varbio
    if ((p2-pvp) .lt. 0.d0) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'VIPVP2','PGAZ-PVAP <=0 A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 30
    endif
    vintp(advico+vicpvp) = pvp - pvp0
! ======================================================================
30  continue
! =====================================================================
    9001 format (a8,2x,a30,2x,a8)
! ======================================================================
end subroutine
