subroutine rcvarp(arret, novrc, poum, valvrc, iret)

use calcul_module, only : ca_iredec_, ca_jvcnom_, ca_jvcval_, ca_nbcvrc_,&
                          ca_td1_, ca_tf1_, ca_timed1_, ca_timef1_

implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"

    character(len=*) :: novrc, poum
    character(len=1) :: arret
    integer :: iret
    real(kind=8) :: valvrc
!-----------------------------------------------------------------------
! but: recuperer la valeur d'une variable de commande  CALC_POINT_MAT
!      pour une valeur d'instant ('+','-','REF')
!
! arguments :
!  in   arret (k1)  : ce qu'il faut faire en cas de probleme
!              = ' ' : on remplit codret et on sort sans message.
!              = 'F' : si la variable n'est pas trouvee, on arrete
!                       en fatal.
!  in   novrc  (k8) : nom de la variable de commande souhaitee
!  in   poum   (k*) : /'+', /'-', /'REF'
!  out  valvrc (r)  : valeur de la variable de commande
!  out  iret   (i)  : code retour : 0 -> ok
!                                   1 -> variable non trouvee
! ---------------------------------------------------------------
    character(len=8) :: novr8
    integer :: kcvrc, iprem
    character(len=24) :: valk(4)
    real(kind=8) :: valvrm, valvrp, tdef, rundf
    save rundf
    data iprem /0/
! ---------------------------------------------------------------
    if (iprem .eq. 0) then
        rundf=r8nnem()
        iprem=1
    endif

    tdef=rundf
    iret=0


!   1. calcul de kcvcrc :
!   ----------------------
    novr8=novrc
    kcvrc=indik8(zk8(ca_jvcnom_),novr8,1,ca_nbcvrc_)

!   -- si la cvrc n'est pas fournie, on rend "r8nnem"
    if (kcvrc .eq. 0) then
        iret=1
        if (arret .eq. ' ') then
            valvrc=rundf
            goto 9999
        else
            valk(1) = novr8
            valk(2) = zk8(ca_jvcnom_-1+kcvrc)
            valk(3) = poum
            call utmess('F', 'CALCUL_26', nk=3, valk=valk)
        endif
    endif


!   2. calcul de valvrc :
!   ----------------------
    if (poum .eq. 'REF') then
        valvrc=zr(ca_jvcval_-1+ 3*(kcvrc-1)+3)

    else if (poum.eq.'+' .and. ca_iredec_.eq.0) then
        valvrc=zr(ca_jvcval_-1+ 3*(kcvrc-1)+2)

    else if (poum.eq.'-' .and. ca_iredec_.eq.0) then
        valvrc=zr(ca_jvcval_-1+ 3*(kcvrc-1)+1)

    else if (ca_iredec_.eq.1) then
        valvrm=zr(ca_jvcval_-1+ 3*(kcvrc-1)+1)
        valvrp=zr(ca_jvcval_-1+ 3*(kcvrc-1)+2)

        if ((.not.isnan(valvrm)) .and. (.not.isnan(valvrp))) then
            if (poum .eq. '-') then
                valvrc=valvrm+(ca_td1_-ca_timed1_)*(valvrp-valvrm)/(ca_timef1_-&
                ca_timed1_)
            else if (poum.eq.'+') then
                valvrc=valvrm+(ca_tf1_-ca_timed1_)*(valvrp-valvrm)/(ca_timef1_-&
                ca_timed1_)
            else
                ASSERT(.false.)
            endif
        else
            valvrc=rundf
        endif

    else
        ASSERT(.false.)
    endif

    iret=0
    if (isnan(valvrc)) iret=1


!   -- traitement si iret=1
!   ------------------------
    if (iret .eq. 1) then
        if (novr8 .eq. 'TEMP') then
            valvrc=tdef
            iret=1
            goto 9999
        endif
        if (arret .eq. ' ') then
            valvrc=rundf
        else
            valk(1) = novr8
            valk(2) = zk8(ca_jvcnom_-1+kcvrc)
            call utmess('F', 'CALCUL_26', nk=2, valk=valk)
        endif
    endif
    goto 9999


9999  continue


end subroutine
