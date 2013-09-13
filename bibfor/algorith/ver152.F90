subroutine ver152(option, moflui, moint, n12, model)
    implicit none
!---------------------------------------------------------------------
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
!
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
! VERIFICATIONS SUPPLEMENTAIRES DANS L OP0152 DE CALCUL DE MASSE
! AJOUTEE, AMORTISSEMENT ET RAIDEUR AJOUTES EN THEORIE POTENTIELLE
! IN : K* : OPTION : OPTION DE CALCUL
! IN : K* : MOFLUI , MOINT : MODELES FLUIDE ET INTERFACE
! IN : K* : MODEL : DIMENSION DU MODELE (3D, 2D OU AXI)
! IN : I  : N12 : PRESENCE DU POTENTIEL PERMANENT
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
    integer :: n12, ibid, ierd
    character(len=*) :: option, model, moflui, moint
    character(len=9) :: optio9
    character(len=16) :: rep, rk16
! -----------------------------------------------------------------
    optio9 = option
!
!        MOFLU8 =
!
    if (n12 .eq. 0 .and. (optio9.eq.'AMOR_AJOU'.or. optio9.eq.('RIGI_AJOU'))) then
        call utmess('F', 'ALGORITH11_24')
    endif
!
    call dismoi('F', 'PHENOMENE', moflui, 'MODELE', ibid,&
                rk16, ierd)
!
    if (rk16(1:9) .ne. 'THERMIQUE') then
        call utmess('F', 'ALGORITH11_25')
    endif
!
    call dismoi('F', 'PHENOMENE', moint, 'MODELE', ibid,&
                rk16, ierd)
!
    if (rk16(1:9) .ne. 'THERMIQUE') then
        call utmess('F', 'ALGORITH11_26')
    endif
!
    call dismoi('F', 'MODELISATION', moflui, 'MODELE', ibid,&
                rep, ierd)
    if (rep .eq. 'PLAN') then
        model='2D'
    else
        if (rep .eq. 'AXIS') then
            model='AX'
        else
            if (rep .eq. '3D') then
                model='3D'
            else
                call utmess('F', 'ALGORITH11_27')
            endif
        endif
    endif
end subroutine
