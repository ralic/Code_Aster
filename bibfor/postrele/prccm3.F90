subroutine prccm3(nommat, para, sm, sn, sp,&
                  ke, salt, nadm)
    implicit   none
#include "asterc/r8maem.h"
#include "asterfort/limend.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mesg.h"
    real(kind=8) :: para(*), sm, sn, sp, ke, salt, nadm
    character(len=*) :: nommat
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR POST_RCCM: CALCUL DU KE, SALT, NADM
!
!     ------------------------------------------------------------------
    real(kind=8) :: un, xm, xn, sns3, troism
    character(len=8) :: kbid
    real(kind=8) :: valr(2)
    integer :: icodre
    logical :: endur
!     ------------------------------------------------------------------
!
    un = 1.0d0
    troism = 3.0d0 * sm
!
! --- CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE :
!     ----------------------------------------------------------
!
! --- SI SN < 3*SM  KE = 1 :
!     --------------------
    if (sn .lt. troism) then
        ke = un
!
! --  SI 3*SM < SN < 3*M*SM
! --- KE = 1 +((1-N)/(N*(M-1)))*((SN/(3*SM))-1) :
!             ------------------------------------- ---
    else if (sn .lt. 3.d0*para(1)*sm) then
        xm = para(1)
        xn = para(2)
        sns3 = sn / 3.d0
        ke = un+((un-xn)/(xn*(xm-un)))*((sns3/sm)-un)
!
! --- SI 3*M*SM < SN   KE = 1/N :
!     -------------------------
    else
        ke = un / para(2)
    endif
!
!
! --- CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
! --- PAR DEFINITION SALT = 0.5*EC/E*KE*SP(TEMP1,TEMP2) :
!     -------------------------------------------------
    salt = 0.5d0 * para(3) * ke * sp
!
!
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM EN UTILISANT
! --- LA COURBE DE WOHLER AUX EXTREMITES DU CHEMIN :
!     --------------------------------------------
    call limend(nommat, salt, 'WOHLER', kbid, endur)
    if (endur) then
        nadm=r8maem()
    else
        call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', salt,&
                    1, 'WOHLER  ', nadm, icodre, 2)
        if (nadm .lt. 0) then
            valr (1) = salt
            valr (2) = nadm
            call u2mesg('A', 'POSTRELE_61', 0, ' ', 0,&
                        0, 2, valr)
        endif
    endif
!
end subroutine
