subroutine hujksi(carac, mater, r, ksi, iret)
    implicit none
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
!     --------------------------------------------------------
!     LOI HUJEUX : CALCUL DU COEFFICIENT DE MOBILISATION
!     OU DE SA DERIVEE
! ============================================================
!   IN
!    CARAC (K6) : 'KSI'  CALCUL DE KSI(R) =
!                        [ (R - RHYS) / (RMOB - RHYS) ] **XM
!             'DKSIDR'   CALCUL DE LA DERIVEE DE KSI
!    MATER  :  COEFFICIENTS MATERIAU
!    R      :  ECROUISSAGE COURANT (MECANISME DEVIATOIRE)
!   OUT
!    KSI (R)    :  VALEUR DE KSI OU DKSIDR
!     --------------------------------------------------------
!
#include "asterfort/infniv.h"
#include "asterfort/u2mess.h"
    integer :: ifm, niv, iret
    real(kind=8) :: mater(22, 2), r, ksi, rhys, rmob, xm, xm1
    real(kind=8) :: zero, un
    character(len=6) :: carac
    character(len=16) :: nomail
    logical :: debug
    parameter   (zero = 0.d0)
    parameter   (un = 1.d0)
!
    common /meshuj/ debug
!
    call infniv(ifm, niv)
!
    rhys = mater(15,2)
    rmob = mater(16,2)
    xm = mater(17,2)
!
    if (carac(1:3) .eq. 'KSI') then
!
        if (r .gt. zero .and. r .le. rhys) then
            ksi = zero
        else if (r.gt.rhys .and. r.le.rmob) then
            ksi = (r - rhys)**xm /(rmob - rhys)**xm
        else if (r.gt.rmob) then
            ksi = un
        else
!          IRET = 1
            ksi=zero
            if (debug) then
!            CALL TECAEL(IADZI,IAZK24)
!            NOMAIL = ZK24(IAZK24-1+3) (1:8)
                nomail='#A FAIRE#'
                write(ifm,'(A)')&
     &      'HUJKSI :: ECROUISSAGE NEGATIF DANS LA MAILLE ',nomail
            endif
        endif
!
    else if (carac(1:6).eq.'DKSIDR') then
!
        if (r .gt. zero .and. r .le. rhys) then
            ksi = zero
        else if (r.gt.rhys .and. r.le.rmob) then
            xm1 = xm-un
            ksi = xm*(r - rhys)**xm1 /(rmob - rhys)**xm
        else if (r.gt.rmob) then
            ksi = zero
        else
!          IRET = 1
            ksi=zero
            if (debug) then
!            CALL TECAEL(IADZI,IAZK24)
!            NOMAIL = ZK24(IAZK24-1+3) (1:8)
                nomail='#A FAIRE#'
                write(ifm,'(A)')&
     &      'HUJKSI :: ECROUISSAGE NEGATIF DANS LA MAILLE ',nomail
            endif
        endif
!
    else
        call u2mess('F', 'COMPOR1_10')
    endif
!
end subroutine
