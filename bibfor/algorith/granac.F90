subroutine granac(fami, kpg, ksp, icdmat, materi,&
                  compo, irrap, irram, tm, tp,&
                  depsgr)
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
!     RECUPERATION DES CARACTERISTIQUES DE GRANDISSEMENT
    implicit none
#include "asterfort/rcvalb.h"
    integer :: icdmat, kpg, ksp, nbpar, codret(1)
    real(kind=8) :: irram, irrap, tm, tp, valres(1)
    real(kind=8) :: depsgm, depsgp, depsgr, valpar(2)
    character(len=8) :: materi, nomgrd, nompar(2)
    character(len=16) :: compo
    character(len=*) :: fami
    data           nomgrd /'GRAN_FO'/
!
    depsgr = 0.0d0
    if (compo(1:13) .eq. 'GRAN_IRRA_LOG' .or. compo(1:13) .eq. 'LEMAITRE_IRRA' .or.&
        compo(1:10) .eq. 'LMARC_IRRA') then
        nbpar=2
        nompar(1)='TEMP'
        nompar(2)='IRRA'
!
!        INSTANT -
        valpar(1)=tm
        valpar(2)=irram
        if (irram .gt. 0.0d0) then
            call rcvalb(fami, kpg, ksp, '+', icdmat,&
                        materi, compo, nbpar, nompar, valpar,&
                        1, nomgrd, valres, codret, 0)
            depsgm = valres(1)
        else
            depsgm = 0.0d0
        endif
!
!        INSTANT +
        valpar(1)=tp
        valpar(2)=irrap
        if (irrap .gt. 0.0d0) then
            call rcvalb(fami, kpg, ksp, '+', icdmat,&
                        materi, compo, nbpar, nompar, valpar,&
                        1, nomgrd, valres, codret, 0)
            depsgp = valres(1)
        else
            depsgp = 0.0d0
        endif
!
        depsgr = (depsgp - depsgm)/100.0d0
        if (depsgr .lt. 0.0d0) then
            depsgr = 0.0d0
        endif
    endif
end subroutine
