!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface 
    subroutine materc(matmas, matrig, matamo, numnu, amor, nommes,&
                      lfreqs, nbfreq,matobs, obsdim, gamma, alpha,eval)
#include "asterf_types.h"       
        character(len=8),intent(out) :: matmas
        character(len=8),intent(out) :: matrig
        character(len=8),intent(out) :: matamo
        character(len=8),intent(out) :: numnu
        aster_logical,intent(out) :: amor
        character(len=8),intent(out) :: nommes
        character(len=24),intent(out) :: lfreqs
        integer,intent(out) :: nbfreq
        character(len=24),intent(out) :: matobs(3)
        integer,intent(out) :: obsdim(3)
        real(kind=8),intent(out) :: gamma, alpha
        aster_logical,intent(out) :: eval
    end subroutine materc
end interface 
