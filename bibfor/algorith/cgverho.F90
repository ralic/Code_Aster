function cgverho(imate)
    implicit none
#include "jeveux.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
!
    integer :: imate
    logical :: cgverho
!
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
!
!    operateur CALC_G : verification au niveau des te
!    ----------------
!
!    but : verifier que si les champs de : -       pesanteur
!                                          - et/ou rotation 
!                                          - et/ou pulsation
!          sont des donnees d'entrees du calcul elementaire, on dispose
!          bien d'une valeur de rho sur cet élément
!
!    in  : imate (adresse du materiau)
!    out : cgverho = .true. si tout est OK, .false. sinon
!
! ----------------------------------------------------------------------
!
    integer :: icodre, iret, codrho(1), ipesa, irota, ipuls
    logical :: rhoabs
    real(kind=8) :: rhobid(1)
    character(len=16) :: phenom
!
! ----------------------------------------------------------------------
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
    call rcvalb('RIGI', 1, 1, '+', zi(imate),' ', phenom, 0, ' ',&
                [0.d0], 1, 'RHO', rhobid(1), codrho(1), 0)
!
!   rhoabs -> .true. si rho est absent
    rhoabs = codrho(1).ne.0
!
    call tecach('ONN', 'PPESANR', 'L', iret, iad=ipesa)
    call tecach('ONN', 'PROTATR', 'L', iret, iad=irota)
    call tecach('ONN', 'PPULPRO', 'L', iret, iad=ipuls)
!
!   si le champ est present, et rho absent -> NOOK
    cgverho = .true.
    if ( (ipesa.ne.0) .and. rhoabs ) cgverho = .false.
    if ( (ipesa.ne.0) .and. rhoabs ) cgverho = .false.
    if ( (ipesa.ne.0) .and. rhoabs ) cgverho = .false.
!
end function
