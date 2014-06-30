subroutine cfelpv(numlia, typlia, resoco, nbliai, lelpiv)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jeveuo.h"
    logical(kind=1) :: lelpiv
    integer :: numlia
    integer :: nbliai
    character(len=2) :: typlia
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCL/ALGOCO/FRO2GD/FROLGD
! ----------------------------------------------------------------------
!
!  SAVOIR SI LA LIAISON EST A PIVOT NUL
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NUMLIA : NUMERO DE LA LIAISON
! IN  TYPLIA : TYPE DE LA LIAISON
!                'C0': CONTACT
!                'F0': FROTTEMENT SUIVANT LES DEUX DIRECTIONS
!                       SIMULTANEES (3D)
!                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
!                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! OUT CFELPV : .TRUE.  SI LIAISON A PIVOT NUL
!              .FALSE. SINON
!
!
!
!
    integer :: iote
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liot
    integer :: jliot
! ======================================================================
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
    liot = resoco(1:14)//'.LIOT'
    call jeveuo(liot, 'L', jliot)
! ======================================================================
    lelpiv = .false.
    if (typlia .eq. typec0) then
        do 10 iote = 1, zi(jliot+4*nbliai)
            if (zi(jliot-1+iote) .eq. numlia) then
                lelpiv = .true.
                goto 100
            endif
10      continue
    else if (typlia.eq.typef0) then
        do 20 iote = 1, zi(jliot+4*nbliai+1)
            if (zi(jliot-1+iote+nbliai) .eq. numlia) then
                lelpiv = .true.
                goto 100
            endif
20      continue
    else if (typlia.eq.typef1) then
        do 30 iote = 1, zi(jliot+4*nbliai+2)
            if (zi(jliot-1+iote+2*nbliai) .eq. numlia) then
                lelpiv = .true.
                goto 100
            endif
30      continue
    else if (typlia.eq.typef2) then
        do 40 iote = 1, zi(jliot+4*nbliai+3)
            if (zi(jliot-1+iote+3*nbliai) .eq. numlia) then
                lelpiv = .true.
                goto 100
            endif
40      continue
    endif
100  continue
! ======================================================================
end subroutine
