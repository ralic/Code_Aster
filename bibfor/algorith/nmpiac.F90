subroutine nmpiac(sdpilo, eta)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sdpilo
    real(kind=8) :: eta
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! REACTUALISATION DES BORNES DE PILOTAGE SI DEMANDE
!
! ----------------------------------------------------------------------
!
!
! IN  SDPILO : SD PILOTAGE
! IN  ETA    : PARAMETRE DE PILOTAGE
!
!
!
!
    character(len=24) :: evolpa, typsel, typpil
    integer :: jpltk, jplir
!
! ----------------------------------------------------------------------
!
    call jeveuo(sdpilo(1:19)// '.PLTK', 'L', jpltk)
    typpil = zk24(jpltk)
    typsel = zk24(jpltk+5)
    evolpa = zk24(jpltk+6)
    call jeveuo(sdpilo(1:19)// '.PLIR', 'E', jplir)
    if (typsel .eq. 'ANGL_INCR_DEPL' .and.&
        (typpil.eq.'LONG_ARC' .or.typpil.eq.'SAUT_LONG_ARC')) then
        zr(jplir-1+6)=zr(jplir)
    endif
!
!
    if (evolpa .eq. 'SANS') goto 9999
    if (evolpa .eq. 'CROISSANT') then
        zr(jplir+4) = eta
    else
        zr(jplir+3) = eta
    endif
!
9999  continue
end subroutine
