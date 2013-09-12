subroutine dfllae(mcfact, iechec, pcplus)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/getvr8.h"
    character(len=16) :: mcfact
    integer :: iechec
    real(kind=8) :: pcplus
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES PARAMETRES L'ACTION DE TYPE ITER_SUPPL
!
! ----------------------------------------------------------------------
!
!
! IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
! IN  IECHEC : NUMERO OCCURRENCE ECHEC
! OUT PCPLUS : VALEUR DE PCENT_ITER_PLUS
!
! ----------------------------------------------------------------------
!
    integer :: iret
    integer :: iarg
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    pcplus = 0.d0
!
! --- OPTIONS DE L'ACTION
!
    call getvr8(mcfact, 'PCENT_ITER_PLUS', iocc=iechec, scal=pcplus, nbret=iret)
!
end subroutine
