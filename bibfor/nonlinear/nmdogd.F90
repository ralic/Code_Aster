subroutine nmdogd(moclef, comp, k, ncomel, lcomel,&
                  defo)
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
#include "asterc/getexm.h"
#include "asterfort/getvtx.h"
    integer :: ncomel, k
    character(len=*) :: comp
    character(len=16) :: lcomel(5), defo, moclef
    integer :: n1
    integer :: exist
!
! ----------------------------------------------------------------------
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
!    SAISIE ET VERIFICATION DU TYPE DE DEFORMATION UTILISEE
!
! IN      COMP    : NOM DU COMPORTEMENT (ICI KIT_*)
! IN      K       : NUMERO D'OCCURRENCE DE COMP_INCR
! IN/OUT  NCOMEL  : NOMBRE TOTAL DE COMPORTEMENTS ELEMENTAIRES
! IN/OUT  LCOMEL  : NOMS DES COMPORTEMENTS ELEMENTAIRES
! OUT     DEFO    : TYPE DE DEFORMATION
! OUT     NBVARI  : NOMBRE DE VARI POUR LA METALLURGIE
! ----------------------------------------------------------------------
!
    exist = getexm(moclef,'DEFORMATION')
    defo='PETIT'
    if (exist .eq. 1) then
        call getvtx(moclef, 'DEFORMATION', iocc=k, scal=defo, nbret=n1)
        ncomel=ncomel+1
        lcomel(ncomel)=defo
    endif
end subroutine
