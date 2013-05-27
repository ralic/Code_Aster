subroutine zbiter(rho, f, rhoopt, fopt, mem,&
                  rhonew, echec)
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
    include 'asterfort/zbarch.h'
    include 'asterfort/zbborn.h'
    include 'asterfort/zbopti.h'
    include 'asterfort/zbproj.h'
    include 'asterfort/zbroot.h'
    real(kind=8) :: mem(2, *)
    real(kind=8) :: rho, rhoopt, rhonew
    real(kind=8) :: f, fopt
    logical :: echec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
!
! RESOLUTION F(RHO) = 0 : ITERATION COURANTE
!
! ----------------------------------------------------------------------
!
!  IN  RHO    : SOLUTION COURANTE
!  IN  F      : VALEUR DE LA FONCTION EN RHO
!  I/O MEM    : COUPLES (RHO,F) ARCHIVES - GESTION INTERNE PAR ZEITER
!  I/O RHOOPT : VALEUR OPTIMALE DU RHO
!  I/O FOPT   : VALEUR OPTIMALE DE LA FONCTIONNELLE
!  OUT RHONEW : NOUVEL ITERE
!  OUT ECHEC  : .TRUE. SI LA RECHERCHE A ECHOUE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhoneg, rhopos
    real(kind=8) :: parmul, fneg, fpos
    integer :: dimcpl, nbcpl
    logical :: bpos, lopti
    common /zbpar/ rhoneg,rhopos,&
     &               parmul,fneg  ,fpos  ,&
     &               dimcpl,nbcpl ,bpos  ,lopti
!
! ----------------------------------------------------------------------
!
    echec = .false.
!
! --- ARCHIVAGE DU NOUVEAU COUPLE (RHO,F)
!
    call zbarch(rho, f, mem)
!
! --- ACTUALISATION DES BORNES FLOTTANTES
!
    call zbborn(rho, f)
!
! --- DETECTION S'IL S'AGIT DE LA SOLUTION OPTIMALE JUSQU'A PRESENT
!
    call zbopti(rho, f, rhoopt, fopt)
!
! --- RECHERCHE DE LA BORNE MAX
!
    if (bpos) then
        call zbroot(mem, rhonew, echec)
        if (echec) goto 999
    else
        rhonew = rho * parmul
    endif
!
! --- PROJECTION DE LA NOUVELLE SOLUTION SUR LES BORNES
!
    call zbproj(rhonew, echec)
!
999  continue
!
end subroutine
