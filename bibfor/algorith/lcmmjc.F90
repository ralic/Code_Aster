subroutine lcmmjc(coeft, ifa, nmat, nbcomm, ir,&
                  is, necrci, dgamms, alphmr, dalpha,&
                  sgnr, daldgr)
    implicit none
#include "asterc/r8prem.h"
    integer :: ifa, nmat, nbcomm(nmat, 3), ir, is
    real(kind=8) :: coeft(nmat), daldgr, dgamms, alphmr, sgnr
    character(len=16) :: necrci
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
!  POUR L'ECROUISSAGE CINEMATIQUE
!       IN  COEFT  :  PARAMETRES MATERIAU
!           IFA    :  NUMERO DE FAMILLE
!           IR     :
!           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           IS     :  NUMERO DU SYSTEME DE GLISSEMENT EN COURS
!           IR     :  NUMERO DU SYSTEME DE GLISSEMENT POUR INTERACTION
!           NECRCI :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
!           DGAMMS :  ACCROISS. GLISSEMENT PLASTIQUE
!           ALPHMR :  VAR. ECR. CIN. INST T
!           DALPHA :  DELTA ALPHA
!           SGNR   : DELTA P ACTUEL
!     OUT:
!           DALDGR : dAlpha/dGamma
!
!     ----------------------------------------------------------------
    real(kind=8) :: d, dalpha
    integer :: iec, nuecin
!     ----------------------------------------------------------------
!
!
    iec=nbcomm(ifa,2)
    nuecin=nint(coeft(iec))
    daldgr=0.d0
!
!--------------------------------------------------------------------
!     POUR UN NOUVEL ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF
!--------------------------------------------------------------------
!
!      IF (NECRCI.EQ.'ECRO_CINE1') THEN
    if (nuecin .eq. 1) then
!          D=COEFT(IEC-1+1)
        d=coeft(iec+1)
        daldgr=0.d0
        if (is .eq. ir) then
            daldgr=(1.d0-d*alphmr*sgnr)/(1.d0+d*abs(dgamms))**2
        endif
    endif
!
!      IF (NECRCI.EQ.'ECRO_CINE2') THEN
    if (nuecin .eq. 2) then
        daldgr=0.d0
        if (is .eq. ir) then
            if (abs(dgamms) .gt. r8prem()) then
                daldgr=dalpha/dgamms
            else
                daldgr=1.d0
            endif
        endif
    endif
!
end subroutine
