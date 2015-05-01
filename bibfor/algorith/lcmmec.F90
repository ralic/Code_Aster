subroutine lcmmec(coeft, ifa, nmat, nbcomm, necrci,&
                  itmax, toler, alpham, dgamma, dalpha,&
                  iret)
    implicit none
! person_in_charge: jean-michel.proix at edf.fr
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
! ======================================================================
#include "asterc/r8miem.h"
#include "asterfort/utmess.h"
    integer :: nmat, ifa, nbcomm(nmat, 3), iret, itmax
    real(kind=8) :: coeft(nmat), dgamma, dalpha, toler
    character(len=16) :: necrci
! ======================================================================
!  INTEGRATION DES LOIS MONOCRISTALLINES : ECROUISSAGE CINEMATIQUE
!  COMME LCMMFC MAIS EXPLICITE
! ======================================================================
!       IN  COEFT   :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           NECRCI  :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
!           DGAMMA  :  DERIVEES DES VARIABLES INTERNES A T
!           ALPHAM  : VARIABLE ECRO CINE A T
!           ITMAX  :  ITER_INTE_MAXI
!           TOLER  :  RESI_INTE_RELA
!     OUT:
!           DALPHA  : VARIABLE INTERNE ECROUISSAGE CINEMATIQUE
!           IRET    : CODE RETOUR
!
!     ----------------------------------------------------------------
    real(kind=8) :: d, gm, pm, c, cc, alpham, absdga
    real(kind=8) :: sgna
    integer :: iec, nuecin
!     ----------------------------------------------------------------
!
!     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
!
    iec=nbcomm(ifa,2)
    absdga=abs(dgamma)
    nuecin=nint(coeft(iec))
!
!----------------------------------------------------------------------
!   POUR UN NOUVEAU TYPE D'ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF
!----------------------------------------------------------------------
!
    iret=0
!      IF (NECRCI.EQ.'ECRO_CINE1') THEN
    if (nuecin .eq. 1) then
        d=coeft(iec+1)
        dalpha=dgamma-d*alpham*absdga
!
!      IF (NECRCI.EQ.'ECRO_CINE2') THEN
    else if (nuecin.eq.2) then
        iret=0
        d =coeft(iec+1)
        gm=coeft(iec+2)
        pm=coeft(iec+3)
        c =coeft(iec+4)
        cc=abs(c*alpham)
        dalpha=dgamma-d*alpham*absdga
        if (cc .gt. r8miem()) then
            sgna=alpham/abs(alpham)
            dalpha=dalpha-sgna*(cc/gm)**pm
        endif
    else
        call utmess('F', 'COMPOR1_19')
    endif
!
end subroutine
