subroutine te0146(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/clcplq.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
! aslint: disable=W0104
    character(len=16) :: option, nomte
!.......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  BUT: CALCUL DE L'OPTION FERRAILLAGE POUR LES ELEMENTS DE COQUE
!.......................................................................
!_____________________________________________________________________
!
! CALCUL DES ARMATURES DE BETON ARME (METHODE DE CAPRA ET MAURY).
!
! VERSION 1.2 DU 31/03/2010
!_____________________________________________________________________
!
!
! PARAMETRES D'ECHANGE ENTRE CODE_ASTER ET CLCPLQ (POINT D'ENTREE DU
! CALCUL DE FERRAILLAGE PAR CAPRA ET MAURY
!
!   PARAMETRES D'ENTREE (FOURNIS PAR CODE_ASTER)
!
!     HT     (DP)  EPAISSEUR DE LA COQUE
!     ENROBG (DP)  ENROBAGE
!     TYPCMB (I)   TYPE DE COMBINAISON
!               0 = ELU, 1 = ELS
!     CEQUI  (DP)  COEFFICIENT D'EQUIVALENCE ACIER/BETON
!     PIVA   (DP)  VALEUR DU PIVOT A
!     PIVB   (DP)  VALEUR DU PIVOT B
!     EA      (DP) MODULE D'YOUNG DE L'ACIER
!     SIGACI (DP)  CONTRAINTE ADMISSIBLE DANS L'ACIER
!     SIGBET (DP)  CONTRAINTE ADMISSIBLE DANS LE BETON
!     EFFRTS (DP-DIM 8) TORSEUR DES EFFORTS
!
!   PARAMETRES DE SORTIE (RENVOYES A CODE_ASTER)
!     DNSITS  (DP-DIM 5) DENSITES
!                      1 A 4 : SURFACES D'ACIER LONGITUDINAL EN CM2/M,
!                      5 TRANSVERSAL: EN CM2/M2
!     SIGMBE (DP)  CONTRAINTE BETON
!     EPSIBE (DP)  DEFORMATION BETON
!     IERR        CODE RETOUR (0 = OK)
!----------------------------------------------------------------------
    real(kind=8) :: cequi, piva, pivb, sigaci, sigbet, effrts(8), dnsits(5)
    real(kind=8) :: sigmbe, epsibe, ht, enrobg, ea
    integer :: ierr, jepais, jefge, jfer1, jfer2, itab(7), nno
    integer :: typcmb, ino, icmp, iret, k
    integer :: iadzi, iazk24
!
!
!
    call tecael(iadzi, iazk24)
!
    call jevech('PCACOQU', 'L', jepais)
    call jevech('PFERRA1', 'L', jfer1)
    call jevech('PFERRA2', 'E', jfer2)
    ht=zr(jepais)
!
    call jevech('PEFFORR', 'L', jefge)
    call tecach('OOO', 'PEFFORR', 'L', iret, nval=7,&
                itab=itab)
    ASSERT(iret.eq.0)
    nno=itab(3)
    ASSERT(nno.gt.0.and.nno.le.9)
    ASSERT(itab(2).eq.8*nno)
!
!       -- CALCUL DE LA CONTRAINTE MOYENNE :
!       ----------------------------------------------
    do 1, icmp=1,8
    effrts(icmp) = 0.d0
    do 2, ino=1,nno
    effrts(icmp) = effrts(icmp) + zr(jefge-1+(ino-1)*8+icmp)/ nno
 2  continue
 1  continue
!
!
!       -- RECUPERATION DES DONNEES DE L'UTILISATEUR :
!       ----------------------------------------------
!       FER1_R=TYPCOMB  ENROBG  CEQUI  SIGACI  SIGBET  PIVA  PIVB ES
!                  1        2      3       4       5       6    7  8
    typcmb=nint(zr(jfer1-1+1))
    enrobg=zr(jfer1-1+2)
    cequi =zr(jfer1-1+3)
    sigaci=zr(jfer1-1+4)
    sigbet=zr(jfer1-1+5)
    piva  =zr(jfer1-1+6)
    pivb  =zr(jfer1-1+7)
    ea = zr(jfer1-1+8)
!
!
!       -- CALCUL PROPREMENT DIT :
!       --------------------------
    sigmbe=0.d0
    epsibe=0.d0
    do 10, k=1,5
    dnsits(k)=0.d0
10  continue
    call clcplq(ht, enrobg, typcmb, piva, pivb,&
                ea, cequi, sigaci, sigbet, effrts,&
                dnsits, sigmbe, epsibe, ierr)
    if (ierr .gt. 0) then
        call utmess('F', 'CALCULEL_72', si=ierr)
    endif
!
!
!       -- stockage des resultats :
!       --------------------------
!     FER2_R=  DNSXI     DNSXS   DNSYI   DNSYS   DNST    SIGMBE   EPSIBE
!               1          2       3       4       5       6        7
    zr(jfer2-1+1)= dnsits(1)
    zr(jfer2-1+2)= dnsits(3)
    zr(jfer2-1+3)= dnsits(2)
    zr(jfer2-1+4)= dnsits(4)
    zr(jfer2-1+5)= dnsits(5)
    zr(jfer2-1+6)= sigmbe
    zr(jfer2-1+7)= epsibe
!
end subroutine
