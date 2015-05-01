subroutine uttcpg(action, typimp)
    implicit none
#include "jeveux.h"
#include "asterfort/uttcpi.h"
#include "asterfort/uttcpl.h"
#include "asterfort/uttcpu.h"
    character(len=*) :: action, typimp
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : GERER LES MESURES DE TEMPS "GENERALES" : CELLES DONT LA LISTE
!        EST DONNEE DANS UTTCPL ET QUI SONT PILOTEES GRACE AU MOT CLE
!        DEBUT/MESURE_TEMPS/NIVE_DETAIL
!
!  ACTION = 'INIT' : ON INITIALISE LES DIFFERENTES MESURES
!  ACTION = 'IMPR' : ON IMPRIME LES DIFFERENTES MESURES
!  TYPIMP = 'CUMU' : ON IMPRIME LE "CUMUL" DE LA MESURE
!  TYPIMP = 'INCR' : ON IMPRIME L'INCREMENT DE LA MESURE
! ----------------------------------------------------------------------
!
!     -- COMMONS POUR MESURE DE TEMPS :
!        MTPNIV : NIVEAU D'IMPRESSION DES MESURES DE TEMPS DEMANDE
!                 PAR L'UTILISATEUR
!        MTPSTA : IMPRESSION DES STATISTIQUES OU NON EN PARALLELE
    integer :: mtpniv, mtpsta, indmax
    parameter (indmax=5)
    character(len=80) :: snolon(indmax)
    real(kind=8) :: valmes(indmax*7), valmei(indmax*7)
    common /mestp1/ mtpniv,mtpsta
    common /mestp2/ snolon
    common /mestp3/ valmes,valmei
!
    integer :: ndim, nbmesu
    parameter (ndim=30)
    integer :: ifm, k
    character(len=1) :: prpal(ndim)
    character(len=24) :: nomc(ndim)
    character(len=80) :: noml(ndim)
!----------------------------------------------------------------------
!
    call uttcpl(ndim, nbmesu, nomc, noml, prpal)
!
    if (action .eq. 'INIT') then
        do 1, k=1,nbmesu
        call uttcpu(nomc(k), 'INIT', noml(k))
 1      continue
!
    else if (action.eq.'IMPR') then
        ifm=6
!
        if (mtpniv .eq. 1) then
            do 2, k=1,nbmesu
            if (prpal(k) .eq. 'S') goto 2
            if (typimp .eq. 'INCR') goto 2
            call uttcpi(nomc(k), ifm, typimp)
 2          continue
!
        else if (mtpniv.eq.2) then
            do 3, k=1,nbmesu
            if (typimp .eq. 'INCR') goto 3
            call uttcpi(nomc(k), ifm, typimp)
 3          continue
!
        else if (mtpniv.eq.3) then
            do 4, k=1,nbmesu
            call uttcpi(nomc(k), ifm, typimp)
 4          continue
!
        endif
!
    endif
!
end subroutine
