subroutine pmfmcf(ip, nbgf, nbfib, nugf, sdcomp,&
                  crit, option, instam, instap,&
                  icdmat, nbvalc, defam, defap, varim,&
                  varimp, contm, defm, defp, epsm,&
                  modf, sigf, varip, isecan, codret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1504
    implicit none
#include "asterfort/pmfcom.h"
    integer :: ip, nbgf, nbfib, nbvalc, nugf(*), icdmat, isecan
    character(len=16) :: option
    character(len=24) :: sdcomp(*)
    real(kind=8) :: varim(*), varimp(*), varip(*), contm(*), defm(*), defp(*)
    real(kind=8) :: crit(*), instam, instap, defap(*), defam(*), epsm
    real(kind=8) :: sigf(*), modf(*)
    integer :: ig, ngf, nbfig
    integer :: idcipv, idcipc, idecc, idecv, icp
    integer :: iposv, iposc
    integer :: codret, codrep
! --- ------------------------------------------------------------------
    codrep = 0
! --- BOUCLE SUR LES GROUPES DE FIBRE
    idcipc = nbfib*(ip-1)
    idcipv = nbvalc*idcipc
    idecc = 1
    idecv = 1

    do 510 ig = 1, nbgf
! ---    NUMERO DU GROUPE DE FIBRE
        ngf = nugf(ig)
        icp = (ngf-1)*6
! ---    NOMBRE DE FIBRES DE CE GROUPE
        read(sdcomp(icp+6),'(I24)') nbfig
! ---    AIGUILLAGE SUIVANT COMPORTEMENT
!        POUR AVOIR MODULE ET CONTRAINTE SUR CHAQUE FIBRE
!        ATTENTION POSITION DU POINTEUR CONTRAINTE ET VARIABLES INTERNES
        iposv = idecv + idcipv
        iposc = idecc + idcipc
        call pmfcom(ip, idecc, option, sdcomp(icp+2), crit, nbfig,&
                    instam, instap, icdmat,&
                    nbvalc, defam, defap, varim(iposv), varimp( iposv),&
                    contm(iposc), defm(idecc), defp(idecc), epsm, modf(idecc),&
                    sigf(idecc), varip(iposv), isecan, codrep)
        if (codrep .ne. 0) then
            codret = codrep
!           CODE 3: ON CONTINUE ET ON LE RENVOIE A LA FIN
!           AUTRE CODES: SORTIE IMMEDIATE
            if (codrep .ne. 3) goto 900
        endif
        idecc = idecc + nbfig
        idecv = idecv + nbvalc*nbfig
! --- FIN BOUCLE GROUPES DE FIBRES$
510  end do
!
900  continue
end subroutine
