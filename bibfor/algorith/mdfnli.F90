subroutine mdfnli(nbmode, depgen, vitgen, accgen, fexgen,&
                  nbchoc, logcho, dplmod, parcho, noecho,&
                  saucho, nbrede, dplred, fonred, saured,&
                  saredi, nbrevi, dplrev, fonrev, saurev,&
                  sarevi, temps, nofdep, nofvit, nofacc,&
                  nbexci, psidel, monmot, nbrfis, fk,&
                  dfk, angini, foncp, numpas, nbpal,&
                  dt, dtsto, vrotat, typal, finpal,&
                  cnpal, prdeff, conv, fsauv)
! aslint: disable=W1504
    implicit none
#include "asterfort/mdfcho.h"
#include "asterfort/mdfedy.h"
#include "asterfort/mdfred.h"
#include "asterfort/mdfrev.h"
#include "asterfort/mdrfis.h"
    integer :: nbmode, nbrede, nbrevi, nbexci, logcho(*), saredi(*), sarevi(*)
    integer :: nbpal, nbchoc
    integer :: numpas
    real(kind=8) :: dt, dtsto, vrotat, conv, angini
    real(kind=8) :: depgen(*), vitgen(*), fexgen(*)
    real(kind=8) :: parcho(*), saucho(nbchoc,*)
    real(kind=8) :: saured(*), dplrev(*), dplred(*), saurev(*)
    real(kind=8) :: accgen(*), dplmod(nbchoc, nbmode, *)
    real(kind=8) :: temps, psidel(nbchoc, nbexci, *), ltemps(3)
    character(len=8) :: noecho(*), fonred(*), fonrev(*), monmot
    character(len=8) :: nofdep(nbexci), nofvit(nbexci), nofacc(nbexci)
    character(len=8) :: fk(2), dfk(2), foncp
!
    logical(kind=1) :: prdeff
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: nbrfis
!-----------------------------------------------------------------------
    parameter (palmax=20)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    real(kind=8) :: fsauv(palmax, 3)
!
!-----------------------------------------------------------------------
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
!
!     AJOUTE AU SECOND MEMBRE LA CONTRIBUTION DES FORCES NON LINEAIRES
!     ------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DEPGEN : DEPLACEMENTS GENERALISES AU PAS COURANT
! IN  : VITGEN : VITESSES GENERALISEES AU PAS COURANT
! IN  : ACCGEN : ACCELERATIONS GENERALISEES AU PAS PRECEDENT
! VAR : FEXGEN : FORCES EXTERIEURES GENERALISEES AU PAS COURANT
! VAR : MASGEN : MASSES GENERALISEES AU PAS COURANT (VAR SI FLUIDE)
! VAR : AMOGEN : AMORTISSEMENTS GENERALISES COURANTS (VAR SI FLUIDE)
! VAR : PULSA2 : CARRES DES PULSATIONS PROPRES (VAR SI FLUIDE)
! IN  : PHICAR : DIAGONALE DU PRODUIT (PHI)T. PHI
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : LOGCHO : INDICATEUR D'ADHERENCE ET DE FORCE FLUIDE
! IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
! IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! OUT : SAUCHO : TABLEAU DES VALEURS A SAUVEGARDER POUR LES CHOCS
! IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
! IN  : DPLRED : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE RED
! IN  : FONRED : TABLEAU DES FONCTIONS AUX NOEUDS DE RED
! OUT : SAURED : TABLEAU DES VALEURS A SAUVEGARDER POUR LES RED
! OUT : SAREDI : TABLEAU DES VALEURS A SAUVEGARDER POUR LES RED
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : DPLREV : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE REV
! IN  : FONREV : TABLEAU DES FONCTIONS AUX NOEUDS DE REV
! OUT : SAUREV : TABLEAU DES VALEURS A SAUVEGARDER POUR LES REV
! OUT : SAREVI : TABLEAU DES VALEURS A SAUVEGARDER POUR LES REV
!
! IN  : TEMPS  : INSTANT DE CALCUL DES DEPL_IMPO
! IN  : NOFDEP : NOM DE LA FONCTION DEPL_IMPO
! IN  : NOFVIT : NOM DE LA FONCTION VITE_IMPO
! IN  : NOFACC : NOM DE LA FONCTION ACCE_IMPO
! IN  : NBEXCI : NOMBRE D'ACCELERO DIFFERENTS
! IN  : PSIDEL : TABLEAU DE VALEURS DE PSI*DELTA
! IN  : MONMOT : = OUI SI MULTI-APPUIS
! ----------------------------------------------------------------------
!
!   FORCES NON-LINEAIRES DE TYPE CHOC
    if (nbchoc .ne. 0 .and. nbrfis .eq. 0) then
        ltemps(1) = temps
        ltemps(2) = dt
        ltemps(3) = numpas
        call mdfcho(nbmode, depgen, vitgen, accgen, fexgen,&
                    nbchoc, logcho, dplmod, parcho, noecho,&
                    saucho, ltemps, nofdep, nofvit, nofacc,&
                    nbexci, psidel, monmot)
    endif
!
!   NON-LINEARITE DE TYPE RELA_EFFO_DEPL
    if (nbrede .ne. 0) then
        call mdfred(nbmode, depgen, fexgen, nbrede, dplred,&
                    fonred, saured, saredi)
    endif
!
!   NON-LINEARITE DE TYPE ROTOR FISSURE
    if (nbrfis .gt. 0) then
        call mdrfis(nbmode, depgen, fexgen, nbchoc, nbrfis,&
                    dplmod, fk, dfk, parcho, angini,&
                    vrotat, foncp, temps)
    endif
!
!   NON-LINEARITE DE TYPE RELA_EFFO_VITE
    if (nbrevi .ne. 0) then
        call mdfrev(nbmode, vitgen, fexgen, nbrevi, dplrev,&
                    fonrev, saurev, sarevi)
    endif
!
!   COUPLAGE AVEC EDYOS
    conv = 1.d0
    if (nbpal .ne. 0) then
        call mdfedy(nbpal, nbmode, numpas, dt, dtsto,&
                    temps, vrotat, dplmod, depgen, vitgen,&
                    fexgen, typal, finpal, cnpal, prdeff,&
                    conv, fsauv)
    endif
!
end subroutine
