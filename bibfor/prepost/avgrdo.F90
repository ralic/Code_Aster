subroutine avgrdo(nbvec, nbordr, vectn, vwork, tdisp,&
                  kwork, sommw, tspaq, i, nommat,&
                  nomcri, nomfor, grdvie, forvie, vala,&
                  coefpa, ncycl, jvmin, jvmax, jomin,&
                  jomax, post, cudomx, vnormx, nbplan)
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/avcdmx.h"
#include "asterfort/avcrit.h"
#include "asterfort/avdomt.h"
#include "asterfort/avdowh.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
!
    integer :: nbvec, nbordr, tdisp, kwork, sommw, tspaq, i
    real(kind=8) :: vectn(3*nbvec)
    real(kind=8) :: vwork(tdisp)
    character(len=16) :: nomcri, forvie, nomfor, grdvie
    character(len=8) :: nommat
!    integer :: omin(nbvec*(nbordr+2)), omax(nbvec*(nbordr+2))
    integer :: jomin, jomax, jvmin, jvmax
    real(kind=8) :: vala, coefpa
!    real(kind=8) :: vmin(nbvec*(nbordr+2)), vmax(nbvec*(nbordr+2))
    integer :: vnormx(2), ncycl(nbvec), nbplan
    aster_logical :: post
    real(kind=8) :: cudomx
!
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
!---------------------------------------------------------------------
! BUT:    POUR LA FATIGUE A AMPLITUDE VARIABLE
!         A PARTIR DES PICS PAR LE COMPTAGE DE RAINFLOW,DETERMINER
!         LE PLAN CRITIQUE OU LE DOMMAGE TOTAL MAXIMAL
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC  : IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  NBORDR : IN   I  : NOMBRE DE NUMEROS D'ORDRE.
!  VECTN  : IN   R  : VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!  VWORK  : IN   R  : VECTEUR DE TRAVAIL CONTENANT
!                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                     DU <<PAQUET>> DE MAILLES.
!  TDISP  : IN   I  : TAILLE DU VECTEUR DE TRAVAIL.
!  KWORK  : IN   I  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET
!                               MAILLES OU LE 1ER NOEUD DU PAQUET DE
!                               NOEUDS;
!                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                               MAILLES OU LE IEME NOEUD DU PAQUET
!                               DE NOEUDS.
!  SOMMW  : IN   I  : SOMME DES POINTS DE GAUSS OU DES NOEUDS DES N
!                     MAILLES PRECEDANT LA MAILLE COURANTE.
!  TSPAQ  : IN   I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                     OU DE NOEUDS COURANT.
!  I      : IN   I  : IEME POINT DE GAUSS OU IEME NOEUD.
!  NOMMAT   IN   K  : NOM DU MATERIAU.
!  NOMCRI : IN  K16 : NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
!  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! JVMIN      IN  I  : ADDRESEE JEUVEUX DES VALEURS MIN DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JVMAX      IN  I  : ADDRESEE JEUVEUX DES VALEURS MAX DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JOMIN      IN  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX VALEURS
!                     MIN DESCYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
! JOMAX      IN  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX VALEURS
!  VNORMX   OUT  I  : NUMERO DU PREMIER VECTEUR NORMAL ASSOCIE AU
!                     MAX DES CUMULS DE DOMMAGE.
!  VNORMX2   OUT  I : NUMERO DU DEUXIEME VECTEUR NORMAL ASSOCIE AU
!                     MAX DES CUMULSDE DOMMAGE.
!  CUDOMX   OUT  R  : VALEUR DU MAX DES CUMULS DE DOMMAGE
!  NBPLAN   OUT  I  : NOMBRE DE PLAN MAX DOMMAGE
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
!  VSIGN  VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
!         NORMALE, POUR TOUS LES NUMEROS D'ORDRE
!         DE CHAQUE VECTEUR NORMAL.
!  VPHYDR VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
!         TOUS LES INSTANTS.
!  GDREQ  VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
!         EQUIVALENTE, POUR TOUS LES NUMEROS D'ORDRE
!         DE CHAQUE VECTEUR NORMAL.
!  DOMEL  VECTEUR CONTENANT LES VALEURS DES DOMMAGES
!         ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!         DE CHAQUE VECTEUR NORMAL.
!  NRUPT  VECTEUR CONTENANT LES NOMBRES DE CYCLES
!         ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!         DE CHAQUE VECTEUR NORMAL.
!  DOMTOT VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
!         DE CHAQUE VECTEUR NORMAL
!
!    real(kind=8) :: gdreq(nbvec*nbordr)
    integer :: jgdreq, jnrupt, jdomel
    real(kind=8) :: domtot(nbvec)
!    real(kind=8) :: nrupt(nbvec*nbordr), domel(nbvec*nbordr), domtot(nbvec)
!  ------------------------------------------------------------------
!  ---------------------
! C ------------------------------------------------------------------
!       DATA  NOMPAR/  'TAUPR_1','TAUPR_2','SIGN_1','SIGN_2',
!      &               'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2'  /
! C-------------------------------------------------------------------
!
! 1.3 CALCUL DE LA GRANDEUR EQUIVALENTE AU SENS DU CRITERE CHOISI :
!     MATAKE_MODI_AV, FATEMI ET SOCIE (ELASTIQUE OU PLASTIQUE), DANG VAN
!
    call wkvect('&&AVGRDO_GDREQ', 'V V R', nbvec*nbordr, jgdreq)
    call wkvect('&&AVGRDO_NRUPT', 'V V R', nbvec*nbordr, jnrupt)
    call wkvect('&&AVGRDO_DOMEL', 'V V R', nbvec*nbordr, jdomel)
!
    call avcrit(nbvec, nbordr, vectn, vwork, tdisp,&
                kwork, sommw, tspaq, i, vala,&
                coefpa, ncycl, jvmin, jvmax, jomin,&
                jomax, nomcri, nomfor, jgdreq)
!
!          CALL AVCRIT(NBVEC, NBORDR, VALA, COEFPA, NCYCL,
!      &               VMIN, VMAX, OMIN, OMAX,NOMCRI,NOMFOR,
!      &               VSIGN, VPHYDR, GDREQ)
!
! 2. CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER
!
    call avdowh(nbvec, nbordr, nommat, nomcri, ncycl,&
                jgdreq, grdvie, forvie, post, jdomel,&
                jnrupt)
!
! 3. CALCUL DU DOMMAGE TOTAL (CUMUL)
!
    call avdomt(nbvec, nbordr, ncycl, jdomel, domtot)
!
! 4. CALCUL DU CUMUL DE DOMMAGE MAXIMAL ET VECTEUR NORMAL ASSOCIE
!
    call avcdmx(nbvec, domtot, cudomx, vnormx, nbplan)
!
    call jedetr('&&AVGRDO_GDREQ')
    call jedetr('&&AVGRDO_NRUPT')
    call jedetr('&&AVGRDO_DOMEL')
!
end subroutine
