subroutine avcipr(nbvec, vectn, vectu, vectv, nbordr,&
                  kwork, sommw, vwork, tdisp, tspaq,&
                  ipgn, nomcri, nomfor, fordef, fatsoc,&
                  proaxe, pseuil, method, ncycl, vmin,&
                  vmax, omin, omax)
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
!
#include "asterfort/avenca.h"
#include "asterfort/avpeak.h"
#include "asterfort/avpic2.h"
#include "asterfort/avrain.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/projax.h"
#include "asterfort/propla.h"
    integer :: nbvec, nbordr, kwork
    integer :: sommw, tdisp, tspaq, ipgn
    real(kind=8) :: vectn(3*nbvec), vectu(3*nbvec), vectv(3*nbvec)
    real(kind=8) :: vwork(tdisp), fatsoc, pseuil
    character(len=16) :: nomcri, nomfor, proaxe
    character(len=8) :: method
    integer :: ncycl(nbvec)
    integer :: omin(nbvec*(nbordr+2)), omax(nbvec*(nbordr+2))
    real(kind=8) :: vmin(nbvec*(nbordr+2)), vmax(nbvec*(nbordr+2))
    logical :: fordef
!
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
!---------------------------------------------------------------------
! BUT:  POUR LA FATIGUE A AMPLITUDE VARIABLE
!       A PARTIR DE l'HISTOIRE DE CISSAILLEMENT, PROJETER SUR UN OU
!       2 AXES ET DETERMINER DES PICS PAR LE COMPTAGE DE RAINFLOW
! REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU UN POINT
!           GAUSSE
!
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN  I  : NOMBRE DE VECTEURS NORMAUX.
!  VECTN    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
!                    VECTEURS NORMAUX.
!  VECTU    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
!                    VECTEURS u DU PLAN DE CISAILLEMENT.
!  VECTV    IN  R  : VECTEUR CONTENANT LES COMPOSANTES DES
!                    VECTEURS v DU PLAN DE CISAILLEMENT.
!  NBORDR   IN  I  : NOMBRE DE NUMEROS D'ORDRE.
!  KWORK    IN  I  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET
!                              MAILLES OU LE 1ER NOEUD DU PAQUET DE
!                              NOEUDS;
!                    KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                              MAILLES OU LE IEME NOEUD DU PAQUET
!                              DE NOEUDS.
!  SOMMW    IN  I  : SOMME DES POINTS DE GAUSS OU DES NOEUDS DES N
!                    MAILLES PRECEDANT LA MAILLE COURANTE.
!  VWORK    IN  R  : VECTEUR DE TRAVAIL CONTENANT
!                    L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                    ATTACHES A CHAQUE POINT DE GAUSS OU NOEUD DES
!                    MAILLE OU NOEUD DU <<PAQUET>> DE MAILLES OU
!                    DE NOEUDS.
!  TDISP    IN  I  : DIMENSION DU VECTEUR VWORK
!  TSPAQ    IN  I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                    OU DE NOEUDS COURANT.
!  I        IN  I  : IEME POINT DE GAUSS OU IEME NOEUD.
!  NOMCRI   IN  K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
!  FATSOC   IN  R  : COEFFICIENT PERMETTANT D'UTILISER LES MEMES
!                    ROUTINES POUR LE TRAITEMENT DES CONTRAINTES ET
!                    DES DEFORMATIONS.
!  PROAXE    IN   K16: TYPE DE PROJECTION (UN OU DEUX AXES).
!  PSEUIL    IN   R  : SEUIL DE DTECTION DES PICS
!
!  METHOD    IN   K  : METHODE D'EXTRACTION DES PICS, PAR EXEMPLE :
!                     RAINFLOW.
! NCYCL     OUT  I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! VMIN      OUT  R  : VALEURS MIN DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! VMAX      OUT  R  : VALEURS MAX DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! OMIN      OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MIN DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
! OMAX      OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MAX DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
! REMARQUE : CETTE ROUTINE SERT POUR LE TRAITEMENT DES POINTS DE GAUSS
!            ET DES NOEUDS.
! ----------------------------------------------------------------------
!
! VECTRA  VECTEUR DE TRAVAIL CONTENANT
!         LES COMPOSANTES u ET v DU VECTEUR TAU
!         (CONTRAINTE DE CISAILLEMENT) OU
!         GAMMA (DEFORMATION DE CISAILLEMENT), POUR TOUS LES
!         NUMEROS D'ORDRE DE CHAQUE VECTEUR NORMAL.
! LSIG0   VARIABLE LOGIQUE QUI INDIQUE :
!         - LSIG0 = FALSE --> CAS GENERAL, LES CONTRAINTES
!                             SONT DIFFERENTES DE ZERO ;
!         - LSIG0 =  TRUE --> LES CONTRAINTES SONT NULLES
!                             A TOUS LES PAS DE TEMPS, QUEL
!                             QUE SOIT LE VECTEUR NORMAL.
! IFLAG  VECTEUR DE DRAPEAUX QUI INDIQUE :
!         - IFLAG(i) = 0 --> CAS GENERAL ;
!         - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
!                            PLAN DE CISAILLEMENT SONT
!                            ALIGNES VERTICALEMENT ;
!         - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
!                            PLAN DE CISAILLEMENT SONT
!                            ALIGNES HORIZONTALEMENT ;
!         - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
!                            PLAN DE CISAILLEMENT SONT
!                            CONTENUS DANS UN CADRE DE
!                            COTES INFERIEURS A EPSILO.
! RMIMA  VECTEUR CONTENANT LES COORDONNEES DES POINTS
!        EXTREMES DU CADRE (CUMIN, CUMAX, CVMIN, CVMAX)
!        POUR TOUS LES VECTEURS NORMAUX.
! RAXE   VECTEUR CONTENANT L'AMPLITUDE DES POINTS
!        PROJETES.
! NPOIN  NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!        NORMAUX.
! VALPOI VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!        NORMAUX.
! VALORD NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!        TOUS LES VECTEURS NORMAUX.
! NPIC   NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!        NORMAUX APRES REARANGEMENT DES PICS.
! PIC    VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!        NORMAUX APRES REARANGEMENT DES PICS.
! ORDPIC NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!        TOUS LES VECTEURS NORMAUX APRES REARANGEMENT
!        DES PICS.
! RTRV   VECTEUR DE TRAVAIL REEL (POUR LES POINTS)
! ITRV   VECTEUR DE TRAVAIL ENTIER (POUR LES NUME_ORDRE)
!
    real(kind=8) :: vectra(2*nbvec*nbordr), rmima(4*nbvec)
    integer :: iflag(nbvec), itrv(2*(nbordr+2))
    logical :: lsig0
    real(kind=8) :: raxe(nbvec*nbordr), valpoi(nbvec*nbordr)
    integer :: npoin(nbvec), valord(nbvec*nbordr)
    integer :: npic(nbvec), ordpic(nbvec*(nbordr+2))
    real(kind=8) :: pic(nbvec*(nbordr+2)), rtrv(nbordr+2)
!      REAL*8        CUDOMX, NXM, NYM, NZM
!     ------------------------------------------------------------------
!
!  PROJECTION DE L'HISTORIQUE DU CISAILLEMENT DANS UN PLAN
    call jemarq()
!
    call propla(nbvec, vectn, vectu, vectv, nbordr,&
                kwork, sommw, vwork, tdisp, tspaq,&
                ipgn, nomcri, nomfor, fordef, fatsoc,&
                vectra)
!
! CALCUL DU DOMMAGE MAX ET DU VECTEUR NORMAL ASSOCIE POUR
! LE NOEUD/POINT GAUSS COURANT DE LA MAILLE COURANTE.
!
! 1. REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
!    DELTA_TAU POUR UN NOEUD ET DU VECTEUR DE TRAVAIL
!    PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
!
! 2. ENCADREMENT DES POINTS DANS LE PLAN
!
    lsig0 = .false.
!
    call avenca(vectra, nbvec, nbordr, lsig0, iflag,&
                rmima)
!
!       IF (LSIG0) THEN
!          CUDOMX = 0.0D0
!          NXM = 0.0D0
!          NYM = 0.0D0
!          NZM = 1.0D0
!          GOTO 555
!       ENDIF
!
! 3. PROJECTION DE L'HISTORIQUE DE CHARGEMENT SUR UN OU DEUX AXES
!
    call projax(vectra, nbvec, nbordr, proaxe, iflag,&
                rmima, raxe)
!
! 4. COMPTAGE RAINFLOW (NORME AFNOR + POSTDAM)
!
! 4.1 PREMIER FILTRAGE DES PICS DE LA FONCTION
!
    call avpeak(raxe, nbvec, nbordr, pseuil, iflag,&
                npoin, valpoi, valord)
!
! 4.2 REARANGEMENT ET EXTRACTION DES PICS
!
!
    call avpic2(method, nbvec, nbordr, rtrv, itrv,&
                npoin, valpoi, valord, npic, pic,&
                ordpic)
!
! 4.3 COMPTAGE RAINFLOW
!
!
    call avrain(nbvec, nbordr, itrv, npic, pic,&
                ordpic, fatsoc, ncycl, vmin, vmax,&
                omin, omax)
!
!
    call jedema()
!
end subroutine
