subroutine avsign(nbvec, nbordr, vectn, vwork, tdisp,&
                  kwork, sommw, tspaq, i, vsign)
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
! person_in_charge: jean.angles at edf.fr
    implicit      none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, tdisp, kwork, sommw, tspaq, i
    real(kind=8) :: vectn(3*nbvec)
    real(kind=8) :: vwork(tdisp), vsign(nbvec*nbordr)
! ----------------------------------------------------------------------
! BUT: CALCULER LA CONTRAINTE NORMALE POUR TOUS LES VECTEURS NORMAUX
!      A TOUS LES NUMEROS D'ORDRE.
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
!  NOMCRI : IN  K16 : NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
!  VSIGN  : OUT  R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
!                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
!                     DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
    integer :: n, decal, ivect, iordr, adrs
    real(kind=8) :: nx, ny, nz
    real(kind=8) :: sixx, siyy, sizz, sixy, sixz, siyz, fx, fy, fz, norm
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
    n = 1
!       IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR.
!      &  FORDEF ) THEN
!          DECAL = 12
!       ELSE
!          DECAL = 6
!       ENDIF
    decal = 18
    do 10 ivect = 1, nbvec
        nx = vectn((ivect-1)*3 + 1)
        ny = vectn((ivect-1)*3 + 2)
        nz = vectn((ivect-1)*3 + 3)
!
        do 20 iordr = 1, nbordr
            adrs = (iordr-1)*tspaq + kwork*sommw*decal + (i-1)*decal
            sixx = vwork(adrs + 1)
            siyy = vwork(adrs + 2)
            sizz = vwork(adrs + 3)
            sixy = vwork(adrs + 4)
            sixz = vwork(adrs + 5)
            siyz = vwork(adrs + 6)
!
! CALCUL DE vect_F = [SIG].vect_n
            fx = sixx*nx + sixy*ny + sixz*nz
            fy = sixy*nx + siyy*ny + siyz*nz
            fz = sixz*nx + siyz*ny + sizz*nz
!
! CALCUL DE NORM = vect_F.vect_n
            norm = fx*nx + fy*ny + fz*nz
            vsign(n) = norm
            n = n + 1
20      continue
10  end do
!
    call jedema()
!
end subroutine
