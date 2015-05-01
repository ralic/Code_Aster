subroutine avphyd(nbordr, vwork, tdisp, kwork, sommw,&
                  tspaq, i, jvphyd)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbordr, tdisp, kwork, sommw, tspaq, i
    real(kind=8) :: vwork(tdisp)
!   vphydr(nbordr)
    integer ::jvphyd
! ----------------------------------------------------------------------
! BUT: CALCULER LA PRESSION HYDROSTATIQUE MAXIMALE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBORDR : IN   I  : NOMBRE DE NUMEROS D'ORDRE.
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
!  VPHYDR : OUT  R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
!                     TOUS LES INSTANTS.
! ----------------------------------------------------------------------
    integer :: iordr, adrs, decal
    real(kind=8) :: sixx, siyy, sizz
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
    decal = 18
!
    do 10 iordr = 1, nbordr
        adrs = (iordr-1)*tspaq + kwork*sommw*decal + (i-1)*decal
        sixx = vwork(adrs + 1)
        siyy = vwork(adrs + 2)
        sizz = vwork(adrs + 3)
!
! CALCUL DE LA PRESSION HYDROSTATIQUE = 1/3 Tr[SIG] A TOUS
! LES INSTANTS
!
        zr(jvphyd+iordr) = (sixx + siyy + sizz)/3.0d0
10  end do
!
    call jedema()
!
end subroutine
