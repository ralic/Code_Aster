subroutine propla(nbvec, vectn, vectu, vectv, nbordr,&
                  kwork, sommw, vwork, tdisp, tspaq,&
                  i, nomcri, nomfor, fordef, fatsoc,&
                  jvectr)
    implicit      none
#include "jeveux.h"
#include "asterfort/anacri.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, kwork
    integer :: sommw, tdisp, tspaq, i, jvectr
    real(kind=8) :: vectn(3*nbvec), vectu(3*nbvec), vectv(3*nbvec)
    real(kind=8) :: vwork(tdisp), fatsoc
    logical :: fordef
    character(len=16) :: nomcri, nomfor
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
! person_in_charge: van-xuan.tran at edf.fr
! ---------------------------------------------------------------------
! BUT: CONSTRUIRE LES COMPOSANTES u ET v DU VECTEUR DE CISAILLEMENT TAU
!      DANS LE REPERE LOCAL PERPENDICULAIRE AU VECTEUR NORMAL, POUR
!      TOUS LES VECTEURS NORMAUX A TOUS LES NUMEROS D'ORDRE.
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
!  VECTRA   OUT R  : VECTEUR DE TRAVAIL CONTENANT
!                    LES COMPOSANTES u ET v DU VECTEUR TAU
!                    (CONTRAINTE DE CISAILLEMENT) OU
!                    GAMMA (DEFORMATION DE CISAILLEMENT), POUR TOUS LES
!                    NUMEROS D'ORDRE DE CHAQUE VECTEUR NORMAL.
!
! REMARQUE : CETTE ROUTINE SERT POUR LE TRAITEMENT DES POINTS DE GAUSS
!            ET DES NOEUDS.
! ----------------------------------------------------------------------
    integer :: ivect, iordr, n, decal, adrs, decpro, paract(35)
    logical :: lbid, crsigm, crepst, crepse, crepsp
    character(len=16) :: typcha
    real(kind=8) :: nx, ny, nz, ux, uy, uz, vx, vy, vz
    real(kind=8) :: cmpxx, cmpyy, cmpzz, cmpxy, cmpxz, cmpyz
    real(kind=8) :: fx, fy, fz
    real(kind=8) :: norm, cisx, cisy, cisz
    real(kind=8) :: cucis, cvcis
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
    typcha = 'NON_PERIODIQUE'
!
    n = 0
    decpro = 0
!    DECPRO POUR IDENTIFIER L'AXE A PRPJECTER
!---    ANALYSER LE CRITERE
!  INITIALISER
    crsigm = .false.
    crepst = .false.
    crepse = .false.
    crepsp = .false.
!
    call anacri(nomcri, nomfor, typcha, 'NON', paract,&
                lbid, crsigm, crepst, crepse, crepsp)
!
!
! TRAITEMENT DES PAQUETS DE NOEUDS.
!
    write(6,*) 'propla', nomcri,  fordef
    if (( nomcri(1:16) .eq. 'FATESOCI_MODI_AV' ) .or. fordef) then
        decpro = 6
        goto 50
    endif
!
    if (crsigm) then
        decpro = 0
    else
        if (crepst) then
            decpro = 6
        else
            if (crepsp) then
                decpro = 12
            endif
        endif
    endif
!
50  continue
!
    decal = 18
!
    do 10 ivect = 1, nbvec
        nx = vectn((ivect-1)*3 + 1)
        ny = vectn((ivect-1)*3 + 2)
        nz = vectn((ivect-1)*3 + 3)
!
        ux = vectu((ivect-1)*3 + 1)
        uy = vectu((ivect-1)*3 + 2)
        uz = vectu((ivect-1)*3 + 3)
!
        vx = vectv((ivect-1)*3 + 1)
        vy = vectv((ivect-1)*3 + 2)
        vz = vectv((ivect-1)*3 + 3)
!
        do 20 iordr = 1, nbordr
!
! ON PROJETTE LA DEFORMATION SI FORDEF = OUI
!             IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR.
!      &         FORDEF ) THEN
!                ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL
!      &                             + (I-1)*DECAL + 6
!             ELSE
!                ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL
!      &                             + (I-1)*DECAL
!            ENDIF
!
            adrs = (iordr-1)*tspaq + kwork*sommw*decal + (i-1)*decal + decpro
!
            cmpxx = vwork(adrs + 1)
            cmpyy = vwork(adrs + 2)
            cmpzz = vwork(adrs + 3)
            cmpxy = vwork(adrs + 4)
            cmpxz = vwork(adrs + 5)
            cmpyz = vwork(adrs + 6)
!
! CALCUL DE vect_F = [CMP].vect_n  AVEC [CMP] = [EPS] OU [SIG]
            fx = cmpxx*nx + cmpxy*ny + cmpxz*nz
            fy = cmpxy*nx + cmpyy*ny + cmpyz*nz
            fz = cmpxz*nx + cmpyz*ny + cmpzz*nz
!
! CALCUL DE NORM = vect_F.vect_n
            norm = fx*nx + fy*ny + fz*nz
!
! CALCUL DE vect_CIS = vect_F - NORM vect_n
! vect_CIS = VECTEUR CISAILLEMENT EN CONTRAINTE OU EN DEFORMATION
            cisx = fx - norm*nx
            cisy = fy - norm*ny
            cisz = fz - norm*nz
!
! PROJECTION DU vect_CIS SUR LES VECTEURS u ET v DU REPERE LOCAL
            cucis = ux*cisx + uy*cisy + uz*cisz
            cvcis = vx*cisx + vy*cisy + vz*cisz
!
            n = n + 1
            zr(jvectr+n*2 - 1) = fatsoc*cucis
            zr(jvectr+n*2) = fatsoc*cvcis
!
20      continue
10  end do
!
    call jedema()
!
end subroutine
