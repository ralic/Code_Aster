subroutine taurlo(nbvec, jvectn, jvectu, jvectv, nbordr,&
                  kwork, sompgw, jrwork, tspaq, ipg,&
                  jvecpg)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbvec, jvectn, jvectu, jvectv, nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg, jvecpg
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ---------------------------------------------------------------------
! BUT: CONSTRUIRE LES COMPOSANTES u ET v DU VECTEUR DE CISAILLEMENT TAU
!      DANS LE REPERE LOCAL PERPENDICULAIRE AU VECTEUR NORMAL, POUR
!      TOUS LES VECTEURS NORMAUX A TOUS LES NUMEROS D'ORDRE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!     NBVEC   : IN  : NOMBRE DE VECTEURS NORMAUX.
!     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS u DU PLAN DE CISAILLEMENT.
!     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS v DU PLAN DE CISAILLEMENT.
!     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
!     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
!                               MAILLES ;
!                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                               MAILLES.
!     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT
!                     LA MAILLE COURANTE.
!     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                     DU <<PAQUET>> DE MAILLES.
!     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                     COURANT.
!     IPG     : IN  : IEME POINT DE GAUSS.
!     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     LES COMPOSANTES u ET v DU VECTEUR TAU
!                     (CISAILLEMENT), POUR TOUS LES NUMEROS
!                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
    integer :: ivect, iordr, n, adrs, decal
    real(kind=8) :: nx, ny, nz, ux, uy, uz, vx, vy, vz
    real(kind=8) :: sixx, siyy, sizz, sixy, sixz, siyz, fx, fy, fz
    real(kind=8) :: norm, taux, tauy, tauz, cutau, cvtau
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
    n = 0
!
    do 10 ivect = 1, nbvec
        nx = zr(jvectn + (ivect-1)*3)
        ny = zr(jvectn + (ivect-1)*3 + 1)
        nz = zr(jvectn + (ivect-1)*3 + 2)
!
        ux = zr(jvectu + (ivect-1)*3)
        uy = zr(jvectu + (ivect-1)*3 + 1)
        uz = zr(jvectu + (ivect-1)*3 + 2)
!
        vx = zr(jvectv + (ivect-1)*3)
        vy = zr(jvectv + (ivect-1)*3 + 1)
        vz = zr(jvectv + (ivect-1)*3 + 2)
!
        do 20 iordr = 1, nbordr
            decal = 18
            adrs = (iordr-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
            sixx = zr(jrwork + adrs + 0)
            siyy = zr(jrwork + adrs + 1)
            sizz = zr(jrwork + adrs + 2)
            sixy = zr(jrwork + adrs + 3)
            sixz = zr(jrwork + adrs + 4)
            siyz = zr(jrwork + adrs + 5)
!
! CALCUL DE vect_F = [SIG].vect_n
            fx = sixx*nx + sixy*ny + sixz*nz
            fy = sixy*nx + siyy*ny + siyz*nz
            fz = sixz*nx + siyz*ny + sizz*nz
!
! CALCUL DE NORM = vect_F.vect_n
            norm = fx*nx + fy*ny + fz*nz
!
! CALCUL DE vect_TAU = vect_F - NORM vect_n
            taux = fx - norm*nx
            tauy = fy - norm*ny
            tauz = fz - norm*nz
!
! PROJECTION DU vect_TAU SUR LES VECTEURS u ET v DU REPERE LOCAL
            cutau = ux*taux + uy*tauy + uz*tauz
            cvtau = vx*taux + vy*tauy + vz*tauz
            n = n + 1
            zr( jvecpg + (n-1)*2 ) = cutau
            zr( jvecpg + (n-1)*2 + 1 ) = cvtau
!
20      continue
!
10  end do
!
    call jedema()
end subroutine
