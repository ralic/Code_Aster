subroutine mdrfis(nbmode, depgen, fexgen, nbnli, nbrfis,&
                  dplmod, fk, dfk, parcho, angini,&
                  vrotat, foncp, temps)
    implicit none
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
! ======================================================================
!
! CALCUL DU SECOND MEMBRE POUR UNE NON-LINEARITE DE TYPE ROTOR FISSURE
! ----------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DEPGEN : DEPLACEMENTS GENERALISES AU PAS COURANT
! VAR : FEXGEN : FORCES GENERALISEES AU PAS COURANT
! IN  : NBRFIS : NOMBRE DE FISSURES
! IN  : DPLMOD : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DES FISSURES
! IN  : FK ET DFK : FONCTIONS DE RAIDEUR DES FISSURES (K ET DK/DPHI)
! IN  : ANGINI : ANGLE INITIAL ENTRE FISSURE ET AXE
! IN  : VROTAT : VITESSE DE ROTATION AXIALE
! ----------------------------------------------------------------------
    include 'asterc/getvtx.h'
    include 'asterc/r8depi.h'
    include 'asterfort/fointe.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/locglo.h'
    include 'asterfort/togene.h'
    include 'asterfort/tophys.h'
    integer :: nbmode, nbnli, nbrfis
    real(kind=8) :: depgen(*), fexgen(*), temps, angini, vrotat
    real(kind=8) :: dplmod(nbnli, nbmode, *), parcho(nbnli, *)
    character(len=8) :: fk(2), dfk(2), foncp, k8b
!
    integer :: ierd, i, ier, iarg, n1
    real(kind=8) :: angrot, phi, r1(3), r2(3)
    real(kind=8) :: fkphi, dfkphi
    real(kind=8) :: orig(3), sina, cosa, sinb, cosb, sing, cosg
    real(kind=8) :: drl(3), drg(3), ml(3), mg(3)
!     --- BOUCLE SUR LES NOEUDS DE NON-LINEARITE ---
!
    call getvtx(' ', 'VITESSE_VARIABLE', 1, iarg, 1,&
                k8b, n1)
    if (k8b .eq. 'OUI') then
        call fointe('F ', foncp, 1, 'INST', temps,&
                    angrot, ier)
    else
        angrot = angini + vrotat * temps
    endif
!
    orig(1)=0.d0
    orig(2)=0.d0
    orig(3)=0.d0
!
    sing=sin(angrot)
    cosg=cos(angrot)
!
!
    do 10 i = nbnli+1-nbrfis, nbnli
!
        sina = parcho(i,17)
        cosa = parcho(i,18)
        sinb = parcho(i,19)
        cosb = parcho(i,20)
!
        call tophys(i, 0, dplmod, nbnli, nbmode,&
                    depgen, r1(1), r1(2), r1(3))
        call tophys(i, 3, dplmod, nbnli, nbmode,&
                    depgen, r2(1), r2(2), r2(3))
!
        drg(1) = r2(1) - r1(1)
        drg(2) = r2(2) - r1(2)
        drg(3) = r2(3) - r1(3)
!
!
        call gloloc(drg, orig, sina, cosa, sinb,&
                    cosb, sing, cosg, drl)
!
!
        phi=atan2(drl(2),drl(3))
        if (phi .lt. 0.d0) phi = r8depi() + phi
!
        call fointe('F', fk(1), 1, 'ABSC', phi,&
                    fkphi, ierd)
        call fointe('F', dfk(1), 1, 'ABSC', phi,&
                    dfkphi, ierd)
!
        ml(1)=0.d0
        ml(2) = fkphi * drl(2) + 0.5d0 * dfkphi * drl(3)
        ml(3) = fkphi * drl(3) - 0.5d0 * dfkphi * drl(2)
!
!
        call locglo(ml, sina, cosa, sinb, cosb,&
                    sing, cosg, mg)
!
        call togene(i, 0, dplmod, nbnli, nbmode,&
                    mg(1), mg(2), mg(3), fexgen)
        call togene(i, 3, dplmod, nbnli, nbmode,&
                    -mg(1), -mg(2), -mg(3), fexgen)
!
!
10  end do
!
end subroutine
