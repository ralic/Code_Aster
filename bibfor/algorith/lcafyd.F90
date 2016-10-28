subroutine lcafyd(comp, materd, materf, nbcomm, cpmono,&
                  nmat, mod, nvi, vind, vinf,&
                  sigd, nr, yd, bnews, mtrac)
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     CHOIX DES VALEURS DE VIND A AFFECTER A YD
!     CAS PARTICULIER DU  MONOCRISTAL  :
!     ON GARDE 1 VARIABLE INTERNE PAR SYSTEME DE GLISSEMENT SUR 3
!     ----------------------------------------------------------------
!     IN
!          COMP   :  NOM MODELE DE COMPORTEMENT
!          MATERD :  COEF MATERIAU A T
!          MATERF :  COEF MATERIAU A T+DT
!          NBCOMM :  INDICES DES COEF MATERIAU
!          NMAT   :  DIMENSION MATER
!          COMP   :  TYPE DE MODELISATION
!          NVI    :  NOMBRE DE VARIABLES INTERNES
!          VIND   :  VARIABLES INTERNES A T
!          VINF   :  VARIABLES INTERNES A T+DT (BASE SUR PRED_ELAS)
!          NR     :  DIMENSION VECTEUR INCOONUES
!          SIGD   :  ETAT DE CONTRAINTES A T
!     OUT  YD     :  VECTEUR INITIAL
!
! --- SPECIFIQUE HUJEUX
!         BNEWS   :  GESTION MECANISMES TRACTION POUR HUJEUX
!         MTRAC   :  GESTION MECANISMES TRACTION POUR HUJEUX (BIS)
!     ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/hujayd.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcgrla.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsv.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: ndt, nvi, nmat, ndi, ns, i, nbcomm(nmat, 3), nr
    real(kind=8) :: yd(*), materd(nmat, 2), materf(nmat, 2), vind(*)
    real(kind=8) :: id(3, 3), hookf(6, 6), dkooh(6, 6), epsegl(6), fe(3, 3)
    real(kind=8) :: eisp, epsfi(6), dtot, vinf(nvi), sigd(6)
    character(len=16) :: loi, comp(*), necoul
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    aster_logical :: bnews(3), mtrac
    common /tdim/   ndt  , ndi
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!     ----------------------------------------------------------------
!
!     INITIALISATION DE YD EN IMPLICITE
    loi=comp(1)
!
!
!     AFFECTATION DE YD = ( SIGD , VIND , (EPSD(3)) )
!
    call lceqvn(ndt, sigd, yd)
!
    if (loi(1:8) .eq. 'MONOCRIS') then
! ATTENTION !         NS=(NVI-8)/3
        ns=nr-ndt
        irr=0
        decirr=0
        if (materf(nbcomm(1,1),2) .ge. 4) then
!           KOCKS-RAUCH ET DD_CFC : VARIABLE PRINCIPALE=DENSITE DISLOC
!           UNE SEULE FAMILLE
            ASSERT(nbcomm(nmat, 2).eq.1)
            do 102 i = 1, ns
                yd(ndt+i)=vind(6+3*(i-1)+1)
102         continue
            necoul=cpmono(3)
            if (necoul .eq. 'MONO_DD_CC_IRRA') then
                irr=1
                decirr=6+3*ns
            endif
            if (necoul .eq. 'MONO_DD_CFC_IRRA') then
                irr=1
                decirr=6+3*ns
            endif
        else
!           AUTRES COMPORTEMENTS MONOCRISTALLINS
            do 103 i = 1, ns
                yd(ndt+i)=vind(6+3*(i-1)+2)
103         continue
        endif
!
!
        if (gdef .eq. 1) then
! les 9 variables internes  de 6+3*ns+1 à 6+3*ns+9
! REPRESENTENT FE - ID
            call dcopy(9, vind(nvi-3-18+10), 1, fe, 1)
            call daxpy(9, +1.d0, id, 1, fe,&
                       1)
            call lcgrla(fe, epsegl)
            if (materf(nmat,2) .eq. 0) then
                call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
            else if (materf(nmat,2).eq.1) then
                call lcopli('ORTHOTRO', mod, materf(1, 1), hookf)
            endif
! Y contient H*(FeT.Fe-Id)/2, ce ne sont pas exactement les PK2
! Y contient ensuite les ns alpha_s ou gamma_s suivant la loi
            call lcprmv(hookf, epsegl, yd)
        endif
!
!
!
    else if (loi(1:7) .eq. 'IRRAD3M') then
!        CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
!        DU SYSTEME DIFFERENTIEL
!        DEFORMATION PLASTIQUE CUMULEE
        yd(ndt+1) = vind(1)
!        FONCTION SEUIL DE FLUAGE
        yd(ndt+2) = vind(2)
!        DEFORMATION EQUIVALENTE DE FLUAGE
        yd(ndt+3) = vind(3)
!        DEFORMATION DE GONFLEMENT
        yd(ndt+4) = vind(4)
    else if (loi(1:12) .eq. 'BETON_BURGER') then
! ===    ============================================================
!        CONSTRUCTION DES DEFORMATIONS IRREVERSIBLES DE FLUAGE PROPRE
! ===    ============================================================
! ---    RECUPERATION PARTIE SPHERIQUE
! ===    ============================================================
        eisp = vind(2)
! ===    ============================================================
! ---    RECUPERATION PARTIE DEVIATOIRE
! ===    ============================================================
        epsfi(1) = vind(4)
        epsfi(2) = vind(6)
        epsfi(3) = vind(8)
        epsfi(4) = vind(13)
        epsfi(5) = vind(15)
        epsfi(6) = vind(17)
! ===    ============================================================
! ---    ASSEMBLAGE PARTIE DEVIATOIRE ET SPHERIQUE
! ===    ============================================================
        do 200 i = 1, ndi
            epsfi(i)=epsfi(i)+eisp
200     continue
! ===    ============================================================
! ---    AFFECTATION DES VALEURS AU VECTEUR YD(NDT+I)
! ===    ============================================================
        do 210 i = 1, ndt
            yd(ndt+i) = epsfi(i)
210     continue
!
    else if (loi(1:4) .eq. 'LETK') then
! --- INITIALISATION A ZERO DU MULTIPLICATEUR PLASTIQUE
        yd(ndt+1) = 0.d0
! --- INITIALISATION A XIP
        yd(ndt+2) = vind(1)
! --- INITIALISATION A XIVP
        yd(ndt+3) = vind(3)
!
    else if (loi .eq. 'HAYHURST') then
        call lcopil('ISOTROPE', mod, materd(1, 1), dkooh)
!        DEFORMATION ELASTIQUE INSTANT PRECEDENT
        call lcprmv(dkooh, sigd, yd)
        dtot=1.d0/(1.d0-vind(11))
        call lcprsv(dtot, yd, yd)
!
!        CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
!        DU SYSTEME DIFFERENTIEL
!        DEFORMATION PLASTIQUE CUMULEE
        yd(ndt+1) = vind(7)
!        H1
        yd(ndt+2) = vind(8)
!        H2
        yd(ndt+3) = vind(9)
!        D
        yd(ndt+4) = vind(11)
!
    else if (loi(1:6).eq.'HUJEUX') then
        call hujayd(nmat, materf, nvi, vind, vinf,&
                    nr, yd, bnews, mtrac)
!
    else
!     CAS GENERAL :
!        TOUTES LES VARIABLES INTERNES SONT RECOPIES
!        LA DERNIERE C'EST TOUJOURS L'INDICATEUR PLASTIQUE
        call lceqvn(nvi-1, vind, yd(ndt+1))
    endif
!
end subroutine
