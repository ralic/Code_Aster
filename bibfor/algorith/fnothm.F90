subroutine fnothm(fnoevo, deltat, perman, nno, nnos,&
                  nnom, npi, npg, ipoids, ipoid2,&
                  ivf, ivf2, idfde, idfde2, geom,&
                  congem, b, dfdi, dfdi2, r,&
                  vectu, imate, mecani, press1, press2,&
                  tempe, dimdef, dimcon, nddls, nddlm,&
                  dimuel, nmec, np1, np2, ndim,&
                  axi)
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! aslint: disable=W1504
    implicit     none
#include "asterfort/cabthm.h"
#include "asterfort/fonoda.h"
    logical :: fnoevo, perman, axi
    integer :: nno, nnos, npg, imate, dimdef, dimcon, nddls, nddlm, nnom
    integer :: dimuel, nmec, np1, np2, ndim, ipoids, ipoid2, ivf, ivf2
    integer :: idfde, idfde2, npi, mecani(5), press1(7), press2(7)
    integer :: tempe(5), yamec, yap1, yap2, yate
    integer :: addeme, addep1
    integer :: addep2, addete
    real(kind=8) :: deltat, dfdi(nno, 3), dfdi2(nnos, 3), geom(ndim, nno)
    real(kind=8) :: congem(1:npi*dimcon), poids, poids2
    real(kind=8) :: vectu(dimuel), b(dimdef, dimuel), r(1:dimdef+1)
! ======================================================================
!     BUT:  CALCUL  DE L'OPTION FORC_NODA EN MECANIQUE
!           DES MILIEUX POREUX AVEC COUPLAGE THM
!
!  SI FNOEVO = VRAI
!  C EST QUE L'ON APPELLE DEPUIS STAT NON LINE  :
!  ET ALORS LES TERMES DEPENDANT DE DELTAT SONT EVALUES
!
!  SI  FNOEVO = FAUX
!  C EST QUE L'ON APPELLE DEPUIS CALCNO  :
!  ET ALORS LES TERMES DEPENDANT DE DELTAT NE SONT PAS EVALUES
! ======================================================================
! IN
! ======================================================================
! AXI       AXISYMETRIQUE ?
! TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
! ======================================================================
! OUT
! ======================================================================
! OUT DFDI    : DERIVEE DES FCT FORME
! OUT R       : TABLEAU DES RESIDUS
! OUT VECTU   : FORCES NODALES
! ======================================================================
    integer :: kpi, i, n
    real(kind=8) :: dt
! ======================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
! ======================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    yap1 = press1(1)
    addep1 = press1(3)
    if (perman) then
        i = 1
    else
        i = 0
    endif
    yap2 = press2(1)
    addep2 = press2(3)
    yate = tempe(1)
    addete = tempe(2)
!
    if (perman) then
        dt = 1.d0
    else
        dt = deltat
    endif
! ======================================================================
! --- INITIALISATION DE VECTU ------------------------------------------
! ======================================================================
    do 1 i = 1, dimuel
        vectu(i)=0.d0
 1  end do
! ======================================================================
! --- CALCUL POUR CHAQUE POINT DE GAUSS : BOUCLE SUR KPG ---------------
! ======================================================================
    do 10 kpi = 1, npg
! ======================================================================
! --- INITIALISATION DE R ----------------------------------------------
! ======================================================================
        do 22 i = 1, dimdef+1
            r(i)=0.d0
22      continue
! ======================================================================
! --- CALCUL DE LA MATRICE B AU POINT DE GAUSS -------------------------
! ======================================================================
        call cabthm(nddls, nddlm, nno, nnos, nnom,&
                    dimuel, dimdef, ndim, kpi, ipoids,&
                    ipoid2, ivf, ivf2, idfde, idfde2,&
                    dfdi, dfdi2, geom, poids, poids2,&
                    b, nmec, yamec, addeme, yap1,&
                    addep1, yap2, addep2, yate, addete,&
                    np1, np2, axi)
! ======================================================================
        call fonoda(imate, perman, mecani, press1, press2,&
                    tempe, dimdef, dimcon, ndim, dt,&
                    fnoevo, congem((kpi-1)*dimcon+1), r)
! ======================================================================
! --- CONTRIBUTION DU POINT D'INTEGRATION KPI AU RESIDU ----------------
! ======================================================================
        do 117 i = 1, dimuel
            do 118 n = 1, dimdef
                vectu(i)=vectu(i)+b(n,i)*r(n)*poids
118          continue
117      continue
! ======================================================================
10  end do
! ======================================================================
end subroutine
