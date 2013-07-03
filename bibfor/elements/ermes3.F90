subroutine ermes3(noe, ifa, tymvol, nnof, typmav,&
                  iref1, ivois, isig, nbcmp, dsg11,&
                  dsg22, dsg33, dsg12, dsg13, dsg23)
!-----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
! =====================================================================
!  ERREUR EN MECANIQUE - TERME DE SAUT - DIMENSION 3
!  **        **                   *                *
! =====================================================================
!
!     BUT:
!         DEUXIEME TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
!         CALCUL DU SAUT DE CONTRAINTE ENTRE UNE FACE D'UN ELEMENT
!         ET SON VOISIN EN 3D.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOE    : LISTE DES NUMEROS DES NOEUDS PAR FACE (VOIR TE0003)
! IN   IFA    : NUMERO LOCAL DE LA FACE
! IN   TYMVOL : NUMERO DU TYPE DE LA MAILLE VOLUMIQUE COURANTE
!               1 : HEXAEDRE; 2 : PENTAEDRE; 3 : TETRAEDRE; 4 : PYRAMIDE
! IN   NNOF   : NOMBRE DE NOEUDS DE LA FACE
! IN   TYPMAV : TYPE DE LA MAILLE VOISINE :
!              'HEXA....', 'TETR....', 'PENT....', 'PYRA....'
! IN   ISIG   : ADRESSE DANS ZR DU TABLEAU DES CONTRAINTES AUX NOEUDS
! IN   NBCMP  : NOMBRE DE COMPOSANTES
!
!      SORTIE :
!-------------
! OUT  DSG11  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 11
! OUT  DSG22  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 22
! OUT  DSG33  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 33
! OUT  DSG12  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 12
! OUT  DSG13  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 13
! OUT  DSG23  : SAUT DE CONTRAINTE AUX NOEUDS - COMPOSANTE 23
!
! ......................................................................
!
! CORPS DU PROGRAMME
    implicit none
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/indiis.h"
    integer :: noe(9, 6, 4), ifa, tymvol, nnof
    integer :: iref1, ivois, isig, nbcmp
    real(kind=8) :: dsg11(9), dsg22(9), dsg33(9), dsg12(9), dsg13(9), dsg23(9)
    character(len=8) :: typmav
!
!
!
!
! DECLARATION VARIABLES LOCALES
    integer :: iarepe, jceld, jcelv, iconx1, iconx2, jad, jadv, imav, igrel, iel
    integer :: adiel, iaval, ino, ncher, inov, in
    integer :: nbnovo
    integer :: iaux, jaux
!
    real(kind=8) :: sig11(9), sig22(9), sig33(9), sig12(9), sig13(9), sig23(9)
    real(kind=8) :: sigv11(9), sigv22(9), sigv33(9)
    real(kind=8) :: sigv12(9), sigv13(9), sigv23(9)
!
! ----------------------------------------------------------------------
! ----- NOMBRE DE NOEUDS DE LA MAILLE VOISINE --------------------------
!
    if (typmav .eq. 'HEXA8') then
        nbnovo=8
    else if (typmav.eq.'HEXA20') then
        nbnovo=20
    else if (typmav.eq.'HEXA27') then
        nbnovo=27
    else if (typmav.eq.'PENTA6') then
        nbnovo=6
    else if (typmav.eq.'PENTA15') then
        nbnovo=15
    else if (typmav.eq.'PENTA18') then
        nbnovo=18
    else if (typmav.eq.'TETRA4') then
        nbnovo=4
    else if (typmav.eq.'TETRA10') then
        nbnovo=10
    else if (typmav.eq.'PYRAM5') then
        nbnovo=5
    else if (typmav.eq.'PYRAM13') then
        nbnovo=13
    else
        call assert(.false.)
    endif
!
! ----- RECHERCHE DES ADRESSES POUR OBTENIR SIGMA SUR LES VOISINS ------
!
    iarepe = zi(iref1-1+1)
    jceld = zi(iref1-1+2)
    jcelv = zi(iref1-1+3)
!
    imav = zi(ivois+ifa)
    igrel = zi(iarepe-1+2*(imav-1)+1)
    iel = zi(iarepe-1+2*(imav-1)+2)
!
! --- ON VERIFIE QU'IL N'Y A PAS DE SOUS POINT -------------------------
!
    iaux=jceld-1+zi(jceld-1+4+igrel)+4+4*(iel-1)+1
    jaux=zi(iaux)
    call assert((jaux.eq.1) .or. (jaux.eq.0))
!
! --- FORMULES MAGIQUES DONNANT LE BON DECALAGE POUR NAVIGUER DANS .CELV
!
    adiel=zi(iaux+3)
    iaval=jcelv-1+adiel
!
! ----- CALCUL DE LA NUMEROTATION DU VOISIN ----------------------------
!
    iconx1=zi(iref1+10)
    iconx2=zi(iref1+11)
    jad=iconx1-1+zi(iconx2+zi(ivois)-1)
    jadv=iconx1-1+zi(iconx2+zi(ivois+ifa)-1)
!
! ----- BOUCLE SUR LES NOEUDS DE LA FACE ----------------------
!
    do 10 , in = 1 , nnof
!
    ino=noe(in,ifa,tymvol)
!
! ----- RECUPERATION DE SIGMA SUR LA MAILLE COURANTE ------
!
    iaux = isig-1+nbcmp*(ino-1)+1
    sig11(in) = zr(iaux)
    sig22(in) = zr(iaux+1)
    sig33(in) = zr(iaux+2)
    sig12(in) = zr(iaux+3)
    sig13(in) = zr(iaux+4)
    sig23(in) = zr(iaux+5)
!
! ----- RECUPERATION DE SIGMA SUR LE VOISIN ------
!
    ncher=zi(jad-1+ino)
    inov=indiis(zi(jadv),ncher,1,nbnovo)
!       ON VERIFIE QUE L'ON TROUVE BIEN UN NOEUD DANS LA LISTE
    call assert(inov.gt.0)
    iaux = iaval-1+nbcmp*(inov-1)+1
    sigv11(in) = zr(iaux)
    sigv22(in) = zr(iaux+1)
    sigv33(in) = zr(iaux+2)
    sigv12(in) = zr(iaux+3)
    sigv13(in) = zr(iaux+4)
    sigv23(in) = zr(iaux+5)
!
! ----- CALCUL DES SAUTS DE CONTRAINTES --------------------------------
!
    dsg11(in)=sig11(in)-sigv11(in)
    dsg22(in)=sig22(in)-sigv22(in)
    dsg33(in)=sig33(in)-sigv33(in)
    dsg12(in)=sig12(in)-sigv12(in)
    dsg13(in)=sig13(in)-sigv13(in)
    dsg23(in)=sig23(in)-sigv23(in)
!
    10 end do
!
    if (nnof .eq. 1789) then
        write(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',tymvol
        write(6,1001)
        1000 format(i3,6x,(6(1x,1pe12.5)))
        1001 format(11x,'SIXX         SIYY         SIZZ         SIXY',&
     &           '         SIXZ         SIYZ')
        do 110 , in = 1 , nnof
        ino=noe(in,ifa,tymvol)
        ncher=zi(jad-1+ino)
        write(6,1000) ncher,sig11(in),sig22(in),sig33(in),&
            sig12(in),sig13(in),sig23(in)
        110     end do
        write(6,*) 'TYPE MAILLE VOISINE :',typmav
        do 120 , in = 1 , nnof
        ino=noe(in,ifa,tymvol)
        ncher=zi(jad-1+ino)
        write(6,1000) ncher,sigv11(in),sigv22(in),sigv33(in),&
            sigv12(in),sigv13(in),sigv23(in)
        120     end do
    endif
!
end subroutine
