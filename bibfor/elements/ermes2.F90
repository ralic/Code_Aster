subroutine ermes2(ino, typema, typmav, iref1, ivois,&
                  isig, nbcmp, dsg11, dsg22, dsg12)
    implicit none
    include 'jeveux.h'
    include 'asterfort/indiis.h'
    integer :: ino, iref1, ivois, isig, nbcmp
    real(kind=8) :: dsg11(3), dsg22(3), dsg12(3)
    character(len=8) :: typema, typmav
! ----------------------------------------------------------------------
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
!  ERREUR EN MECANIQUE - TERME DE SAUT - DIMENSION 2
!  **        **                   *                *
! ======================================================================
!
!     BUT:
!         DEUXIEME TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
!         CALCUL DU SAUT DE CONTRAINTE ENTRE UN ELEMENT ET SON VOISIN.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   INO      : NUMERO DE L'ARETE
! IN   TYPEMA   : TYPE DE LA MAILLE COURANTE
!               'QU4', 'QU8', 'QU9'
!               'TR3', 'TR6', 'TR7'
! IN   TYPMAV   : TYPE DE LA MAILLE VOISINE
!               'QUAD4', 'QUAD8', 'QUAD9'
!               'TRIA3', 'TRIA6', 'TRIA7'
! IN   IREF1    : ADRESSE DES CHARGEMENTS DE TYPE FORCE (CONTENANT AUSSI
!                 LES INFOS SUR LES VOISINS)
! IN   IVOIS    : ADRESSE DES VOISINS
! IN   ISIG     : ADRESSE DES CONTRAINTES AUX NOEUDS
! IN   NBCMP    : NOMBRE DE COMPOSANTES DU VECTEUR CONTRAINTE PAR NOEUD
!
!      SORTIE :
!-------------
! OUT  DSG11  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 11
! OUT  DSG22  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 22
! OUT  DSG12  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 12
!
! ......................................................................
!
!
!
!
    integer :: iarepe, jceld, jcelv, imav, igrel, iel, iaval, iconx1, iconx2
    integer :: jad, jadv, ncher
    integer :: nbs, nbna, nbsv, nbnv, i, jno, mno, inov, jnov, mnov
!
    real(kind=8) :: sig11(3), sig22(3), sig12(3), sigv11(3), sigv22(3)
    real(kind=8) :: sigv12(3)
!
    character(len=2) :: form, noeu, formv, noeuv
!
! ----------------------------------------------------------------------
!
!              X1          X2          X3
!               o-----------o-----------o
!              INO         MNO         JNO
!
!         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
!                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
!                 3 --> MNO NOEUD MILIEU S'IL EXISTE
!
! ----- RECHERCHE DES ADRESSES POUR OBTENIR SIGMA SUR LES VOISINS ------
!
    iarepe=zi(iref1)
    jceld=zi(iref1+1)
    jcelv=zi(iref1+2)
    imav=zi(ivois+ino)
    igrel=zi(iarepe+2*(imav-1))
    iel=zi(iarepe+2*(imav-1)+1)
    iaval=jcelv-1+zi(jceld-1+zi(jceld-1+4+igrel)+8)
!
! ----- TESTS SUR LA MAILLE COURANTE -----------------------------------
!
    form=typema(1:2)
    noeu=typema(3:3)
!
    if (form .eq. 'TR') then
        nbs=3
    else
        nbs=4
    endif
    if (noeu .eq. '3' .or. noeu .eq. '4') then
        nbna=2
    else
        nbna=3
    endif
!
! ----- TESTS SUR LA MAILLE VOISINE ------------------------------------
!
    formv=typmav(1:2)
    noeuv=typmav(5:5)
!
    if (formv .eq. 'TR') then
        nbsv=3
        if (noeuv .eq. '3') then
            nbnv=3
        else if (noeuv.eq.'6') then
            nbnv=6
        else
            nbnv=7
        endif
    else if (formv.eq.'QU') then
        nbsv=4
        if (noeuv .eq. '4') then
            nbnv=4
        else if (noeuv.eq.'8') then
            nbnv=8
        else
            nbnv=9
        endif
    endif
!
! ----- CALCUL DE LA NUMEROTATION DU VOISIN ----------------------------
!
    if (ino .eq. nbs) then
        jno=1
    else
        jno=ino+1
    endif
!
    iconx1=zi(iref1+10)
    iconx2=zi(iref1+11)
    jad=iconx1-1+zi(iconx2+zi(ivois)-1)
    jadv=iconx1-1+zi(iconx2+zi(ivois+ino)-1)
    ncher=zi(jad-1+ino)
    inov=indiis(zi(jadv),ncher,1,nbnv)
    ncher=zi(jad-1+jno)
    jnov=indiis(zi(jadv),ncher,1,nbnv)
!
! ----- RECUPERATION DE SIGMA SUR LA MAILLE COURANTE ET LE VOISIN ------
!
    sig11(1)=zr(isig-1+nbcmp*(ino-1)+1)
    sig22(1)=zr(isig-1+nbcmp*(ino-1)+2)
    sig12(1)=zr(isig-1+nbcmp*(ino-1)+4)
!
    sigv11(1)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(inov-1)+1)
    sigv22(1)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(inov-1)+2)
    sigv12(1)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(inov-1)+4)
!
    sig11(2)=zr(isig-1+nbcmp*(jno-1)+1)
    sig22(2)=zr(isig-1+nbcmp*(jno-1)+2)
    sig12(2)=zr(isig-1+nbcmp*(jno-1)+4)
!
    sigv11(2)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(jnov-1)+1)
    sigv22(2)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(jnov-1)+2)
    sigv12(2)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(jnov-1)+4)
!
    if (nbna .eq. 3) then
        mno=nbs+ino
        mnov=nbsv+jnov
!
        sig11(3)=zr(isig-1+nbcmp*(mno-1)+1)
        sig22(3)=zr(isig-1+nbcmp*(mno-1)+2)
        sig12(3)=zr(isig-1+nbcmp*(mno-1)+4)
!
        sigv11(3)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(mnov-1)+1)
        sigv22(3)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(mnov-1)+2)
        sigv12(3)=zr(iaval-1+nbcmp*nbnv*(iel-1)+nbcmp*(mnov-1)+4)
    endif
!
! ----- CALCUL DES SAUTS DE CONTRAINTES --------------------------------
!
    do 10 i = 1, nbna
        dsg11(i)=sig11(i)-sigv11(i)
        dsg22(i)=sig22(i)-sigv22(i)
        dsg12(i)=sig12(i)-sigv12(i)
10  end do
!
end subroutine
