subroutine tstvec(perm, iad, nlong, type, sommi,&
                  sommr, nbign)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_18 CRS_508
    implicit none
!
! BUT : RECUPERER 2 NOMBRES RESUMANT UN VECTEUR JEVEUX
!
! IN: PERM  K3 : /OUI/NON
!           NON : ON FAIT LA SOMME BETE DES ELEMENTS DU VECTEUR
!                 => UNE PERMUTATION DU VECTEUR NE SE VOIT PAS !
!           OUI : ON FAIT UNE "SOMME" QUI DONNE UN RESULTAT
!                 DEPENDANT UN PEU DE L'ORDRE DES ELEMENTS DU VECTEUR
! IN: IAD   I  : ADRESSE DU VECTEUR
! IN: NLONG  I  : LONGUEUR DU VECTEUR
! IN: TYPE  K3 : TYPE DES ELEMENTS DU VECTEUR :
!                   I/L/R/C/K8/K16/K24/K32/K80
!
! OUT: SOMMI   I      : SOMME(V(I)) QUELQUE SOIT LE TYPE DE V
! OUT: SOMMR   R      : SOMME(V(I)) SI V EST DE TYPE "R/C"
! OUT: NBIGN   I      : NOMBRE DE VALEURS IGNOREES DANS SOMMR :
!                       (UNDEF OU TRES GRAND )
!
!
    include 'jeveux.h'
!
    include 'asterc/iisnan.h'
    include 'asterfort/tstk2i.h'
    character(len=3) :: type
    real(kind=8) :: sommr
    integer :: sommi, nbign
    character(len=24) :: k24
    logical :: l
    character(len=*) :: perm
    character(len=8) :: k8
    real(kind=8) :: x
    integer :: iad, ix, c1, nlong, ico, k
    integer(kind=8) :: sommi2, i8
!
    equivalence (x,ix)
    equivalence (l,ix)
    equivalence (k8,ix)
!
!
    if (perm .eq. 'NON') then
        c1=0
    else
        c1=1
    endif
!
!
!     -- CALCUL DE SOMMR :
!     --------------------
    sommr = 0.d0
    ico=0
    nbign=0
    if (type .eq. 'R') then
        do 10,k = 1,nlong
        x = zr(iad-1+k)
        if (iisnan(x) .eq. 0) then
            if (abs(x) .lt. 1.d300) then
                ico=ico+1
                sommr = sommr + (c1*mod(k,3)+1)*x
            endif
        endif
10      continue
        nbign=nlong-ico
    endif
    if (type .eq. 'C') then
        do 20,k = 1,nlong
        x = dble(zc(iad-1+k))
        if (iisnan(x) .eq. 0) then
            if (abs(x) .lt. 1.d300) then
                ico=ico+1
                sommr = sommr + (c1*mod(k,3)+1)*x
            endif
        endif
        x = dimag(zc(iad-1+k))
        if (iisnan(x) .eq. 0) then
            if (abs(x) .lt. 1.d300) then
                ico=ico+1
                sommr = sommr + (c1*mod(k,3)+1)*x
            endif
        endif
20      continue
        nbign=2*nlong-ico
    endif
!
!
!     -- CALCUL DE SOMMI :
!     --------------------
    sommi2 = 0
    if (type .eq. 'I') then
        do 30,k = 1,nlong
        i8= zi(iad-1+k)
        sommi2 = sommi2 + (c1*mod(k,3)+1)*i8
30      continue
    else if (type.eq.'S') then
        do 31,k = 1,nlong
        i8= zi4(iad-1+k)
        sommi2 = sommi2 + (c1*mod(k,3)+1)*i8
31      continue
    else if (type.eq.'L') then
        do 40,k = 1,nlong
        l = zl(iad-1+k)
        if (l) sommi2 = sommi2 + (c1*mod(k,3)+1)
40      continue
    else if (type.eq.'R') then
        sommi2=0
    else if (type.eq.'C') then
        sommi2=0
    else if (type.eq.'K8') then
        do 70,k = 1,nlong
        sommi2 = sommi2 + (c1*mod(k,3)+1)*tstk2i(8,zk8(iad-1+k))
70      continue
    else if (type.eq.'K16') then
        do 80,k = 1,nlong
        sommi2 = sommi2 + (c1*mod(k,3)+1)*tstk2i(16,zk16(iad-1+k))
80      continue
    else if (type.eq.'K24') then
        do 90,k = 1,nlong
        sommi2 = sommi2 + (c1*mod(k,3)+1)*tstk2i(24,zk24(iad-1+k))
90      continue
    else if (type.eq.'K32') then
        do 100,k = 1,nlong
        sommi2 = sommi2 + (c1*mod(k,3)+1)*tstk2i(32,zk32(iad-1+k))
100      continue
    else if (type.eq.'K80') then
        do 110,k = 1,nlong
        sommi2 = sommi2 + (c1*mod(k,3)+1)*tstk2i(80,zk80(iad-1+k))
110      continue
    endif
!
!     -- ON TRONQUE SOMMI2 (9 DERNIERS CHIFFRES) POUR AVOIR
!        LE MEME RESULTAT SUR LES PLATEFORMES I4 ET I8 :
    write(k24,'(I24)') sommi2
    read(k24(16:24),'(I9)') sommi
!
!
end subroutine
