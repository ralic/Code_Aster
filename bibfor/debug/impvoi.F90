subroutine impvoi(texte, nbma, iaddvo, iadvoi)
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
    implicit none
#include "jeveux.h"
    character(len=*) :: texte
    integer :: nbma, iaddvo, iadvoi
    integer :: ima, iv, is
!-----------FONCTIONS  D ACCES A VGE -----------------------------------
    integer :: zzadvo, zznbvo, zzadve, zzmavo, zztyvo, zznbno, zznbsc, zzloc1
    integer :: zzloc2
!     IADDVO : ADRESSE JEVEUX DU TABLEAU DE POINTEURS DANS LA SD EL_VOIS
!     IADVOI : ADRESSE JEVEUX DE LA SD EL_VOIS
!
!     DES DONNEES DES VOISINS DE LA MAILLE IMA (0 SI MAILLE PAS ACTIVE)
    zzadvo(ima)=zi(iaddvo+ima-1)+iadvoi
!
!     NOMBBRE DE VOISINS DE IMA EME MAILLE
    zznbvo(ima)=zi(zzadvo(ima)-1+1)
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!     ADRESSE DES DONNEES
! FAUX ???      ZZADVE(IMA,IV) = ZI(ZZADVO(IMA)-1+1+IV)+IADVOI-1
    zzadve(ima,iv)=zi(zzadvo(ima)-1+1+iv)+zzadvo(ima)-1
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!     TYPE DE VOISINAGE :
!        3D PAR FACE    : F3 : 1
!        2D PAR FACE    : F2 : 2
!        3D PAR ARRETE  : A3 : 3
!        2D PAR ARRETE  : A2 : 4
!        1D PAR ARRETE  : A1 : 5
!        3D PAR SOMMET  : S3 : 6
!        2D PAR SOMMET  : S2 : 7
!        1D PAR SOMMET  : S1 : 8
!        0D PAR SOMMET  : S0 : 9
    zztyvo(ima,iv)=zi(zzadve(ima,iv)-1+1)
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!     NUMERO DE MAILLE
    zzmavo(ima,iv)=zi(zzadve(ima,iv)-1+2)
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!     NOMBRE DE NOEUDS DE MAILLE
    zznbno(ima,iv)=zi(zzadve(ima,iv)-1+3)
!
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!        NOMBRE DE SOMMETS COMMUNS
    zznbsc(ima,iv)=zi(zzadve(ima,iv)-1+4)
!
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!     POUR LE SOMMET COMMUN IS
!     NUMERO LOCAL DANS IMA
    zzloc1(ima,iv,is)=zi(zzadve(ima,iv)-1+4+1+2*(is-1))
!
!
!
!     POUR LA MAILLE IMA
!     POUR LE VOISIN IV
!    POUR LE SOMMET COMMUN IS
!     NUMERO LOCAL DANS IV
    zzloc2(ima,iv,is)=zi(zzadve(ima,iv)-1+4+1+2*(is-1)+1)
!-----------FIN FONCTIONS  D ACCES A VGE -------------------------------
!
!
    write (6,*)
    write (6,*)' IMPRESSION OBJET VOISINAGE VGE '
    write (6,*)texte
    write (6,*)
!
    do 30 ima = 1, nbma
        write (6,9000)ima,zznbvo(ima)
        do 20 iv = 1, zznbvo(ima)
            write (6,9010)iv,zztyvo(ima,iv),zzmavo(ima,iv), zznbno(&
            ima,iv),zznbsc(ima,iv)
            do 10 is = 1, zznbsc(ima, iv)
                write (6,9020)is,zzloc1(ima,iv,is),zzloc2(ima,iv,is)
10          continue
20      continue
30  end do
    write (6,*)' FIN IMPRESSION OBJET VOISINAGE VGE '
    write (6,*)
!
    9000 format (' MAILLE ',i8,' NB VOIS ',i2)
    9010 format (' VOISIN ',i2,' TYPE ',i2,' MAILLE ',i8,' NB NOEUDS ',i2,&
     &       ' NB SOMMETS COMMUN ',i2)
    9020 format (' IS ',i2,' NUMLOC ',i2,' NUMLOC DANS VOISIN ',i2)
end subroutine
