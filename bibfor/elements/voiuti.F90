subroutine voiuti(numa, codvoi, nvoima, nscoma, iarepe,&
                  iaddvo, iadvoi, nbvois, livois, tyvois,&
                  nbnovo, nbsoco, lisoco)
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
    implicit none
!   IN  NUMA     : NUMERO DE MAILLE DU MAILLAGE
!       CODVOI   :  CODE DONNANT LES COMBINAISONS
!                   DES VOISINAGES POSSIBLES
!                   3D PAR FACE    : F3 : 1
!                   2D PAR FACE    : F2 : 2
!                   3D PAR ARRETE  : A3 : 3
!                   2D PAR ARRETE  : A2 : 4
!                   1D PAR ARRETE  : A1 : 5
!                   3D PAR SOMMET  : S3 : 6
!                   2D PAR SOMMET  : S2 : 7
!                   1D PAR SOMMET  : S1 : 8
!                   0D PAR SOMMET  : S0 : 9
!       NVOIMA   :  NOMBRE MAX DE VOISINS POSSIBLES
!       NSCOMA   :  NOMBRE MAX DE SOMMETS COMMUNS
!       IAREPE   :  ADRESSE JEVEUX D UN OPJET DE TYPE REPE
!       IADDVO   :  ADRESSE JEVEUX DU POINEUR DE VALEURS DANS VGE
!       IADVOI   :  ADRESSE JEVEUX DES VALEURS DE VGE
!  OUT
!       NBVOIS   :  NOMBRE DE VOISINS
!       LIVOIS   :  LISTE DE CES VOISINS (NUM. DE MAILLES AFFECTEES)
!       TYVOIS   :  TYPE(INTEGER) DES VOISINS
!       NBNOVO   :  NOMBRE DE NOEUDS DE CHACUN DE CES VOISINS
!       NBSOCO   :  POUR CHAQUE VOISINS NOMBRE DE SOMMETS PARTAGES
!       LISOCO   :  LISTE DE CES SOMMETS
!                  (IMA,IS,1) EN NUMEROTATION LOCALE MAILLE NUMA
!                  (IMA,IS,2) EN NUMEROTATION LOCALE MAILLE VOISINE
    include 'jeveux.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesg.h'
    integer :: numa, nvoima, nscoma, iarepe, iaddvo, iadvoi, nbvois
!     PARAMETER(NVOIMA=100,NSCOMA=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
    character(len=*) :: codvoi
    integer :: iv, is, iel, ideb, ifin, icode, jcode, numav, ielv, typev
    integer :: ntymax
    parameter(ntymax=3)
    integer :: lcod, ityvo, ntyvo, lityvo(1:ntymax)
    character(len=2) :: tybase(9)
!-----------FONCTIONS  D ACCES A VGE -----------------------------------
    integer :: zzadvo, zznbvo, zzadve, zzmavo, zztyvo, zznbno, zznbsc, zzloc1
    integer :: zzloc2
!     IADDVO : ADRESSE JEVEUX DU TABLEAU DE POINTEURS DANS LA SD EL_VOIS
!     IADVOI : ADRESSE JEVEUX DE LA SD EL_VOIS
!
!     DES DONNEES DES VOISINS DE LA MAILLE NUMA (0 SI MAILLE PAS ACTIVE)
    zzadvo(numa)=zi(iaddvo+numa-1)+iadvoi
!
!     NOMBBRE DE VOISINS DE NUMA EME MAILLE
    zznbvo(numa)=zi(zzadvo(numa)-1+1)
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!     ADRESSE DES DONNEES
! FAUX ???      ZZADVE(NUMA,IV) = ZI(ZZADVO(NUMA)-1+1+IV)+IADVOI-1
    zzadve(numa,iv)=zi(zzadvo(numa)-1+1+iv)+zzadvo(numa)-1
!
!     POUR LA MAILLE NUMA
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
    zztyvo(numa,iv)=zi(zzadve(numa,iv)-1+1)
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!     NUMERO DE MAILLE
    zzmavo(numa,iv)=zi(zzadve(numa,iv)-1+2)
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!     NOMBRE DE NOEUDS DE MAILLE
    zznbno(numa,iv)=zi(zzadve(numa,iv)-1+3)
!
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!        NOMBRE DE SOMMETS COMMUNS
    zznbsc(numa,iv)=zi(zzadve(numa,iv)-1+4)
!
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!     POUR LE SOMMET COMMUN IS
!     NUMERO LOCAL DANS NUMA
    zzloc1(numa,iv,is)=zi(zzadve(numa,iv)-1+4+1+2*(is-1))
!
!
!
!     POUR LA MAILLE NUMA
!     POUR LE VOISIN IV
!    POUR LE SOMMET COMMUN IS
!     NUMERO LOCAL DANS IV
    zzloc2(numa,iv,is)=zi(zzadve(numa,iv)-1+4+1+2*(is-1)+1)
!-----------FIN FONCTIONS  D ACCES A VGE -------------------------------
    data tybase/'F3','F2','A3','A2','A1','S3','S2','S1','S0'/
!
!  1 CETTE MAILLE EST ELLE AFFECTEE DANS LE MODELE ?
!
    iel=zi(iarepe-1+2*(numa-1)+2)
    if (iel .eq. 0) then
        nbvois=0
        goto 80
!
    endif
!
!
!  1 RECHERCHE DES TYPES DES VOISINAGE ATTENDUS
!
    ntyvo=0
    lcod=lxlgut(codvoi)
    if (lcod .gt. 2*ntymax) then
        call u2mesg('F', 'VOLUFINI_7', 1, codvoi, 1,&
                    lcod, 0, 0.d0)
    endif
    do 30 icode = 1, lcod/2
        ideb=2*(icode-1)+1
        ifin=2*icode
        do 10 jcode = 1, 9
            if (codvoi(ideb:ifin) .eq. tybase(jcode)) then
                ntyvo=ntyvo+1
                lityvo(ntyvo)=jcode
                goto 20
!
            endif
10      continue
        call u2mesg('F', 'VOLUFINI_6', 1, codvoi(ideb:ifin), 0,&
                    0, 0, 0.d0)
20      continue
30  end do
    if (ntyvo .eq. 0) then
        nbvois=0
        goto 80
!
    endif
!
!      REMPLISSAGE DES TABLEAUX
!
    nbvois=0
    do 70 iv = 1, zznbvo(numa)
        numav=zzmavo(numa,iv)
        ielv=zi(iarepe-1+2*(numav-1)+2)
!
!  LE VOISIN EST IL UNE MAILLE AFFECTEE DANS LE MODELE
!
        if (ielv .eq. 0) then
            goto 70
!
        endif
        typev=zztyvo(numa,iv)
!
!  LE VOISIN EST IL D UN TYPE ATTENDU
!
        do 40 ityvo = 1, ntyvo
            if (typev .eq. lityvo(ityvo)) then
                goto 50
!
            endif
40      continue
        goto 70
!
50      continue
        nbvois=nbvois+1
        livois(nbvois)=numav
        tyvois(nbvois)=typev
        nbnovo(nbvois)=zznbno(numa,iv)
        nbsoco(nbvois)=zznbsc(numa,iv)
        do 60 is = 1, zznbsc(numa, iv)
            lisoco(nbvois,is,1)=zzloc1(numa,iv,is)
            lisoco(nbvois,is,2)=zzloc2(numa,iv,is)
60      continue
70  end do
80  continue
!
end subroutine
