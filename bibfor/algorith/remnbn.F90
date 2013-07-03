subroutine remnbn(basmod, nbmod, nbddr, nbdax, flexdr,&
                  flexga, flexax, tetgd, tetax, cmode,&
                  vecmod, neq, beta)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dcapno.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: nbmod, nbddr, nbdax, neq
    real(kind=8) :: beta
    complex(kind=8) :: cmode(nbmod+nbddr+nbdax), vecmod(neq)
    character(len=8) :: basmod
    character(len=24) :: flexdr, flexga, flexax, tetgd, tetax
!-----------------------------------------------------------------------
!
!  BUT:     <RESTITUTION MAC-NEAL DE BAS NIVEAU>
!
!  CALCUL DU VECTEUR COMPLEXE EN DDL PHYSIQUES A PARTIR DU MODE
!   COMPLEXE ISSU DU CALCUL CYCLIQUE ET DES VECTEURS DES NUMERO ORDRE
!  DES DEFORMEES RELATIVES AUX INTERFACES DROITE GAUCHE ET AXE DE TYPE
!  MAC-NEAL
!
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UT DE LA BASE MODALE EN AMONT
! NBMOD    /I/: NOMBRE DE MODES PROPRES UTILISES POUR CALCUL CYCLIQUE
! NBDDR    /I/: NOMBRE DE DEFORMEES INTERFACE DROITE (ET GAUCHE)
! NBDAX    /I/: NOMBRE DE DEFORMEES INTERFACE AXE
! FLEXDR   /I/: NOM K24 FLEXIBILITE TOUTES-LIGNES/COLONNES-DROITES
! FELXGA   /I/: NOM K24 FELXIBILITE TOUTES-LIGNES/COLONNES-GAUCHES
! FLEXAX   /I/: NOM K24 FELXIBILITE TOUTES-LIGNES/COLONNES-AXES
! TETGD    /I/: NOM K24 DE LA MATRICE DE PASSAGE GAUCHE-DROITE
! TETAX    /I/: NOM K24 DE LA MATRICE DE PASSAGE AXE-AXE
! IORD     /I/: VECTEUR DES NUMEROS ORDRE DEFORMEES DE DROITE
! IORG     /I/: VECTEUR DES NUMEROS ORDRE DES DEFORMEES DE GAUCHE
! IORA     /I/: VECTEUR DES NUMEROS ORDRE DES DEFORMEES  AXE
! CMODE    /I/: MODE COMPLEXES ISSU DU CALCUL CYCLIQUE
! VECMOD   /I/: VECTEUR MODAL COMPLEXE EN DDL PHYSIQUE
! NEQ      /I/: NOMBRE DE DDL PHYSIQUES ASSEMBLES
! BETA     /I/: DEPHASAGE INTER-SECTEUR
!
!
!
!
    integer :: i, j, iad, llcham, llfdr, llfga, lltgd, lltax, llfax
    real(kind=8) :: abeta, bbeta
    complex(kind=8) :: dephc, cfact, cmult
    character(len=24) :: chaval
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
! --- MISE A ZERO DU MODE PROPRES RESULTAT
!
    do 10 i = 1, neq
        vecmod(i) = dcmplx(0.d0,0.d0)
10  end do
!
    abeta = cos(beta)
    bbeta = sin(beta)
    dephc = dcmplx( abeta,bbeta )
!
! --- CONTRIBUTION DES MODES PROPRES
!
    do 20 i = 1, nbmod
        call dcapno(basmod, 'DEPL    ', i, chaval)
        call jeveuo(chaval, 'L', llcham)
        do 22 j = 1, neq
            cfact = dcmplx( zr(llcham+j-1),0.d0 )
            vecmod(j) = vecmod(j) + cmode(i)*cfact
22      continue
        call jelibe(chaval)
20  end do
!
! --- CONTRIBUTION DES DEFORMEES DE DROITE
!
    call jeveuo(flexdr, 'L', llfdr)
    do 30 i = 1, nbddr
        do 32 j = 1, neq
            iad = llfdr + ((i-1)*neq) + j - 1
            cfact = dcmplx( zr(iad),0.d0 )
            vecmod(j) = vecmod(j) + cmode(i+nbmod)*cfact
32      continue
30  end do
    call jelibe(flexdr)
!
! --- CONTRIBUTION DES DEFORMEES DE GAUCHE
!
    call jeveuo(tetgd, 'L', lltgd)
    call jeveuo(flexga, 'L', llfga)
    do 40 i = 1, nbddr
!
        cmult = dcmplx( 0.d0,0.d0 )
        do 42 j = 1, nbddr
            iad = lltgd + ((j-1)*nbddr) + i - 1
            cfact = dcmplx( zr(iad),0.d0 )*cmode(j+nbmod)
            cmult = cmult - cfact
42      continue
!
        iad = llfga + ((i-1)*neq)
        do 44 j = 1, neq
            cfact = dcmplx( zr(iad+j-1),0.d0 )
            vecmod(j) = vecmod(j) + dephc*cfact*cmult
44      continue
!
40  end do
    call jelibe(flexga)
    call jelibe(tetgd)
!
! --- EVENTUELLE CONTRIBUTION DE L'AXE
!
    if (nbdax .gt. 0) then
        call jeveuo(tetax, 'L', lltax)
        call jeveuo(flexax, 'L', llfax)
        do 50 i = 1, nbdax
!
            cmult = dcmplx( 1.d0,0.d0 )
            do 52 j = 1, nbdax
                iad = lltax + ((j-1)*nbdax) + i - 1
                cfact = dcmplx( zr(iad),0.d0 )*cmode(nbmod+nbddr+j)
                cmult = cmult - (dephc*cfact)
52          continue
!
            iad = llfax + ((i-1)*neq)
            do 54 j = 1, neq
                cfact = dcmplx( 2*zr(iad+j-1),0.d0 )
                vecmod(j) = vecmod(j) + cfact*cmult
54          continue
!
50      continue
        call jelibe(flexax)
        call jelibe(tetax)
    endif
!
    call jedema()
end subroutine
