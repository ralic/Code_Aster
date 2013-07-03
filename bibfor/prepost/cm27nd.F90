subroutine cm27nd(nbno, nbnomi, nbnohe, nbma, lima,&
                  typema, connez, prefix, ndinit, nomipe,&
                  nomnoe, coor)
!
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
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
    integer :: nbno, nbnomi, nbnohe, nomipe(8, nbnomi), ndinit, nbma
    integer :: lima(*), typema(*)
    real(kind=8) :: coor(3, *)
    character(len=8) :: prefix
    character(len=24) :: nomnoe, connex
    character(len=*) :: connez
!
!
! ----------------------------------------------------------------------
!         CREATION DES NOEUDS MILIEUX (CREA_MAILLAGE HEXA20_27)
! ----------------------------------------------------------------------
! IN        NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! IN        NBNOMI  NOMBRE DE NOEUDS CREES DES FACES ( S Y AJOUTE LE
!                   NOEUD CENTRAL POUR LES HEXA20 )
! IN        NBNOHE  NOMBRE DE NOEUDS CENTRAUX
!                   ( IE NOMBRE DE MAILLE HEXA20)
! IN        NBMA    NOMBRE DE MAILLE
! IN        TYPEMA  TYPE DE MAILLES
! IN        CONNEZ  CONNECTIVITE DES MAILLES
! IN        PREFIX  PREFIXE POUR LE NOM DES NOEUDS (EX : N, NS, ...)
! IN        NDINIT  NUMERO INITIAL DES NOEUDS CREES
! IN        NOMIPE  LISTE DES PERES PAR NOEUDS CREES (NOEUDS SOMMETS)
! IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
! VAR       COOR    COORDONNEES DES NOEUDS
! ----------------------------------------------------------------------
!
!
    integer :: no, no1, no2, no3, no4, no5, no6, no7, no8, no9, lgpref, lgnd
    integer :: iret
    integer :: no10, no11, no12, no13, no14, no15, no16, no17, no18, no19, no20
    integer :: ima, tyma, jnoma, ma, ihex27
!
    character(len=8) :: nomnd
    character(len=24) :: valk
    character(len=80) :: knume
! ----------------------------------------------------------------------
    call jemarq()
!
    connex=connez
!
! - INSERTION DES NOUVEAUX NOEUDS
!
    lgpref = lxlgut(prefix)
    do 10 no = 1, nbnomi+nbnohe
!
!      NOM DU NOEUD CREE
        call codent(ndinit-1+no, 'G', knume)
        lgnd = lxlgut(knume)
        if (lgnd+lgpref .gt. 8) call u2mess('F', 'ALGELINE_16')
        nomnd = prefix(1:lgpref) // knume
!
!      DECLARATION DU NOEUD CREE
        call jeexin(jexnom(nomnoe, nomnd), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nomnoe, nomnd))
        else
            valk = nomnd
            call u2mesg('F', 'ALGELINE4_5', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
!
10  end do
!
! - CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS DES FACES
    do 20 no = 1, nbnomi
        no1 = nomipe(1,no)
        no2 = nomipe(2,no)
        no3 = nomipe(3,no)
        no4 = nomipe(4,no)
        no5 = nomipe(5,no)
        no6 = nomipe(6,no)
        no7 = nomipe(7,no)
        no8 = nomipe(8,no)
        coor(1,no+nbno) = -(&
                          coor(1,no1) + coor(1,no2) + coor(1,no3) + coor(1,no4))/4.d0+ (coor(1,no&
                          &5) + coor(1,no6)+ coor(1,no7) + coor(1,no8)&
                          )/2.d0
!
        coor(2,no+nbno) = -(&
                          coor(2,no1) + coor(2,no2) + coor(2,no3) + coor(2,no4))/4.d0+ (coor(2,no&
                          &5) + coor(2,no6)+ coor(2,no7) + coor(2,no8)&
                          )/2.d0
!
        coor(3,no+nbno) = -(&
                          coor(3,no1) + coor(3,no2) + coor(3,no3) + coor(3,no4))/4.d0+ (coor(3,no&
                          &5) + coor(3,no6)+ coor(3,no7) + coor(3,no8)&
                          )/2.d0
20  end do
!
! - CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS CENTRAUX A LA MAILLE
    ihex27=0
    do 30 ima = 1, nbma
        ma=lima(ima)
        tyma = typema(ma)
        if (tyma .eq. 26) then
            call jeveuo(jexnum(connex, ma), 'L', jnoma)
            ihex27=ihex27+1
!
            no1 = zi(jnoma-1+1)
            no2 = zi(jnoma-1+2)
            no3 = zi(jnoma-1+3)
            no4 = zi(jnoma-1+4)
            no5 = zi(jnoma-1+5)
            no6 = zi(jnoma-1+6)
            no7 = zi(jnoma-1+7)
            no8 = zi(jnoma-1+8)
            no9 = zi(jnoma-1+9)
            no10 = zi(jnoma-1+10)
            no11 = zi(jnoma-1+11)
            no12 = zi(jnoma-1+12)
            no13 = zi(jnoma-1+13)
            no14 = zi(jnoma-1+14)
            no15 = zi(jnoma-1+15)
            no16 = zi(jnoma-1+16)
            no17 = zi(jnoma-1+17)
            no18 = zi(jnoma-1+18)
            no19 = zi(jnoma-1+19)
            no20 = zi(jnoma-1+20)
!
            coor(1,nbnomi+nbno+ihex27) = (&
                                         -coor(1,no1) - coor(1,no2) - coor(1,no3) - coor(1,no4) -&
                                         & coor(1,no5) - coor(1,no6) - coor(1,no7) - coor(1,no8) &
                                         &+ coor(1,no9) + coor(1,no10) + coor(1,no11) + coor(1,no&
                                         &12) + coor(1,no13) + coor(1,no14) + coor(1,no15) + coor&
                                         &(1,no16) + coor(1,no17) + coor(1, no18) + coor(1,no19) &
                                         &+ coor(1,no20)&
                                         )/4.d0
!
            coor(2,nbnomi+nbno+ihex27) = (&
                                         -coor(2,no1) - coor(2,no2) - coor(2,no3) - coor(2,no4) -&
                                         & coor(2,no5) - coor(2,no6) - coor(2,no7) - coor(2,no8) &
                                         &+ coor(2,no9) + coor(2,no10) + coor(2,no11) + coor(2,no&
                                         &12) + coor(2,no13) + coor(2,no14) + coor(2,no15) + coor&
                                         &(2,no16) + coor(2,no17) + coor(2, no18) + coor(2,no19) &
                                         &+ coor(2,no20)&
                                         )/4.d0
!
            coor(3,nbnomi+nbno+ihex27) = (&
                                         -coor(3,no1) - coor(3,no2) - coor(3,no3) - coor(3,no4) -&
                                         & coor(3,no5) - coor(3,no6) - coor(3,no7) - coor(3,no8) &
                                         &+ coor(3,no9) + coor(3,no10) + coor(3,no11) + coor(3,no&
                                         &12) + coor(3,no13) + coor(3,no14) + coor(3,no15) + coor&
                                         &(3,no16) + coor(3,no17) + coor(3, no18) + coor(3,no19) &
                                         &+ coor(3,no20)&
                                         )/4.d0
        endif
30  end do
!
    call jedema()
end subroutine
