subroutine cmlqnd(nbno, nbnomi, prefix, ndinit, nomipe,&
                  nomnoe, coor)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
!
    integer :: nbno, nbnomi, nomipe(2, nbnomi), ndinit
    real(kind=8) :: coor(3, *)
    character(len=8) :: prefix
    character(len=24) :: nomnoe
!
!
! ----------------------------------------------------------------------
!         CREATION DES NOEUDS MILIEUX (CREA_MAILLAGE LINE_QUAD)
! ----------------------------------------------------------------------
! IN        NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! IN        NBNOMI  NOMBRE DE NOEUDS CREES
! IN        PREFIX  PREFIXE POUR LE NOM DES NOEUDS (EX : N, NS, ...)
! IN        NDINIT  NUMERO INITIAL DES NOEUDS CREES
! IN        NOMIPE  LISTE DES PERES PAR NOEUDS CREES (NOEUDS SOMMETS)
! IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
! VAR       COOR    COORDONNEES DES NOEUDS
! ----------------------------------------------------------------------
!
!
    integer :: no, no1, no2, lgpref, lgnd, iret
!
    character(len=8) :: nomnd
    character(len=24) :: valk
    character(len=80) :: knume
! ----------------------------------------------------------------------
!
!
!
! - INSERTION DES NOUVEAUX NOEUDS
!
    lgpref = lxlgut(prefix)
    do 10 no = 1, nbnomi
!
!      NOM DU NOEUD CREE
        call codent(ndinit-1+no, 'G', knume)
        lgnd = lxlgut(knume)
        if (lgnd+lgpref .gt. 8) then
            call utmess('F', 'ALGELINE_16')
        endif
        nomnd = prefix(1:lgpref) // knume
!
!      DECLARATION DU NOEUD CREE
        call jeexin(jexnom(nomnoe, nomnd), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nomnoe, nomnd))
        else
            valk = nomnd
            call utmess('F', 'ALGELINE4_5', sk=valk)
        endif
!
10  end do
!
!
! - CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS
    do 20 no = 1, nbnomi
        no1 = nomipe(1,no)
        no2 = nomipe(2,no)
        coor(1,no+nbno) = (coor(1,no1) + coor(1,no2))/2
        coor(2,no+nbno) = (coor(2,no1) + coor(2,no2))/2
        coor(3,no+nbno) = (coor(3,no1) + coor(3,no2))/2
20  end do
!
end subroutine
