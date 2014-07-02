subroutine nmcmat(oper, typmaz, optcaz, optasz, lcalc,&
                  lasse, nbmatr, ltypma, loptme, loptma,&
                  lcalme, lassme)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
    character(len=4) :: oper
    character(len=*) :: optcaz, optasz, typmaz
    aster_logical :: lasse, lcalc
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    aster_logical :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! GESTION DE LA LISTE DES MATR_ELEM A CALCULER ET ASSEMBLER
!
! ----------------------------------------------------------------------
!
!
! IN  OPER   : OPERATION SUR LA LISTE
!                INIT
!                AJOU
! IN  TYPMAT : TYPE DES MATRICES CONCERNEES
!                MERIGI  - MATRICE POUR RIGIDITE
!                MEDIRI  - MATRICE POUR CL DIRICHLET LAGRANGE
!                MEGEOM  - MATRICE POUR NON-LIN. GEOMETRIQUE
!                MEAMOR  - MATRICE POUR AMORTISSEMENT
!                MEMASS  - MATRICE POUR MASSE
!                MESUIV  - MATRICE POUR CHARGEMENT SUIVEUR
!                MESSTR  - MATRICE POUR SOUS-STRUCTURES
!                MECTCC  - MATRICE POUR CONTACT CONTINU
!                MECTCF  - MATRICE POUR FROTTEMENT CONTINU
!                MEXFEC  - MATRICE POUR CONTACT XFEM
!                MEXFEF  - MATRICE POUR FROTTEMENT XFEM
!                MEXFTC  - MATRICE POUR CONTACT XFEM (GRD GLIS)
!                MEXFTF  - MATRICE POUR FROTT. XFEM (GRD GLIS)
! IN  OPTCAL : OPTION DE CALCUL DU MATR_ELEM
! IN  OPTASS : OPTION D'ASSEMBLAGE DU MATR_ASSS
! IN  LCALC  : LE MATR_ELEM SERA A CALCULER
! IN  LASSE  : LE MATR_ELEM SERA A ASSEMBLER
! I/O NBMATR : NOMBRE DE MATR_ELEM DANS LA LISTE
! I/O LTYPMA : LISTE DES NOMS DES MATR_ELEM
! I/O LOPTME : LISTE DES OPTIONS DES MATR_ELEM
! I/O LOPTMA : LISTE DES OPTIONS DES MATR_ASSE
! I/O LCALME : SI MATR_ELEM A CALCULER
! I/O LASSME : SI MATR_ELEM A ASSEMBLER
!
! ----------------------------------------------------------------------
!
    character(len=16) :: optcal, optass
    character(len=6) :: k6bla, typmat
    integer :: i
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    optcal = optcaz
    optass = optasz
    typmat = typmaz
    k6bla = ' '
!
! --- OPERATIONS
!
    if (oper .eq. 'INIT') then
        do 10 i = 1, 20
            ltypma(i) = k6bla
 10     continue
        nbmatr = 0
    else if (oper.eq.'AJOU') then
        nbmatr = nbmatr + 1
        if (nbmatr .eq. 21) then
            ASSERT(.false.)
        endif
        ltypma(nbmatr) = typmat
        loptme(nbmatr) = optcal
        loptma(nbmatr) = optass
        lassme(nbmatr) = lasse
        lcalme(nbmatr) = lcalc
    else
        ASSERT(.false.)
    endif
!
end subroutine
