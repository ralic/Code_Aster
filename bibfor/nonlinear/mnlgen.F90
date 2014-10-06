subroutine mnlgen(numdrv, matdrv, ninc)
    implicit none
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE -- INITIALISATION SDs MATRICE JACOBIENNE
!     -    -                -            -   -
! ----------------------------------------------------------------------
!
! INITIALISE LE NUME_DDL_GENE ET LE MATR_ASSE_GENE DE LA MATRICE
! JACOBIENNE SANS LES ATTRIBUTS SMOS (NUME_DDL_GENE)
! ET VALM (MATR_ASSE_GENE)
! ----------------------------------------------------------------------
! OUT  NUMDRV : K14  : NUME_DDL_GENE DE LA MATRICE JACOBIENNE
! OUT  MATDRV : K19  : NOM DE  LA MATRICE JACOBIENNE
! IN   NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cresol.h"
#include "asterfort/crnslv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/profgene_crsd.h"
#include "asterfort/wkvect.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    character(len=14) :: numdrv
    character(len=19) :: matdrv
    integer :: ninc
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    character(len=19) :: prof_gene, solveu
    character(len=24) :: lili, orig, prno
    integer :: i_ligr_link, i_ligr_sstr
    integer, pointer :: prgene_orig(:) => null()
    integer, pointer :: prgene_prno(:) => null()
    integer :: ibid, mrefa, mdesc, ismde
!
! ----------------------------------------------------------------------
! --- RECUPERATION DES PARAMETRES ET CREATION DU SOLVEUR
! ----------------------------------------------------------------------
    solveu=numdrv//'.SOLV'
    call cresol(solveu)
! ----------------------------------------------------------------------
! --- CREATION DU NUME_DDL_GENE ASSOCIEE A LA MATRICE JACOBIENNE
! ----------------------------------------------------------------------
! --- CREATION DU PROF_GENE
    prof_gene=numdrv//'.NUME'
    lili=prof_gene//'.LILI'
    orig=prof_gene//'.ORIG'
    prno=prof_gene//'.PRNO'
!
! - Create PROF_GENE
!
    call profgene_crsd(prof_gene, 'V', ninc, nb_sstr = 1, nb_link = 1,&
                       model_genez = ' ', gran_namez = 'DEPL_R')
!
! - Set sub_structures
!
    call jenonu(jexnom(lili, '&SOUSSTR'), i_ligr_sstr)
    ASSERT(i_ligr_sstr.eq.1)
    call jeveuo(jexnum(prno, i_ligr_sstr), 'E', vi = prgene_prno)
    call jeveuo(jexnum(orig, i_ligr_sstr), 'E', vi = prgene_orig)
    prgene_prno(1) = 1
    prgene_prno(2) = ninc
    prgene_orig(1) = 1
!
! - Set links
!
    call jenonu(jexnom(lili, 'LIAISONS'), i_ligr_link)
    call jeveuo(jexnum(orig, i_ligr_link), 'E', vi = prgene_prno)
    call jeveuo(jexnum(orig, i_ligr_link), 'E', vi = prgene_orig)
    prgene_prno(1) = 0
    prgene_orig(1) = 1

! --- CREATION DU SMOS
    call wkvect(numdrv//'.SMOS.SMDI', 'V V I', ninc, ibid)
    call wkvect(numdrv//'.SMOS.SMDE', 'V V I', 3, ismde)
    zi(ismde-1+1)=ninc
    zi(ismde-1+3)=1
!
! ----------------------------------------------------------------------
! --- CREATION DU MATR_ASSE_GENE ASSOCIEE A LA MATRICE JACOBIENNE
! ----------------------------------------------------------------------
! --- REFA
    call wkvect(matdrv//'.REFA', 'V V K24', 20, mrefa)
    zk24(mrefa-1+1)=' '
    zk24(mrefa-1+2)=numdrv
    zk24(mrefa-1+3)=' '
    zk24(mrefa-1+4)='&&MELANGE'
    zk24(mrefa-1+5)=' '
    zk24(mrefa-1+6)=' '
    zk24(mrefa-1+7)=solveu
!    numdrv//'.SOLV'
    zk24(mrefa-1+8)=' '
    zk24(mrefa-1+9)='MR'
    zk24(mrefa-1+10)='GENE'
    zk24(mrefa-1+11)='MPI_COMPLET'
! --- DESC
    call wkvect(matdrv//'.DESC', 'V V I', 3, mdesc)
    zi(mdesc-1+1)=2
    zi(mdesc-1+3)=2
!
end subroutine
