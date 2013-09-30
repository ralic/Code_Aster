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
#include "asterfort/wkvect.h"
#include "asterfort/cresol.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    character(len=14) :: numdrv
    character(len=19) :: matdrv
    integer :: ninc
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    character(len=19) :: prgene, solveu
    character(len=8) :: k8bid
    integer :: lddesc, ldnequ, jrefn, lddeeq, lddelg, jnslv
    integer :: ldnueq, ibid, ldprno, ldorig, mrefa, mdesc, k, ismde

! ----------------------------------------------------------------------
! --- RECUPERATION DES PARAMETRES ET CREATION DU SOLVEUR
! ----------------------------------------------------------------------
    solveu=numdrv//'.SOLV'
    call cresol(solveu)
    call wkvect(numdrv//'.NSLV', 'V V K24', 1, jnslv)
    zk24(jnslv)=solveu
! ----------------------------------------------------------------------
! --- CREATION DU NUME_DDL_GENE ASSOCIEE A LA MATRICE JACOBIENNE
! ----------------------------------------------------------------------
! --- CREATION DU PROF_GENE
    prgene=numdrv//'.NUME'
! --- DESC
    call wkvect(prgene//'.DESC', 'V V I', 1, lddesc)
    zi(lddesc)=2
! --- NEQU
    call wkvect(prgene//'.NEQU', 'V V I', 1, ldnequ)
    zi(ldnequ)=ninc
! --- REFN
    call wkvect(prgene//'.REFN', 'V V K24', 4, jrefn)
    zk24(jrefn+1)='DEPL_R'
! --- DEEQ
    call wkvect(prgene//'.DEEQ', 'V V I', ninc*2, lddeeq)
    do 30 k = 1, ninc
        zi(lddeeq-1+(k-1)*2+1)=k
        zi(lddeeq-1+(k-1)*2+2)=1
30  continue
! --- DELG
    call wkvect(prgene//'.DELG', 'V V I', ninc, lddelg)
! --- LILI
    call jecreo(prgene//'.LILI', 'V N K8')
    call jeecra(prgene//'.LILI', 'NOMMAX', 2, k8bid)
    call jecroc(jexnom(prgene//'.LILI', '&SOUSSTR'))
    call jecroc(jexnom(prgene//'.LILI', 'LIAISONS'))
! --- NUEQ
    call wkvect(prgene//'.NUEQ', 'V V I', ninc, ldnueq)
    do 20 k = 1, ninc
        zi(ldnueq-1+k)=k
20  continue
! --- PRNO
    call jecrec(prgene//'.PRNO', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
    call jecrec(prgene//'.ORIG', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprno)
    zi(ldprno-1+1)=1
    zi(ldprno-1+2)=ninc
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorig)
    zi(ldorig-1+1)=1
    zi(ldorig-1+2)=0
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprno)
    zi(ldprno-1+1)=0
    zi(ldprno-1+2)=0
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorig)
    zi(ldorig-1+1)=1
    zi(ldorig-1+2)=1
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
    zk24(mrefa-1+1)=''
    zk24(mrefa-1+2)=numdrv
    zk24(mrefa-1+3)=''
    zk24(mrefa-1+4)='&&MELANGE'
    zk24(mrefa-1+5)=''
    zk24(mrefa-1+6)=''
    zk24(mrefa-1+7)=solveu
!    numdrv//'.SOLV'
    zk24(mrefa-1+8)=''
    zk24(mrefa-1+9)='MR'
    zk24(mrefa-1+10)='GENE'
    zk24(mrefa-1+11)='MPI_COMPLET'
! --- DESC
    call wkvect(matdrv//'.DESC', 'V V I', 3, mdesc)
    zi(mdesc-1+1)=2
    zi(mdesc-1+3)=2

end subroutine
