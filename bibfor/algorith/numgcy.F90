subroutine numgcy(nugene, modgen)
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
!***********************************************************************
!    O. NICOLAS
!-----------------------------------------------------------------------
!  BUT:      < NUMEROTATION GENERALISEE >
    implicit none
!
!  DETERMINER LA NUMEROTATION DES DEGRES DE LIBERTE GENERALISES
!   A PARTIR D'UN MODELE GENERALISE CAS CYCLIQUE POUR DESACORDAGE
!
!-----------------------------------------------------------------------
!
! NUGENE   /I/: NOM K14 DU NUME_DDL_GENE
! MODGEN   /I/: NOM K8 DU MODELE GENERALISE
!
!
!
#include "jeveux.h"
!
#include "asterfort/crsmos.h"
#include "asterfort/iunifi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/wkvect.h"
!
!
    character(len=8) :: modgen, nomcou, kbid
    character(len=14) :: nugene
    character(len=19) :: prgene, stomor
    character(len=24) :: defli, fprofl, nomsst
    integer :: ibid, jrefn, lddesc, ldnequ, ldors, ldprs, ldorl, ldprl, lddeeq
    integer :: ldnueq, i, j, lddelg
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icompl, icomps, ifimes, lldesc, llprof, nblia
    integer :: nblig, nbmod, nbsst, neq
!-----------------------------------------------------------------------
    call jemarq()
    ifimes=iunifi('MESSAGE')
!-----------------------------------------------------------------------
!
    kbid=' '
    defli=modgen//'      .MODG.LIDF'
    fprofl=modgen//'      .MODG.LIPR'
    nomsst=modgen//'      .MODG.SSNO'
    prgene=nugene//'.NUME'
    stomor=nugene//'.SMOS'
!
!-----CREATION DU .REFN
!
    call wkvect(prgene//'.REFN', 'G V K24', 4, jrefn)
    zk24(jrefn)=modgen
    zk24(jrefn+1)='DEPL_R'
!
!-----CREATION DU .DESC
!
    call wkvect(prgene//'.DESC', 'G V I', 1, lddesc)
    zi(lddesc)=2
!
!---------------------------DECLARATION JEVEUX--------------------------
!
!     CREATION DE LA COLLECTION .LILI
!
    call jecreo(prgene//'.LILI', 'G N K8')
    call jeecra(prgene//'.LILI', 'NOMMAX', 2, ' ')
    call jecroc(jexnom(prgene//'.LILI', '&SOUSSTR'))
    call jecroc(jexnom(prgene//'.LILI', 'LIAISONS'))
!
!     CREATION DES COLLECTIONS
!
    call jecrec(prgene//'.PRNO', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
    call jecrec(prgene//'.ORIG', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
!
!------RECUPERATION DES DIMENSIONS PRINCIPALES
    call wkvect(prgene//'.NEQU', 'G V I', 1, ldnequ)
! ON RECUPERE LE NOMBRE DE LIAISON
    call jelira(defli, 'NMAXOC', nblia, kbid)
! ON RECUPERE LE NOMBRE DE SOUS-STRUCTURE
    call jelira(nomsst, 'NOMMAX', nbsst, kbid)
!
!----------------------BOUCLES DE COMPTAGE DES DDL----------------------
!
! ICOMPS EST LE NOMBRE TOTAL DE MODES DANS LES SOUS-STRUCTURES
    icomps=0
! ICOMPS EST LE NOMBRE TOTAL DE MODES D'INTERFACE DANS LES
! SOUS-STRUCTURES
    icompl=0
!
!   BOUCLE SUR LES SOUS-STRUCTURES
!
    do 10 i = 1, nbsst
        call mgutdm(modgen, kbid, i, 'NOM_MACR_ELEM', ibid,&
                    nomcou)
        call jeveuo(nomcou//'.MAEL_RAID_DESC', 'L', lldesc)
        nbmod=zi(lldesc+1)
        icomps=icomps+nbmod
10  end do
!
!   BOUCLE SUR LES LIAISONS
!
    call jeveuo(fprofl, 'L', llprof)
    do 20 i = 1, nblia
        nblig=zi(llprof+(i-1)*9)
        icompl=icompl+nblig
20  end do
!
    neq=icomps-icompl
!
    zi(ldnequ)=neq
!
    write (ifimes,*)'+++ NOMBRE DE SOUS-STRUSTURES: ',nbsst
    write (ifimes,*)'+++ NOMBRE DE LIAISONS: ',nblia
    write (ifimes,*)'+++ NOMBRE TOTAL D''EQUATIONS: ',neq
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS STRUCTURE: ',icomps
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS LIAISON: ',icompl
!
!-----ECRITURE DIMENSIONS
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
!
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprs)
!
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldors)
!
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
!
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 1, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprl)
!
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 1, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorl)
!
    zi(ldors)=1
    zi(ldprs)=1
    zi(ldprs+1)=neq
    zi(ldorl)=1
    zi(ldprl)=0
!
!-----ALLOCATIONS DIVERSES
!
    call wkvect(prgene//'.DEEQ', 'G V I', neq*2, lddeeq)
    call wkvect(prgene//'.NUEQ', 'G V I', neq, ldnueq)
    call wkvect(prgene//'.DELG', 'G V I', neq, lddelg)
!
!     REMPLISSAGE DU .DEEQ ET DU .NUEQ
!
    do 30 j = 1, neq
        zi(ldnueq+j-1)=j
        zi(lddelg+j-1)=0
        zi(lddeeq+2*j-1)=1
        zi(lddeeq+2*j-2)=j
30  end do
!
!
!     CREATION DU STOCKAGES MORSE :
    call crsmos(stomor, 'PLEIN', neq)
!
!
    call jedema()
end subroutine
