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
#include "asterfort/crsmos.h"
#include "asterfort/assert.h"
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
#include "asterfort/profgene_crsd.h"
#include "asterfort/mgutdm.h"
#include "asterfort/wkvect.h"
!
!
    character(len=8) :: modgen, nomcou, kbid
    character(len=14) :: nugene
    character(len=19) :: prof_gene, stomor
    character(len=24) :: defli, fprofl, nomsst
    integer :: ibid, i, i_ligr_link, nb_link, nb_sstr, i_ligr_sstr
    character(len=24) :: lili, prno, orig
    integer, pointer :: prgene_orig(:) => null()
    integer, pointer :: prgene_prno(:) => null()

!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icompl, icomps, ifimes,  llprof, nblia
    integer :: nblig, nbmod, nbsst, neq
    integer, pointer :: mael_raid_desc(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ifimes=iunifi('MESSAGE')
!-----------------------------------------------------------------------
!
    kbid=' '
    defli=modgen//'      .MODG.LIDF'
    fprofl=modgen//'      .MODG.LIPR'
    nomsst=modgen//'      .MODG.SSNO'
    prof_gene=nugene//'.NUME'
    stomor=nugene//'.SMOS'
    lili=prof_gene//'.LILI'
    prno=prof_gene//'.PRNO'
    orig=prof_gene//'.ORIG'

! ON RECUPERE LE NOMBRE DE LIAISON
    call jelira(defli, 'NMAXOC', nblia)
! ON RECUPERE LE NOMBRE DE SOUS-STRUCTURE
    call jelira(nomsst, 'NOMMAX', nbsst)
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
    do i = 1, nbsst
        call mgutdm(modgen, kbid, i, 'NOM_MACR_ELEM', ibid,&
                    nomcou)
        call jeveuo(nomcou//'.MAEL_RAID_DESC', 'L', vi=mael_raid_desc)
        nbmod=mael_raid_desc(2)
        icomps=icomps+nbmod
    end do
!
!   BOUCLE SUR LES LIAISONS
!
    call jeveuo(fprofl, 'L', llprof)
    do i = 1, nblia
        nblig=zi(llprof+(i-1)*9)
        icompl=icompl+nblig
    end do
!
    neq=icomps-icompl
!
    write (ifimes,*)'+++ NOMBRE DE SOUS-STRUCTURES: ',nbsst
    write (ifimes,*)'+++ NOMBRE DE LIAISONS: ',nblia
    write (ifimes,*)'+++ NOMBRE TOTAL D''EQUATIONS: ',neq
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS STRUCTURE: ',icomps
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS LIAISON: ',icompl
!  ON REMPLIT LE NUME_DDL COMME S'IL N'Y AVAIT QU'UNE SEULE SOUS
!  STRUCTURE. 
    nb_sstr = 1
    nb_link = 1
!
! - Create PROF_GENE
!
    call profgene_crsd(prof_gene, 'G', neq, nb_sstr = nb_sstr, nb_link = nb_link,&
                       model_genez = modgen, gran_namez = 'DEPL_R')
!
! - Set sub_structures
!
    call jenonu(jexnom(lili, '&SOUSSTR'), i_ligr_sstr)
    ASSERT(i_ligr_sstr.eq.1)
    call jeveuo(jexnum(prno, i_ligr_sstr), 'E', vi = prgene_prno)
    call jeveuo(jexnum(orig, i_ligr_sstr), 'E', vi = prgene_orig)
    prgene_prno(1) = 1
    prgene_prno(2) = neq
    prgene_orig(1) = 1
!
! - Set links
!
    call jenonu(jexnom(lili, 'LIAISONS'), i_ligr_link)
    call jeveuo(jexnum(prno, i_ligr_link), 'E', vi = prgene_prno)
    call jeveuo(jexnum(orig, i_ligr_link), 'E', vi = prgene_orig)
    prgene_prno(1) = 0
    prgene_orig(1) = 1

!
!     CREATION DU STOCKAGES MORSE :
    call crsmos(stomor, 'PLEIN', neq)
!
!
    call jedema()
end subroutine
