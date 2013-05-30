subroutine nummo1(nugene, modmec, nbmode, typrof)
    implicit    none
    include 'jeveux.h'
!
    include 'asterfort/crsmos.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: nbmode
    character(len=8) :: modmec
    character(len=*) :: typrof
    character(len=14) :: nugene
    character(len=19) :: prgene, stomor
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    BUT: < NUMEROTATION GENERALISEE >
!
!    DETERMINER LA NUMEROTATION GENERALISEE A PARTIR D'UN MODE_MECA
!    OU D'UN MODE_GENE
!
! IN : NUGENE : NOM K14 DU NUME_DDL_GENE
! IN : MODMEC : NOM K8 DU MODE_MECA OU DU MODE_GENE
! IN : NBMODE : NOMBRE DE MODES
! IN : TYPROF : TYPE DE STOCKAGE
!-----------------------------------------------------------------------
!
!
!
    integer :: ibid, jrefn, jdesc, ldnequ, ldors, ldprs, ldorl, ldprl, lddeeq
    integer :: ldnueq, j, lddelg
!     ------------------------------------------------------------------
!
    call jemarq()
    prgene=nugene//'.NUME'
    stomor=nugene//'.SMOS'
!
!-----CREATION DU .REFN
!
    call wkvect(prgene//'.REFN', 'G V K24', 4, jrefn)
    zk24(jrefn)=modmec
    zk24(jrefn+1)='DEPL_R'
!
!-----CREATION DU .DESC
!
    call wkvect(prgene//'.DESC', 'G V I', 1, jdesc)
    zi(jdesc)=2
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
!
    call wkvect(prgene//'.NEQU', 'G V I', 1, ldnequ)
    zi(ldnequ)=nbmode
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
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprl)
!
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorl)
!
    zi(ldors)=1
    zi(ldprs)=1
    zi(ldprs+1)=nbmode
    zi(ldorl)=1
    zi(ldorl+1)=1
    zi(ldprl)=0
    zi(ldprl+1)=0
!
!-----BOUCLES DE COMPTAGE DES DDL
!
!
!-----ALLOCATIONS DIVERSES
!
    call wkvect(prgene//'.DELG', 'G V I', nbmode, lddelg)
    call wkvect(prgene//'.DEEQ', 'G V I', nbmode*2, lddeeq)
    call wkvect(prgene//'.NUEQ', 'G V I', nbmode, ldnueq)
!
!     REMPLISSAGE DU .DEEQ ET DU .NUEQ
!
    do 10 j = 1, nbmode
        zi(ldnueq+j-1)=j
        zi(lddelg+j-1)=0
        zi(lddeeq+2*j-1)=1
        zi(lddeeq+2*j-2)=j
10  end do
!
!
!     CREATION DU STOCKAGE MORSE :
    call crsmos(stomor, typrof, nbmode)
!
    call jedema()
end subroutine
