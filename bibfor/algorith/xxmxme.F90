subroutine xxmxme(noma, nomo, fonact, defico, resoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xmele1.h'
    integer :: fonact(*)
    character(len=24) :: defico, resoco
    character(len=8) :: noma, nomo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES XFEM)
!
! CREATION SD DE RESOLUTION RESOCO
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
!
!
!
    integer :: nfiss, jnfis, nfismx
    parameter    (nfismx=100)
    integer :: ifm, niv
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: ntpc
    integer :: ztabf
    character(len=19) :: ligrel
    character(len=19) :: xindc0, xseuc0, xcohe0
    logical :: lxffm, lxfcm, lxczm
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    ntpc = cfdisi(defico,'NTPC' )
    lxfcm = isfonc(fonact,'CONT_XFEM')
    lxffm = isfonc(fonact,'FROT_XFEM')
    lxczm = cfdisl(defico,'EXIS_XFEM_CZM')
    call assert(lxfcm)
!
! --- INITIALISATIONS
!
    ligrel = nomo//'.MODELE'
!
! --- NOMBRE DE FISSURES
!
    call jeveuo(nomo//'.NFIS', 'L', jnfis)
    nfiss = zi(jnfis)
    if (nfiss .gt. nfismx) then
        call u2mesi('F', 'XFEM_2', 1, nfismx)
    endif
    if (nfiss .le. 0) then
        call u2mess('F', 'XFEM_3')
    endif
!
! --- NOM DES CHAMPS
!
    xindc0 = resoco(1:14)//'.XFI0'
    xseuc0 = resoco(1:14)//'.XFS0'
    xcohe0 = resoco(1:14)//'.XCO0'
    ztabf = cfmmvd('ZTABF')
!
! --- FONCTIONNALITES ACTIVEES
!
    ntpc = cfdisi(defico,'NTPC' )
    lxfcm = isfonc(fonact,'CONT_XFEM')
    lxffm = isfonc(fonact,'FROT_XFEM')
    lxczm = cfdisl(defico,'EXIS_XFEM_CZM')
!
! --- TABLEAU CONTENANT LES INFORMATIONS DIVERSES
!
    tabfin = resoco(1:14)//'.TABFIN'
    call wkvect(tabfin, 'V V R', ztabf*ntpc+1, jtabf)
    zr(jtabf) = ntpc
!
! --- PREPARATION CHAM_ELEM VIERGES
!
    call xmele1(noma, nomo, defico, ligrel, nfiss,&
                xindc0, 'PINDCOI', 'RIGI_CONT')
    if (lxczm) then
        call xmele1(noma, nomo, defico, ligrel, nfiss,&
                    xcohe0, 'PCOHES', 'RIGI_CONT')
    endif
    if (lxffm) then
        call xmele1(noma, nomo, defico, ligrel, nfiss,&
                    xseuc0, 'PSEUIL', 'RIGI_CONT')
    endif
!
    call jedema()
!
end subroutine
