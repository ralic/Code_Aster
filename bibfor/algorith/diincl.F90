function diincl(sddisc, nomchz, force)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    logical :: diincl
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sddisc
    character(len=*) :: nomchz
    logical :: force
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - DISCRETISATION)
!
! INDIQUE S'IL FAUT ARCHIVER UN CHAMP
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NOMCHA : NOM DU CHAMP
! IN  FORCE  : INDIQUE SI ON FORCE L'ARCHIVAGE POUR LES CHAMPS EXCLUS
! OUT DIINCL : VRAI SI LE CHAMP DOIT ETRE SAUVE
!
!
!
!
    integer :: iret, nb, i
    character(len=8) :: k8bid
    character(len=16) :: nomcha
    character(len=19) :: sdarch
    character(len=24) :: arcexc
    integer :: jarexc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomcha = nomchz
!
! --- ACCES SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcexc = sdarch(1:19)//'.AEXC'
!
! --- AUCUN CHAMP N'EST EXCLU
!
    call jeexin(arcexc, iret)
    if (iret .eq. 0) then
        diincl = .true.
        goto 9999
    endif
!
! --- LE CHAMP EST-IL EXCLU ?
!
    diincl = .false.
    call jeveuo(arcexc, 'L', jarexc)
    call jelira(arcexc, 'LONMAX', nb, k8bid)
    do 10 i = 1, nb
        if (nomcha .eq. zk16(jarexc-1 + i)) then
            diincl = .false.
            goto 999
        endif
10  end do
    diincl = .true.
!
999  continue
!
! --- ON STOCKE LES CHAMPS EXCLUS SI ON FORCE L'ARCHIVAGE
!
    if (force) then
        diincl = .true.
    endif
!
9999  continue
!
    call jedema()
end function
