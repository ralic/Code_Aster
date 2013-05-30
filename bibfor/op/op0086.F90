subroutine op0086()
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
    implicit none
!     COMMANDE:  MACR_ELEM_STAT
!
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/sschge.h'
    include 'asterfort/ssdege.h'
    include 'asterfort/ssmage.h'
    include 'asterfort/ssrige.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: nomu
    character(len=16) :: kbi1, kbi2
!
!
!-----------------------------------------------------------------------
    integer :: iarefm, iret, nocc
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomu, kbi1, kbi2)
!
!
!     --TRAITEMENT DES MOTS CLEFS 'DEFINITION' ET 'EXTERIEUR'
!     --------------------------------------------------------
    call getfac('DEFINITION', nocc)
    if (nocc .eq. 1) then
        call jeexin(nomu//'.REFM', iret)
        if (iret .gt. 0) then
            call u2mesk('F', 'SOUSTRUC_9', 1, nomu)
        else
            call ssdege(nomu)
        endif
    endif
!
!
!     --TRAITEMENT DU MOT CLEF 'RIGI_MECA'
!     --------------------------------------
    call getfac('RIGI_MECA', nocc)
    if (nocc .eq. 1) then
        call jeexin(nomu//'.REFM', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'SOUSTRUC_10')
        endif
        call jeveuo(nomu//'.REFM', 'L', iarefm)
        if (zk8(iarefm-1+6) .eq. 'OUI_RIGI') then
            call u2mesk('F', 'SOUSTRUC_11', 1, nomu)
        else
            call ssrige(nomu)
        endif
    endif
!
!
!     --TRAITEMENT DU MOT CLEF 'MASS_MECA':
!     --------------------------------------
    call getfac('MASS_MECA', nocc)
    if (nocc .eq. 1) then
        call jeexin(nomu//'.REFM', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'SOUSTRUC_12')
        endif
        call jeveuo(nomu//'.REFM', 'L', iarefm)
        if (zk8(iarefm-1+6) .ne. 'OUI_RIGI') then
            call u2mess('F', 'SOUSTRUC_12')
        endif
        if (zk8(iarefm-1+7) .eq. 'OUI_MASS') then
            call u2mess('F', 'SOUSTRUC_13')
        else
            call ssmage(nomu, 'MASS_MECA')
        endif
    endif
!
!
!     --TRAITEMENT DU MOT CLEF 'AMOR_MECA':
!     --------------------------------------
    call getfac('AMOR_MECA', nocc)
    if (nocc .eq. 1) then
        call jeexin(nomu//'.REFM', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'SOUSTRUC2_4')
        endif
        call jeveuo(nomu//'.REFM', 'L', iarefm)
        if (zk8(iarefm-1+8) .eq. 'OUI_AMOR') then
            call u2mesk('F', 'SOUSTRUC2_5', 1, nomu)
        else
            call ssmage(nomu, 'AMOR_MECA')
        endif
    endif
!
!
!     --TRAITEMENT DU MOT CLEF 'CAS_CHARGE'
!     --------------------------------------
    call getfac('CAS_CHARGE', nocc)
    if (nocc .gt. 0) then
        call jeexin(nomu//'.REFM', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'SOUSTRUC_14')
        endif
        call jeveuo(nomu//'.REFM', 'L', iarefm)
        if (zk8(iarefm-1+6) .ne. 'OUI_RIGI') then
            call u2mess('F', 'SOUSTRUC_14')
        endif
        call sschge(nomu)
    endif
!
!
    call jedema()
end subroutine
