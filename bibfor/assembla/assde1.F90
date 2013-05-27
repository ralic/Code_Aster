subroutine assde1(champ)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
    include 'asterfort/chlici.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: champ
! ----------------------------------------------------------------------
!     IN:
!       NOMU   : NOM D'UN CONCEPT DE TYPE
!                    CHAMP_GD(K19)
!
!     RESULTAT:
!     ON DETRUIT TOUS LES OBJETS JEVEUX CORRESPONDANT A CE CONCEPT.
! ----------------------------------------------------------------------
!
!
    integer :: iret, idd, nbsd, ifetc, ilimpi
    character(len=5) :: refe, vale, desc
    character(len=8) :: k8bid
    character(len=19) :: k19b
    character(len=24) :: k24b
    character(len=19) :: champ2
    logical :: dbg
! -DEB------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    champ2 = champ
!
    dbg=.true.
    dbg=.false.
    if (dbg) call chlici(champ2, 19)
!
!
!
!        POUR LES CARTE, CHAM_NO, CHAM_ELEM, ET RESU_ELEM :
    call jedetr(champ2//'.CELD')
    call jedetr(champ2//'.CELV')
    call jedetr(champ2//'.CELK')
    call jedetr(champ2//'.DESC')
    call jedetr(champ2//'.VALE')
    call jedetr(champ2//'.REFE')
    call jedetr(champ2//'.LIMA')
    call jedetr(champ2//'.NOMA')
    call jedetr(champ2//'.NOLI')
    call jedetr(champ2//'.RESL')
    call jedetr(champ2//'.RSVI')
    call jedetr(champ2//'.VALV')
    call jedetr(champ2//'.NCMP')
    call jedetr(champ2//'.PTMA')
    call jedetr(champ2//'.PTMS')
!
! DESTRUCTION DE LA LISTE DE CHAM_NO LOCAUX SI FETI
    k24b=champ2//'.FETC'
    call jeexin(k24b, iret)
! FETI OR NOT ?
    if (iret .gt. 0) then
        desc='.DESC'
        refe='.REFE'
        vale='.VALE'
        call jelira(k24b, 'LONMAX', nbsd, k8bid)
        call jeveuo(k24b, 'L', ifetc)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        do 5 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                k19b=zk24(ifetc+idd-1)(1:19)
                call jedetr(k19b//desc)
                call jedetr(k19b//refe)
                call jedetr(k19b//vale)
            endif
 5      continue
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        call jedetr(k24b)
    endif
!
end subroutine
