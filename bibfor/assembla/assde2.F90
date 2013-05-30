subroutine assde2(champ)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assde1.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: champ
! ----------------------------------------------------------------------
!    PENDANT DE ASSDE1, ROUTINE DE NETTOYAGE ADAPTEE A FETI POUR LIMITER
!     LES STOCKAGE D'INFO DANS LES SD RESULTATS.
!     IN:
!       NOMU   : NOM D'UN CONCEPT DE TYPE
!                    CHAMP_GD(K19)
!
!     RESULTAT:
!     ON DETRUIT TOUS LES OBJETS JEVEUX FILS AU SENS FETI DU TERME
!     (C'EST A DIRE POINTES PAR UN .FETC) CORRESPONDANT A CE CONCEPT.
!    LE CONCEPT INITIAL EST LAISSE INTACT SAUF SON .FETC QUI EST AUSSI
!    DETRUIT.
! ----------------------------------------------------------------------
!
!
    integer :: iret, idd, nbsd, ifetc, ilimpi
    character(len=8) :: k8bid
    character(len=24) :: k24b, k24b1
    character(len=19) :: champ2
! -DEB------------------------------------------------------------------
    champ2 = champ(1:19)
!
! DESTRUCTION DE LA LISTE DE CHAM_NO LOCAUX SI FETI
    k24b=champ2//'.FETC'
    call jeexin(k24b, iret)
! FETI OR NOT ?
    if (iret .ne. 0) then
        call jelira(k24b, 'LONMAX', nbsd, k8bid)
        call jeveuo(k24b, 'L', ifetc)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        do 5 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                k24b1=zk24(ifetc+idd-1)
                call assde1(k24b1)
            endif
 5      continue
        call jedetr(k24b)
    endif
!
end subroutine
