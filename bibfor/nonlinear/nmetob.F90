subroutine nmetob(sdieto, nomcha, icham)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: sdieto, nomcha
    integer :: icham
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! RETOURNE L'INDICE DU CHAMP OBSERVABLE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  NOMCHA : NOM DU CHAMP
! OUT ICHAM  : INDEX DU CHAMP DANS SDIETO
!              0 SI NON OBSERVABLE
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: nbcham, zioch
    integer :: ich
    character(len=24) :: motcob
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    icham = 0
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'L', jiolch)
    nbcham = zi(jioinf+1-1)
    zioch = zi(jioinf+4-1)
!
! --- CHAMP OBSERVABLE ?
!
    do 10 ich = 1, nbcham
        motcob = zk24(jiolch+zioch*(ich-1)+10-1)
        if (motcob .eq. nomcha) icham = ich
10  end do
!
    call jedema()
end subroutine
