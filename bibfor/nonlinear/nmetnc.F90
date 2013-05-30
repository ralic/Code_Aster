subroutine nmetnc(sdieto, icham, nomcha)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: sdieto
    character(len=*) :: nomcha
    integer :: icham
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! RETOURNE LE NOM DU CHAMP DANS L'OPERATEUR
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  ICHAM  : INDEX DU CHAMP DANS SDIETO
! OUT NOMCHA : NOM DU CHAMP DANS L'OPERATEUR
!
! ----------------------------------------------------------------------
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: zioch
    character(len=24) :: nomchx
    character(len=6) :: tychap, tyvari
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'L', jiolch)
    zioch = zi(jioinf+4-1)
!
! --- NOM DU CHAMP DANS L'OPERATEUR
!
    nomchx = zk24(jiolch+zioch*(icham-1)+6-1)
!
! --- NOM DU CHAMP A STOCKER
!
    if (nomchx(1:5) .eq. 'CHAP#') then
        tychap = nomchx(6:11)
        tyvari = nomchx(13:18)
        if (tychap .eq. 'VALINC') then
            if (tyvari .eq. 'TEMP') then
                nomcha = '&&NXLECTVAR_____'
            else
                nomcha = '&&NMCH1P.'//tyvari
            endif
        else if (tychap.eq.'VEASSE') then
            nomcha = '&&NMCH5P.'//tyvari
        else
            call assert(.false.)
        endif
    else
        nomcha = nomchx
    endif
!
    call jedema()
end subroutine
