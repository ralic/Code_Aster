subroutine nmetpl(sdieto)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: sdieto
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! CHANGEMENT DU NOM DU CHAMP DANS L'OPERATEUR
!
! ----------------------------------------------------------------------
!
! CETTE ROUTINE TRANSFORME LES VARIABLES CHAPEAUX DE TYPE "MOI"
!  (INSTANT MOINS)
! EN VARIABLE DE TYPE "PLU" (INSTANT PLUS)
!
! IN  SDIETO : SD GESTION IN ET OUT
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: nbcham, zioch
    integer :: icham
    character(len=24) :: nomchx, newchx
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
    call jeveuo(iolcha, 'E', jiolch)
    zioch = zi(jioinf+4-1)
    nbcham = zi(jioinf+1-1)
!
! --- BOUCLE SUR LES CHAMPS A LIRE
!
    do 10 icham = 1, nbcham
        nomchx = zk24(jiolch+zioch*(icham-1)+6-1)
        if (nomchx(1:5) .eq. 'CHAP#') then
            tychap = nomchx(6:11)
            if (tychap .eq. 'VALINC') then
                tyvari = nomchx(13:18)
                if (tyvari .eq. 'TEMP') call assert(.false.)
                newchx = nomchx
                newchx(16:18) = 'PLU'
                zk24(jiolch+zioch*(icham-1)+6-1) = newchx
            endif
        endif
10  end do
!
    call jedema()
end subroutine
