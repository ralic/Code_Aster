subroutine nmsigi(ligrmo, compor, sigini)
!
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterfort/calcul.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=24) :: ligrmo, compor
    character(len=19) :: sigini
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! RECUPERATION D'UN CHARGEMENT MECANIQUE DE TYPE PRECONTRAINTE
!
! ----------------------------------------------------------------------
!
!
! IN  LIGRMO : NOM DU LIGREL DU MODELE
! IN  COMPOR : NOM DE LA CARTE DE COMPORTEMENT
! I/O SIGINI : NOM DU CHAMP DE CONTRAINTE INITIAL VIERGE.
!                    ON LUI ADDITIONNE LES EVENTUELS CHAMPS :
!                    LCHAR(I)//.CHME.SIGIN'
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=2)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: ibid, iocc, iret, nbocc
    character(len=8) :: charge
    character(len=16) :: option
    character(len=19) :: sigcha
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call getfac('EXCIT', nbocc)
    if (nbocc .gt. 0) then
        option = 'ADD_SIGM'
        lchin(1) = sigini
        lpain(1) = 'PEPCON1'
        lpain(2) = 'PEPCON2'
        lchout(1) = '&&NMSIGI.PEPCON3'
        lpaout(1) = 'PEPCON3'
        do 10 iocc = 1, nbocc
            call getvid('EXCIT', 'CHARGE', iocc, iarg, 1,&
                        charge, ibid)
            sigcha = charge//'.CHME.SIGIN'
            call exisd('CHAMP_GD', sigcha, iret)
            if (iret .gt. 0) then
                lchin(2) = sigcha
                call copisd('CHAM_ELEM_S', 'V', compor, lchout(1))
                call calcul('S', option, ligrmo, 2, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call detrsd('CHAM_ELEM_S', lchout(1))
                call copisd('CHAMP_GD', 'V', '&&NMSIGI.PEPCON3', sigini)
            endif
10      continue
        call detrsd('CHAMP_GD', '&&NMSIGI.PEPCON3')
    endif
!
    call jedema()
!.
end subroutine
