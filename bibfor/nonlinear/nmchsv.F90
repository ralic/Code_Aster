subroutine nmchsv(fonact, veasse, sddyna)
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
    include 'jeveux.h'
    include 'asterfort/copisd.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchex.h'
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=19) :: veasse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE - ALGORITHME)
!
! SAUVEGARDE DES VECTEURS SECONDS MEMBRES ET EFFORTS INTERIEURS
!
! ----------------------------------------------------------------------
!
!
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
!
!
!
!
    character(len=19) :: olfedo, olfsdo, oldido, oldidi, olfint
    character(len=19) :: olondp, ollapl, olcine
    character(len=19) :: cnfedo, cnfsdo, cndido, cndidi, cnfint
    character(len=19) :: cnondp, cnlapl, cncine
    logical :: londe, llapl, ldidi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    londe = ndynlo(sddyna,'ONDE_PLANE')
    llapl = isfonc(fonact,'LAPLACE')
    ldidi = isfonc(fonact,'DIDI')
!
! --- NOM DES CHAMPS PAS PRECEDENT
!
    call ndynkk(sddyna, 'OLDP_CNFEDO', olfedo)
    call ndynkk(sddyna, 'OLDP_CNFSDO', olfsdo)
    call ndynkk(sddyna, 'OLDP_CNDIDO', oldido)
    call ndynkk(sddyna, 'OLDP_CNDIDI', oldidi)
    call ndynkk(sddyna, 'OLDP_CNFINT', olfint)
    call ndynkk(sddyna, 'OLDP_CNONDP', olondp)
    call ndynkk(sddyna, 'OLDP_CNLAPL', ollapl)
    call ndynkk(sddyna, 'OLDP_CNCINE', olcine)
!
! --- NOM DES CHAMPS PAS COURANT
!
    call nmchex(veasse, 'VEASSE', 'CNFEDO', cnfedo)
    call nmchex(veasse, 'VEASSE', 'CNFSDO', cnfsdo)
    call nmchex(veasse, 'VEASSE', 'CNDIDO', cndido)
    call nmchex(veasse, 'VEASSE', 'CNDIDI', cndidi)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNONDP', cnondp)
    call nmchex(veasse, 'VEASSE', 'CNLAPL', cnlapl)
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- RECOPIE DES CHAMPS
!
    call copisd('CHAMP_GD', 'V', cnfint, olfint)
    call copisd('CHAMP_GD', 'V', cnfedo, olfedo)
    call copisd('CHAMP_GD', 'V', cnfsdo, olfsdo)
    call copisd('CHAMP_GD', 'V', cndido, oldido)
    call copisd('CHAMP_GD', 'V', cncine, olcine)
    if (londe) then
        call copisd('CHAMP_GD', 'V', cnondp, olondp)
    endif
    if (ldidi) then
        call copisd('CHAMP_GD', 'V', cndidi, oldidi)
    endif
    if (llapl) then
        call copisd('CHAMP_GD', 'V', cnlapl, ollapl)
    endif
!
    call jedema()
end subroutine
