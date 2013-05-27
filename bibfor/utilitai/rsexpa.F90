subroutine rsexpa(resu, icode, nompar, iret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsnopa.h'
    integer :: icode, iret
    character(len=*) :: resu, nompar
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: jacques.pellet at edf.fr
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
!      VERIFICATION DE L'EXISTANCE D'UN NOM DE PARAMETRE OU DE
!      VARIABLE D'ACCES DANS UN RESULTAT COMPOSE
! ----------------------------------------------------------------------
! IN  : RESU  : NOM DE LA SD_RESULTAT
! IN  : NOMPAR : NOM SYMBOLIQUE DU PARAMETRE OU VARIABLE D'ACCES DONT
!                ON DESIRE VERIFIER L'EXISTANCE
! IN  : ICODE  : CODE = 0 : VARIABLE D'ACCES
!                     = 1 : PARAMETRE
!                     = 2 : VARIABLE D'ACCES OU PARAMETRE
! OUT : IRET   : = 0  LE NOM SYMBOLIQUE N'EXISTE PAS
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: nbac, nbpa, lpout
!
!-----------------------------------------------------------------------
    integer :: ipa, ire1
!-----------------------------------------------------------------------
    call jemarq()
    iret=0
!
    call rsnopa(resu, icode, '&&RSEXPA.NOM_PAR', nbac, nbpa)
    call jeexin('&&RSEXPA.NOM_PAR', ire1)
    if (ire1 .gt. 0) call jeveuo('&&RSEXPA.NOM_PAR', 'E', lpout)
    if ((nbac+nbpa) .ne. 0) then
        do 10 ipa = 1, nbac+nbpa
            if (nompar .eq. zk16(lpout-1+ipa)) then
                iret=100
            endif
10      continue
    endif
!
    call jedetr('&&RSEXPA.NOM_PAR')
!
    call jedema()
end subroutine
