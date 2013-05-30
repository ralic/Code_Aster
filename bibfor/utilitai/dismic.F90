subroutine dismic(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(INCONNU)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsdocu.h'
    integer :: repi, ierd
    character(len=19) :: nomob
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE "INCONNU"(K19)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=4) :: docu
!
!
!-----------------------------------------------------------------------
    integer :: ibid, ire1, ire2, ire3, ire4, ire5, ire6
    integer :: ire7, iret, jpro
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
!
    if (questi(1:8) .eq. 'RESULTAT') then
        call jeexin(nomob//'.NOVA', ire3)
        call jeexin(nomob//'.DESC', ire4)
        call jeexin(nomob//'.ORDR', ire5)
        call jeexin(nomob//'.TAVA', ire6)
        call jeexin(nomob//'.TACH', ire7)
        if (ire3 .gt. 0 .and. ire4 .gt. 0 .and. ire5 .gt. 0 .and. ire6 .gt. 0 .and. ire7&
            .gt. 0) then
            repk = 'OUI'
            goto 9999
        endif
!
    else if (questi(1:5) .eq. 'TABLE') then
        call jeexin(nomob//'.TBBA', ire3)
        call jeexin(nomob//'.TBNP', ire4)
        call jeexin(nomob//'.TBLP', ire5)
        if (ire3 .gt. 0 .and. ire4 .gt. 0 .and. ire5 .gt. 0) then
            repk = 'OUI'
            goto 9999
        endif
!
    else if (questi(1:7) .eq. 'CHAM_NO') then
        call jeexin(nomob//'.DESC', iret)
        if (iret .gt. 0) then
            call jelira(nomob//'.DESC', 'DOCU', ibid, docu)
            if (docu .eq. 'CHNO') then
                repk = 'OUI'
                goto 9999
            endif
        endif
!
    else if (questi(1:9) .eq. 'CHAM_ELEM') then
        call jeexin(nomob//'.CELD', iret)
        if (iret .gt. 0) then
            repk = 'OUI'
            goto 9999
        endif
!
    else if (questi(1:4).eq.'TYPE') then
        call jeexin(nomob//'.TYPE', ire1)
        call jeexin(nomob//'.NOPA', ire2)
        call jeexin(nomob//'.NOVA', ire3)
        if (ire1 .gt. 0 .and. ire2 .gt. 0 .and. ire3 .gt. 0) then
            repk = 'TABLE'
            goto 9999
        endif
!
        call jeexin(nomob//'.DESC', ire1)
        if (ire1 .gt. 0) then
            call jelira(nomob//'.DESC', 'DOCU', ibid, docu)
            if (docu .eq. 'CHNO') then
                repk = 'CHAM_NO'
                goto 9999
            else
                call rsdocu(docu, repk, iret)
                if (iret .ne. 0) ierd=1
                goto 9999
            endif
        endif
!
        call jeexin(nomob//'.CELD', ire1)
        if (ire1 .gt. 0) then
            repk='CHAM_ELEM'
            goto 9999
        endif
!
        call jeexin(nomob//'.PROL', ire1)
        if (ire1 .gt. 0) then
            call jeveuo(nomob//'.PROL', 'L', jpro)
            if (zk24(jpro) .eq. 'CONSTANTE' .or. zk24(jpro) .eq. 'FONCTION' .or. zk24(jpro)&
                .eq. 'NAPPE' .or. zk24(jpro) .eq. 'FONCT_C') then
                repk='FONCTION'
                goto 9999
            endif
        endif
!
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
