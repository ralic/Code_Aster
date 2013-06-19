subroutine ircmcc(idfimd, nomamd, nochmd, existc, ncmpve,&
                  ntncmp, ntucmp, codret)
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
! person_in_charge: nicolas.sellenet at edf.fr
!_______________________________________________________________________
!     ECRITURE D'UN CHAMP -  FORMAT MED - CREATION DU CHAMP
!        -  -       -               -     -           -
!_______________________________________________________________________
!     ENTREES :
!        IDFIMD : IDENTIFIANT DU FICHIER MED
!        NOCHMD : NOM MED DU CHAMP A ECRIRE
!        EXISTC : 0 : LE CHAMP EST INCONNU DANS LE FICHIER
!                >0 : LE CHAMP EST CREE AVEC :
!                 1 : LES COMPOSANTES VOULUES NE SONT PAS TOUTES
!                     ENREGISTREES
!                 2 : AUCUNE VALEUR POUR CE TYPE ET CE NUMERO D'ORDRE
!        NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!        NTNCMP : SD DES NOMS DES COMPOSANTES A ECRIRE (K16)
!        NTUCMP : SD DES UNITES DES COMPOSANTES A ECRIRE (K16)
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mfchac.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomamd
    character(len=*) :: nochmd
    character(len=*) :: ntncmp, ntucmp
!
    integer :: idfimd
    integer :: existc
    integer :: ncmpve
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: edfl64
    parameter (edfl64=6)
!
    integer :: adncmp, aducmp
    integer :: ifm, nivinf
    integer :: iaux
!
    character(len=8) :: saux08
!
!====
! 1. PREALABLES
!====
!
    call infniv(ifm, nivinf)
!
    if (existc .eq. 1) then
        call u2mesk('F', 'MED_31', 1, nochmd)
    endif
!
!====
! 2. CREATION DU CHAMP
!====
!
    if (existc .eq. 0) then
!
        if (nivinf .gt. 1) then
            write (ifm,2100) nochmd
        endif
        2100  format(2x,'DEMANDE DE CREATION DU CHAMP MED : ',a)
!
! 2.1. ==> ADRESSES DE LA DESCRIPTION DES COMPOSANTES
!
        call jeveuo(ntncmp, 'L', adncmp)
        call jeveuo(ntucmp, 'L', aducmp)
!
! 2.2. ==> APPEL DE LA ROUTINE MED
!
        call mfchac(idfimd, nochmd, nomamd, edfl64, zk16(adncmp),&
                    zk16(aducmp), ncmpve, codret)
!
        if (codret .ne. 0) then
            saux08='MFCHAC  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
! 2.3. ==> IMPRESSION D'INFORMATION
!
        if (nivinf .gt. 1) then
            if (ncmpve .eq. 1) then
                write (ifm,2301) zk16(adncmp)(1:8)
            else
                write (ifm,2302) ncmpve
                write (ifm,2303) (zk16(adncmp-1+iaux)(1:8),iaux=1,&
                ncmpve)
            endif
        endif
        2301  format(2x,'LE CHAMP MED EST CREE AVEC LA COMPOSANTE : ',a8)
        2302  format(2x,'LE CHAMP MED EST CREE AVEC ',i3,' COMPOSANTES :')
        2303  format(5(a8:,', '),:)
!
    endif
!
end subroutine
