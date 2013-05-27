subroutine lrcmpr(idfimd, nomprf, ntproa, lgproa, codret)
!_____________________________________________________________________
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
! ======================================================================
!     LECTURE D'UN CHAMP - FORMAT MED - PROFIL
!     -    -       -              -     --
!-----------------------------------------------------------------------
!      ENTREES:
!       IDFIMD : IDENTIFIANT DU FICHIER MED
!       NOMPRF : NOM MED DU PROFIL A LIRE
!      SORTIES:
!       NTPROA : TABLEAU QUI CONTIENT LE PROFIL ASTER
!       LGPROA : LONGUEUR DU PROFIL ASTER
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/mfnpfl.h'
    include 'asterfort/mfpfll.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    integer :: idfimd
    integer :: lgproa
    integer :: codret
!
    character(len=*) :: nomprf
    character(len=*) :: ntproa
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    character(len=8) :: saux08
    parameter ( nompro = 'LRCMPR' )
!
    integer :: ifm, nivinf
!
    integer :: adproa, adprom
    integer :: lgprom
    integer :: iaux
!
    character(len=24) :: ntprom
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
    1001 format(/,10('='),a,10('='),/)
!
! 1.2. ==> NOMS DES TABLEAUX DE TRAVAIL
!               12   345678   9012345678901234
    ntprom = '&&'//nompro//'.PROFIL_MED     '
!
!====
! 2. NOMBRE DE VALEURS LIEES AU PROFIL
!====
!
    call mfnpfl(idfimd, nomprf, lgprom, codret)
    if (codret .ne. 0) then
        saux08='MFNPFL  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,4101) nomprf, lgprom
    endif
    4101 format('. LECTURE DU PROFIL : ',a,&
     &     /,'... LONGUEUR : ',i8)
!
!====
! 3. LECTURE DES VALEURS DU PROFIL MED
!====
!
    call wkvect(ntprom, 'V V I', lgprom, adprom)
!
    call mfpfll(idfimd, zi(adprom), lgprom, nomprf, codret)
    if (codret .ne. 0) then
        saux08='MFPFLL  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
    if (nivinf .gt. 1) then
        if (lgprom .ge. 10) then
            write (ifm,4201) zi(adprom), zi(adprom+1), zi(adprom+2)
            write (ifm,4202) zi(adprom+lgprom-3), zi(adprom+lgprom-2),&
            zi(adprom+lgprom-1)
        else
            write (ifm,4203) (zi(adprom+iaux),iaux=0,lgprom-1)
        endif
    endif
    4201 format('... 3 1ERES VALEURS     : ',3i8)
    4202 format('... 3 DERNIERES VALEURS : ',3i8)
    4203 format('... VALEURS : ',10i8)
!
!====
! 4. TRANSFERT EN UN PROFIL ASTER
!====
!          EN FAIT, DANS LE CAS DES NOEUDS, IL Y A IDENTITE ENTRE LES
!          DEUX CAR ON NE RENUMEROTE PAS LES NOEUDS (CF IRMMNO)
!
    lgproa = lgprom
    call wkvect(ntproa, 'V V I', lgproa, adproa)
!
    do 41 , iaux = 0 , lgprom-1
    zi(adproa+iaux) = zi(adprom+iaux)
    41 end do
!
    call jedetr(ntprom)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
