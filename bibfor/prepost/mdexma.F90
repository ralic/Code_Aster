subroutine mdexma(nofimd, idfimd, nomamd, option, existm,&
                  ndim, codret)
!_____________________________________________________________________
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!        FORMAT MED : EXISTENCE D'UN MAILLAGE DANS UN FICHIER
!               - -   --             --
! ______________________________________________________________________
! .        .     .        .                                            .
! .  NOM   . E/S . TAILLE .           DESCRIPTION                      .
! .____________________________________________________________________.
! . NOFIMD .  E  .   1    . NOM DU FICHIER MED                         .
! . NOFIMD .  E  .   1    . OU NUMERO DU FICHIER DEJA OUVERT           .
! . NOMAMD .  E  .   1    . NOM DU MAILLAGE MED VOULU                  .
! . OPTION .  E  .   1    . QUE FAIT-ON SI LE MAILLAGE EST ABSENT :    .
! .        .     .        . 0 : RIEN DE SPECIAL                        .
! .        .     .        . 1 : ON IMPRIME LA LISTE DES MAILLAGES      .
! .        .     .        .     PRESENTS                               .
! . EXISTM .  S  .   1    . .TRUE. OU .FALSE., SELON QUE LE MAILLAGE   .
! .        .     .        . EST PRESENT OU NON                         .
! . NDIM   .  S  .   1    . LA DIMENSION DU MAILLAGE QUAND IL EXISTE   .
! . CODRET .  S  .   1    . CODE DE RETOUR DES MODULES                 .
! ______________________________________________________________________
!
!====
! 0. DECLARATIONS ET DIMENSIONNEMENT
!====
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/lxlgut.h'
    include 'asterfort/mfferm.h'
    include 'asterfort/mfmaai.h'
    include 'asterfort/mfnmaa.h'
    include 'asterfort/mfouvr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: nofimd, nomamd
!
    logical :: existm, ficexi, dejouv
!
    integer :: option, ndim, codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: edlect
    parameter (edlect=0)
    integer :: ednstr
    parameter (ednstr=0)
!
!
    integer :: lnomam
    integer :: idfimd, nbmaie
    integer :: iaux, jaux, kaux, tyaux
    integer :: vali(2)
!
    character(len=8) :: saux08
    character(len=64) :: noma64
    character(len=64) :: saux64
    character(len=200) :: daux
    character(len=24) :: valk
! ______________________________________________________________________
!
!====
! 1. ON OUVRE LE FICHIER EN LECTURE
!    ON PART DU PRINCIPE QUE SI ON N'A PAS PU OUVRIR, C'EST QUE LE
!    FICHIER N'EXISTE PAS, DONC SANS MAILLAGE A FORTIORI
!====
!
    existm = .false.
    codret = 0
    inquire(file=nofimd,exist=ficexi)
!
    if (.not. ficexi) then
!
        existm = .false.
        codret = 0
!
    else
        if (idfimd .eq. 0) then
            call mfouvr(idfimd, nofimd, edlect, iaux)
            dejouv = .false.
        else
            dejouv = .true.
            iaux = 0
        endif
        if (iaux .eq. 0) then
!
!====
! 2. LE MAILLAGE EST-IL PRESENT ?
!====
!
! 2.1. ==> COMBIEN DE MAILLAGES DANS LE FICHIER
!
            call mfnmaa(idfimd, nbmaie, codret)
            if (codret .ne. 0) then
                saux08='MFNMAA  '
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
!
! 2.2. ==> RECHERCHE DU NUMERO ET DE LA DIMENSION DU MAILLAGE VOULU
!
!
!               12345678901234567890123456789012
            noma64 = '                                  '//'                    '
            lnomam = lxlgut(nomamd)
            noma64(1:lnomam) = nomamd(1:lnomam)
!
            do 22 , iaux = 1 , nbmaie
!
!               12345678901234567890123456789012
            saux64 = '                                '//'                      '
            call mfmaai(idfimd, iaux, saux64, kaux, tyaux,&
                        daux, codret)
            if (codret .ne. 0) then
                saux08='MFMAAI  '
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
            if (tyaux .ne. ednstr) then
                call u2mess('A', 'MED_79')
            endif
!
            jaux = lxlgut(saux64)
!
            if (jaux .eq. lnomam) then
                if (saux64 .eq. noma64) then
                    ndim = kaux
                    existm = .true.
                    goto 221
                endif
            endif
!
            22         end do
!
            existm = .false.
!
! 2.3. ==> IMPRESSION EVENTUELLE DES MAILLAGES PRESENTS
!
            if (option .ne. 0) then
!
                valk = nofimd
                vali (1) = nbmaie
                call u2mesg('A+', 'MED_88', 1, valk, 1,&
                            vali, 0, 0.d0)
                do 23 , iaux = 1 , nbmaie
!                   12345678901234567890123456789012
                saux64 = '                                '//'                  '
                call mfmaai(idfimd, iaux, saux64, kaux, tyaux,&
                            daux, codret)
                jaux = lxlgut(saux64)
                valk = saux64(1:jaux)
                call u2mesg('A+', 'MED_85', 1, valk, 0,&
                            0, 0, 0.d0)
                if (tyaux .ne. ednstr) then
                    call u2mess('A', 'MED_79')
                endif
23              continue
                call u2mesk('A', 'MED_80', 1, noma64(1:lnomam))
!
            endif
!
221          continue
!
! 2.3. ==> FERMETURE DU FICHIER
!
            if (.not.dejouv) then
                call mfferm(idfimd, codret)
                if (codret .ne. 0) then
                    saux08='MFFERM  '
                    call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                                codret, 0, 0.d0)
                endif
                idfimd = 0
            endif
!
        endif
    endif
!
end subroutine
