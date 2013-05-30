subroutine lrcmle(idfimd, nochmd, nbcmfi, nbvato, numpt,&
                  numord, typent, typgeo, ntvale, nomprf,&
                  codret)
!_____________________________________________________________________
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
! person_in_charge: nicolas.sellenet at edf.fr
! ======================================================================
!     LECTURE D'UN CHAMP - FORMAT MED - LECTURE
!     -    -       -              -     --
!-----------------------------------------------------------------------
!      ENTREES:
!        IDFIMD : IDENTIFIANT DU FICHIER MED
!        NOCHMD : NOM MED DU CHAMP A LIRE
!        NBCMFI : NOMBRE DE COMPOSANTES DANS LE FICHIER      .
!        NBVATO : NOMBRE DE VALEURS TOTAL (VALEUR DE MFNVAL)
!        NUMPT  : NUMERO DE PAS DE TEMPS
!        NUMORD : NUMERO D'ORDRE DU CHAMP
!        TYPENT : TYPE D'ENTITE AU SENS MED
!        TYPGEO : TYPE DE SUPPORT AU SENS MED
!      SORTIES:
!        NOMPRF : NOM DU PROFIL ASSOCIE
!        NTVALE : TABLEAU QUI CONTIENT LES VALEURS LUES
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/infniv.h'
    include 'asterfort/mfchrl.h'
    include 'asterfort/mfnnop.h'
    include 'asterfort/mfprlo.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    integer :: idfimd
    integer :: nbcmfi, nbvato, numpt, numord
    integer :: typent, typgeo
    integer :: codret
!
    character(len=*) :: nochmd, nomprf
    character(len=64) :: nomloc
    character(len=*) :: ntvale
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: edfuin
    parameter (edfuin=0)
    integer :: edall
    parameter (edall=0)
    integer :: edcomp
    parameter (edcomp=2)
    integer :: iterma
    parameter (iterma=1)
!
    integer :: ifm, nivinf
!
    integer :: advale, iprof, nbprof, n, taipro, nip
!
    character(len=8) :: saux08
    character(len=64) :: nomamd
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
        write(ifm,*) 'NB CMP :',nbcmfi,'  NB TOTAL VALEURS :',nbvato
    endif
!
!====
! 2. LECTURE DU NOM DU PROFIL
!====
!
    call mfprlo(idfimd, nochmd, numpt, numord, typent,&
                typgeo, iterma, nomamd, nomprf, nomloc,&
                nbprof, codret)
    do 10, iprof = 1, nbprof
    call mfnnop(idfimd, nochmd, typent, typgeo, nomamd,&
                numpt, numord, iprof, nomprf, edcomp,&
                taipro, nomloc, nip, n, codret)
    10 end do
!
!====
! 3. LECTURE DU CHAMP DANS UN TABLEAU TEMPORAIRE
!====
!
    call wkvect(ntvale, 'V V R', nbcmfi*nbvato, advale)
!
    call mfchrl(idfimd, nochmd, zr(advale), edfuin, edall,&
                nomprf, edcomp, typent, typgeo, numpt,&
                numord, codret)
!
    if (codret .ne. 0) then
        saux08='MFCHRL  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
end subroutine
