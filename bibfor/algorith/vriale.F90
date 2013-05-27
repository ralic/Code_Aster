subroutine vriale()
    implicit   none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT: VERIFICATIONS SYNTAXIQUES POUR
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/u2mess.h'
    integer :: ibid, nbamor, nbmode, nindex, nbindi, nbindj, nbcmpi, nbcmpj
    integer :: nnoeex, nvasex, ncmpex, nmost1, napexc
    real(kind=8) :: r8b, fremin, fremax
    character(len=4) :: excmod
    character(len=8) :: k8b, intrep
    character(len=16) :: tyconc, nomcmd, graexc
    integer :: iarg
!     ------------------------------------------------------------------
!
    call getres(intrep, tyconc, nomcmd)
!
!---NB MODES=NB AMORTISSEMENTS
!
    call getvr8('BASE_MODALE', 'AMOR_REDUIT', 1, iarg, 0,&
                r8b, nbamor)
    call getvis('BASE_MODALE', 'NUME_ORDRE', 1, iarg, 0,&
                ibid, nbmode)
    nbamor = -nbamor
    nbmode = -nbmode
    if (nbamor .ne. 0 .and. nbamor .ne. nbmode) then
        call u2mess('E', 'ALGORITH11_29')
    endif
!
    call getvtx('EXCIT', 'MODAL', 1, iarg, 1,&
                excmod, ibid)
    call getvis('EXCIT', 'NUME_ORDRE_I', 1, iarg, 0,&
                ibid, nindex)
    call getvtx('EXCIT', 'GRANDEUR', 1, iarg, 1,&
                graexc, ibid)
    call getvis('EXCIT', 'NUME_ORDRE_I', 1, iarg, 0,&
                ibid, nbindi)
    call getvis('EXCIT', 'NUME_ORDRE_J', 1, iarg, 0,&
                ibid, nbindj)
!
!--- COHERENCE ENTRE LES MODES ET L'INTERSPECTRE DE LA FONCTION
!                                  ACCEPTANCE
!
    nindex = -nindex
    if (excmod .eq. 'OUI' .and. nbmode .ne. nindex) then
        call u2mess('A', 'ALGORITH11_30')
    endif
!
!---NNOEEX=NINDEX OU NNOEEX=2*NINDEX
!
    if (nbindi .eq. 0) then
        call getvtx('EXCIT', 'NOEUD_I', 1, iarg, 0,&
                    k8b, nbindi)
        call getvtx('EXCIT', 'NOEUD_J', 1, iarg, 0,&
                    k8b, nbindj)
        call getvtx('EXCIT', 'NOM_CMP_I', 1, iarg, 0,&
                    k8b, nbcmpi)
        call getvtx('EXCIT', 'NOM_CMP_J', 1, iarg, 0,&
                    k8b, nbcmpj)
        if (nbcmpi .ne. nbcmpj) then
            call u2mess('E', 'PREPOST3_84')
        endif
        if (nbcmpi .ne. nbindi) then
            call u2mess('E', 'PREPOST3_85')
        endif
    endif
    if (nbindj .ne. nbindi) then
        call u2mess('E', 'ALGORITH11_31')
    endif
    nindex = -nbindi
!
    call getvtx('EXCIT', 'NOEUD', 1, iarg, 0,&
                k8b, nnoeex)
    nnoeex = -nnoeex
    if (nnoeex .ne. 0) then
        napexc = nnoeex
    else
        napexc=0
    endif
!
    call getvid('EXCIT', 'CHAM_NO', 1, iarg, 0,&
                k8b, nvasex)
    nvasex = -nvasex
    if (nvasex .ne. 0) then
        napexc = nvasex
        graexc = 'EFFO'
    endif
!
    if ((graexc.eq.'SOUR_PRESS') .or. (graexc.eq.'SOUR_FORCE')) then
        if (nnoeex .ne. 2*nindex) then
            call u2mess('E', 'ALGORITH11_32')
        endif
    else if ((napexc.ne.nindex).and.(excmod.eq.'NON')) then
        call u2mess('E', 'ALGORITH11_33')
    endif
!
!------NNOEEX=NCMPEX
!
    call getvtx('EXCIT', 'NOM_CMP', 1, iarg, 0,&
                k8b, ncmpex)
    ncmpex = -ncmpex
    if (nnoeex .ne. ncmpex) then
        call u2mess('E', 'ALGORITH11_34')
    endif
!
!---PRESENCE DE MODE STATIQUE QUAND ON EST EN DEPL IMPOSE
!
    call getvid(' ', 'MODE_STAT', 0, iarg, 0,&
                k8b, nmost1)
    if ((graexc.eq.'DEPL_R') .and. (nmost1.eq.0) .and. (nvasex.eq.0)) then
        call u2mess('E', 'ALGORITH11_35')
    else if ((graexc.ne.'DEPL_R').and.(nmost1.ne.0)) then
        call u2mess('E', 'ALGORITH11_36')
    endif
!
!---FREMIN < FREMAX
!
    call getvr8('REPONSE', 'FREQ_MIN', 1, iarg, 0,&
                r8b, ibid)
    if (ibid .ne. 0) then
        call getvr8('REPONSE', 'FREQ_MIN', 1, iarg, 1,&
                    fremin, ibid)
    endif
    call getvr8('REPONSE', 'FREQ_MAX', 1, iarg, 0,&
                r8b, ibid)
    if (ibid .ne. 0) then
        call getvr8('REPONSE', 'FREQ_MAX', 1, iarg, 1,&
                    fremax, ibid)
        if (fremin .ge. fremax) then
            call u2mess('E', 'ALGORITH11_37')
        endif
    endif
!
end subroutine
