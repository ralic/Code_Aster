subroutine nmdomt(method, parmet)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: method(*)
    real(kind=8) :: parmet(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (LECTURE)
!
! LECTURE DES DONNEES DE RESOLUTION
!
! ----------------------------------------------------------------------
!
!
! OUT METHOD  : DESCRIPTION DE LA METHODE DE RESOLUTION
!                 1 : NOM DE LA METHODE NON LINEAIRE (NEWTON OU IMPLEX)
!                     (NEWTON OU NEWTON_KRYLOV OU IMPLEX)
!                 2 : TYPE DE MATRICE (TANGENTE OU ELASTIQUE)
!                 3 : -- INUTILISE --
!                 4 : -- INUTILISE --
!                 5 : METHODE D'INITIALISATION
!                 6 : NOM CONCEPT EVOL_NOLI SI PREDICTION 'DEPL_CALCULE'
!                 7 : METHODE DE RECHERCHE LINEAIRE
! OUT PARMET  : PARAMETRES DE LA METHODE
!                 1 : REAC_INCR
!                 2 : REAC_ITER
!                 3 : PAS_MINI_ELAS
!                 4 : REAC_ITER_ELAS
!                 5 : ITER_LINE_MAXI
!                 6 : RESI_LINE_RELA
!                 7 : RHO_MIN
!                 8 : RHO_MAX
!                 9 : RHO_EXCL
!
! ----------------------------------------------------------------------
!
    integer :: reincr, reiter, reitel, itrlmx
    real(kind=8) :: pasmin, relirl
    real(kind=8) :: rhomin, rhomax, rhoexc
    integer :: ifm, niv
    integer :: iarg, ibid, iret, nocc
    character(len=16) :: relmet
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE DONNEES RESOLUTION'
    endif
!
! --- RECUPERATION DE LA METHODE DE RESOLUTION
!
    call getvtx(' ', 'METHODE', 1, iarg, 1,&
                method(1), ibid)
!
! --- INITIALISATIONS
!
    if ((method(1) .eq. 'NEWTON') .or. (method(1) .eq. 'NEWTON_KRYLOV')) then
!
        call getvtx('NEWTON', 'MATRICE', 1, iarg, 1,&
                    method(2), iret)
!
        call getvis('NEWTON', 'REAC_INCR', 1, iarg, 1,&
                    reincr, iret)
        if (reincr .lt. 0) then
            call assert(.false.)
        else
            parmet(1) = reincr
        endif
!
        call getvis('NEWTON', 'REAC_ITER', 1, iarg, 1,&
                    reiter, iret)
        if (reiter .lt. 0) then
            call assert(.false.)
        else
            parmet(2) = reiter
        endif
!
        call getvr8('NEWTON', 'PAS_MINI_ELAS', 1, iarg, 1,&
                    pasmin, iret)
        if (iret .le. 0) then
            parmet(3) = -9999.0d0
        else
            parmet(3) = pasmin
        endif
!
        call getvis('NEWTON', 'REAC_ITER_ELAS', 1, iarg, 1,&
                    reitel, iret)
        if (reiter .lt. 0) then
            call assert(.false.)
        else
            parmet(4) = reitel
        endif
!
        call getvtx('NEWTON', 'PREDICTION', 1, iarg, 1,&
                    method(5), iret)
        if (iret .le. 0) then
            method(5) = method(2)
        endif
!
        if (method(5) .eq. 'DEPL_CALCULE') then
            call getvid('NEWTON', 'EVOL_NOLI', 1, iarg, 1,&
                        method(6), iret)
            if (iret .le. 0) call u2mess('F', 'MECANONLINE5_45')
        endif
!
    else if (method(1) .eq. 'IMPLEX') then
        parmet(1) = 1
        method(5) = 'TANGENTE'
!
    else
        call assert(.false.)
    endif
!
! --- PARAMETRES DE LA RECHERCHE LINEAIRE
!
    relmet = 'CORDE'
    itrlmx = 0
    relirl = 1.d-3
    rhomin = 0.d0
    rhomax = 1.d0
    rhoexc = 0.d0
!
    call getfac('RECH_LINEAIRE', nocc)
    if (nocc .ne. 0) then
        call getvtx('RECH_LINEAIRE', 'METHODE', 1, iarg, 1,&
                    relmet, iret)
        call getvr8('RECH_LINEAIRE', 'RESI_LINE_RELA', 1, iarg, 1,&
                    relirl, iret)
        call getvis('RECH_LINEAIRE', 'ITER_LINE_MAXI', 1, iarg, 1,&
                    itrlmx, iret)
        call getvr8('RECH_LINEAIRE', 'RHO_MIN', 1, iarg, 1,&
                    rhomin, iret)
        call getvr8('RECH_LINEAIRE', 'RHO_MAX', 1, iarg, 1,&
                    rhomax, iret)
        call getvr8('RECH_LINEAIRE', 'RHO_EXCL', 1, iarg, 1,&
                    rhoexc, iret)
!
        if (rhomin .ge. -rhoexc .and. rhomin .le. rhoexc) then
            call u2mess('A', 'MECANONLINE5_46')
            rhomin = +rhoexc
        endif
        if (rhomax .ge. -rhoexc .and. rhomax .le. rhoexc) then
            call u2mess('A', 'MECANONLINE5_47')
            rhomax = -rhoexc
        endif
!
        if (rhomax .lt. rhomin) then
            call u2mess('A', 'MECANONLINE5_44')
            call getvr8('RECH_LINEAIRE', 'RHO_MIN', 1, iarg, 1,&
                        rhomax, iret)
            call getvr8('RECH_LINEAIRE', 'RHO_MAX', 1, iarg, 1,&
                        rhomin, iret)
        endif
!
        if (abs(rhomax-rhomin) .le. r8prem()) then
            call u2mess('F', 'MECANONLINE5_43')
        endif
!
    endif
!
    method(7) = relmet
    parmet(5) = itrlmx
    parmet(6) = relirl
    parmet(7) = rhomin
    parmet(8) = rhomax
    parmet(9) = rhoexc
!
end subroutine
