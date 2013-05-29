subroutine nmdopo(sddyna, method, sdpost)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmcrpx.h'
    include 'asterfort/nmcrsd.h'
    include 'asterfort/nmecsd.h'
    include 'asterfort/omega2.h'
    include 'asterfort/wkvect.h'
    character(len=16) :: method(*)
    character(len=19) :: sddyna, sdpost
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (LECTURE)
!
! LECTURE DES DONNEES DE POST-TRAITEMENT (CRIT_STAB ET MODE_VIBR)
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  METHOD : DESCRIPTION DE LA METHODE DE RESOLUTION
!                1 : NOM DE LA METHODE NON LINEAIRE (NEWTON OU IMPLEX)
!                2 : TYPE DE MATRICE (TANGENTE OU ELASTIQUE)
!                3 : PAS UTILISE
!                4 : PAS UTILISE
!                5 : METHODE D'INITIALISATION
!                6 : NOM CONCEPT EVOL_NOLI SI PREDI. 'DEPL_CALCULE'
! OUT SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
!
!
!
!
    integer :: maxddl
    parameter   (maxddl=40)
!
    integer :: iflamb, imvibr, iret, iocc, nddle, ibid, numord, nsta
    integer :: jpexcl, jpstab
    integer :: ifm, niv
    logical :: ldyna, lstat, limpl
    logical :: lflam, lmvib
    character(len=16) :: option, optmod, optrig, modrig, opmrig
    character(len=16) :: matrig, motfac, motpas, typmat, ngeo, k16bid, ddlexc
    character(len=16) :: dlstab, sign
    character(len=24) :: k24bid
    integer :: nfreq, cdsp
    real(kind=8) :: bande(2), r8bid, freqr, csta, prec
    character(len=19) :: nomlis, vibmod, flamod, stamod
    character(len=1) :: base
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE DONNEES POST-TRAITEMENT'
    endif
!
! --- INITIALISATIONS
!
    lflam = .false.
    lmvib = .false.
    motpas = 'PAS_CALC'
    iocc = 1
    base = 'V'
    option = ' '
    optmod = ' '
    optrig = ' '
    opmrig = ' '
    sign = ' '
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lstat = ndynlo(sddyna,'STATIQUE')
    limpl = ndynlo(sddyna,'IMPLICITE')
!
! --- CREATION DE SDPOST
!
    call nmcrsd('POST_TRAITEMENT', sdpost)
!
! --- PRESENCE DES MOTS-CLEFS - FLAMBEMENT OU STABILITE
!
    call getfac('CRIT_STAB', iflamb)
    call assert(iflamb.le.1)
    if (iflamb .ne. 0) then
        call nmecsd('POST_TRAITEMENT', sdpost, 'CRIT_STAB', ibid, r8bid,&
                    k24bid)
        lflam = .true.
    endif
!
! --- PRESENCE DES MOTS-CLEFS - MODES VIBRATOIRES
!
    if (ldyna) then
        call getfac('MODE_VIBR', imvibr)
        call assert(imvibr.le.1)
        if (imvibr .ne. 0) then
            call nmecsd('POST_TRAITEMENT', sdpost, 'MODE_VIBR', ibid, r8bid,&
                        k24bid)
            lmvib = .true.
        endif
    else
        lmvib = .false.
    endif
!
! --- NOM DES OPTIONS DE CALCUL
!
    if (lflam) then
        if (lstat) then
            option = 'FLAMBSTA'
        else if (limpl) then
            option = 'FLAMBDYN'
        else
            call assert(.false.)
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'OPTION_CALCUL_FLAMB', ibid, r8bid,&
                    option)
    endif
!
    if (lmvib) then
        if (lstat) then
            call assert(.false.)
        else if (limpl) then
            option = 'VIBRDYNA'
        else
            call assert(.false.)
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'OPTION_CALCUL_VIBR', ibid, r8bid,&
                    option)
    endif
!
! --- OPTIONS POUR LE CALCUL DE MODES VIBRATOIRES
!
    if (lmvib) then
        motfac = 'MODE_VIBR'
!
! ----- TYPE DE MATRICE DE RIGIDITE
!
        call getvtx(motfac, 'MATR_RIGI', iocc, iarg, 1,&
                    matrig, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'TYPE_MATR_VIBR', ibid, r8bid,&
                    matrig)
!
! ----- NOMBRE DE FREQUENCES
!
        call getvis(motfac, 'NB_FREQ', iocc, iarg, 1,&
                    nfreq, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'NB_FREQ_VIBR', nfreq, r8bid,&
                    k24bid)
!
! ----- DIMENSION SOUS-ESPACE
!
        call getvis(motfac, 'COEF_DIM_ESPACE', iocc, iarg, 1,&
                    cdsp, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'COEF_DIM_VIBR', cdsp, r8bid,&
                    k24bid)
!
! ----- BANDE DE RECH. DE FREQ.
!
        call getvr8(motfac, 'BANDE', iocc, iarg, 2,&
                    bande, iret)
        if (iret .eq. 0) then
            optmod = 'PLUS_PETITE'
        else
            optmod = 'BANDE'
            call nmecsd('POST_TRAITEMENT', sdpost, 'BANDE_VIBR_1', ibid, omega2(bande(1)),&
                        k24bid)
            call nmecsd('POST_TRAITEMENT', sdpost, 'BANDE_VIBR_2', ibid, omega2(bande(2)),&
                        k24bid)
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'OPTION_EXTR_VIBR', ibid, r8bid,&
                    optmod)
!
! ----- FREQUENCE DE CALCUL
!
        nomlis = sdpost(1:14)//'.VIBR'
        call nmcrpx(motfac, motpas, iocc, nomlis, base)
    endif
!
! --- OPTIONS POUR LE CALCUL DE FLAMBEMENT
!
    if (lflam) then
        motfac = 'CRIT_STAB'
!
! ----- TYPE DE MATRICE DE RIGIDITE
!
        typmat = method(5)
        call nmecsd('POST_TRAITEMENT', sdpost, 'TYPE_MATR_FLAMB', ibid, r8bid,&
                    typmat)
!
! ----- NOMBRE DE FREQUENCES
!
        call getvis(motfac, 'NB_FREQ', iocc, iarg, 1,&
                    nfreq, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'NB_FREQ_FLAMB', nfreq, r8bid,&
                    k24bid)
!
! ----- DIMENSION SOUS-ESPACE
!
        call getvis(motfac, 'COEF_DIM_ESPACE', iocc, iarg, 1,&
                    cdsp, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'COEF_DIM_FLAMB', cdsp, r8bid,&
                    k24bid)
!
! ----- BANDE DE RECH. DE FREQ.
!
        call getvr8(motfac, 'CHAR_CRIT', iocc, iarg, 2,&
                    bande, iret)
        if (iarg .eq. 0) then
            optmod = 'BANDE'
        else
            optmod = 'PLUS_PETITE'
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'BANDE_FLAMB_1', ibid, bande(1),&
                    k24bid)
        call nmecsd('POST_TRAITEMENT', sdpost, 'BANDE_FLAMB_2', ibid, bande(2),&
                    k24bid)
        call nmecsd('POST_TRAITEMENT', sdpost, 'OPTION_EXTR_FLAM', ibid, r8bid,&
                    optmod)
!
! ----- PRISE EN COMPTE MATRICE RIGIDITE GEOMETRIQUE OU PAS
!
        call getvtx(motfac, 'RIGI_GEOM', iocc, iarg, 1,&
                    ngeo, iret)
        if (ngeo .eq. 'NON') then
            optrig = 'RIGI_GEOM_NON'
        else if (ngeo.eq.'OUI') then
            optrig = 'RIGI_GEOM_OUI'
        else
            call assert(.false.)
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'RIGI_GEOM_FLAMB', ibid, r8bid,&
                    optrig)
!
! ----- EXCLUSION DE CERTAINS DDLS  ET MODIFICATION RIGIDITE
!
        call getvtx(motfac, 'DDL_EXCLUS', iocc, iarg, 0,&
                    k16bid, nddle)
        nddle = -nddle
        call nmecsd('POST_TRAITEMENT', sdpost, 'NB_DDL_EXCLUS', nddle, r8bid,&
                    k24bid)
        if (nddle .ne. 0) then
            if (nddle .le. maxddl) then
                ddlexc = sdpost(1:14)//'.EXCL'
                call wkvect(ddlexc, 'V V K8', nddle, jpexcl)
                call getvtx(motfac, 'DDL_EXCLUS', iocc, iarg, nddle,&
                            zk8(jpexcl), iret)
                call nmecsd('POST_TRAITEMENT', sdpost, 'NOM_DDL_EXCLUS', ibid, r8bid,&
                            ddlexc)
            else
                call assert(.false.)
            endif
        endif
!
! ----- ETUDE DE STABILITE
!
        call getvtx(motfac, 'DDL_STAB', iocc, iarg, 0,&
                    k16bid, nsta)
        nsta = -nsta
        call nmecsd('POST_TRAITEMENT', sdpost, 'NB_DDL_STAB', nsta, r8bid,&
                    k24bid)
        if (nsta .ne. 0) then
            if (nsta .le. maxddl) then
                dlstab = sdpost(1:14)//'.STAB'
                call wkvect(dlstab, 'V V K8', nsta, jpstab)
                call getvtx(motfac, 'DDL_STAB', iocc, iarg, nsta,&
                            zk8(jpstab), iret)
                call nmecsd('POST_TRAITEMENT', sdpost, 'NOM_DDL_STAB', ibid, r8bid,&
                            dlstab)
            else
                call assert(.false.)
            endif
        endif
        call getvtx(motfac, 'MODI_RIGI', iocc, iarg, 1,&
                    modrig, iret)
        if (modrig(1:3) .eq. 'OUI') then
            opmrig = 'MODI_RIGI_OUI'
        else if (modrig(1:3).eq.'NON') then
            opmrig = 'MODI_RIGI_NON'
        else
            call assert(.false.)
        endif
        call nmecsd('POST_TRAITEMENT', sdpost, 'MODI_RIGI', ibid, r8bid,&
                    opmrig)
        call getvr8(motfac, 'PREC_INSTAB', iocc, iarg, 1,&
                    prec, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'PREC_INSTAB', ibid, prec,&
                    k24bid)
        call getvtx(motfac, 'SIGNE', iocc, iarg, 1,&
                    sign, iret)
        call nmecsd('POST_TRAITEMENT', sdpost, 'SIGN_INSTAB', ibid, r8bid,&
                    sign)
!
!
! ----- FREQUENCE DE CALCUL
!
        nomlis = sdpost(1:14)//'.FLAM'
        call nmcrpx(motfac, motpas, iocc, nomlis, base)
    endif
!
! --- INITIALISATIONS
!
    numord = -1
    freqr = r8vide()
    csta = r8vide()
    vibmod = '&&NMDOPO.VIBMOD'
    flamod = '&&NMDOPO.FLAMOD'
    stamod = '&&NMDOPO.STAMOD'
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_VIBR', ibid, freqr,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_VIBR', numord, r8bid,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_VIBR', ibid, r8bid,&
                vibmod)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_FLAM', ibid, freqr,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_FLAM', numord, r8bid,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_FLAM', ibid, r8bid,&
                flamod)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_STAB', ibid, csta,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_NUME_STAB', numord, r8bid,&
                k24bid)
    call nmecsd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_STAB', ibid, r8bid,&
                stamod)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        if (lmvib) then
            write (ifm,*) '<MECANONLINE> ...... MODES VIBRATOIRES'
        else if (lflam) then
            write (ifm,*) '<MECANONLINE> ...... MODES DE FLAMBEMENT'
        else
            write (ifm,*) '<MECANONLINE> ...... RIEN'
        endif
    endif
!
    call jedema()
end subroutine
