subroutine dfllty(sdlist, metlis, dtmin)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfllli.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/nmarnr.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/tbexve.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: sdlist
    character(len=16) :: metlis
    real(kind=8) :: dtmin
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DU TYPE DE CONSTRUCTION DE LA LISTE D'INSTANTS
!
! MOT-CLEF DEFI_LIST
!
! ----------------------------------------------------------------------
!
! CONSTRUCTION DE SDLIST//'.LIST.INFOR'
!
!     ZR(JLINR-1 + 1)  <===> 'METHODE' = 1 SI 'MANUEL'
!                                      = 2 SI 'AUTO'
!     ZR(JLINR-1 + 2)  <===> 'PAS_MINI
!     ZR(JLINR-1 + 3)  <===> 'PAS_MAXI'
!     ZR(JLINR-1 + 4)  <===> 'NB_PAS_MAX'
!     ZR(JLINR-1 + 5)  <===> DTMIN
!     ZR(JLINR-1 + 6)  <===> DT ACTUEL (VOIR NMCRLI...)
!     ZR(JLINR-1 + 7)  <===> REDECOUPE SI DIVE_ERRE (POUR CRESOL)
!     ZR(JLINR-1 + 8)  <===> NBINST
!     ZR(JLINR-1 + 9)  <===> NECHEC
!     ZR(JLINR-1 + 10) <===> NADAPT
!
! IN  SDLIST : NOM DE LA SD RESULTAT
! OUT METLIS : NOM DE LA METHODE DE GESTION DE LA LISTE D'INSTANTS
! OUT DTMIN  : INTERVALLE DE TEMPS MINIMUM SUR LA LISTE
!
! ----------------------------------------------------------------------
!
    character(len=16) :: motfac
    character(len=16) :: modetp
    integer :: llinr
    character(len=24) :: lisifr
    integer :: jlinr
    character(len=19) :: lisins, tablpc, lisres
    integer :: nbinst, nadapt
    integer :: jinst, jditr, jlisre
    integer :: n1, n2, nval, i, iret, ibid, n3, j
    real(kind=8) :: pasmin, pasmax, pas0, r8b, dt
    integer :: nbpamx, nbdec
    integer :: iarg, numrep
    character(len=8) :: resu
    character(len=2) :: type
    logical :: exist
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    motfac = 'DEFI_LIST'
    metlis = ' '
    dtmin = 0.d0
!
! --- TAILLE DES VECTEURS
!
    llinr = dfllvd('LLINR')
!
! --- CONSTRUCTION DE LA SD
!
    lisifr = sdlist(1:8)//'.LIST.INFOR'
    call wkvect(lisifr, 'G V R', llinr, jlinr)
!
! --- LECTURE METHODE
!
    call getvtx(motfac, 'METHODE', 1, iarg, 1,&
                metlis, ibid)
!
! --- METHODE DE CONSTRUCTION DE LA LISTE D'INSTANT
!
    if (metlis .eq. 'MANUEL') then
        zr(jlinr-1+1)= 1.d0
    else if (metlis.eq.'AUTO') then
        zr(jlinr-1+1)= 2.d0
    else
        call assert(.false.)
    endif
!
! --- RECUPERATION DE LA LISTE D'INSTANTS FOURNIE
!
    call getvid(motfac, 'LIST_INST', 1, iarg, 1,&
                lisins, n1)
    call getvr8(motfac, 'VALE', 1, iarg, 0,&
                r8b, n2)
    call getvid(motfac, 'RESULTAT', 1, iarg, 1,&
                resu, n3)
!
!     CAS OU ON A RENSEIGNE VALE : ON RECONSTRUIT UNE SD LISTE
    if (n2 .ne. 0) then
        lisins = '&&DFLLTY.LIST_INST'
        nval = -n2
        call wkvect(lisins//'.VALE', 'V V R', nval, jinst)
        call getvr8(motfac, 'VALE', 1, iarg, nval,&
                    zr(jinst), n2)
    endif
!
!     CAS OU ON A RENSEIGNE RESU : ON RECONSTRUIT UNE LISTE PLUS FINE
    if (n3 .ne. 0) then
!
!       VERIF QUE LA COLONNE 'INST' EXISTE BIEN DANS LA TABLE
        call ltnotb(resu, 'PARA_CALC', tablpc)
        call tbexip(tablpc, 'INST', exist, type)
        if (.not.exist .or. type .ne. 'R') then
            call u2mesk('F', 'DISCRETISATION_3', 1, resu)
        endif
!
        call nmarnr(resu, 'PARA_CALC', numrep)
!
        if (numrep .gt. 1) then
            call u2mesk('F', 'DISCRETISATION_4', 1, resu)
        endif
!
!       EXTRACTION DE LA COLONNE 'INST' DANS UN OBJET TEMPORAIRE
!       NVAL EST LE NOMBRE D'INSTANTS CALCULES DANS LA SD_RESULTAT
        lisres = '&&DFLLTY.RESU_INST'
        call tbexve(tablpc, 'INST', lisres, 'V', nval,&
                    type)
        call jeveuo(lisres, 'L', jlisre)
!
!       RECUPERATION DU NOMBRE DE DECOUPE
        call getvis(motfac, 'SUBD_PAS', 1, iarg, 1,&
                    nbdec, iret)
        call assert(iret.ne.0)
        call assert(nbdec.gt.0)
!
!       CREATION DE LA LISTE "NBDEC" FOIS PLUS FINE
        lisins = '&&DFLLTY.LIST_INST'
        call wkvect(lisins//'.VALE', 'V V R', nbdec*(nval-1)+1, jinst)
        do 10 i = 1, nval-1
            do 11 j = 1, nbdec
                dt = zr(jlisre-1+i+1)-zr(jlisre-1+i)
                zr(jinst-1+nbdec*(i-1)+j) = zr(jlisre-1+i) + dt*(j-1)/ nbdec
11          continue
10      continue
        zr(jinst-1+nbdec*(nval-1)+1)=zr(jlisre-1+nval)
!
    endif
!
! --- VERIFICATIONS LISTE D'INSTANTS (CROISSANCE, TAILLE, ETC.)
!
    call dfllli(lisins, dtmin, nbinst)
!
! --- ACCES LISTE INSTANTS
!
    call jeveuo(lisins//'.VALE', 'L', jinst)
!
! --- CREATION LISTE D'INSTANTS DANS SDLIST
!
    call wkvect(sdlist//'.LIST.DITR', 'G V R', nbinst, jditr)
    do 20 i = 1, nbinst
        zr(jditr-1+i) = zr(jinst-1+i)
20  end do
!
! --- INTERVALLE MINIMUM + NOMBRE INSTANTS STOCKES
!
    zr(jlinr-1+5) = dtmin
    zr(jlinr-1+8) = nbinst
!
! --- A CAUSE D IMPLEX, ON VA RECUPERER DE SUITE LE MODE DE CALCUL
! --- DE T+
!
    if (metlis .eq. 'AUTO') then
        call getfac('ADAPTATION', nadapt)
        call getvtx('ADAPTATION', 'MODE_CALCUL_TPLUS', nadapt, iarg, 1,&
                    modetp, ibid)
        if (nadapt .ne. 1 .and. modetp .eq. 'IMPLEX') then
            call u2mess('F', 'DISCRETISATION_15')
        endif
    endif
!
! --- PARAMETRES DE LA METHODE AUTOMATIQUE
!
    if (metlis .eq. 'AUTO') then
!
        call getvr8(motfac, 'PAS_MAXI', 1, iarg, 1,&
                    pasmax, iret)
        if (iret .eq. 0) pasmax = zr(jditr-1+nbinst) - zr(jditr-1+1)
!
! ----- CAS D'IMPLEX
!
        if (modetp .eq. 'IMPLEX') then
            pas0 = zr(jinst+1)-zr(jinst)
            pasmin = pas0/1000
            if (iret .eq. 0) pasmax = pas0*10
        else
!         PASMIN = CELLE DE VAL_MIN DE PAS_MINI (DEFI_LIST_INST.CAPY)
            pasmin = 1.d-12
            if (iret .eq. 0) pasmax = zr(jditr-1+nbinst) - zr(jditr-1+1)
        endif
!
        call getvr8(motfac, 'PAS_MINI', 1, iarg, 1,&
                    pasmin, iret)
        if (pasmin .gt. dtmin) call u2mess('F', 'DISCRETISATION_1')
!
        call getvis(motfac, 'NB_PAS_MAXI', 1, iarg, 1,&
                    nbpamx, iret)
!
        zr(jlinr-1+2) = pasmin
        zr(jlinr-1+3) = pasmax
        zr(jlinr-1+4) = nbpamx
    endif
!
    call jedema()
end subroutine
