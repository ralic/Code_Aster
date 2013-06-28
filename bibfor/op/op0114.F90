subroutine op0114()
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
! person_in_charge: nicolas.greffet at edf.fr
! aslint: disable=W1304
    implicit   none
!  ----- OPERATEUR RECU_PARA_YACS             --------------------------
!  RECUPERATION DE VALEURS D'INITIALISATION POUR COUPLAGE IFS VIA YACS
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/cpedb.h'
    include 'asterc/cpldb.h'
    include 'asterc/cplen.h'
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liimpr.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: ifm, niv, nbvale, ndim, jpas, jnbp, jbor, jval, i
    character(len=19) :: resu
    character(len=16) :: nomcmd, concep
!     ------------------------------------------------------------------
!     COUPLAGE =>
    integer(kind=4) :: lenvar, cpiter, numpa4, taille, eyacs, ibid4
    integer :: icompo, ibid, numpas, iadr
    parameter (lenvar = 144)
    parameter (cpiter= 41)
    real(kind=4) :: ti4, tf4
    real(kind=8) :: dt, ryacs, ti, tf
    character(len=16) :: option, valk(3)
    character(len=24) :: ayacs
    character(len=lenvar) :: nomvar
    integer :: iarg
!     COUPLAGE <=
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    ayacs='&ADR_YACS'
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!
    call getres(resu, concep, nomcmd)
!
    call getvtx(' ', 'DONNEES', 0, iarg, 1,&
                option, ibid)
    if (niv .eq. 2) then
        valk(1) = 'OP0114'
        valk(2) = 'DONNEES'
        valk(3) = option
        call u2mesk('I+', 'COUPLAGEIFS_11', 3, valk)
    endif
!
    nbvale = 7
    if (( option(1:3) .eq. 'FIN' ) .or. ( option(1:4) .eq. 'CONV' ) .or.&
        ( option(1:3) .eq. 'PAS' )) nbvale = 1
!
    ndim = max(1,nbvale-1)
    call wkvect(resu//'.LPAS', 'G V R', ndim, jpas)
    call wkvect(resu//'.NBPA', 'G V I', ndim, jnbp)
    call wkvect(resu//'.BINT', 'G V R', nbvale, jbor)
    call wkvect(resu//'.VALE', 'G V R', nbvale, jval)
!
    if (option(1:4) .eq. 'INIT') then
        call getvr8(' ', 'PAS', 0, iarg, 1,&
                    dt, ibid)
! reception des parametres utilisateurs a l iteration 0
        numpa4 = 0
        numpas = 0
        ti = 0.d0
        tf = 0.d0
        ti4 = 0.d0
        tf4 = 0.d0
!
        do 10 i = 1, nbvale-1
            zr(jpas+i-1) = 0.1d0
            zi(jnbp+i-1) = 1
10      continue
        zr(jpas) = 0.1d0
        zi(jnbp) = 1
!  RECEPTION NOMBRE DE PAS DE TEMPS
        nomvar = 'NBPDTM'
        call cplen(icompo, cpiter, ti4, tf4, numpa4,&
                   nomvar, int(1,4), taille, eyacs, ibid4)
        zr(jbor) = eyacs
        zr(jval) = eyacs
!  RECEPTION NOMBRE DE SOUS-ITERATIONS
        nomvar = 'NBSSIT'
        call cplen(icompo, cpiter, ti4, tf4, numpa4,&
                   nomvar, int(1,4), taille, eyacs, ibid4)
        zr(jbor+1) = eyacs
        zr(jval+1) = eyacs
!  RECEPTION EPSILON
        nomvar = 'EPSILO'
        call cpldb(icompo, cpiter, ti, tf, numpa4,&
                   nomvar, int(1,4), taille, ryacs, ibid4)
        zr(jbor+2) = ryacs
        zr(jval+2) = ryacs
        nomvar = 'ISYNCP'
        call cplen(icompo, cpiter, ti4, tf4, numpa4,&
                   nomvar, int(1,4), taille, eyacs, ibid4)
        zr(jbor+3) = eyacs
        zr(jval+3) = eyacs
        nomvar = 'NTCHRO'
        call cplen(icompo, cpiter, ti4, tf4, numpa4,&
                   nomvar, int(1,4), taille, eyacs, ibid4)
        zr(jbor+4) = eyacs
        zr(jval+4) = eyacs
        nomvar = 'TTINIT'
        call cpldb(icompo, cpiter, ti, tf, numpa4,&
                   nomvar, int(1,4), taille, ryacs, ibid4)
        zr(jbor+5) = ryacs
        zr(jval+5) = ryacs
!  RECEPTION PAS DE TEMPS DE REFERENCE
        nomvar = 'PDTREF'
        call cpldb(icompo, cpiter, ti, tf, numpa4,&
                   nomvar, int(1,4), taille, ryacs, ibid4)
        zr(jbor+6) = ryacs
        zr(jval+6) = ryacs
    else
        call getvr8(' ', 'INST', 0, iarg, 1,&
                    tf, ibid)
        call getvis(' ', 'NUME_ORDRE_YACS', 0, iarg, 1,&
                    numpas, ibid)
        numpa4 = numpas
        call getvr8(' ', 'PAS', 0, iarg, 1,&
                    dt, ibid)
        ti = tf
        if (option(1:3) .eq. 'FIN') then
            nomvar = 'END'
            ryacs = 0.d0
        else
            if (option(1:4) .eq. 'CONV') then
                nomvar = 'ICVAST'
                call cplen(icompo, cpiter, ti4, tf4, numpa4,&
                           nomvar, int(1,4), taille, eyacs, ibid4)
                ryacs = eyacs
            else
                nomvar = 'DTAST'
                call cpedb(icompo, cpiter, ti, numpa4, nomvar,&
                           1, dt, ibid4)
                nomvar = 'DTCALC'
                call cpldb(icompo, cpiter, ti, tf, numpa4,&
                           nomvar, int(1,4), taille, ryacs, ibid4)
            endif
        endif
        zr(jbor) = ryacs
        zr(jval) = ryacs
        zr(jpas) = 0.1d0
        zi(jnbp) = 1
!
    endif
!     --- TITRE ---
    call titre()
!     --- IMPRESSION ---
    if (niv .gt. 1) call liimpr(resu, niv, 'MESSAGE')
!
    call jedema()
end subroutine
