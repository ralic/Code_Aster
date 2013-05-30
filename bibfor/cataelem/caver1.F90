subroutine caver1()
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
!
! ----------------------------------------------------------------------
!     BUT: VERIFIER LA COHERENCE DES OBJETS DES CATALOGUES.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/kndoub.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: opt, te
    logical :: error
    character(len=8) :: para, typmai
    character(len=16) :: nomopt, nomte
    character(len=24) :: valk(4)
    character(len=8) :: kbid, gd1, gd2, tgd1(10), tgd2(10), typout, typou2
!
!
!
!-----------------------------------------------------------------------
    integer :: iadesc, iamolo, ianblc, iaopmo, iaopno, iaopte, iapara
    integer :: icode, ier, igd, igdop, imolo, ioptte, ipara
    integer :: iret, itrou, jnbno, jnocm1, jnocm2, jtypma, k
    integer :: kk, lgco, n1, n2, nbgd, nbin, nbinte
    integer :: nbno, nbopt, nbout, nboute, nbpt1, nbpt2, nbte
    integer :: nbvol, nucalc
!-----------------------------------------------------------------------
    call jemarq()
    call jelira('&CATA.OP.NOMOPT', 'NOMMAX', nbopt, kbid)
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte, kbid)
    call jeveuo('&CATA.TE.OPTTE', 'L', iaopte)
    call jeveuo('&CATA.TE.NBLIGCOL', 'L', ianblc)
    lgco = zi(ianblc-1+1)
!
    ier = 0
!
    call jeveuo('&CATA.TE.TYPEMA', 'L', jtypma)
!
!
    do 40,opt = 1,nbopt
    call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iadesc)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iapara)
    nbin = zi(iadesc-1+2)
    nbout = zi(iadesc-1+3)
    nbvol = zi(iadesc-1+4)
    if (nbvol .ne. 0) then
        call u2mesk('E', 'CATAELEM_1', 1, nomopt)
        ier = ier + 1
    endif
!
!
!       -- ON VERIFIE QU'UNE OPTION N'A JAMAIS 2 PARAMETRES
!          DE MEME NOM
!          -------------------------------------------------
    call kndoub(8, zk8(iapara), nbin+nbout, iret)
    if (iret .gt. 0) call u2mesk('E', 'CATAELEM_2', 1, nomopt)
!
!
    do 30,te = 1,nbte
    call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
    ioptte = zi(iaopte-1+ (te-1)*lgco+opt)
    if (ioptte .eq. 0) goto 30
    call jeveuo(jexnum('&CATA.TE.OPTMOD', ioptte), 'L', iaopmo)
    call jeveuo(jexnum('&CATA.TE.OPTNOM', ioptte), 'L', iaopno)
    nucalc = zi(iaopmo-1+1)
    nbinte = zi(iaopmo-1+2)
!
    typmai = zk8(jtypma-1+te)
    call jeveuo(jexnom('&CATA.TM.NBNO', typmai), 'L', jnbno)
    nbno = zi(jnbno)
!
!              ON NE TRAQUE PAS LES ERREURS SI NUCALC=0,-1 OU -2
    if ((nucalc.le.0) .and. (nucalc.ge.-2)) goto 30
    do 10,ipara = 1,nbinte
    para = zk8(iaopno-1+ipara)
    imolo = zi(iaopmo-1+3+ipara)
    if (imolo .eq. 0) then
        valk(1) = para
        valk(2) = nomopt
        valk(3) = nomte
        call u2mesk('E', 'CATAELEM_3', 3, valk)
        ier = ier + 1
        goto 10
    endif
!
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', iamolo)
    igd = zi(iamolo-1+2)
    itrou = indik8(zk8(iapara-1+1),para,1,nbin)
    igdop = zi(iadesc-1+4+itrou)
    if ((itrou.eq.0) .or. (igdop.ne.igd)) then
        if (itrou .eq. 0) then
            valk(1) = para
            valk(2) = nomopt
            valk(3) = nomte
            call u2mesk('E', 'CATAELEM_4', 3, valk)
            ier = ier + 1
        endif
        if (igdop .ne. igd) then
            valk(1) = para
            valk(2) = nomopt
            valk(3) = nomte
            call u2mesk('E', 'CATAELEM_5', 3, valk)
            ier = ier + 1
        endif
    endif
!
!              -- ON VERIFIE QUE POUR LES MODE LOCAUX AUX NOEUDS
!                 LE NOMBRE DE NOEUDS EST LE NOMBRE DE NOEUDS DE
!                 LA MAILLE SUPPORT :
!              ---------------------------------------------------
    icode = zi(iamolo-1+1)
    nbpt2 = -1
    if (icode .eq. 2) then
        nbpt2 = mod(zi(iamolo-1+4),10000)
    else if (icode.eq.3) then
        nbpt1 = zi(iamolo-1+4)
        if (nbpt1 .lt. 0) then
            nbpt2 = mod(abs(nbpt1),10000)
        endif
    endif
    if (nbpt2 .ge. 0) then
        if (nbpt2 .ne. nbno) then
            valk(1) = para
            valk(2) = nomopt
            valk(3) = nomte
            call u2mesk('E', 'CATAELEM_6', 3, valk)
            ier = ier + 1
        endif
    endif
!
!
10  continue
!
!
!         -- VERIFICATION DES MODES LOCAUX "OUT" DES TE/OPTIONS:
!         ------------------------------------------------------
    nboute = zi(iaopmo-1+3)
    do 20,ipara = 1,nboute
    para = zk8(iaopno-1+nbinte+ipara)
    imolo = zi(iaopmo-1+3+nbinte+ipara)
    if (imolo .eq. 0) then
        valk(1) = para
        valk(2) = nomopt
        valk(3) = nomte
        call u2mesk('E', 'CATAELEM_3', 3, valk)
        ier = ier + 1
        goto 20
    endif
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', iamolo)
    igd = zi(iamolo-1+2)
    itrou = indik8(zk8(iapara-1+nbin+1),para,1,nbout)
    igdop = zi(iadesc-1+4+nbin+itrou)
    if ((itrou.eq.0) .or. (igdop.ne.igd)) then
        if (itrou .eq. 0) then
            valk(1) = para
            valk(2) = nomopt
            valk(3) = nomte
            call u2mesk('E', 'CATAELEM_4', 3, valk)
            ier = ier + 1
        endif
        if (igdop .ne. igd) then
            valk(1) = para
            valk(2) = nomopt
            valk(3) = nomte
            call u2mesk('E', 'CATAELEM_5', 3, valk)
            ier = ier + 1
        endif
    endif
!
!           -- ON VERIFIE QUE  LE TYPE DU CHAMP LOCAL EST COHERENT
!               AVEC CELUI DECLARE DANS L'OPTION :
!           ---------------------------------------------------
    icode = zi(iamolo-1+1)
    typou2=zk8(iapara-1+nbin+nbout+itrou)
    typout='????'
    if (icode .ge. 4) then
        typout='RESL__'
    else if (icode.eq.3) then
        typout='ELGA__'
    else if (icode.eq.2) then
        typout='ELNO__'
    else if (icode.eq.1) then
        typout='ELEM__'
    endif
!
    if (typout .ne. typou2) then
        valk(1) = para
        valk(2) = nomopt
        valk(3) = nomte
        valk(4) = typou2
        call u2mesk('E', 'CATAELEM_7', 4, valk)
!             IER = IER + 1
    endif
20  continue
!
30  continue
!
    40 end do
!
!    -- ON VERIFIE QUE CERTAINES GRANDEURS SONT "PARALLELES" :
!       ELLES DOIVENT AVOIR EXACTEMENT LES MEMES CMPS :
!    ----------------------------------------------------------
    nbgd = 2
    tgd1(1) = 'DEPL_R'
    tgd2(1) = 'DEPL_C'
    tgd1(2) = 'DEPL_R'
    tgd2(2) = 'DEPL_F'
    error = .false.
    do 90,k = 1,nbgd
    gd1 = tgd1(k)
    gd2 = tgd2(k)
    call jelira(jexnom('&CATA.GD.NOMCMP', gd1), 'LONMAX', n1, kbid)
    call jelira(jexnom('&CATA.GD.NOMCMP', gd2), 'LONMAX', n2, kbid)
    if (n1 .ne. n2) then
        error = .true.
    else
        call jeveuo(jexnom('&CATA.GD.NOMCMP', gd1), 'L', jnocm1)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', gd2), 'L', jnocm2)
        do 80,kk = 1,n1
        if (zk8(jnocm1-1+kk) .ne. zk8(jnocm1-1+kk)) error = .true.
80      continue
    endif
    if (error) then
        valk(1) = gd1
        valk(2) = gd2
        call u2mesk('E', 'CATAELEM_8', 2, valk)
        ier = ier + 1
    endif
    90 end do
!
!
    if (ier .gt. 0) then
        call u2mess('F', 'CATAELEM_9')
    endif
!
    call jedema()
end subroutine
