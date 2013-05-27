subroutine op0076()
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
    implicit none
!     RECUPERE LES CHAMPS GENERALISES (DEPL, VITE, ACCE) D'UN CONCEPT
!     TRAN_GENE.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/extrac.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/zxtrac.h'
    real(kind=8) :: temps, prec, freq
    character(len=8) :: nomres, trange, basemo, nomcha, interp, crit
    character(len=16) :: concep, nomcmd
    character(len=24) :: nddlge
    character(len=1) :: k1bid
    integer :: iarg
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadesc, iarefe, idcham, iddesc, idinst, idrefe, idvecg
    integer :: ierd, n1, nbinst, nbmode, nt, nf, ni
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomres, concep, nomcmd)
!
!     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
!
    call getvid(' ', 'RESU_GENE', 0, iarg, 1,&
                trange, n1)
    call getvtx(' ', 'NOM_CHAM', 0, iarg, 1,&
                nomcha, n1)
    call getvr8(' ', 'INST', 0, iarg, 1,&
                temps, nt)
    call getvr8(' ', 'FREQ', 0, iarg, 1,&
                freq, nf)
    call getvtx(' ', 'INTERPOL', 0, iarg, 1,&
                interp, ni)
    call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                crit, n1)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                prec, n1)
!
    if (ni .eq. 0) interp(1:3) = 'NON'
!
!     --- RECUPERATION DES INFORMATIONS MODALES ---
!
    call jeveuo(trange//'           .DESC', 'L', iadesc)
    call jeveuo(trange//'           .REFD', 'L', iarefe)
    call jeveuo(trange//'           .DISC', 'L', idinst)
    call jelira(trange//'           .DISC', 'LONMAX', nbinst, k1bid)
    call jeveuo(trange//'           .'//nomcha(1:4), 'L', idcham)
!
!     --- RECUPERATION DE LA NUMEROTATION GENERALISEE NUME_DDL_GENE
    nddlge = zk24(iarefe+3)(1:8)
!     --- RECUPERATION DE LA BASE MODALE ET LE NOMBRE DE MODES
    basemo = zk24(iarefe+4)(1:8)
    nbmode = zi(iadesc+1)
!
    call wkvect(nomres//'           .REFE', 'G V K24', 2, idrefe)
    call wkvect(nomres//'           .DESC', 'G V I', 2, iddesc)
    call jeecra(nomres//'           .DESC', 'DOCU', 0, 'VGEN')
!
    zi(iddesc) = 1
    zi(iddesc+1) = nbmode
!
    zk24(idrefe) = basemo
    zk24(idrefe+1) = nddlge
!
!     --- CAS DU TRAN_GENE
    if (zi(iadesc) .ne. 4) then
!
!       ---  ON PLANTE SI LE MOT CLE DEMANDE EST FREQ POUR UN TRAN_GENE
        if (nf .ne. 0) call u2mess('E', 'ALGORITH9_51')
!
!       --- CREATION DU VECT_ASSE_GENE RESULTAT ---
        call wkvect(nomres//'           .VALE', 'G V R', nbmode, idvecg)
!
!       --- RECUPERATION DU CHAMP ---
        call extrac(interp, prec, crit, nbinst, zr(idinst),&
                    temps, zr( idcham), nbmode, zr(idvecg), ierd)
!
        if (ierd .ne. 0) then
            call u2mess('E', 'ALGORITH9_49')
        endif
!
! --- CAS DU HARM_GENE
!
    else
!       ---  ON PLANTE SI LE MOT CLE DEMANDE EST INST POUR UN HARM_GENE
        if (nt .ne. 0) call u2mess('E', 'ALGORITH9_52')
!
!       --- CREATION DU VECT_ASSE_GENE RESULTAT ---
        call wkvect(nomres//'           .VALE', 'G V C', nbmode, idvecg)
!
!       --- RECUPERATION DU CHAMP ---
        call zxtrac(interp, prec, crit, nbinst, zr(idinst),&
                    freq, zc(idcham), nbmode, zc(idvecg), ierd)
!
        if (ierd .ne. 0) then
            call u2mess('E', 'ALGORITH9_50')
        endif
!
    endif
!
    call jedema()
end subroutine
