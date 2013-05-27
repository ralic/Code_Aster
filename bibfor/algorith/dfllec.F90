subroutine dfllec(sdlist, dtmin)
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
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dfdevn.h'
    include 'asterfort/dfllac.h'
    include 'asterfort/dfllne.h'
    include 'asterfort/dfllpe.h'
    include 'asterfort/dfllsv.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: sdlist
    real(kind=8) :: dtmin
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES ECHECS
!
! MOT-CLEF ECHEC
!
! ----------------------------------------------------------------------
!
! IN  SDLIST : NOM DE LA SD RESULTAT
!     CONSTRUCTION DE SDLIST//'.ECHE.EVENR'
!     CONSTRUCTION DE SDLIST//'.ECHE.EVENK'
!     CONSTRUCTION DE SDLIST//'.ECHE.SUBDR'
! IN  DTMIN  : INTERVALLE DE TEMPS MINIMUM SUR LA LISTE
!
!
!
!
    integer :: nbordr
    parameter    (nbordr = 6)
    character(len=16) :: evdord(nbordr)
    integer :: lisord(nbordr)
    logical :: oblord(nbordr)
!
    character(len=16) :: mcfact
    integer :: ibid
    character(len=16) :: nocham, nocmp, cricmp
    real(kind=8) :: valere, pasmin
    integer :: nerreu
    integer :: nechec, neche2
    integer :: iechec, iordr, isauve
    character(len=24) :: lisifr
    integer :: jlinr
    character(len=16) :: even, action, evd
    character(len=16) :: submet, subaut
    integer :: nbrpas, niveau
    real(kind=8) :: nivmax, nivear, pcplus, penmax
    real(kind=8) :: cmmaxi, delcol, durdec
    character(len=24) :: lisevr, lisevk, lisesu
    integer :: leevr, leevk, lesur
    integer :: jeevr, jeevk, jesur
    logical :: loblig, lsave
    integer :: jtrav, ilast, iplus
    integer :: iarg
!
    data evdord  /'ERRE'     ,'DELTA_GRANDEUR'  ,&
     &              'COLLISION','INTERPENETRATION',&
     &              'DIVE_RESI','INSTABILITE'/
    data oblord  /.true. ,.false.,&
     &              .false.,.false.,&
     &              .false.,.false./
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    mcfact = 'ECHEC'
    nerreu = 0
    nechec = 0
    neche2 = 0
    nivmax = 0.d0
    do 10 iordr = 1, nbordr
        lisord(iordr) = 0
10  end do
    lisifr = sdlist(1:8)//'.LIST.INFOR'
    call jeveuo(lisifr, 'E', jlinr)
!
! --- TAILLE DES VECTEURS
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! --- DECOMPTE DES OCCURRENCES MOT-CLEF ECHEC
!
    call dfllne(mcfact, nechec, nerreu)
    neche2 = nechec
!
! --- L'EVENEMENT 'ERREUR' EST OBLIGATOIRE
!
    if (nerreu .eq. 0) then
        neche2 = neche2 + 1
    endif
    call wkvect('&&DFLLEC.TRAV', 'V V I', neche2, jtrav)
!
! --- CREATION SD
!
    lisevr = sdlist(1:8)//'.ECHE.EVENR'
    lisevk = sdlist(1:8)//'.ECHE.EVENK'
    lisesu = sdlist(1:8)//'.ECHE.SUBDR'
    call wkvect(lisevr, 'G V R', neche2*leevr, jeevr)
    call wkvect(lisevk, 'G V K16', neche2*leevk, jeevk)
    call wkvect(lisesu, 'G V R', neche2*lesur, jesur)
    zr(jlinr-1+9) = neche2
!
! --- CREATION DES EVENEMENTS DANS LEUR ORDRE DE PRIORITE
!
    ilast = 1
    do 100 iordr = 1, nbordr
        evd = evdord(iordr)
        loblig = oblord(iordr)
        do 101 iechec = 1, nechec
            call getvtx(mcfact, 'EVENEMENT', iechec, iarg, 1,&
                        even, ibid)
            if (loblig) then
                if (even .eq. 'ERREUR') then
                    lisord(iordr) = iechec
                endif
            else
                if (even .eq. evd) then
                    lisord(iordr) = iechec
                    if (even .eq. 'DELTA_GRANDEUR') then
                        zi(jtrav+ilast-1) = iechec
                        ilast = ilast + 1
                    endif
                endif
            endif
101      continue
100  end do
!
! --- TRAITEMENT DES OCCURRENCES
!
    isauve = 0
    iplus = 0
    ilast = 1
    do 110 iordr = 1, nbordr
!
! ----- OCCURRENCE DE ECHEC
!
        iechec = lisord(iordr)
        evd = evdord(iordr)
        loblig = oblord(iordr)
!
! ----- EVENEMENT A CREER
!
157      continue
        if (iechec .eq. 0) then
            if (loblig) then
                even = evd
                iplus = 0
                isauve = isauve + 1
            endif
        else
            even = evd
            if (even .eq. 'DELTA_GRANDEUR') then
                iplus = zi(jtrav+ilast-1)
                iechec = iplus
                if (iplus .eq. 0) then
                    iechec = 0
                else
                    isauve = isauve + 1
                    ilast = ilast + 1
                endif
            else
                isauve = isauve+ 1
            endif
        endif
!
! ----- PARAMETRES DE L'ACTION
!
        lsave = .false.
        if (iechec .eq. 0) then
            if (loblig) then
!
! --------- VALEURS PAR DEFAUT POUR UNE DECOUPE MANUELLE
!
                call dfdevn(action, submet, pasmin, nbrpas, niveau)
                lsave = .true.
            endif
        else
!
! ------- LECTURE DES PARAMETRES DE L'EVENEMENT
!
            call dfllpe(mcfact, iechec, even, penmax, nocham,&
                        nocmp, cricmp, valere)
!
! ------- LECTURE DES PARAMETRES DE L'ACTION
!
            call dfllac(mcfact, iechec, dtmin, even, action,&
                        submet, subaut, pasmin, nbrpas, niveau,&
                        pcplus, cmmaxi, delcol, durdec)
            lsave = .true.
        endif
!
! ----- SAUVEGARDE DES INFORMATIONS
!
        if (lsave) then
            call dfllsv(lisifr, lisevr, lisevk, lisesu, isauve,&
                        even, action, submet, subaut, pasmin,&
                        nbrpas, niveau, pcplus, cmmaxi, delcol,&
                        durdec, penmax, cricmp, valere, nocham,&
                        nocmp)
        endif
        if (iplus .ne. 0) goto 157
!
110  end do
!
! --- TRAITEMENT PARTICULIER DU MOT-CLE 'SUBD_NIVEAU' QUI EST EN FAIT
! --- UN MOT-CLE GLOBAL A TOUTE LES METHODES DE SOUS-DECOUPAGE
! --- ON PREND LE MAX SUR TOUS LES EVENEMENTS
!
    do 120 iechec = 1, neche2
        nivear = zr(jesur-1+lesur*(iechec-1)+4)
        nivmax = max(nivear,nivmax)
120  end do
!
! --- ENREGISTREMENT DU NIVEAU MAX
!
    do 130 iechec = 1, neche2
        zr(jesur-1+lesur*(iechec-1)+4) = nivmax
130  end do
!
    call jedetr('&&DFLLEC.TRAV')
    call jedema()
end subroutine
