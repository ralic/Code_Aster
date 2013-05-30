subroutine rvlieu(mailla, typco, courbe, nlsnac, sdlieu)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/codent.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rvabsc.h'
    include 'asterfort/rvnchm.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: nlsnac, sdlieu
    character(len=8) :: typco, courbe, mailla
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     GENERATION DE LA SD LIEU (MAILLAGE DU LIEU)
!     ------------------------------------------------------------------
! IN  COURBE : K : NOM DU CONCEPT COURBE
! IN  MAILLA : K : NOM DU MAILLAGE
! IN  NLSNAC : K :  NOM DU VECTEUR DES NOEUDS ACTIFS
! OUT SDLIEU : K : NOM DU VECTEUR DES NOMS DE SD LIEU
!     ------------------------------------------------------------------
!     ORGANISATION SD_LIEU
!       .REFE : S E K8 DOCU(/'SGTD'/'SGT3'/'ARCC'/'CHMM'/'LSTN')
!               <-- NOM_MAILLAGE CAS LSTN, NOM_COURBE AUTRE CAS
!       .ABSC : XD V R NB_OC = NBR_PART_CONNEXE
!               <-- ABSCISSES CURVILIGNE
!       .COOR : XD V R  NB_OC = NBR_PART_CONNEXE
!               <-- COORDONNEE X, Y ET Z
!       .DESC : CHOIX DOCU PARMI
!               -----      -----
!                 SGT3      : (XA,YA,ZA,XB,YB,ZB)
!                 SGTD      : (XA,YA,   XB,YB   )
!                 ARCC      : (XC,YC,R,S1,S2)
!                 CHMM,LSTN : LISTE DES NOMS DE NOEUDS
!               FIN_CHOIX
!               ---------
!        .NUME : S E I
!               <-- NUMERO DE PARTIE DANS LE CAS D' UNE COURBE
!     ------------------------------------------------------------------
!
!
!
    character(len=24) :: nabsc, nrefe, ndesc, lnumnd, nnume, ncoor, ncrb3d
    character(len=19) :: sdcour
    character(len=10) :: iden
    character(len=4) :: docu
    integer :: aabsc, arefe, adesc, aasgt, absgt, acarc, asarc, ararc, acoor
    integer :: ansdl, acnxo, acnxe, adr, achm, aosgt, aesgt, anumnd, anume
    integer :: nbsd, nbsgt, nbarc, isd, nbpt, ipt, ideb, ifin, ioc, nbm, nboc
    integer :: asds
    logical :: sgtarc, okcrb, sgt3d
    real(kind=8) :: a, b, c, d, e, f, s, l, zero
    character(len=1) :: k1bid
!
!====================== CORPS DE LA ROUTINE ===========================
!
    call jemarq()
    okcrb = .false.
    sgtarc = .false.
    zero = 0.0d0
    lnumnd = '&&RVLIEU.LISTE.NUM.NOEUD'
    if (typco .eq. 'CHEMIN') then
        okcrb = .true.
        call jeexin(courbe//'.TYPCOURBE', adr)
        if (adr .ne. 0) then
            call jeveuo(courbe//'.TYPCOURBE', 'L', adr)
            sgtarc = ( zk8(adr) .eq. 'SGTDARCC' )
            sgt3d = .false.
            if (sgtarc) then
                call jelira(courbe//'.XYASGT', 'LONMAX', nbsgt, k1bid)
                call jelira(courbe//'.XYCARC', 'LONMAX', nbarc, k1bid)
                call jeveuo(courbe//'.XYASGT', 'L', aasgt)
                call jeveuo(courbe//'.XYBSGT', 'L', absgt)
                call jeveuo(courbe//'.XYCARC', 'L', acarc)
                call jeveuo(courbe//'.XRARC', 'L', ararc)
                call jeveuo(courbe//'.XSARC', 'L', asarc)
                nbsgt = (nbsgt/2) - 1
                nbarc = (nbarc/2) - 1
                nbsd = nbarc + nbsgt
                if (nbarc .ne. 0 .and. nbsgt .ne. 0) then
                    call u2mess('F', 'POSTRELE_23')
                endif
            else
                call jelira(courbe//'.CHEMIN', 'NMAXOC', nbsd, k1bid)
                call jeveuo(courbe//'.CHEMIN', 'L', achm)
            endif
        else
            call jelira(courbe//'.NSDS', 'LONMAX', nbsd, k1bid)
            call jeveuo(courbe//'.NSDS', 'L', asds)
            sgt3d = .true.
        endif
    else
        call jelira(nlsnac, 'LONMAX', nbpt, k1bid)
        nbsd = 1
    endif
    call wkvect(sdlieu, 'V V K24', nbsd, ansdl)
    do 100, isd = 1, nbsd, 1
    call codent(isd, 'G', iden)
    sdcour = '&&RVLIEU.'//iden
    zk24(ansdl + isd-1)(1:19) = sdcour
    nrefe = sdcour//'.REFE'
    nabsc = sdcour//'.ABSC'
    ndesc = sdcour//'.DESC'
    nnume = sdcour//'.NUME'
    ncoor = sdcour//'.COOR'
    call wkvect(nrefe, 'V V K8', 1, arefe)
    call wkvect(nnume, 'V V I', 1, anume)
    zi(anume) = isd
    if (okcrb .and. sgtarc) then
        call jelira(jexnum(courbe//'.CNXOR', isd), 'LONMAX', nboc, k1bid)
        call jeveuo(jexnum(courbe//'.CNXOR', isd), 'L', acnxo)
        call jeveuo(jexnum(courbe//'.CNXEX', isd), 'L', acnxe)
        call jeveuo(jexnum(courbe//'.ORSGT', isd), 'L', aosgt)
        call jeveuo(jexnum(courbe//'.EXSGT', isd), 'L', aesgt)
        call jecrec(nabsc, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nboc)
        call jecrec(ncoor, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nboc)
        zk8(arefe) = courbe
        if (isd .le. nbsgt) then
            docu = 'SGTD'
            call wkvect(ndesc, 'V V R', 4, adesc)
            a = zr(aasgt + 2*isd + 1-1)
            b = zr(aasgt + 2*isd + 2-1)
            c = zr(absgt + 2*isd + 1-1)
            d = zr(absgt + 2*isd + 2-1)
            zr(adesc + 1-1) = a
            zr(adesc + 2-1) = b
            zr(adesc + 3-1) = c
            zr(adesc + 4-1) = d
            c = c - a
            d = d - b
            l = sqrt(c*c+d*d)
            do 110, ioc = 1, nboc, 1
            ideb = zi(acnxo + ioc-1)
            ifin = zi(acnxe + ioc-1)
            nbpt = ifin - ideb + 2
            call jecroc(jexnum(nabsc, ioc))
            call jeecra(jexnum(nabsc, ioc), 'LONMAX', nbpt, ' ')
            call jeveuo(jexnum(nabsc, ioc), 'E', aabsc)
            call jecroc(jexnum(ncoor, ioc))
            call jeecra(jexnum(ncoor, ioc), 'LONMAX', 3*nbpt, ' ')
            call jeveuo(jexnum(ncoor, ioc), 'E', acoor)
            do 10, ipt = 1, nbpt-1, 1
            s = zr(aosgt + ideb-1 + ipt-1)
            zr(aabsc + ipt-1) = s*l
            zr(acoor + 3*(ipt-1) + 1-1) = a + s*c
            zr(acoor + 3*(ipt-1) + 2-1) = b + s*d
            zr(acoor + 3*(ipt-1) + 3-1) = zero
10          continue
            s = zr(aesgt + ifin-1)
            zr(aabsc + nbpt-1) = s*l
            zr(acoor + 3*(ipt-1) + 1-1) = a + s*c
            zr(acoor + 3*(ipt-1) + 2-1) = b + s*d
            zr(acoor + 3*(ipt-1) + 3-1) = zero
110          continue
        else
            docu = 'ARCC'
            call wkvect(ndesc, 'V V R', 5, adesc)
            a = zr(acarc + 2*(isd-nbsgt) + 1-1)
            b = zr(acarc + 2*(isd-nbsgt) + 2-1)
            c = zr(ararc + (isd-nbsgt) + 1-1)
            d = zr(asarc + 2*(isd-nbsgt) + 1-1)
            e = zr(asarc + 2*(isd-nbsgt) + 2-1)
            zr(adesc + 1-1) = a
            zr(adesc + 2-1) = b
            zr(adesc + 3-1) = c
            zr(adesc + 4-1) = d
            zr(adesc + 5-1) = e
            do 111, ioc = 1, nboc, 1
            ideb = zi(acnxo + ioc-1)
            ifin = zi(acnxe + ioc-1)
            nbpt = ifin - ideb + 2
            call jecroc(jexnum(nabsc, ioc))
            call jeecra(jexnum(nabsc, ioc), 'LONMAX', nbpt, ' ')
            call jeveuo(jexnum(nabsc, ioc), 'E', aabsc)
            call jecroc(jexnum(ncoor, ioc))
            call jeecra(jexnum(ncoor, ioc), 'LONMAX', 3*nbpt, ' ')
            call jeveuo(jexnum(ncoor, ioc), 'E', acoor)
            do 11, ipt = 1, nbpt-1, 1
            s = zr(aosgt + ideb-1 + ipt-1)
            zr(aabsc + ipt-1) = s*c
            zr(acoor + 3*(ipt-1) + 1-1) = a + c*cos(s)
            zr(acoor + 3*(ipt-1) + 2-1) = b + c*sin(s)
            zr(acoor + 3*(ipt-1) + 3-1) = zero
11          continue
            s = zr(aesgt + ifin-1)
            zr(aabsc + ipt-1) = s*c
            zr(acoor + 3*(ipt-1) + 1-1) = a + c*cos(s)
            zr(acoor + 3*(ipt-1) + 2-1) = b + c*sin(s)
            zr(acoor + 3*(ipt-1) + 3-1) = zero
111          continue
        endif
    else if (okcrb .and. sgt3d) then
        ncrb3d = zk24(asds + isd-1)
        docu = 'SGT3'
        zk8(arefe) = courbe
        call jelira(ncrb3d(1:13)//'.CONEX.ORIG', 'LONMAX', nboc, k1bid)
        call jeveuo(ncrb3d(1:13)//'.SGTEL.ORIG', 'L', aosgt)
        call jeveuo(ncrb3d(1:13)//'.SGTEL.EXTR', 'L', aesgt)
        call jeveuo(ncrb3d(1:13)//'.CONEX.ORIG', 'L', acnxo)
        call jeveuo(ncrb3d(1:13)//'.CONEX.EXTR', 'L', acnxe)
        call jeveuo(ncrb3d(1:13)//'.DESC', 'L', aasgt)
        call jecrec(nabsc, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nboc)
        call jecrec(ncoor, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    nboc)
        call wkvect(ndesc, 'V V R', 6, adesc)
        a = zr(aasgt + 1-1)
        b = zr(aasgt + 2-1)
        c = zr(aasgt + 3-1)
        d = zr(aasgt + 4-1)
        e = zr(aasgt + 5-1)
        f = zr(aasgt + 6-1)
        zr(adesc + 1-1) = a
        zr(adesc + 2-1) = b
        zr(adesc + 3-1) = c
        zr(adesc + 4-1) = d
        zr(adesc + 5-1) = e
        zr(adesc + 6-1) = f
        d = d - a
        e = e - b
        f = f - c
        l = sqrt(d*d+e*e+f*f)
        do 210, ioc = 1, nboc, 1
        ideb = zi(acnxo + ioc-1)
        ifin = zi(acnxe + ioc-1)
        nbpt = ifin - ideb + 2
        call jecroc(jexnum(nabsc, ioc))
        call jeecra(jexnum(nabsc, ioc), 'LONMAX', nbpt, ' ')
        call jeveuo(jexnum(nabsc, ioc), 'E', aabsc)
        call jecroc(jexnum(ncoor, ioc))
        call jeecra(jexnum(ncoor, ioc), 'LONMAX', 3*nbpt, ' ')
        call jeveuo(jexnum(ncoor, ioc), 'E', acoor)
        do 20, ipt = 1, nbpt-1, 1
        s = zr(aosgt + ideb-1 + ipt-1)
        zr(aabsc + ipt-1) = s*l
        zr(acoor + 3*(ipt-1) + 1-1) = a + s*d
        zr(acoor + 3*(ipt-1) + 2-1) = b + s*e
        zr(acoor + 3*(ipt-1) + 3-1) = c + s*f
20      continue
        s = zr(aesgt + ifin-1)
        zr(aabsc + nbpt-1) = s*l
        zr(acoor + 3*(ipt-1) + 1-1) = a + s*d
        zr(acoor + 3*(ipt-1) + 2-1) = b + s*e
        zr(acoor + 3*(ipt-1) + 3-1) = c + s*f
210      continue
    else
        if (okcrb) then
            zk8(arefe) = courbe
            docu = 'CHMM'
            call jeveuo(jexnum(courbe//'.CHEMIN', isd), 'L', achm)
            call jelira(jexnum(courbe//'.CHEMIN', isd), 'LONMAX', nbm, k1bid)
            nbm = nbm - 1
            call rvnchm(mailla, zi(achm), nbm, lnumnd, ndesc)
            call jelira(ndesc, 'LONMAX', nbpt, k1bid)
            call jeveuo(lnumnd, 'L', anumnd)
        else
            zk8(arefe) = mailla
            docu = 'LSTN'
            call jelira(nlsnac, 'LONMAX', nbpt, k1bid)
            call jeveuo(nlsnac, 'L', anumnd)
            call wkvect(ndesc, 'V V K8', nbpt, adesc)
            do 30, ipt = 1, nbpt, 1
            call jenuno(jexnum(mailla//'.NOMNOE', zi(anumnd+ ipt-1)), zk8(adesc + ipt-1))
30          continue
        endif
        call jecrec(nabsc, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    1)
        call jecroc(jexnum(nabsc, 1))
        call jeecra(jexnum(nabsc, 1), 'LONMAX', nbpt, ' ')
        call jeveuo(jexnum(nabsc, 1), 'E', aabsc)
        call jecrec(ncoor, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    1)
        call jecroc(jexnum(ncoor, 1))
        call jeecra(jexnum(ncoor, 1), 'LONMAX', 3*nbpt, ' ')
        call jeveuo(jexnum(ncoor, 1), 'E', acoor)
        call rvabsc(mailla, zi(anumnd), nbpt, zr(aabsc), zr(acoor))
        call jeexin(lnumnd, adr)
        if (adr .ne. 0) then
            call jedetr(lnumnd)
        endif
    endif
    call jeecra(nrefe, 'DOCU', adr, docu)
    100 end do
    call jedema()
end subroutine
