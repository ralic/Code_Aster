subroutine rvchlo(epsi, ssch19, nbcp, nbco, nbsp,&
                  nbcm, nbsm, m, f, n,&
                  r, valcp)
    implicit none
!
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
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rvechb.h'
    include 'asterfort/wkvect.h'
    integer :: m, f(*), n, nbcp, nbcm, nbsm, nbco, nbsp
    character(len=19) :: ssch19
    real(kind=8) :: r(*), valcp(*), epsi
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     SAISIE DES VALEURS DE CMPS EN DES POINTS DE FACES D' UNE MAILLE
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     SSCH19 : NOM DU SOUS_CHAMP_GD
!     M      : NUMERO DE LA MAILLE
!     F      : TABLE DES FACES
!     R      : TABLE VALEUR DU PARAMETRE DE REPERAGE
!              POUR LE POINT CONSIDERE (2D : 1 PARAM, 3D : 2 PARAM)
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     VALCP   : TABLE DES VALEURS DES CMP (DANS L' ORDRE D' APPARITION
!               DANS L' EXTRACTION)
!
!**********************************************************************
!
!  FONCTIONS EXTERNES
!  ------------------
!
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=8) :: mailla, typmai
    character(len=4) :: docu
    character(len=24) :: npcmp, nvale, npadr, npnbn, nvalcm, nndfac
!
    integer :: apcmp, avale, apadr, apnbn, avalcm, andfac
    integer :: adrm, i, adr, nbnf, j, nbf, nbnm, fac, iatyma
    real(kind=8) :: acc
    character(len=1) :: k1bid
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: ind, k, l
!-----------------------------------------------------------------------
    call jemarq()
!
    nvalcm = '&&RVCHLO.VAL.CMP.ND.MAIL'
    nndfac = '&&RVCHLO.NUM.LOC.ND.FACE'
!
    call jeveuo(ssch19//'.NOMA', 'L', adr)
!
    mailla = zk8(adr)
!
    nvale = ssch19//'.VALE'
    npcmp = ssch19//'.PCMP'
    npadr = ssch19//'.PADR'
    npnbn = ssch19//'.PNBN'
!
    call jeveuo(nvale, 'L', avale)
    call jeveuo(npcmp, 'L', apcmp)
    call jeveuo(npadr, 'L', apadr)
    call jelira(nvale, 'DOCU', i, docu)
!
!  VALEUR DES CMP SUR TOUTE LA MAILLE
!  ----------------------------------
!
    if (docu .eq. 'CHNO') then
!
        call jelira(jexnum(mailla//'.CONNEX', m), 'LONMAX', nbnm, k1bid)
!
!
    else if (docu .eq. 'CHLM') then
!
        call jeveuo(npnbn, 'L', apnbn)
!
        nbnm = zi(apnbn + m-1)
!
    else
!
!        /* EN CAS D' EVOLUTION ... */
!
    endif
!
    call wkvect(nvalcm, 'V V R', nbcp*nbnm*nbcm*nbsm, avalcm)
!
    if (docu .eq. 'CHNO') then
!
        call jeveuo(jexnum(mailla//'.CONNEX', m), 'L', adrm)
!
        do 10, i = 1, nbnm, 1
!
        do 11, j = 1, nbcp, 1
!
        zr(avalcm + (i-1)*nbcp + j-1) = zr( avale + zi(apadr + zi(adrm + i-1)-1 )+j-2 )
!
11      continue
!
10      continue
!
    else if (docu .eq. 'CHLM') then
!
        do 12, i = 1, nbcp*nbnm*nbcm*nbsm, 1
!
        zr(avalcm + i-1) = zr(avale + zi(apadr + m-1)+i-2)
!
12      continue
!
!C       CALL JXVERI('RESULTAT','AP : 12')
    else
!
!        /* EN CAS D' EVOLUTION ... */
!
    endif
!
    do 150, i = 1, n, 1
!
!        DESCRIPTEUR DE FACE
!        -------------------
!
    fac = f(i)
!
    if (fac .gt. 0) then
!
        call jeveuo(mailla//'.TYPMAIL', 'L', iatyma)
        adr=iatyma-1+m
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(adr)), typmai)
!
        if (typmai .eq. 'POI1') then
!
            nbnf = 1
            nbf = 1
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = 1
!
            else if ((typmai .eq. 'SEG2').or.(typmai .eq. 'SEG3'))&
            then
!
            nbnf = 1
            nbf = 1
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = fac
!
        else if (typmai .eq. 'TRIA3') then
!
            nbnf = 3
            nbf = 3
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = fac
            zi(andfac + 2-1) = mod(fac,nbf) + 1
            zi(andfac + 3-1) = 0
!
        else if (typmai .eq. 'TRIA6') then
!
            nbnf = 3
            nbf = 3
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = fac
            zi(andfac + 2-1) = mod(fac,nbf) + 1
            zi(andfac + 3-1) = fac + nbf
!
        else if (typmai .eq. 'QUAD4') then
!
            nbnf = 3
            nbf = 4
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = fac
            zi(andfac + 2-1) = mod(fac,nbf) + 1
            zi(andfac + 3-1) = 0
!
        else if ((typmai.eq.'QUAD8').or.(typmai.eq.'QUAD9')) then
!
            nbnf = 3
            nbf = 4
!
            call wkvect(nndfac, 'V V I', nbnf, andfac)
!
            zi(andfac + 1-1) = fac
            zi(andfac + 2-1) = mod(fac,nbf) + 1
            zi(andfac + 3-1) = fac + nbf
!
        else
!
!           /* TRAITEMENT 3D */
!
        endif
!
        do 151, j = 1, nbco, 1
!
        call rvechb(epsi, typmai, zi(andfac), r(i), zr(avalcm + ( j-1)*nbnm*nbsm*nbcp),&
                    nbcp, nbsp, nbsm, valcp(1 + ((j-1) *n+i-1)*nbsp*nbcp))
!
!C       CALL JXVERI('RESULTAT','AP : RVECHB')
151      continue
!
        call jedetr(nndfac)
!
    else
!
        do 155, j = 1, nbco, 1
!
        do 156, k = 1, nbcp*nbsp, 1
!
        acc = 0.0d0
        ind = (j-1)*nbnm*nbsm*nbcp + k-1
!
        do 156, l = 1, nbnm, 1
!
        acc = acc + zr(avalcm + ind + (l-1)*nbsm*nbcp )
!
        valcp(((j-1)*n+i-1)*nbsp*nbcp+k) = acc/nbnm
!
156      continue
!
155      continue
!
    endif
!
    150 end do
!
    call jedetr(nvalcm)
!
    call jedema()
end subroutine
