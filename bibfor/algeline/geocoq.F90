subroutine geocoq(noma, nomgrp, caelem, iaxe, geom)
    implicit none
    include 'asterc/indik8.h'
    include 'asterc/r8prem.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: noma, caelem
    character(len=24) :: nomgrp(*)
    integer :: iaxe
    real(kind=8) :: geom(9)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     DETERMINATION DES GRANDEURS GEOMETRIQUES CARACTERISTIQUES D'UNE
!              CONFIGURATION "COQUES CYLINDRIQUES COAXIALES"
!
! APPELANT : FLUST4
!
!-----------------------------------------------------------------------
!  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
!  IN : NOMGRP : LISTE DES NOMS DES GROUPES DE NOEUDS/GROUPES DE MAILLES
!                CORRESPONDANT AUX COQUES (LES GROUPES DE NOEUDS ONT ETE
!                PREALABLEMENT CREES A PARTIR DES GROUPES DE MAILLES ET
!                ON LEUR A AFFECTE LES MEMES NOMS)
!  IN : CAELEM : NOM DU CONCEPT DE TYPE CARA_ELEM
!  IN : IAXE   : INDICE CARACTERISANT L'AXE DE REVOLUTION DES COQUES
!                IAXE = 1 : AXE X DU REPERE GLOBAL
!                IAXE = 2 : AXE Y DU REPERE GLOBAL
!                IAXE = 3 : AXE Z DU REPERE GLOBAL
! OUT : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
!       GEOM(1)= HMOY  EPAISSEUR MOYENNE DE L'ESPACE ANNULAIRE
!       GEOM(2)= RMOY  RAYON MOYEN
!       GEOM(3)= LONG  LONGUEUR DU DOMAINE DE RECOUVREMENT DES COQUES
!       GEOM(4)= Z0    BORNE INF DU DOMAINE DE RECOUVREMENT DES COQUES
!       GEOM(5)= Z1    BORNE SUP DU DOMAINE DE RECOUVREMENT DES COQUES
!       GEOM(6)= EPINT EPAISSEUR DE LA COQUE INTERNE
!       GEOM(7)= EPEXT EPAISSEUR DE LA COQUE EXTERNE
!       GEOM(8)= RINT  RAYON DE LA COQUE INTERNE
!       GEOM(9)= REXT  RAYON DE LA COQUE EXTERNE
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
!
    integer :: ias, iascqi, iascqx, iasedi, iasmax, icmp, icode
    integer :: icoor, idesc, idir1, idir2, idir3, ier
    integer :: ino, inomcp, inunoe, inunoi, irang, iranv, iret
    integer :: ivale, nbcmp, nbec, nbnoex, nbnoin, nucoqi
    integer :: nucoqx, nuenti, nunoe, nunoex, nunoi, nunoin
!
!
    real(kind=8) :: difz, difz1, difz2, epext, epint, hmoy
    real(kind=8) :: rext, rint, rmoy, tole, x2, x3, z0, long
    real(kind=8) :: z0ext, z0int, z1, z1ext, z1int, zno
!
    character(len=1) :: k1bid
    character(len=8) :: nomcmp, k8bid
    character(len=24) :: grpma, coorno, carte, cadesc, cavale
    character(len=24) :: coquei, coquex
    character(len=32) :: grpno, kjexn
!-----------------------------------------------------------------------
    call jemarq()
!
! --- 1.INITIALISATIONS ET ACCES AUX OBJETS UTILES
    tole = 100.d0*r8prem()
!
    if (iaxe .eq. 1) then
        idir1 = 1
        idir2 = 2
        idir3 = 3
    else if (iaxe.eq.2) then
        idir1 = 2
        idir2 = 3
        idir3 = 1
    else
        idir1 = 3
        idir2 = 1
        idir3 = 2
    endif
!
    coquei = nomgrp(1)
    coquex = nomgrp(2)
!
    grpma = noma//'.GROUPEMA'
!
    coorno = noma//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', icoor)
!
! --- 2. DETERMINATION DES BORNES DU DOMAINE DE RECOUVREMENT DES DEUX
!        COQUES. DEDUCTION DE LA LONGUEUR DE RECOUVREMENT
!
! --- 2.1.BORNE INF ET BORNE SUP ASSOCIEES A LA COQUE INTERNE
!
    grpno='&&MEFGMN.00000001       '
    call jelira(grpno, 'LONMAX', nbnoin, k1bid)
    if (nbnoin .lt. 4) call u2mess('F', 'ALGELINE_49')
    call jeveuo(grpno, 'L', inunoi)
    nunoi = zi(inunoi)
    z0int = zr(icoor+3*(nunoi-1)+idir1-1)
    z1int = z0int
    do 10 ino = 2, nbnoin
        nunoi = zi(inunoi+ino-1)
        zno = zr(icoor+3*(nunoi-1)+idir1-1)
        if (zno .lt. z0int) z0int = zno
        if (zno .gt. z1int) z1int = zno
10  end do
    difz = dble(abs(z1int-z0int))
    if (difz .lt. tole) call u2mess('F', 'ALGELINE_50')
!
! --- 2.2.BORNE INF ET BORNE SUP ASSOCIEES A LA COQUE EXTERNE
!
    grpno='&&MEFGMN.00000002       '
    call jelira(grpno, 'LONMAX', nbnoex, k1bid)
    if (nbnoex .lt. 4) call u2mess('F', 'ALGELINE_51')
    call jeveuo(grpno, 'L', inunoe)
    nunoe = zi(inunoe)
    z0ext = zr(icoor+3*(nunoe-1)+idir1-1)
    z1ext = z0ext
    do 20 ino = 2, nbnoex
        nunoe = zi(inunoe+ino-1)
        zno = zr(icoor+3*(nunoe-1)+idir1-1)
        if (zno .lt. z0ext) z0ext = zno
        if (zno .gt. z1ext) z1ext = zno
20  end do
    difz = dble(abs(z1ext-z0ext))
    if (difz .lt. tole) call u2mess('F', 'ALGELINE_52')
!
! --- 2.3.SORTIE EN ERREUR SI NON RECOUVREMENT DES DOMAINES ASSOCIES
!         AUX DEUX COQUES
!
    difz1 = dble(abs(z1int-z0ext))
    difz2 = dble(abs(z1ext-z0int))
    if (z1int .lt. z0ext .or. z1ext .lt. z0int .or. difz1 .lt. tole .or. difz2 .lt. tole) &
    call u2mess('F', 'ALGELINE_53')
!
! --- 2.4.DEDUCTION DES BORNES DU DOMAINE DE RECOUVREMENT
!         ET DE LA LONGUEUR DU DOMAINE
!
    z0 = z0int
    if (z0ext .gt. z0int) z0 = z0ext
    z1 = z1int
    if (z1ext .lt. z1int) z1 = z1ext
    long = z1 - z0
    geom(3) = long
    geom(4) = z0
    geom(5) = z1
!
! --- 3.RECUPERATION DE EPINT ET EPEXT
!
! --- 3.1. CARTE DES CARACTERISTIQUES DES ELEMENTS DE COQUE
!         (OBJETS DU CONCEPT DE TYPE CARA_ELEM)
!
    carte = caelem//'.CARCOQUE       '
    cadesc = carte(1:19)//'.DESC'
    cavale = carte(1:19)//'.VALE'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) call u2mess('F', 'ALGELINE_54')
!     NOMBRE D'ENTIERS CODES DANS LA CARTE
    call dismoi('F', 'NB_EC', 'CACOQU', 'GRANDEUR', nbec,&
                k8bid, ier)
    call jeveuo(cadesc, 'L', idesc)
    call jeveuo(cavale, 'L', ivale)
!
! --- 3.2. GROUPES DE MAILLES ASSOCIES AUX COQUES INTERNE ET EXTERNE
    call jenonu(jexnom(grpma, coquei), nucoqi)
    call jenonu(jexnom(grpma, coquex), nucoqx)
    iasmax = zi(idesc+1)
    iasedi = zi(idesc+2)
    iascqi = 0
    iascqx = 0
    do 30 ias = 1, iasedi
        icode = zi(idesc+3+2*(ias-1))
        if (icode .eq. 2) then
            nuenti = zi(idesc+3+2*(ias-1)+1)
            if (nuenti .eq. nucoqi) iascqi = ias
            if (nuenti .eq. nucoqx) iascqx = ias
        endif
30  end do
    if (iascqi .eq. 0 .or. iascqx .eq. 0) call u2mess('F', 'ALGELINE_56')
!
! --- 3.3. RANG DE LA COMPOSANTE <EP> DANS LA GRANDEUR
    kjexn = jexnom('&CATA.GD.NOMCMP','CACOQU')
    call jelira(kjexn, 'LONMAX', nbcmp, k1bid)
    call jeveuo(kjexn, 'L', inomcp)
    nomcmp = 'EP'
    irang = indik8( zk8(inomcp) , nomcmp , 1 , nbcmp )
    if (irang .eq. 0) call u2mess('F', 'ALGELINE_57')
!
! --- 3.4. VALEUR DE L'EPAISSEUR DE LA COQUE INTERNE
    icode = zi(idesc-1+3+2*iasmax+nbec*(iascqi-1)+1)
    iranv = 0
    do 50 icmp = 1, irang
        if (exisdg(icode,icmp)) iranv = iranv + 1
50  end do
    if (iranv .eq. 0) call u2mess('F', 'ALGELINE_58')
    epint = zr(ivale-1+nbcmp*(iascqi-1)+iranv)
    if (epint .eq. 0.d0) call u2mess('F', 'ALGELINE_59')
    geom(6) = epint
!
! --- 3.5. VALEUR DE L'EPAISSEUR DE LA COQUE EXTERNE
    icode = zi(idesc-1+3+2*iasmax+nbec*(iascqx-1)+1)
    iranv = 0
    do 51 icmp = 1, irang
        if (exisdg(icode,icmp)) iranv = iranv + 1
51  end do
    if (iranv .eq. 0) call u2mess('F', 'ALGELINE_60')
    epext = zr(ivale-1+nbcmp*(iascqx-1)+iranv)
    if (epext .eq. 0.d0) call u2mess('F', 'ALGELINE_61')
    geom(7) = epext
!
! --- 4.DETERMINATION DE RINT, REXT, RMOY ET HMOY
    nunoin = zi(inunoi)
    x2 = zr(icoor+3*(nunoin-1)+idir2-1)
    x3 = zr(icoor+3*(nunoin-1)+idir3-1)
    rint = dble(sqrt(x2*x2 + x3*x3))
!
    nunoex = zi(inunoe)
    x2 = zr(icoor+3*(nunoex-1)+idir2-1)
    x3 = zr(icoor+3*(nunoex-1)+idir3-1)
    rext = dble(sqrt(x2*x2 + x3*x3))
!
    if (rint .eq. 0.d0 .or. rext .eq. 0.d0) call u2mess('F', 'ALGELINE_62')
!
    geom(8) = rint
    geom(9) = rext
!
    rmoy = ((rint+epint/2.d0)+(rext-epext/2.d0))/2.d0
    hmoy = (rext-epext/2.d0) - (rint+epint/2.d0)
    if (hmoy .le. 0.d0) call u2mess('F', 'ALGELINE_63')
!
    geom(1) = hmoy
    geom(2) = rmoy
!
    call jedema()
!
end subroutine
