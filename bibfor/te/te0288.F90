subroutine te0288(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jevecd.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rcvad2.h'
    include 'asterfort/teattr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/xcgfvo.h'
    include 'asterfort/xgelem.h'
    include 'asterfort/xsifle.h'
    include 'asterfort/xteini.h'
    character(len=16) :: option, nomte
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT
!                          EN MÉCANIQUE DE LA RUPTURE
!                          POUR LES ÉLÉMENTS X-FEM (OPTION CALC_G)
!
!             ATTENTION : PAS D'ETAT INITIAL
!                         PAS DE GRANDES DEF, GRANDES ROT
!                         PAS DE COMP_INCR
!
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
    integer :: ndim, nno, nnop, npg, nptf, jbasec, nfiss, jfisno
    integer :: nfh, nfe, ddlc, nse, ise, in, ino
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, igeom, idepl
    integer :: ipres, ipref, itemps, jptint, jaint, jcface, jlongc, imate, icomp
    integer :: ithet, i, j, compt, igthet, ibid, jlsn, jlst, icode
    integer :: ninter, nface, cface(5, 3), ifa, singu, jpmilt, irese, ddlm
    real(kind=8) :: thet, valres(3), devres(3), presn(27), valpar(4)
    real(kind=8) :: pres, rho, fno(81), coorse(81)
    integer :: icodre(3)
    character(len=8) :: elrefp, elrese(6), fami(6), nomres(3), nompar(4), enr
    character(len=16) :: compor(4)
    logical :: grand, incr
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','RIGI','XINT','BID','RIGI','XINT'/
    data    nomres /'E','NU','ALPHA'/
!
!
    call elref1(elrefp)
    call jevech('PTHETAR', 'L', ithet)
    call elref4(' ', 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SI LA VALEUR DE THETA EST NULLE SUR L'ÉLÉMENT, ON SORT
    compt = 0
    do 10 i = 1, nnop
        thet = 0.d0
        do 11 j = 1, ndim
            thet = thet + abs(zr(ithet+ndim*(i-1)+j-1))
11      continue
        if (thet .lt. r8prem()) compt = compt + 1
10  end do
    if (compt .eq. nnop) goto 9999
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG ET IVF
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
    call elref4(elrese(ndim+irese), fami(ndim+irese), ibid, nno, ibid,&
                npg, ibid, ibid, ibid, ibid)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                ibid, ibid, ibid, ddlm, nfiss,&
                ibid)
!
!
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
    call jevech('PGTHETA', 'E', igthet)
!
    do 50 i = 1, 4
        compor(i) = zk16(icomp+i-1)
50  end do
!
    incr = compor(4).eq.'COMP_INCR'
    grand = compor(3).eq.'GROT_GDEP'
!
    if (incr) call u2mess('F', 'XFEM_48')
    if (grand) call u2mess('F', 'XFEM_49')
!
!     ------------------------------------------------------------------
!              CALCUL DE G SUR L'ELEMENT MASSIF
!     ------------------------------------------------------------------
!
!     PARAMÈTRES PROPRES À X-FEM
!
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!
!     PROPRES AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if (ibid .eq. 0 .and. (nomte(3:4).ne.'AX') .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!     CALCUL DES FORCES NODALES CORRESPONDANT AUX CHARGES VOLUMIQUES
    call xcgfvo(option, ndim, nnop, fno, rho)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        do 111 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
112          continue
111      continue
!
        call xgelem(elrefp, ndim, coorse, igeom, jheavt,&
                    ise, nfh, ddlc, ddlm, nfe,&
                    zr(jbaslo), nnop, idepl, zr(jlsn), zr(jlst),&
                    igthet, fno, nfiss, jfisno)
!
!
110  end do
!
!     ------------------------------------------------------------------
!              CALCUL DE G SUR LES LEVRES
!     ------------------------------------------------------------------
!
    if (option .eq. 'CALC_G') then
!       SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
        call jevecd('PPRESSR', ipres, 0.d0)
    else if (option.eq.'CALC_G_F') then
        call jevech('PPRESSF', 'L', ipref)
        call jevech('PTEMPSR', 'L', itemps)
!
!       RECUPERATION DES PRESSIONS AUX NOEUDS PARENTS
        nompar(1)='X'
        nompar(2)='Y'
        if (ndim .eq. 3) nompar(3)='Z'
        if (ndim .eq. 3) nompar(4)='INST'
        if (ndim .eq. 2) nompar(3)='INST'
        do 70 i = 1, nnop
            do 80 j = 1, ndim
                valpar(j) = zr(igeom+ndim*(i-1)+j-1)
80          continue
            valpar(ndim+1)= zr(itemps)
            call fointe('FM', zk8(ipref), 4, nompar, valpar,&
                        presn(i), icode)
70      continue
    endif
!
!     SI LA VALEUR DE LA PRESSION EST NULLE SUR L'ÉLÉMENT, ON SORT
    compt = 0
    do 90 i = 1, nnop
        if (option .eq. 'CALC_G') pres = abs(zr(ipres-1+i))
        if (option .eq. 'CALC_G_F') pres = abs(presn(i))
        if (pres .lt. r8prem()) compt = compt + 1
90  end do
    if (compt .eq. nnop) goto 9999
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONGCO', 'L', jlongc)
    call jevech('PBASECO', 'L', jbasec)
!
!     RÉCUPÉRATIONS DES DONNÉES SUR LA TOPOLOGIE DES FACETTES
    ninter=zi(jlongc-1+1)
    nface=zi(jlongc-1+2)
    nptf=zi(jlongc-1+3)
    if (ninter .lt. ndim) goto 9999
!
    do 20 i = 1, nface
        do 21 j = 1, nptf
            cface(i,j)=zi(jcface-1+ndim*(i-1)+j)
21      continue
20  end do
!
!     RECUPERATION DES DONNEES MATERIAU AU 1ER POINT DE GAUSS DE
!     DE L'ELEMENT PARENT !!
!     LE MATÉRIAU DOIT ETRE HOMOGENE DANS TOUT L'ELEMENT
    call rcvad2('RIGI', 1, 1, '+', zi(imate),&
                'ELAS', 3, nomres, valres, devres,&
                icodre)
    if ((icodre(1).ne.0) .or. (icodre(2).ne.0)) then
        call u2mess('F', 'RUPTURE1_25')
    endif
    if (icodre(3) .ne. 0) then
        valres(3) = 0.d0
        devres(3) = 0.d0
    endif
!
!     BOUCLE SUR LES FACETTES
    do 200 ifa = 1, nface
        call xsifle(ndim, ifa, jptint, jaint, cface,&
                    igeom, nfh, singu, nfe, ddlc,&
                    ddlm, jlst, ipres, ipref, itemps,&
                    idepl, nnop, valres, zr( jbaslo), ithet,&
                    nompar, presn, option, igthet, jbasec)
200  end do
!
!
9999  continue
!
!
end subroutine
