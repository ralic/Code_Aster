subroutine te0338(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/epdcp.h'
    include 'asterfort/fgequi.h'
    include 'asterfort/jevech.h'
    include 'asterfort/psvari.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecach.h'
    character(len=*) :: option, nomte
!     -----------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     FONCTION REALISEE :
!
!         CALCUL DU CHAM_ELEM DE WEIBULL
!         COMPORTEMENT NON-LINEAIRE.
!         ELEMENTS ISOPARAMETRIQUES 3D.
!
!         OPTION : 'WEIBULL'
!
! ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
!         --->  NOMTE  : NOM DU TYPE D'ELEMENT
!
!
!-DEL CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
!
    integer :: icodre(4)
    integer :: codres
    character(len=4) :: fami
    character(len=8) :: nomres(4)
    character(len=16) :: optcal(12), phenom
!
    real(kind=8) :: sigm(6), sig1, sigwk, valres(4), epsg(6), eps1
    real(kind=8) :: m, vref, sref, seuil, dvpg, poids, vkp, dfdbid(30)
    real(kind=8) :: equi(6), pp, ppt, vkpact
    real(kind=8) :: sigold, signew, tg, tmoy
!
    integer :: i, kp, ndim, icompo, nbvari, ipopp, ipoppt
    integer :: jgano, ipoids, ivf, idfde, npg, nno, nnos
    integer :: imate, igeom, icong, ivarig, issopt, iweib, idefg, nbvp
    integer :: isigie, isigis, jtab(7), iret
!
!======================== CORPS DU PROGRAMME ===========================
!
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    nbvp = 3
!
!     1.2 CHAMPS IN
!     -------------
    call jevech('PMATERC', 'L', imate)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRG', 'L', icong)
    call jevech('PVARIPG', 'L', ivarig)
    call jevech('PSOUSOP', 'L', issopt)
    call jevech('PDOMMAG', 'L', isigie)
    call tecach('OON', 'PVARIPG', 'L', 7, jtab,&
                iret)
    nbvari = max(jtab(6),1)*jtab(7)
    call jevech('PCOMPOR', 'L', icompo)
!     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
!
    call psvari(zk16(icompo), nbvari, '3D', ipopp, ipoppt)
!
!     1.3 CHAMPS OUT
!     --------------
    call jevech('PWEIBUL', 'E', iweib)
    call jevech('PSIGISG', 'E', isigis)
!
!     1.4 OPTIONS DE CALCUL
!     ---------------------
    optcal(1) = zk24(issopt) (1:16)
    optcal(2) = zk24(issopt) (17:19)
!
!     1.5 DONNES DE LA RC DU MATERIAU
!     -------------------------------
    nomres(1) = 'M'
    nomres(2) = 'VOLU_REFE'
    nomres(3) = 'SEUIL_EPSP_CUMU'
    nomres(4) = 'SIGM_REFE'
!
    call rccoma(zi(imate), 'WEIBULL', 1, phenom, codres)
!
!     --- S'IL N Y A PAS DE CHAMP DE TEMPERATURE
!     ARRET
!
    if (optcal(1) .eq. 'SIGM_ELMOY') then
        tmoy = 0.d0
    endif
!
    call rcvalb(fami, 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', 0.d0,&
                3, nomres, valres, icodre, 1)
    call rcvalb(fami, 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', 0.d0,&
                1, nomres(3), valres(3), icodre(3), 1)
    if (icodre(3) .ne. 0) valres(3) = 1.0d-6
    m = valres(1)
    vref = valres(2)
    seuil = valres(3)
!
!     1.6 INITIALISATION
!     ------------------
    poids = 0.d0
    dvpg = 0.d0
    vkp = 0.d0
    vkpact = 0.d0
    sigwk = 0.d0
    do 10,i = 1,6,1
    sigm(i) = 0.d0
    epsg(i) = 0.d0
    10 end do
! -CRITERE PLASTIQUE
    ppt = 0.d0
    pp = 0.d0
!
!
!     2. BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL
!     -------------------------------------------------------
!     2.1 SIGM_W A PARTIR DE SIGM MOYENNE SANS CORRECTION PLASTIQUE
!     -------------------------------------------------------------
    if ((optcal(1).eq.'SIGM_ELMOY') .and. (optcal(2).eq.'NON')) then
!        2.1.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
!        --------------------------------------------------
        do 40,kp = 1,npg,1
! VOLUME PLASTIFIE
        pp = zr(ivarig+nbvari* (kp-1)+ipopp-1)
        if (pp .ge. seuil) then
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, dfdbid, poids)
            dvpg = poids
            vkp = vkp + dvpg
            do 20,i = 1,6,1
            sigm(i) = sigm(i) + dvpg*zr(icong+6*kp+i-7)
20          continue
!           --- TEMPERATURE MOYENNE
            call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                        1, tg, iret)
            if (iret .ne. 0) tg = 0.d0
            tmoy = tmoy + tg * dvpg
        endif
! VOLUME PLASTIQUE ACTIF
        if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
            ppt = 1.d0
        else
            ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
        endif
        if (ppt .ge. (1.d0)) then
            dvpg = poids
            vkpact = vkpact + dvpg
        endif
!
40      continue
!
        sig1 = 0.d0
        if ((vkp.ne.0.0d0) .and. (vkpact.ne.0.d0)) then
!           2.1.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
!           --------------------------------------------------------
            do 50,i = 1,6,1
            sigm(i) = sigm(i)/vkp
50          continue
!
            tmoy = tmoy/vkp
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        ' ', phenom, 1, 'TEMP', tmoy,&
                        1, nomres(4), valres(4), icodre(4), 1)
            sref = valres(4)
!
!           2.1.3 CALCUL DE SIGM_W
!           ----------------------
            call fgequi(sigm, 'SIGM', nbvp, equi)
            sig1 = max(equi(3),equi(4),equi(5))
            sig1 = sig1/sref
        endif
!
        sigold = zr(isigie)
        if (sig1 .gt. sigold) then
            zr(isigis) = sig1
        else
            zr(isigis) = zr(isigie)
        endif
        sig1 = zr(isigis)
!
        sigwk = (vkp/vref)* (sig1**m)
!
!     2.2 SIGM_W A PARTIR DE SIGM MOYENNE AVEC CORRECTION PLASTIQUE
!     -------------------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELMOY') .and. (optcal(2).eq.'OUI'))&
    then
!        2.2.1 INTEGRATION DE SIGM SUR LA PARTIE PLASTIFIEE
!        --------------------------------------------------
        call jevech('PDEFORR', 'L', idefg)
        do 80,kp = 1,npg,1
! VOLUME PLASTIFIE
        pp = zr(ivarig+nbvari* (kp-1)+ipopp-1)
        if (pp .ge. seuil) then
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, dfdbid, poids)
            dvpg = poids
            vkp = vkp + dvpg
            do 60,i = 1,6,1
            sigm(i) = sigm(i) + dvpg*zr(icong+6*kp+i-7)
            epsg(i) = epsg(i) + dvpg*zr(idefg+6*kp+i-7)
60          continue
!           --- TEMPERATURE AU PG
            call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                        1, tg, iret)
            if (iret .ne. 0) tg = 0.d0
            tmoy = tmoy + tg * dvpg
        endif
! VOLUME PLASTIQUE ACTIF
        if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
            ppt = 1.d0
        else
            ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
        endif
        if (ppt .ge. (1.d0)) then
            dvpg = poids
            vkpact = vkpact + dvpg
        endif
80      continue
!
        signew = 0.d0
        if ((vkp.ne.0.0d0) .and. (vkpact.ne.0.d0)) then
!           2.2.2 CALCUL DE LA VALEUR MOYENNE DE SIGM SUR LA  MAILLE
!           --------------------------------------------------------
            do 90,i = 1,6,1
            sigm(i) = sigm(i)/vkp
            epsg(i) = epsg(i)/vkp
90          continue
!
            tmoy = tmoy/vkp
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        ' ', phenom, 1, 'TEMP', tmoy,&
                        1, nomres(4), valres(4), icodre(4), 1)
            sref = valres(4)
!
!           2.2.3 CALCUL DE SIGM_W
!           ----------------------
            call epdcp(sigm, epsg, sig1, eps1)
            signew = (sig1/sref)*exp(-eps1*0.5d0)
        endif
!
        sigold = zr(isigie)
        if (signew .gt. sigold) then
            zr(isigis) = signew
        else
            zr(isigis) = zr(isigie)
        endif
        signew = zr(isigis)
!
        sigwk = (vkp/vref)* (signew**m)
!
!     2.3 SIGM_W A PARTIR DE SIGM ORIGINAL AVEC CORRECTION PLASTIQUE
!     -------------------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELGA') .and. (optcal(2).eq.'OUI'))&
    then
        call jevech('PDEFORR', 'L', idefg)
        do 120,kp = 1,npg,1
        pp = zr(ivarig+nbvari* (kp-1)+ipopp-1)
        signew = 0.d0
        if (pp .ge. seuil) then
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, dfdbid, poids)
            dvpg = poids
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
            if (ppt .ge. (1.d0)) then
                do 100,i = 1,6,1
                sigm(i) = zr(icong+6*kp+i-7)
                epsg(i) = zr(idefg+6*kp+i-7)
100              continue
                call epdcp(sigm, epsg, sig1, eps1)
                call rcvalb(fami, kp, 1, '+', zi(imate),&
                            ' ', phenom, 0, ' ', 0.d0,&
                            1, nomres(4), valres(4), icodre(4), 1)
                sref = valres(4)
                signew = (sig1/sref)*exp(-eps1*0.5d0)
            endif
        endif
        sigold = zr(isigie+kp-1)
        if (signew .gt. sigold) then
            zr(isigis+kp-1) = signew
        else
            zr(isigis+kp-1) = zr(isigie+kp-1)
        endif
        signew = zr(isigis+kp-1)
        sigwk = sigwk + (dvpg/vref)* (signew**m)
120      continue
!
!     2.4 SIGM_W A PARTIR DE SIGM ORIGINAL SANS CORRECTION
!     ----------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELGA') .and. (optcal(2).eq.'NON'))&
    then
        do 150,kp = 1,npg,1
        pp = zr(ivarig+nbvari* (kp-1)+ipopp-1)
        if (pp .ge. seuil) then
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, dfdbid, poids)
            dvpg = poids
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
            sig1 = 0.d0
            if (ppt .ge. (1.d0)) then
                do 130,i = 1,6,1
                sigm(i) = zr(icong+6*kp+i-7)
130              continue
                call fgequi(sigm, 'SIGM', nbvp, equi)
                sig1 = max(equi(3),equi(4),equi(5))
                call rcvalb(fami, kp, 1, '+', zi(imate),&
                            ' ', phenom, 0, ' ', 0.d0,&
                            1, nomres(4), valres(4), icodre(4), 1)
                sref = valres(4)
                sig1 = sig1/sref
            endif
        else
            sig1=0.d0
        endif
        sigold = zr(isigie+kp-1)
        if (sig1 .gt. sigold) then
            zr(isigis+kp-1) = sig1
        else
            zr(isigis+kp-1) = zr(isigie+kp-1)
        endif
        sig1 = zr(isigis+kp-1)
!
        sigwk = sigwk + (dvpg/vref)* (sig1**m)
150      continue
!
!     2.5 TRAITEMENT DES OPTIONS INVALIDES
!     ------------------------------------
    else
!       OPTION DE CALCUL NON VALIDE
        call assert(.false.)
    endif
!
!
!     3. ECRITURE DU CHAM_ELEM DE WEIBULL
!     -----------------------------------
    zr(iweib) = sigwk
!
end subroutine
