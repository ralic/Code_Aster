subroutine te0331(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/epdcp.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/psvari.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecach.h'
    include 'asterfort/vpri2d.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     FONCTION REALISEE :
!
!         CALCUL DE LA CONTRAINTE DE WEIBULL D'UNE STRUCTURE
!         EN COMPORTEMENT NON-LINEAIRE.
!         ELEMENTS ISOPARAMETRIQUES 2D.
!
!         OPTION : 'WEIBULL'
!
! ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
!         --->  NOMTE  : NOM DU TYPE D'ELEMENT
!
!     ------------------------------------------------------------------
!
    integer :: icodre(4)
    integer :: codres
    character(len=4) :: fami
    character(len=8) :: nomres(4)
    character(len=16) :: optcal(12), phenom
    real(kind=8) :: sig(6), sigi, dsigwb, valres(4), epsgi, r8bid
    real(kind=8) :: poids, r, volume, volact, dvol, seuil, m, v0
    real(kind=8) :: cong(4), epsq(4), dfdx(9), dfdy(9), pp, ppt
    real(kind=8) :: tc(6), tdp(6), sigold, signew, sref, tg, tmoy
!
    integer :: nno, kp, npg, k, ii, iweib, jtab(7), nnos, jgano, ndim
    integer :: idefg, issopt, ipopp, ipoppt
    integer :: ipoids, ivf, idfde, imate
    integer :: igeom, icong, ivarig
    integer :: isigie, isigis, icompo, nbvari
    logical :: laxi
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iret
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    poids=0.d0
    dsigwb=0.d0
    volume=0.d0
    volact=0.d0
    dvol=0.d0
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    nomres(1) = 'M'
    nomres(2) = 'VOLU_REFE'
    nomres(3) = 'SEUIL_EPSP_CUMU'
    nomres(4) = 'SIGM_REFE'
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTRG', 'L', icong)
    call jevech('PVARIPG', 'L', ivarig)
    call jevech('PSOUSOP', 'L', issopt)
    call jevech('PDOMMAG', 'L', isigie)
    call jevech('PWEIBUL', 'E', iweib)
    call jevech('PSIGISG', 'E', isigis)
!
    call tecach('OON', 'PVARIPG', 'L', 7, jtab,&
                iret)
    nbvari = max(jtab(6),1)*jtab(7)
    call jevech('PCOMPOR', 'L', icompo)
!     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
!
    call psvari(zk16(icompo), nbvari, '2D', ipopp, ipoppt)
!
    optcal(1) = zk24(issopt)(1:16)
    optcal(2) = zk24(issopt)(17:19)
!
!
    do 150 ii = 1, 4
        cong(ii)=0.d0
        epsq(ii)=0.d0
150  end do
! -FONCTION SEUIL
    ppt = 0.d0
    pp = 0.d0
!
!
!     --- CAS SIGU DEPEND DE LA TEMPERATURE (WEIBULL_FO)?
!     SI OUI ET QU IL N Y A PAS DE CHAMP DE TEMPERATURE
!     ARRET
!
    call rccoma(zi(imate), 'WEIBULL', 1, phenom, codres)
!
    if (optcal(1) .eq. 'SIGM_ELMOY') then
        tmoy = 0.d0
    endif
!
!
!     --- RECUPERATION DES DONNEES MATERIAU ---
!
    call rcvalb(fami, 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', r8bid,&
                3, nomres, valres, icodre, 1)
    call rcvalb(fami, 1, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', r8bid,&
                1, nomres(3), valres(3), icodre(3), 1)
    if (icodre(3) .ne. 0) valres(3) = 1.d-6
    m = valres(1)
    v0 = valres(2)
    seuil = valres(3)
!
!     --- BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL ---
!
!=================================================================
!=================================================================
    if ((optcal(1).eq.'SIGM_ELMOY') .and. (optcal(2).eq.'NON')) then
        do 200 kp = 1, npg
            k=(kp-1)*nno
            r=0.d0
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
            if (laxi) then
                do 160 ii = 1, nno
                    r=r+zr(igeom+2*ii-2)*zr(ivf+k+ii-1)
160              continue
                poids=poids*r
            endif
! VOLUME PLASTIFIE
            pp =zr(ivarig+nbvari*(kp-1)+ipopp-1)
            if (pp .ge. seuil) then
                dvol=poids
                volume=volume+dvol
                do 165 ii = 1, 4
                    cong(ii)=cong(ii)+dvol*zr(icong+4*kp+ii-5)
165              continue
!           --- TEMPERATURE MOYENNE
                call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                            1, tg, iret)
                if (iret .ne. 0) tg = 0.d0
                tmoy = tmoy + tg*dvol
            endif
! VOLUME PLASTIQUE ACTIF
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
            if (ppt .ge. (1.0d0)) then
                dvol=poids
                volact=volact+dvol
            endif
200      continue
!
        sigi = 0.d0
        if ((volact.ne.0.d0) .and. (volume.ne.0.d0)) then
            sig(1) =cong(1)/volume
            sig(2) =cong(2)/volume
            sig(3) =cong(3)/volume
            sig(4) =cong(4)/volume
            call vpri2d(sig, sigi)
!
            tmoy = tmoy/volume
            call rcvalb(fami, 1, 1, '+', zi(imate),&
                        ' ', phenom, 1, 'TEMP', tmoy,&
                        1, nomres(4), valres(4), icodre(4), 1)
            sref = valres(4)
            sigi = sigi/sref
        endif
        sigold=zr(isigie)
        if (sigi .gt. sigold) then
            zr(isigis)=sigi
        else
            zr(isigis)=zr(isigie)
        endif
        sigi=zr(isigis)
!
        dsigwb=volume/v0*(sigi**m)
!=================================================================
!=================================================================
        elseif ((optcal(1).eq.'SIGM_ELGA').and.(optcal(2).eq.'OUI'))&
    then
        do 300 kp = 1, npg
            r=0.d0
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
            if (laxi) then
                do 170 ii = 1, nno
                    r=r+zr(igeom+2*ii-2)*zr(ivf+k+ii-1)
170              continue
                poids=poids*r
            endif
            volume=poids
            call jevech('PDEFORR', 'L', idefg)
            do 180 ii = 1, 4
                cong(ii)=zr(icong+4*kp+ii-5)
                epsq(ii)=zr(idefg+4*kp+ii-5)
180          continue
            pp=zr(ivarig+nbvari*(kp-1)+ipopp-1)
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
!
            signew = 0.d0
            if (ppt .ge. (1.d0)) then
!
!              ------CALCUL DE SIGI ET EPSI---------
!
                tc(1) = cong(1)
                tc(2) = cong(2)
                tc(3) = cong(3)
                tc(4) = cong(4)
                tc(5) = 0.d0
                tc(6) = 0.d0
!
                tdp(1) = epsq(1)
                tdp(2) = epsq(2)
                tdp(3) = epsq(3)
                tdp(4) = epsq(4)
                tdp(5) = 0.d0
                tdp(6) = 0.d0
                call epdcp(tc, tdp, sigi, epsgi)
                call rcvalb(fami, kp, 1, '+', zi(imate),&
                            ' ', phenom, 0, ' ', 0.d0,&
                            1, nomres(4), valres(4), icodre(4), 1)
                sref = valres(4)
!
                signew=exp((-epsgi/2.d0))*sigi/sref
            endif
            sigold=zr(isigie+kp-1)
            if (signew .gt. sigold) then
                zr(isigis+kp-1)=signew
            else
                zr(isigis+kp-1)=zr(isigie+kp-1)
            endif
            signew=zr(isigis+kp-1)
            dsigwb=dsigwb+volume*(signew**m)/v0
!
300      continue
!=================================================================
!=================================================================
        elseif ((optcal(1).eq.'SIGM_ELMOY').and.(optcal(2).eq.'OUI'))&
    then
!
        do 400 kp = 1, npg
            r=0.d0
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
            if (laxi) then
                do 210 ii = 1, nno
                    r=r+zr(igeom+2*ii-2)*zr(ivf+k+ii-1)
210              continue
                poids=poids*r
            endif
! VOL PLASTIFIE
            pp =zr(ivarig+nbvari*(kp-1)+ipopp-1)
            call jevech('PDEFORR', 'L', idefg)
            if (pp .ge. seuil) then
                dvol=poids
                volume=volume+dvol
                do 220 ii = 1, 4
                    cong(ii)=cong(ii)+dvol*zr(icong+4*kp+ii-5)
                    epsq(ii)=epsq(ii)+dvol*zr(idefg+4*kp+ii-5)
220              continue
!           --- TEMPERATURE MOYENNE
                call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                            1, tg, iret)
                if (iret .ne. 0) tg = 0.d0
                tmoy = tmoy + tg*dvol
            endif
! VOL PLASTIQUE ACTIF
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
            if (ppt .ge. (1.0d0)) then
                dvol=poids
                volact=volact+dvol
            endif
!
400      continue
!
        signew = 0.d0
        if ((volact.ne.(0.d0)) .and. (volume.ne.0.d0)) then
            tc(1) = cong(1)/volume
            tc(2) = cong(2)/volume
            tc(3) = cong(3)/volume
            tc(4) = cong(4)/volume
            tc(5) = 0.d0
            tc(6) = 0.d0
!
            tdp(1) = epsq(1)/volume
            tdp(2) = epsq(2)/volume
            tdp(3) = epsq(3)/volume
            tdp(4) = epsq(4)/volume
            tdp(5) = 0.d0
            tdp(6) = 0.d0
            call epdcp(tc, tdp, sigi, epsgi)
            tmoy = tmoy/volume
            call rcvalb(fami, 1, 1, '+', zi(imate),&
                        ' ', phenom, 1, 'TEMP', tmoy,&
                        1, nomres(4), valres(4), icodre(4), 1)
            sref = valres(4)
            signew=exp((-epsgi/2.d0))*sigi/sref
        endif
        sigold=zr(isigie)
        if (signew .gt. sigold) then
            zr(isigis)=signew
        else
            zr(isigis)=zr(isigie)
        endif
        signew= zr(isigis)
        dsigwb=volume*(signew**m)/v0
!=================================================================
!=================================================================
        elseif ((optcal(1).eq.'SIGM_ELGA').and.(optcal(2).eq.'NON'))&
    then
        do 100 kp = 1, npg
            k=(kp-1)*nno
            r=0.d0
            do 175 ii = 1, 4
                cong(ii)=zr(icong+(4*kp)-5+ii)
175          continue
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
            if (laxi) then
                do 240 ii = 1, nno
                    r=r+zr(igeom+2*ii-2)*zr(ivf+k+ii-1)
240              continue
                poids=poids*r
            endif
            volume=poids
!
            sigi = 0.d0
            pp=zr(ivarig+nbvari*(kp-1)+ipopp-1)
            if ((zk16(icompo).eq.'LEMAITRE') .and. (pp.ge.seuil)) then
                ppt = 1.d0
            else
                ppt =zr(ivarig+nbvari*(kp-1)+ipoppt-1)
            endif
            if (ppt .ge. (1.d0)) then
! CALCUL DE SIGI
                call vpri2d(cong, sigi)
                call rcvalb(fami, kp, 1, '+', zi(imate),&
                            ' ', phenom, 0, ' ', 0.d0,&
                            1, nomres(4), valres(4), icodre(4), 1)
                sref = valres(4)
                sigi = sigi/sref
            endif
            sigold=zr(isigie+kp-1)
            if (sigi .gt. sigold) then
                zr(isigis+kp-1)=sigi
            else
                zr(isigis+kp-1)=zr(isigie+kp-1)
            endif
            sigi=zr(isigis+kp-1)
            dsigwb=dsigwb+volume*(sigi**m)/v0
!
100      continue
!
    else
!        OPTION DE CALCUL NON VALIDE
        call assert(.false.)
    endif
!=================================================================
!=================================================================
    zr(iweib)=dsigwb
!
!     DESTRUCTION DES OBJETS CREES DANS LA BASE
!
end subroutine
