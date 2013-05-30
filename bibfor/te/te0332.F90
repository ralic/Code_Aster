subroutine te0332(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
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
!         CALCUL DU TAUX DE CROISSANCE DE CAVITES SELON UNE LOI DE
!         RICE ET TRACEY EN COMPORTEMENT NON-LINEAIRE.
!         ELEMENTS ISOPARAMETRIQUES 2D.
!
!         OPTION : 'RICE_TRACEY'
!
! ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
!         --->  NOMTE  : NOM DU TYPE D'ELEMENT
!
!     ------------------------------------------------------------------
!
!
    character(len=16) :: optcal(12)
    real(kind=8) :: sig(6), triax, volu, rsr0, numema, depseq
    real(kind=8) :: poids, r, volume, dvol, sigm, sigeq
    real(kind=8) :: dfdx(9), dfdy(9)
    real(kind=8) :: cong(4), varigp, varigm, sdrsrp, sdrsrm, crois
    integer :: nno, kp, npg, k, iritra, icompo, jtab(7)
    integer :: issopt, ima, nbvari, ipopp, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, ii, iret, iadzi, ivarmg, iazk24
    integer :: igeom, icong, ivarpg, isdrmr, isdrpr, kq
    logical :: laxi
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!     RECUPERATION DU NUMERO DE LA MAILLE :
!     -------------------------------------
    call tecael(iadzi, iazk24)
    ima =zi(iadzi)
    numema= dble(ima)
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    poids=0.d0
    triax=0.d0
    rsr0=0.d0
    volu=0.d0
    volume=0.d0
    dvol=0.d0
    depseq = 0.d0
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTPR', 'L', icong)
    call jevech('PVARIMR', 'L', ivarmg)
    call jevech('PVARIPR', 'L', ivarpg)
    call jevech('PSDRMR', 'L', isdrmr)
    call jevech('PSOUSOP', 'L', issopt)
    call tecach('OON', 'PVARIPR', 'L', 7, jtab,&
                iret)
    nbvari = max(jtab(6),1)*jtab(7)
    call jevech('PCOMPOR', 'L', icompo)
!     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
!
    if ((zk16(icompo).eq.'VMIS_ISOT_TRAC') .or. (zk16(icompo).eq.'VMIS_ISOT_LINE') .or.&
        (zk16(icompo).eq.'LEMAITRE') .or. (zk16(icompo).eq.'VMIS_ECMI_TRAC') .or.&
        (zk16(icompo).eq.'VMIS_ECMI_LINE') .or. (zk16(icompo).eq.'VISC_CIN1_CHAB') .or.&
        (zk16(icompo).eq.'VISC_CIN2_CHAB')) then
        ipopp = 1
    else
        call u2mesk('F', 'ELEMENTS3_74', 1, zk16(icompo))
    endif
!
    call jevech('PRICTRA', 'E', iritra)
    call jevech('PSDRPR', 'E', isdrpr)
!
    optcal(1) = zk24(issopt)(1:16)
    optcal(2) = zk24(issopt)(17:19)
!
    do 150 ii = 1, 4
        cong(ii)=0.d0
150  end do
    varigm =0.d0
    varigp =0.d0
!
!     --- BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL ---
!
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
            dvol=poids
            volume=volume+dvol
            do 165 ii = 1, 4
                cong(ii)=cong(ii)+dvol*zr(icong+4*kp+ii-5)
165          continue
            varigm = varigm+dvol*zr(ivarmg+nbvari*(kp-1)+ipopp-1)
            varigp = varigp+dvol*zr(ivarpg+nbvari*(kp-1)+ipopp-1)
200      continue
!        ------- SIGXX MOYENNEE SUR L'ELEMENT -----------
        sig(1) =cong(1)/volume
!        ------- SIGYY MOYENNEE SUR L'ELEMENT -----------
        sig(2) =cong(2)/volume
!        ------- SIGZZ MOYENNEE SUR L'ELEMENT -----------
        sig(3) =cong(3)/volume
!        ------- SIGXY MOYENNEE SUR L'ELEMENT -----------
        sig(4) =cong(4)/volume
!        ------- EPSPEQ MOYENNEE SUR L'ELEMENT ----------
        varigm =varigm/volume
        varigp =varigp/volume
!
        sigm = (sig(1)+sig(2)+sig(3))/3.d0
        sigeq = (&
                sig(1)-sigm)*(sig(1)-sigm)+(sig(2)-sigm)* (sig(2)- sigm)+(sig(3)-sigm)*(sig(3)-si&
                &gm)+2.d0*sig(4)* sig(4&
                )
        sigeq = sqrt(1.5d0*sigeq)
        triax = sigm/sigeq
        volu = volume
        depseq = varigp-varigm
        do 167 kq = 1, npg
            zr(isdrpr+kq-1) = zr(isdrmr+kq-1)
167      continue
!
!
        elseif ((optcal(1).eq.'SIGM_ELGA').and.(optcal(2).eq.'OUI'))&
    then
        do 300 kp = 1, npg
            r=0.d0
            sdrsrm = zr(isdrmr+kp-1)
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
            do 180 ii = 1, 4
                cong(ii)=zr(icong+4*kp+ii-5)
180          continue
            varigm =zr(ivarmg+nbvari*(kp-1)+ipopp-1)
            varigp =zr(ivarpg+nbvari*(kp-1)+ipopp-1)
!
            sigm = (cong(1)+cong(2)+cong(3))/3.d0
            sigeq = (&
                    cong(1)-sigm)*(cong(1)-sigm)+(cong(2)-sigm)* (cong(2)-sigm)+(cong(3)-sigm)*(c&
                    &ong(3)-sigm)+ 2.d0*cong(4) * cong(4&
                    )
            sigeq = sqrt(1.5d0*sigeq)
            triax = sigm/sigeq
            volu = volume
            depseq = varigp-varigm
            sdrsrp=sdrsrm+0.283d0*sign(1.d0,triax)*exp(1.5d0* abs(&
            triax))*depseq
            zr(isdrpr+kp-1) = sdrsrp
            crois = exp(sdrsrp)
            if (crois .gt. rsr0) then
                rsr0=crois
                volu=volume
            endif
!
300      continue
!
!
        elseif ((optcal(1).eq.'SIGM_ELMOY').and.(optcal(2).eq.'OUI'))&
    then
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
            dvol=poids
            volume=volume+dvol
            do 220 ii = 1, 4
                cong(ii)=cong(ii)+zr(icong+4*kp+ii-5)*dvol
220          continue
            varigm=varigm+dvol*zr(ivarmg+nbvari*(kp-1)+ipopp-1)
            varigp=varigp+dvol*zr(ivarpg+nbvari*(kp-1)+ipopp-1)
400      continue
!        ------- SIGXX MOYENNEE SUR L'ELEMENT -----------
        sig(1) =cong(1)/volume
!        ------- SIGYY MOYENNEE SUR L'ELEMENT -----------
        sig(2) =cong(2)/volume
!        ------- SIGZZ MOYENNEE SUR L'ELEMENT -----------
        sig(3) =cong(3)/volume
!        ------- SIGXY MOYENNEE SUR L'ELEMENT -----------
        sig(4) =cong(4)/volume
!        ------- EPSPEQ MOYENNEE SUR L'ELEMENT ----------
        varigm=varigm/volume
        varigp=varigp/volume
!
        sigm = (sig(1)+sig(2)+sig(3))/3.d0
        sigeq = (&
                sig(1)-sigm)*(sig(1)-sigm)+(sig(2)-sigm)* (sig(2)- sigm)+(sig(3)-sigm)*(sig(3)-si&
                &gm)+2.d0*sig(4)* sig(4&
                )
        sigeq = sqrt(1.5d0*sigeq)
        triax = sigm/sigeq
        volu = volume
        depseq = varigp-varigm
        sdrsrm = zr(isdrmr)
        sdrsrp = sdrsrm+0.283d0*sign(1.d0,triax)*exp(1.5d0* abs(triax) )*depseq
        do 225 kq = 1, npg
            zr(isdrpr+kq-1) = sdrsrp
225      continue
        rsr0 = exp(sdrsrp)
!
!
        elseif ((optcal(1).eq.'SIGM_ELGA').and.(optcal(2).eq.'NON'))&
    then
        do 100 kp = 1, npg
            k=(kp-1)*nno
            r=0.d0
            do 175 ii = 1, 4
                cong(ii)=zr(icong+(4*kp)-5+ii)
175          continue
            varigm=zr(ivarmg+nbvari*(kp-1)+ipopp-1)
            varigp=zr(ivarpg+nbvari*(kp-1)+ipopp-1)
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, poids)
            if (laxi) then
                do 240 ii = 1, nno
                    r=r+zr(igeom+2*ii-2)*zr(ivf+k+ii-1)
240              continue
                poids=poids*r
            endif
            dvol=poids
            volume=volume+dvol
            sigm = (cong(1)+cong(2)+cong(3))/3.d0
            sigeq = (&
                    cong(1)-sigm)*(cong(1)-sigm)+(cong(2)-sigm)* (cong(2)-sigm)+(cong(3)-sigm)*(c&
                    &ong(3)-sigm)+ 2.d0*cong(4) * cong(4&
                    )
            sigeq = sqrt(1.5d0*sigeq)
            triax = triax+dvol*sigm/sigeq
            depseq = depseq+(varigp-varigm)*dvol
!
100      continue
!
        triax = triax/volume
        depseq = depseq/volume
        volu = volume
        do 177 kq = 1, npg
            zr(isdrpr+kq-1) = zr(isdrmr+kq-1)
177      continue
!
!
    else
!       OPTION DE CALCUL NON VALIDE
        call assert(.false.)
    endif
!
!
    zr(iritra) = triax
    zr(iritra+1) = rsr0
    zr(iritra+2) = volu
    zr(iritra+3) = numema
    zr(iritra+4) = depseq
!
!     DESTRUCTION DES OBJETS CREES DANS LA BASE
!
end subroutine
