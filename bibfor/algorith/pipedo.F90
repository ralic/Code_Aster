subroutine pipedo(ndim, typmod, tau, mate, vim,&
                  epsm, epspc, epsdc, etamin, etamax,&
                  a0, a1, a2, a3, etas)
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
! aslint: disable=W1501
    implicit none
    include 'asterc/r8vide.h'
    include 'asterfort/criteo.h'
    include 'asterfort/diago3.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/zerod2.h'
    include 'asterfort/zerog2.h'
    character(len=8) :: typmod(*)
    integer :: ndim, mate
    real(kind=8) :: vim(7), epsm(6), epspc(6), epsdc(6)
    real(kind=8) :: etamin, etamax, tau
    real(kind=8) :: a0, a1, a2, a3, etas
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
!
! LOI DE COMPORTEMENT ENDO_ORTH_BETON
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  TYPMOD : TYPE DE MODELISATION
! IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
! IN  MATE   : MATERIAU CODE
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  EPSM   : DEFORMATIONS EN T-
! IN  EPSPC  : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSDC  : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! IN  ETAMIN : DONNEE UTILISATEUR DU MINIMUM DE ETA
! IN  ETAMAX : DONNEE UTILISATEUR DU MAXIMUM DE ETA
! OUT A0     : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
! OUT A1     : CF A0
! OUT A2     : IDEM A0 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
! OUT A3     : IDEM A1 POUR LA SECONDE SOLUTION EVENTUELLE;R8VIDE SINON
! OUT ETAS   : SI PAS DE SOLUTION : LE MINIMUM ; R8VIDE SINON
!
! ----------------------------------------------------------------------
!
    integer :: nbres
    parameter   (nbres=6)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
    real(kind=8) :: valres(nbres)
!
!
    logical :: cplan, rechbg, rechbd
    integer :: ndimsi, k, nsol, iter, nitmax
    integer :: i, j, l, t(3, 3), kpg, spt
    real(kind=8) :: coplan, un
    real(kind=8) :: rac2, critp
    real(kind=8) :: eta
    real(kind=8) :: e, nu, lambda, mu, seuil, trepsm
    real(kind=8) :: k0, k1, k2, alpha
    real(kind=8) :: epsp(6), epsd(6), x(4), y(4), z(4)
    real(kind=8) :: epstol
    real(kind=8) :: treps, eta1, eta2, etac, crit1, crit2, critc, critp3
    real(kind=8) :: seuila, critp1, critp2, crit3, rpas
    real(kind=8) :: eta3, c, r
    real(kind=8) :: b(6), d, rec(6), br(6), vecb(3, 3), valb(3), tole
    real(kind=8) :: epsdp(6), epsdm(6), ccp(6), ccm(6), veccp(3, 3), veccm(3, 3)
    real(kind=8) :: valccp(3), valccm(3), ccpp(6), ccpm(6), cpep(6), cpem(6)
    real(kind=8) :: fbp(6), fbm(6), trebp, trebm, vecfbp(3, 3), valfbp(3)
    real(kind=8) :: vecfbm(3, 3), valfbm(3), rtempp, rtempm, vecc(3, 3)
    real(kind=8) :: valcc(3)
    real(kind=8) :: dcoefd, ene, fdp, fdm, trem
    real(kind=8) :: stra, trb
    real(kind=8) :: ecrob, ecrod
!
! ----------------------------------------------------------------------
!
    un = 1.d0
    nitmax = 50
    epstol = 1.d-1
    r = 0.61803399d0
    c = 1.d0-r
    nsol = 0
!
! TOLE: TOLERANCE POUR ARRET EVOLUTION DE L ENDOMMAGEMENT
    tole=1.d-2
!
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
! -- OPTION ET MODELISATION
    cplan = (typmod(1).eq.'C_PLAN  ')
    ndimsi = 2*ndim
    rac2 = sqrt(2.d0)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
! -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    mu = e/(2.d0*(1.d0+nu))
!
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'ALPHA'
    nomres(2) = 'K0'
    nomres(3) = 'K1'
    nomres(4) = 'K2'
    nomres(5) = 'ECROB'
    nomres(6) = 'ECROD'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'ENDO_ORTH_BETON', 0, ' ', 0.d0,&
                nbres, nomres, valres, icodre, 1)
    alpha = valres(1)
    k0 = valres(2)
    k1 = valres(3)
    k2 = valres(4)
    ecrob = valres(5)
    ecrod = valres(6)
!
!
    trepsm=epsm(1)+epsm(2)+epsm(3)
    if (trepsm .gt. 0.d0) then
        trepsm=0.d0
    endif
!
    stra = trepsm
    seuil = k0-k1*stra*(atan2(-stra/k2,un))
    seuila = seuil
!
!
!
!
!
! ======================================================================
!                CALCUL DES DEFORMATIONS POUR LINEARISATION
! ======================================================================
!
!
!
!    ETAT MECANIQUE EN T-
    do 3 i = 1, 3
        b(i) = 1.d0-vim(i)
 3  end do
    do 300 i = 4, 6
        b(i) = -vim(i)
300  end do
    d = vim(7)
!
!      SEUIL=SEUIL+K0*TAU
    seuil = seuil+seuila*tau
    trb = b(1)+b(2)+b(3)
!
! -- CAS DE L'ENDOMMAGEMENT SATURE
    if ((trb.le.tole) .or. (d.ge.(1.d0-tole))) then
        a0 = 0.d0
        a1 = 0.d0
        a2 = r8vide()
        a3 = r8vide()
        etas = r8vide()
        goto 9999
    endif
!
! -- CALCUL DES DEFORMATIONS EN PRESENCE DE CONTRAINTES PLANES
!
    if (cplan) then
        coplan = - nu/(1.d0-nu)
        epspc(3) = coplan * (epspc(1)+epspc(2))
        epsdc(3) = coplan * (epsdc(1)+epsdc(2))
    endif
    do 44 k = 1, 3
        epsp(k) = epspc(k)
        epsd(k) = epsdc(k)
44  end do
    do 45 k = 4, ndimsi
        epsp(k) = epspc(k)/rac2
        epsd(k) = epsdc(k)/rac2
45  end do
    if (ndimsi .lt. 6) then
        do 46 k = ndimsi+1, 6
            epsp(k)=0.d0
            epsd(k)=0.d0
46      continue
    endif
!
!
!-- CALCUL DES FORCES THERMO CALCULEES AVEC +EPSD ET -EPSD
!-- ON SUPPOSE EPS=ETA*EPSD POUR ETA>INFINI
!--IL FAUT TRAVAILLER DANS L ESPACE PROPRE DE B
!
!
    call r8inir(6, 1.d0, rec, 1)
    call r8inir(6, 0.d0, br, 1)
    call r8inir(6, 0.d0, epsdp, 1)
    call r8inir(6, 0.d0, epsdm, 1)
!
    call diago3(b, vecb, valb)
    do 701 i = 1, 3
        br(i)=valb(i)
701  end do
!
    if (abs(valb(1)) .lt. tole) then
        rec(1)=0.d0
        rec(4)=0.d0
        rec(5)=0.d0
    endif
    if (abs(valb(2)) .lt. tole) then
        rec(2)=0.d0
        rec(4)=0.d0
        rec(6)=0.d0
    endif
    if (abs(valb(3)) .lt. tole) then
        rec(3)=0.d0
        rec(5)=0.d0
        rec(6)=0.d0
    endif
!
    do 202 i = 1, 3
        do 203 j = i, 3
            do 204 k = 1, 3
                do 205 l = 1, 3
                    epsdp(t(i,j))=epsdp(t(i,j))+vecb(k,i)*epsd(t(k,l))&
                    *vecb(l,j)
                    epsdm(t(i,j))=epsdm(t(i,j))-vecb(k,i)*epsd(t(k,l))&
                    *vecb(l,j)
205              continue
204          continue
203      continue
202  continue
!
    call r8inir(6, 0.d0, ccp, 1)
    call r8inir(6, 0.d0, ccm, 1)
!
    do 9 i = 1, 3
        do 10 j = i, 3
            do 11 k = 1, 3
                ccp(t(i,j))=ccp(t(i,j))+br(t(i,k))*epsdp(t(k,j))+&
                br(t(j,k))*epsdp(t(k,i))
                ccm(t(i,j))=ccm(t(i,j))+br(t(i,k))*epsdm(t(k,j))+&
                br(t(j,k))*epsdm(t(k,i))
11          continue
10      continue
 9  end do
    call diago3(ccp, veccp, valccp)
    call diago3(ccm, veccm, valccm)
!
!
    call r8inir(6, 0.d0, ccpp, 1)
    call r8inir(6, 0.d0, ccpm, 1)
    call r8inir(6, 0.d0, cpep, 1)
    call r8inir(6, 0.d0, cpem, 1)
!
!
    do 12 i = 1, 3
        if (valccp(i) .lt. 0.d0) then
            valccp(i)=0.d0
        endif
        if (valccm(i) .lt. 0.d0) then
            valccm(i)=0.d0
        endif
12  end do
!
    do 13 i = 1, 3
        do 14 j = i, 3
            do 15 k = 1, 3
                ccpp(t(i,j))=ccpp(t(i,j))+veccp(i,k)*valccp(k)*veccp(&
                j,k)
                ccpm(t(i,j))=ccpm(t(i,j))+veccm(i,k)*valccm(k)*veccm(&
                j,k)
15          continue
14      continue
13  end do
!
    do 16 i = 1, 3
        do 17 j = i, 3
            do 18 k = 1, 3
                cpep(t(i,j))=cpep(t(i,j))+ ccpp(t(i,k))*epsdp(t(k,j))+&
                ccpp(t(j,k))*epsdp(t(k,i))
                cpem(t(i,j))=cpem(t(i,j))+ ccpm(t(i,k))*epsdm(t(k,j))+&
                ccpm(t(j,k))*epsdm(t(k,i))
18          continue
17      continue
16  end do
!
    call r8inir(6, 0.d0, fbp, 1)
    call r8inir(6, 0.d0, fbm, 1)
!
    trebp=0.d0
    trebm=0.d0
!
    do 301 i = 1, 3
        trebp=trebp+ccp(i)/2
        trebm=trebm+ccm(i)/2
301  end do
!
    if (trebp .gt. 0.d0) then
        do 19 i = 1, 6
            fbp(i)=-lambda*trebp*epsdp(i)
19      continue
    endif
    if (trebm .gt. 0.d0) then
        do 21 i = 1, 6
            fbm(i)=-lambda*trebm*epsdm(i)
21      continue
    endif
!
    do 20 i = 1, 6
        fbp(i)=(fbp(i)-mu/2.d0*cpep(i))
        fbm(i)=(fbm(i)-mu/2.d0*cpem(i))
20  end do
!
    call diago3(fbp, vecfbp, valfbp)
    call diago3(fbm, vecfbm, valfbm)
!
    rtempp=0.d0
    rtempm=0.d0
!
    do 29 i = 1, 3
        if (valfbp(i) .gt. 0.d0) then
            valfbp(i)=0.d0
        endif
        rtempp=rtempp+valfbp(i)*valfbp(i)
        if (valfbm(i) .gt. 0.d0) then
            valfbm(i)=0.d0
        endif
        rtempm=rtempm+valfbm(i)*valfbm(i)
29  end do
!
!
    treps=epsdp(1)+epsdp(2)+epsdp(3)
    call diago3(epsdp, vecc, valcc)
    do 22 i = 1, 3
        if (valcc(i) .gt. 0.d0) then
            valcc(i)=0.d0
        endif
22  end do
    trem=valcc(1)**2+valcc(2)**2+valcc(3)**2
    if (treps .gt. 0.d0) then
        treps=0.d0
    endif
    dcoefd=2.d0*(1.d0-d)
    ene=lambda/2*treps**2+mu*trem
!      FDP=DCOEFD*ENE-2.d0*ECROD*D
    fdp=dcoefd*ene
    if (fdp .lt. 0.d0) then
        fdp=0.d0
    endif
!
    treps=epsdm(1)+epsdm(2)+epsdm(3)
    call diago3(epsdm, vecc, valcc)
    do 32 i = 1, 3
        if (valcc(i) .gt. 0.d0) then
            valcc(i)=0.d0
        endif
32  end do
    trem=valcc(1)**2+valcc(2)**2+valcc(3)**2
    if (treps .gt. 0.d0) then
        treps=0.d0
    endif
    ene=lambda/2*treps**2+mu*trem
!      FDM=DCOEFD*ENE-2.d0*ECROD*D
    fdm=dcoefd*ene
    if (fdm .lt. 0.d0) then
        fdm=0.d0
    endif
!
!----------------------------------------------------------
!---COMPORTEMENT A L INFINI ET NOMBRE DE SOLUTIONS---------
!
!   DE MANIERE GENERALE: NSOL=2 (ou NSOL=0)
!                        RECHBG=TRUE    RECHBD=TRUE
!
!   EXCEPTIONS:  RTEMPM=0 ET FDM=0   NSOL=1
!                        RECHBG=FALSE   RECHBD=TRUE
!                RTEMPP=0 ET FDP=0   NSOL=-1
!                        RECHBG=TRUE    RECHBD=FALSE
!
!  on ne considere pas le cas proche de zero pour l instant
!
!-----------------------------------------------------------
!
!
!
! -- RECHBG : VRAI -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
!                     AVOIR F(ETA)>0 ET F'(ETA)<0
!             FAUX -> IL FAUT TROUVER ETA SUFFISAMMENT PETIT POUR
!                     AVOIR F(ETA)<0
!    RECHBD : IDEM A DROITE
!
    nsol=2
    if ((rtempm.eq.0.d0) .and. (fdm.eq.0.d0)) nsol=1
    if ((rtempp.eq.0.d0) .and. (fdp.eq.0.d0)) nsol=-1
!
!
    if (abs(nsol) .gt. 0) then
        if ((nsol.eq.2) .or. (nsol.eq.-1)) rechbg=.true.
        if ((nsol.eq.2) .or. (nsol.eq.1)) rechbd=.true.
!
        eta=etamin
!
        call criteo(epsp, epsd, eta, b, d,&
                    lambda, mu, alpha, ecrob, ecrod,&
                    seuil, crit1, critp1)
!
        iter=0
        rpas=(etamax-etamin)
!
        if (rechbg) then
60          continue
            iter=iter+1
            rpas=rpas*2
            if ((crit1.lt.0.d0) .or. (critp1.ge.0.d0)) then
                eta=eta-rpas
                call criteo(epsp, epsd, eta, b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, crit1, critp1)
                goto 60
            endif
!          write (6,*) 'ITER-1 = ',ITER
        else
30          continue
            iter=iter+1
            rpas=rpas*2
            if (crit1 .ge. 0.d0) then
                eta=eta-rpas
                call criteo(epsp, epsd, eta, b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, crit1, critp1)
                goto 30
            endif
!          write (6,*) 'ITER-1b = ',ITER
        endif
        eta1=eta
!
!
        eta=etamax
        rpas=(etamax-etamin)
        call criteo(epsp, epsd, eta, b, d,&
                    lambda, mu, alpha, ecrob, ecrod,&
                    seuil, crit2, critp2)
        iter=0
        if (rechbd) then
40          continue
            iter=iter+1
            rpas=rpas*2
            if ((crit2.lt.0.d0) .or. (critp2.le.0.d0)) then
                eta=eta+rpas
                call criteo(epsp, epsd, eta, b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, crit2, critp2)
                goto 40
            endif
!          write (6,*) 'ITER-2 = ',ITER
        else
50          continue
            iter=iter+1
            rpas=rpas*2
            if (crit2 .ge. 0.d0) then
                eta=eta+rpas
                call criteo(epsp, epsd, eta, b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, crit2, critp2)
                goto 50
            endif
!          write (6,*) 'ITER-2b = ',ITER
        endif
        eta2=eta
!
!
    endif
!
! -- CAS A UNE SOLUTION
    if (abs(nsol) .eq. 1) then
        if (nsol .eq. 1) then
            x(1)=eta1
            y(1)=crit1
            z(1)=critp1
            x(2)=eta2
            y(2)=crit2
            z(2)=critp2
        else
            x(1)=eta2
            y(1)=crit2
            z(1)=critp2
            x(2)=eta1
            y(2)=crit1
            z(2)=critp1
        endif
        x(3)=x(1)
        y(3)=y(1)
        z(3)=z(1)
        do 200 iter = 1, nitmax
            if (abs(y(3)) .le. epstol*seuila*tau) goto 201
            if (mod(iter,5) .ne. 0) then
                call zerog2(x, y, z, iter)
            else
                call zerod2(x, y, z)
            endif
            call criteo(epsp, epsd, x(3), b, d,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, y(3), z(3))
200      continue
        call u2mess('F', 'UTILITAI2_53')
201      continue
!        write (6,*) 'ITER-3 = ',ITER
        eta=x(3)
        nsol=1
    endif
!
! -- CAS A MINIMUM (ZERO OU DEUX SOLUTIONS)
    if (nsol .eq. 2) then
        etamin = eta1
        etamax = eta2
        iter = 0
! -- ON CHERCHE LE MINIMUM : ON SE DEPLACE SUR LE SEGMENT [ETA1,ETA2]
!    ET ON RACCOURCIT L'INTERVALLE EN UTILISANT LA DERIVEE
250      continue
!     TEST D'ARRET POUR UN MINIMUM AU-DESSUS DE 0
        iter=iter+1
        if (iter .gt. nitmax) then
!            write (6,*) 'ETAMIN = ',ETAMIN,' ; ETAMAX = ',ETAMAX
!            write (6,*) 'ETA1 = ',ETA1,' ; CRIT1 = ',CRIT1,
!     &                     ' ; CRITP1',CRITP1
!            write (6,*) 'ETA2 = ',ETA2,' ; CRIT2 = ',CRIT2,
!     &                     ' ; CRITP2',CRITP2
            call u2mess('F', 'PILOTAGE_83')
        endif
        if ((abs(critp1*(eta2-eta1)).lt.epstol*seuila*tau) .and.&
            (abs(critp2*(eta2-eta1)).lt.epstol*seuila*tau)) then
            if ((crit1+critp1*(eta2-eta1)) .gt. 0.d0) then
                if ((crit2+critp2*(eta1-eta2)) .gt. 0.d0) then
                    goto 260
                endif
            endif
        endif
        if (crit1 .lt. crit2) then
            etac=c*eta1+r*eta2
        else
            etac=c*eta2+r*eta1
        endif
        call criteo(epsp, epsd, etac, b, d,&
                    lambda, mu, alpha, ecrob, ecrod,&
                    seuil, critc, critp)
!     TEST D'ARRET SI ON PASSE EN DESSOUS DE 0 (-> 2 SOLUTIONS)
        if (critc .lt. 0.d0) then
            goto 260
        endif
        if (critp .gt. 0.d0) then
            eta2=etac
            crit2=critc
            critp2=critp
        else
            eta1=etac
            crit1=critc
            critp1=critp
        endif
!     TEST D'ARRET DE PRECISION NUMERIQUE
        if (eta2 .eq. eta1) call u2mess('F', 'PILOTAGE_84')
        goto 250
!
! -- SI MINIMUM SOUS 0 : 2 SOLUTIONS, SINON : 0 SOLUTION
!
260      continue
!       write (6,*) 'ITER-4 = ',ITER
        if (critc .lt. 0.d0) then
            nsol=2
            eta3=etac
            crit3=critc
            critp3=critp
!
            x(1)=etac
            y(1)=critc
            z(1)=critp
            x(2)=etamax
            call criteo(epsp, epsd, x(2), b, d,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, y(2), z(2))
            x(3)=x(2)
            y(3)=y(2)
            z(3)=z(2)
            do 400 iter = 1, nitmax
                if (abs(y(3)) .le. epstol*seuila*tau) goto 401
                if (mod(iter,5) .ne. 0) then
                    call zerog2(x, y, z, iter)
                else
                    call zerod2(x, y, z)
                endif
                call criteo(epsp, epsd, x(3), b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, y(3), z(3))
400          continue
            call u2mess('F', 'PILOTAGE_83')
401          continue
!          write (6,*) 'ITER-5 = ',ITER
            eta1=x(3)
!
            x(1)=eta3
            y(1)=crit3
            z(1)=critp3
            x(2)=etamin
            call criteo(epsp, epsd, x(2), b, d,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, y(2), z(2))
            x(3)=x(2)
            y(3)=y(2)
            z(3)=z(2)
            do 500 iter = 1, nitmax
                if (abs(y(3)) .le. epstol*seuila*tau) goto 501
                if (mod(iter,5) .ne. 0) then
                    call zerog2(x, y, z, iter)
                else
                    call zerod2(x, y, z)
                endif
!
!
                call criteo(epsp, epsd, x(3), b, d,&
                            lambda, mu, alpha, ecrob, ecrod,&
                            seuil, y(3), z(3))
!
500          continue
!          WRITE(6,*) 'ETA=',X(3)
!          WRITE(6,*) 'CRITERE=',Y(3)
!          WRITE(6,*) 'DCRIT=',Z(3)
            call u2mess('F', 'PILOTAGE_83')
501          continue
!          write (6,*) 'ITER-5b = ',ITER
            eta2=x(3)
        else
            eta=etac
            nsol=0
        endif
    endif
!
    if (nsol .eq. 0) then
        etas=eta
        call criteo(epsp, epsd, eta, b, d,&
                    lambda, mu, alpha, ecrob, ecrod,&
                    seuila, crit1, critp)
        a0=crit1/seuila
    else
        seuil=seuila
        etas=r8vide()
!
!        WRITE(6,*) 'ETA1=',ETA1
!        WRITE(6,*) 'ETA2=',ETA2
!
        if (nsol .eq. 2) then
            eta=eta1
        endif
        call criteo(epsp, epsd, eta, b, d,&
                    lambda, mu, alpha, ecrob, ecrod,&
                    seuila, crit1, critp)
!
!
!
!
!
! ======================================================================
!                        LINEARISATION DU CRITERE
! ======================================================================
!
        a0 = (crit1 - eta*critp ) /seuila
        a1 =critp/ seuila
!
        if (nsol .eq. 2) then
            eta=eta2
            call criteo(epsp, epsd, eta, b, d,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuila, crit1, critp)
! ======================================================================
!                        LINEARISATION DU CRITERE
! ======================================================================
            a2 = (crit1 - eta*critp ) /seuila
            a3 =critp/ seuila
        else
            a2=r8vide()
            a3=r8vide()
        endif
    endif
!         WRITE(6,*) 'A0=',A0
!         WRITE(6,*) 'A1=',A1
!         WRITE(6,*) 'A2=',A2
!         WRITE(6,*) 'A3=',A3
!
9999  continue
end subroutine
