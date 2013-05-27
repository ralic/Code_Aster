subroutine pipeds(ndim, typmod, tau, mate, vim,&
                  epsm, epspc, epsdc, etamin, etamax,&
                  a0, a1, a2, a3, etas)
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
! TOLE CRP_20
!
    implicit none
    include 'asterc/r8vide.h'
    include 'asterfort/critet.h'
    include 'asterfort/diagp3.h'
    include 'asterfort/infniv.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/zerod2.h'
    include 'blas/ddot.h'
    character(len=8) :: typmod(*)
    integer :: ndim, mate
    real(kind=8) :: vim(2), epsm(6), epspc(6), epsdc(6)
    real(kind=8) :: etamax, tau, etamin
    real(kind=8) :: a0, a1, a2, a3, etas
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
!
! LOI DE COMPORTEMENT ENDO_ISOT_BETON EN LOCAL GRAD_VARI
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
    parameter   (nbres=3)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
    real(kind=8) :: valres(nbres)
!
    logical :: cplan
    integer :: ndimsi, k, iter, nitmax, ifm, niv, kpg, spt
    real(kind=8) :: trepsd, coplan, sigeld(6)
    real(kind=8) :: tr(6), vecp(3, 3), rac2
    real(kind=8) :: fpd, dm, d, eta, epm(3)
    real(kind=8) :: e, nu, lambda, deuxmu, gamma, seuil, trepsm
    real(kind=8) :: k0, k1, sicr
    real(kind=8) :: epsp(6), epsd(6), x(4), y(4), z(4)
    real(kind=8) :: epstol
    real(kind=8) :: crit1, crit2, critp1, critp2
    real(kind=8) :: epsvp, epsmax, etasup, etainf, epsnor
    real(kind=8) :: dsigm, syt, syc
    real(kind=8) :: treinf, tresup
    real(kind=8) :: linter, epsto2
    real(kind=8) :: xs, ys, zs
    real(kind=8) :: x1, y1, z1
    real(kind=8) :: x2, y2, z2
    real(kind=8) :: kron(6)
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
!----- GET INFO=1,2
    call infniv(ifm, niv)
!
! ----------------------------------------------------------------------
!
    nitmax = 100
    epstol = 1.d-6
    epsvp = 1.d-6/abs(etamax-etamin)
    epsto2 = 1.d-2
    etas = r8vide()
!
!
! -- OPTION ET MODELISATION
    cplan = (typmod(1).eq.'C_PLAN  ')
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
!
! -- CAS DE L'ENDOMMAGEMENT SATURE, ON NE PILOTE PAS
    if ((nint(vim(2)) .eq. 2)) then
        if (niv .eq. 2) call u2mess('I', 'PILOTAGE_2')
        goto 666
    endif
!
! -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
    nomres(1) = 'E'
    nomres(2) = 'NU'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'D_SIGM_EPSI'
    nomres(2) = 'SYT'
    nomres(3) = 'SYC'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'BETON_ECRO_LINE', 0, ' ', 0.d0,&
                nbres, nomres, valres, icodre, 0)
    dsigm = valres(1)
    syt = valres(2)
    syc = valres(3)
!
    gamma = -e/dsigm
    k0 = (syt*syt)*(1.d0+gamma)/(2.d0*e)* (1.d0+nu-2.d0*nu*nu)/(1.d0+nu)
    if (nu .eq. 0) then
        if (icodre(3) .eq. 0) then
            call u2mess('F', 'ALGORITH4_52')
        else
            seuil = k0
        endif
    else
        sicr=sqrt((1.d0+nu-2.d0*nu**2)/(2.d0*nu**2))*syt
        if (icodre(3) .eq. 1) then
            seuil=k0
        else
            if (syc .lt. sicr) then
                call u2mess('F', 'ALGORITH4_53')
            else
                k1 = syc*(1.d0+gamma)*nu**2/ (1.d0+nu)/(1.d0-2.d0*nu) -k0*e/(1.d0-2.d0*nu)/syc
                trepsm = 0.d0
                do 1 k = 1, ndim
                    trepsm = trepsm+epsm(k)
 1              continue
                if (trepsm .gt. 0.d0) then
                    trepsm = 0.d0
                endif
                seuil = k0-k1*trepsm
            endif
        endif
    endif
!
!    ETAT MECANIQUE EN T-
!
    dm = vim(1)
    d = dm+tau
    fpd = (1+gamma) / (1+gamma*d)**2
!
! -- CAS DE L'ENDOMMAGEMENT QUI SATURERA, ON NE PILOTE PAS
    if (d .gt. 1.d0) then
        if (niv .eq. 2) call u2mess('I', 'PILOTAGE_2')
        goto 666
    endif
!
!
! -- CALCUL DES DEFORMATIONS EN PRESENCE DE CONTRAINTES PLANES
!
    if (cplan) then
        coplan = - nu/(1.d0-nu)
        epspc(3) = coplan * (epspc(1)+epspc(2))
        epsdc(3) = coplan * (epsdc(1)+epsdc(2))
    endif
!
    do 44 k = 1, 3
        epsp(k) = epspc(k)
        epsd(k) = epsdc(k)
44  end do
!
    do 45 k = 4, ndimsi
        epsp(k) = epspc(k)
        epsd(k) = epsdc(k)
45  end do
!
    if (ndimsi .lt. 6) then
        do 46 k = ndimsi+1, 6
            epsp(k)=0.d0
            epsd(k)=0.d0
46      continue
    endif
!
! Calcul du nombre de solutions sur un intervalle raisonnable
!
! - ON COMMENCE DONC PAR REGARDER LES VP DE EPSD
    tr(1) = epsd(1)
    tr(2) = epsd(4)/rac2
    tr(3) = epsd(5)/rac2
    tr(4) = epsd(2)
    tr(5) = epsd(6)/rac2
    tr(6) = epsd(3)
!
!
!
! -- DIAGONALISATION AVEC TRI EN VAL RELATIVE CROISSANT
    call diagp3(tr, vecp, epm)
!
!
! On prend la valeur absolue max des valeurs propres de EPSD
    epsmax = max(abs(epm(1)),abs(epm(3)))
!
! Si les valeurs propres sont trop petites, on ne pilote pas ce point
    if (epsmax .lt. epsvp) goto 666
!
!
!
! on "normalise" les deformations pilotees
!
    trepsd = epsd(1)+epsd(2)+epsd(3)
    do 60 k = 1, ndimsi
        sigeld(k) = lambda*trepsd*kron(k) + deuxmu*epsd(k)
60  end do
!
    epsnor = 1.d0/sqrt(0.5d0 * ddot(ndimsi,epsd,1,sigeld,1))
!
    do 678 k = 1, 6
        epsd(k) = epsd(k)*epsnor
678  end do
!
!
!
!
! CALIBRAGE DE L'INTERVALLE DE RECHERCHE POUR EVITER LES DIVERGENCES
! DE L'ALGORITHME DE RECHERCHE
!
! On repercute la normalisation sur les bornes de ETA pour
! definir l'intervalle de recherche
!
    etasup = etamax/epsnor
    etainf = -etamax/epsnor
!
!
! Test sur la valeur de la trace de la deformee pour eta=etainf
! pour s'assurer qu'elle ne diverge pas. On fixe une borne tr(eps)<1
    treinf = epsp(1)+epsp(2)+epsp(3)+etainf*(epsd(1)+epsd(2)+epsd(3))
    if (abs(treinf) .gt. 1.d0) then
        etainf= (treinf/abs(treinf)-(epsp(1)+epsp(2)+epsp(3)))&
        /(epsd(1)+epsd(2)+epsd(3))
    endif
!
    eta = etainf
    call critet(epsp, epsd, eta, lambda, deuxmu,&
                fpd, seuil, crit1, critp1)
!
!
! Test sur la valeur de la trace de la deformee pour eta=etasup
! pour s'assurer qu'elle ne diverge pas. On fixe une borne tr(eps)<1
!
    tresup = epsp(1)+epsp(2)+epsp(3)+etasup*(epsd(1)+epsd(2)+epsd(3))
    if (abs(tresup) .gt. 1.d0) then
        etasup=(tresup/abs(tresup)-(epsp(1)+epsp(2)+epsp(3))) /(epsd(&
        1)+epsd(2)+epsd(3))
    endif
!
    eta = etasup
    call critet(epsp, epsd, eta, lambda, deuxmu,&
                fpd, seuil, crit2, critp2)
!
!
!
! Longueur de l'intervalle
    linter = abs(etasup-etainf)
!
!
!
!###############################################################
! RECHERCHE DES SOLUTIONS SUR L'INTERVALLE [-ETASUP,ETASUP]
!###############################################################
!
!
! CAS A 0 SOLUTION
!
! on reste en dessous du seuil sur l'intervalle
    if ((crit1.lt.0.d0) .and. (crit2.lt.0.d0)) then
        goto 666
    endif
!
! on reste au dessus du seuil sur l'intervalle,
!        on utilise la convexite pour le voir
!
    if (((crit1.gt.0.d0).and.(critp1.gt.(-crit1/linter))) .or.&
        ((crit2.gt.0.d0).and.(critp2.lt.(crit2/linter)))) then
        goto 666
    endif
!
!
! CAS A 1 SOLUTION
!
    if ((crit1.lt.0.d0) .and. (crit2.gt.0.d0)) then
        x(1)=etainf
        y(1)=crit1
        z(1)=critp1
        x(2)=etasup
        y(2)=crit2
        z(2)=critp2
!
        x(3)=x(1)
        y(3)=y(1)
        z(3)=z(1)
!
        do 200 iter = 1, nitmax
!
            if (abs(y(3)) .le. epstol*seuil) goto 201
            if (abs(z(1)-z(2)) .lt. epsto2*abs(z(2))) then
                x(3)=(-y(3)+z(3)*x(3))/z(3)
                goto 555
            endif
            call zerod2(x, y, z)
555          continue
!
            call critet(epsp, epsd, x(3), lambda, deuxmu,&
                        fpd, seuil, y(3), z(3))
!
200      continue
        call u2mess('F', 'PILOTAGE_87')
201      continue
!
        a1 =z(3)/epsnor
        a0 = tau-x(3)*a1*epsnor
        a2=r8vide()
        a3=r8vide()
!
        goto 9999
!
    endif
!
    if ((crit1.gt.0.d0) .and. (crit2.lt.0.d0)) then
        x(2)=etainf
        y(2)=crit1
        z(2)=critp1
        x(1)=etasup
        y(1)=crit2
        z(1)=critp2
!
        x(3)=x(1)
        y(3)=y(1)
        z(3)=z(1)
        do 202 iter = 1, nitmax
            if (abs(y(3)) .le. epstol*seuil) goto 203
!
            if (abs(z(1)-z(2)) .lt. epsto2*abs(z(2))) then
                x(3)=(-y(3)+z(3)*x(3))/z(3)
                goto 556
            endif
            call zerod2(x, y, z)
556          continue
!
            call critet(epsp, epsd, x(3), lambda, deuxmu,&
                        fpd, seuil, y(3), z(3))
202      continue
        call u2mess('F', 'PILOTAGE_87')
203      continue
!
        a1 =z(3)/epsnor
        a0 = tau-x(3)*a1*epsnor
        a2=r8vide()
        a3=r8vide()
!
        goto 9999
!
    endif
!
!
!
! CAS A 2 OU 0 SOLUTIONS
!
    if (((crit1.gt.0.d0).and.(critp1.lt.(-crit1/linter))) .and.&
        ((crit2.gt.0.d0).and.(critp2.gt.(crit2/linter)))) then
!
!
!
! il faut chercher s'il y a une valeur dans l'intervalle qui donne une
! valeur du critere negative
! s'il y en a une, il y a 2 solutions, sinon 0 solution
!
! On utilise les tangentes pour aller vers le "minimum"
! on s'arrete quand le critere est negatif, on se fiche
! de trouver exactement le minimum
!
        x1=etainf
        y1=crit1
        z1=critp1
        x2=etasup
        y2=crit2
        z2=critp2
!
        ys=y1
!
        iter=0
!
750      continue
!
        if (iter .lt. nitmax) then
            xs=(y2-y1+z1*x1-z2*x2)/(z1-z2)
            call critet(epsp, epsd, xs, lambda, deuxmu,&
                        fpd, seuil, ys, zs)
            if (ys .lt. 0.d0) goto 751
!
            if (zs .gt. 0.d0) then
                x2=xs
                y2=ys
                z2=zs
                linter=x2-x1
                if ((z1.gt.(-y1/linter)) .or. (z2.lt.(y2/linter))) then
                    goto 666
                endif
                goto 750
            else
                x1=xs
                y1=ys
                z1=zs
                linter=x2-x1
                if ((z1.gt.(-y1/linter)) .or. (z2.lt.(y2/linter))) then
                    goto 666
                endif
                goto 750
            endif
        else
            goto 666
        endif
!
751      continue
!
!
! il y a une solution sur [ETAINF,XS] et une sur [XS,ETASUP]
!
!
! Calcul de la solution sur [XS,ETASUP]
        x(1)=xs
        y(1)=ys
        z(1)=zs
        x(2)=etasup
        y(2)=crit2
        z(2)=critp2
!
        x(3)=x(1)
        y(3)=y(1)
        z(3)=z(1)
!
        do 204 iter = 1, nitmax
            if (abs(y(3)) .le. epstol*seuil) goto 205
!
            if (abs(z(1)-z(2)) .lt. epsto2*abs(z(2))) then
                x(3)=(-y(3)+z(3)*x(3))/z(3)
                goto 557
            endif
            call zerod2(x, y, z)
557          continue
!
            call critet(epsp, epsd, x(3), lambda, deuxmu,&
                        fpd, seuil, y(3), z(3))
!
204      continue
        call u2mess('F', 'PILOTAGE_87')
205      continue
!
        a1 =z(3)/epsnor
        a0 = tau-x(3)*a1*epsnor
!
!
!
! Calcul de la solution sur [-ETASUP,XS]
        x(1)=xs
        y(1)=ys
        z(1)=zs
        x(2)=etainf
        y(2)=crit1
        z(2)=critp1
!
        x(3)=x(1)
        y(3)=y(1)
        z(3)=z(1)
!
        do 206 iter = 1, nitmax
            if (abs(y(3)) .le. epstol*seuil) goto 207
            if (abs(z(1)-z(2)) .lt. epsto2*abs(z(2))) then
                x(3)=(-y(3)+z(3)*x(3))/z(3)
                goto 558
            endif
            call zerod2(x, y, z)
558          continue
!
            call critet(epsp, epsd, x(3), lambda, deuxmu,&
                        fpd, seuil, y(3), z(3))
!
206      continue
        call u2mess('F', 'PILOTAGE_87')
207      continue
!
        a3 =z(3)/epsnor
        a2 = tau-x(3)*a3*epsnor
!
        goto 9999
!
!
    endif
!
666  continue
    a0 = 0.d0
    a1 = 0.d0
    a2 = r8vide()
    a3 = r8vide()
!
!
9999  continue
!
! on "redonne" le vrai EPSD
    do 679 k = 1, 6
        epsd(k)=epsd(k)/epsnor
679  end do
!
!
end subroutine
