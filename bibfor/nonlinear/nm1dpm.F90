subroutine nm1dpm(fami, kpg, ksp, imate, option,&
                  nvar, ncstpm, cstpm, sigm, vim,&
                  deps, vip, sigp, dsde)
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
! -----------------------------------------------------------------
    implicit none
    character(len=*) :: fami, option
    integer :: kpg, ksp, nvar
    integer :: ncstpm, imate
    real(kind=8) :: cstpm(ncstpm)
    real(kind=8) :: sigm, vim(nvar)
    real(kind=8) :: deps
    real(kind=8) :: vip(nvar), sigp, dsde
! -----------------------------------------------------------------
!
!    TRAITEMENT DE LA RELATION DE COMPORTEMENT -ELASTOPLASTICITE-
!    ECROUISSAGE NON LINEAIRE - MODELE DE PINTO MENEGOTTO
!    POUR UN ELEMENT BARRE DE TYPE MECA_ BARRE
!
! -----------------------------------------------------------------
! IN
!       OPTION : OPTION DEMANDEE (R_M_T,FULL OU RAPH_MECA)
!       NVAR   : NOMNBRE DE VARIABLES INTERNES
!       ALPHA  : COEFFICIENT DE DILATATION THERMIQUE
!       TERF   : TEMPERATURE DE REFERENCE
!       NCSTPM : NOMBRE DE CONSTANTES DE MATERIAU
!       CSTPM  : CONSTANTES DE MATERIAU :
!           E      : MODULE D'YOUNG
!           SY     : LIMITE ELASTIQUE
!           EPSU   : DEFORMATION ULTIME
!           SU     : CONTRAINTE ULTIME
!           EPSH   : DEFORMATION A LA FIN DU PALIER PLASTIQUE PARFAIT
!           R0     : COEFFICIENT EXPERIMENTAL
!           B      : COEFFICIENT
!           A1     : COEFFICIENT EXPERIMENTAL
!           A2     : COEFFICIENT EXPERIMENTAL
!           ELAN   : RAPPORT LONGUEUR/DIAMETRE DE LA BARRE
!           A6     : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!           C      : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!           COA    : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!       SIGM   : CONTRAINTE INSTANT MOINS
!       VI M   : VARIABLES INTERNES INSTANT MOINS
!       DEPS   : DEFORMATION TOTALE INSTANT PLUS
!               - DEFORMATION TOTALE INSTANT PLUS
!               - INCREMENT DEFORMATION THERMIQUE
!       TEMPP  : TEMPERATURE IMPOSEE A L'INSTANT PLUS
!
! OUT : SIGP   : CONTRAINTE A L'INSTANT ACTUEL
!       VIP    : VARIABLE INTERNE A L'INSTANT ACTUEL
!       DSDE   : TANGENTE
!
!----------VARIABLES LOCALES
!
    real(kind=8) :: cycl, plasti, eh
    real(kind=8) :: epsy, r, sigmax
    real(kind=8) :: epsrm, epsrp, sigrp, epsm, depsm
    real(kind=8) :: sigel, palel, palec, palsu, palgiu
    real(kind=8) :: sigeps, epsmec, depmec, eps0, sig0
    real(kind=8) :: flbg
    real(kind=8) :: a5, xisec, xiprim, bc, bt, gas, b0, siginf
    real(kind=8) :: chgdir, er, epsetp, xi, sigetp
    real(kind=8) :: e, sy, epsu, su, epsh, r0, b, a1
    real(kind=8) :: a2, elan, a6, c, coa
!
!
!---------- INITIALISATION DES VARIABLES DE SORTIE
!
    if ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA')) then
        vip(1) = 0.d0
        vip(2) = 0.d0
        vip(3) = 0.d0
        vip(4) = 0.d0
        vip(5) = 0.d0
        vip(6) = 0.d0
        vip(7) = 0.d0
        vip(8) = 0.d0
        sigp = 0.d0
    endif
    if ((option.eq.'FULL_MECA') .or. (option(1:9).eq.'RIGI_MECA')) then
        dsde = 0.d0
    endif
!
!----------RECUPERATION DES CARACTERISTIQUES
!
    e=cstpm(1)
    sy=cstpm(2)
    epsu=cstpm(3)
    su=cstpm(4)
    epsh=cstpm(5)
    r0=cstpm(6)
    b=cstpm(7)
    a1=cstpm(8)
    a2=cstpm(9)
    elan=cstpm(10)
    a6=cstpm(11)
    c=cstpm(12)
    coa=cstpm(13)
!
    epsy= sy/e
    eh = (su-sy)/(epsu-epsy)
    if (b .eq. -1.d0) then
        b=eh/e
    endif
    b0 = b
!
    epsrm     =vim(1)
    epsrp     =vim(2)
    sigrp     =vim(3)
!
!     EPSM EST EN FAIT EPSMEC DU TEMPS PRECEDENT
!
    epsm      =vim(4)
!
!    DEPSM EST EN FAIT DEPMEC DU TEMPS PRECEDENT
!
    depsm     =vim(5)
    cycl      =vim(6)
    plasti    =vim(7)
    flbg      =vim(8)
!
!  ON APPELLE DEFORMATION MECANIQUE DEF TOTALE - ALPHA(T-TREF)
!
!   DEFORMATION I   OBTENU PAR       I SIGNIFICATION      I
!---------------I--------------------I--------------------I
!   DEPS        I ARGUMENT           I DEF TOTALE TEMPS + I
!               I                    I-DEF TOTALE TEMPS - I
!               I                    I-DEF THERMIQUE      I
!---------------I--------------------I--------------------I
!   EPSM        I VARIABLE INTERNE - I DEF MECA TEMPS -   I
!---------------I--------------------I--------------------I
!   DEPSM       I VARIABLE INTERNE - I DEF MECA TEMPS -   I
!               I                    I-(DEF MECA TEMPS --)I
!---------------I--------------------I--------------------I
!   DEPMEC      I CALCULEE           I DEF MECA   TEMPS + I
!               I                    I-(DEF MECA TEMPS - )I
!               I                    I =DEPS-ALPH(TP-TM)  I
!---------------I--------------------I--------------------I
!   EPSMEC      I CALCULEE           I DEF MECA   TEMPS + I
!               I                    I =EPSM+DEPMEC       I
!---------------I--------------------I--------------------I
!
!12345678901234567890123456789012345678901234567890123456789012345678901
    depmec  =deps
    epsmec  =epsm + depmec
    chgdir  =depsm*depmec
    sigeps  =sigm*depmec
    a5      =1.d0+(5.d0-elan)/7.5d0
    siginf  =4.d0*sy/elan
    gas     =(11.d0-elan)/(10.d0*(exp(c*elan)-1.d0))
!
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
!
!**********PREMIER CHARGEMENT********************************
!
        if (cycl .lt. 0.5d0) then
!
!----------CALCUL DES SEUILS PALEL,PALEC,PALSU
!
            sigel=sigm + e * (depmec)
            sigmax=su-(su-sy)*((epsu-abs(epsmec))/(epsu-epsh))**4
            palel=abs(sigel)-sy
            palec=abs(epsmec)-epsh
            palsu=abs(sigel)-sigmax
            palgiu=abs(epsrp-epsmec)-sy/(3.d0*e)
            flbg=0.d0
!
!----------CALCUL DE SIGP
!
!-----------CAS OU ON A DEJA DECHARGE
!
            if ((plasti.gt.0.5d0) .and. (sigeps.lt.0.d0)) then
                if (palgiu .lt. 0.d0) then
                    sigp=sigm+e*(depmec)
                else
                    cycl=1
                    if (sigm .gt. 0.d0) then
                        epsrm=-epsy
                    else
                        epsrm=epsy
                    endif
                    epsrp=epsm
                    sigrp=sigm
                    if (depmec .ge. 0.d0) then
                        eps0=-(sigrp-sy+eh*epsy-e*epsrp)/(e-eh)
                        sig0=eh*(eps0-epsy)+sy
!
!-----------CAS FLAMBAGE
                        if (elan .gt. 5.0d0 .and. flbg .gt. 0.5d0) then
                            xisec=epsrm-eps0
                            er =e*(a5+(1.d0-a5)*exp(-a6*xisec**2))
                            bt =b*e/er
                            b0 =bt
                        endif
!------------------------
                    else if (depmec.lt.0.d0) then
                        eps0=-(sigrp+sy-eh*epsy-e*epsrp)/(e-eh)
                        sig0=eh*(eps0+epsy)-sy
!
!-----------CAS FLAMBAGE
                        if (elan .gt. 5.0d0) then
                            flbg=1.d0
                            xiprim=epsrp-epsrm
                            bc =coa*(5.d0-elan)*exp(xiprim*b*e/(sy-&
                            siginf))
                            b0 =bc
                            sig0 =sig0-gas*b*e*(b-b0)/(1.d0-b0)
                        endif
!----------------------
                    endif
!
!---------------CALCUL DE EPSETP,R
!
                    epsetp= (epsmec-epsrp)/(eps0-epsrp)
                    xi = (epsrm-eps0)/(eps0-epsrp)
                    r = r0-a1*xi/(a2+xi)
                    sigetp=b0*epsetp+((1-b0)/(1+(epsetp)**r)**(1/r))*&
                    epsetp
                    sigp=(sig0-sigrp)* sigetp+sigrp
                endif
            else
!
!------CAS OU ON RESTE MONOTONE
!
                cycl=0.d0
                if (palel .le. 0.d0) then
                    sigp=sigm+e*depmec
                else
                    plasti=1
                    if (palec .le. 0.d0) then
                        if (sigel .ge. 0.d0) then
                            sigp=sy
                        else
                            sigp=-sy
                        endif
                    else
                        if (palsu .le. 0.d0) then
                            sigp=sigm + e *(depmec)
                        else
                            if (sigel .ge. 0.d0) then
                                if (abs(epsmec) .lt. abs(epsu)) then
                                    sigp=su-(su-sy)*((epsu-epsmec)/(&
                                    epsu-epsh))**4
                                else
                                    sigp=su
                                endif
                            else
                                if (abs(epsmec) .lt. abs(epsu)) then
                                    sigp=-(su-(su-sy)*((epsu+epsmec)/(&
                                    epsu-epsh))**4)
                                else
                                    sigp=-su
                                endif
                            endif
                        endif
                    endif
                endif
            endif
!
!
!**********CYCLE*****************************************************
!
        else if (cycl.gt.0.5d0) then
!
!---------------CALCUL DE EPSR,SIGR,EPS0,SIG0 A L'INSTANT DU CALCUL
!
!-------RECUPERATION DES COORD. EPSRP,SIGRP SI CHANGT DE DIRECTION
!
            if (chgdir .lt. 0.d0) then
                epsrm=epsrp
                epsrp=epsm
                sigrp=sigm
            endif
!------------------
!
            if (depmec .ge. 0.d0) then
                eps0=-(sigrp-sy+eh*epsy-e*epsrp)/(e-eh)
                sig0=eh*(eps0-epsy)+sy
!
!----------CAS FLAMBAGE
                if ((elan.gt.5.0d0) .and. (flbg.gt.0.5d0)) then
                    xisec=epsrm-eps0
                    er =e*(a5+(1.d0-a5)*exp(-a6*xisec**2))
                    eps0=-(sigrp-sy+eh*epsy-er*epsrp)/(er-eh)
                    sig0=eh*(eps0-epsy)+sy
                    bt =b*e/er
                    b0 =bt
                endif
!----------------------
            else if (depmec.lt.0.d0) then
                eps0=-(sigrp+sy-eh*epsy-e*epsrp)/(e-eh)
                sig0=eh*(eps0+epsy)-sy
!
!---------CAS FLAMBAGE
                if (elan .gt. 5.0d0) then
                    flbg=1.d0
                    xiprim=epsrp-epsrm
                    bc =coa*(5.d0-elan)*exp(xiprim*b*e/(sy-siginf))
                    b0 =bc
                    sig0 =sig0-gas*b*e*(b-b0)/(1.d0-b0)
                endif
!--------------------
            endif
!
!---------------CALCUL DE EPSETP,R
!
            epsetp= (epsmec-epsrp)/(eps0-epsrp)
            xi = (epsrm-eps0)/(eps0-epsrp)
            r = r0-a1*xi/(a2+xi)
            if (epsetp .ne. 0.d0) then
                sigetp=b0*epsetp+((1-b0)/(1+(epsetp)**r)**(1/r))*&
                epsetp
                sigp=(sig0-sigrp)* sigetp+sigrp
            else
                sigetp=0.d0
                sigp=sigrp
            endif
!
        endif
!
        vip(1)=epsrm
        vip(2)=epsrp
        vip(3)=sigrp
        vip(4)=epsmec
        vip(5)=depmec
        vip(6)=cycl
        vip(7)=plasti
        vip(8)=flbg
!
        if (option .eq. 'FULL_MECA') then
!
! --- CALCUL DE LA PENTE
!
            sigeps=sigp*(depmec)
!
            if (cycl .lt. 0.5d0) then
!
                if ((plasti.gt.0.5d0) .and. (sigeps.ge.0.d0)) then
                    dsde= (e*eh/(eh+e))
                else
                    dsde=e
                endif
!
            else if (cycl.gt.0.5d0) then
                if (abs(epsmec) .gt. abs(eps0)) then
                    dsde=e
                else
                    if (epsetp .ne. 0.d0) then
                        dsde = (&
                               b0+(b0-1)*(1+epsetp**r)**(-(1+1/r))* epsetp**r +(1-b0)/(1+epsetp**&
                               &r)**(1/r)) *(sig0-sigrp)/(eps0-epsrp&
                               )
                    else
                        dsde=(sig0-sigrp)/(eps0-epsrp)
                    endif
                endif
            endif
!
        else if (option.eq.'FULL_MECA_ELAS') then
!
            dsde=e
!
        endif
!
    else if (option(1:14) .eq. 'RIGI_MECA_TANG') then
!
! --- CALCUL DE LA PENTE
!
        if (cycl .lt. 0.5d0) then
!
            if (plasti .gt. 0.5d0) then
                dsde= (e*eh/(eh+e))
            else
                dsde=e
            endif
!
        else
            dsde=e
        endif
!
    else if (option(1:14).eq.'RIGI_MECA_ELAS') then
!
        dsde=e
!
    endif
!
! ----------------------------------------------------------------
!
end subroutine
