subroutine dicrgr(fami, option, neq, nc, icodma,&
                  ulm, dul, sim, varim, pgl,&
                  klv, varip, fono, sip)
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/moytem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
    integer :: neq, icodma, nc
    real(kind=8) :: ulm(neq), dul(neq), sim(neq), sip(neq), varim(6)
    real(kind=8) :: pgl(3, 3), varip(6), fono(neq), klv(78)
    character(len=*) :: fami
    character(len=16) :: option
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
!  COMPORTEMENT DIS_GRICRA : APPLICATION : LIAISON GRILLE-CRAYON COMBU
!           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
!           SAUF SUIVANT Y LOCAL : FROTTEMENT DE COULOMB
!       ELEMENTS MECA_DIS_TR_L ET MECA_DIS_T_L
!
! IN  : OPTION : RIGI_MECA*, FULL_MECA* OU RAPH_MECA
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       NC     : NOMBRE DE DDL PAR NOEUD = 6
!       ICODMA : ADRESSE DU MATERIAU CODE
!       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
!       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
!       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
!       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
!       ITEMP  : INDICATEUR DU CHAMP DE TEMPERATURE
!       TEMPM  : TEMPERATURE A L'INSTANT PRECEDENT
!       TEMPP  : TEMPERATURE A L'INSTANT ACTUEL
!       IRRAP  : IRRADIATION
!
! OUT : KLV    : MATRICE TANGENTE
!       VARIP  : VARIABLE INTERNE REACTUALISEE
!       FONI   : FORCES NODALES
!       SIP    : EFFORTS INTERNES
!------------------------------------------------------------------
!------------------------------------------------------------------
!
!
!
!
    integer :: codre1(4), codre2(2), codre3(1), codre4(5), codre5(5), iret2
    character(len=8) :: nomre1(4), nomre2(2), nomre3, nomre4(5), nomre5(5)
    character(len=8) :: nompar(2)
!
    integer :: npg, nno, nbpar
    integer :: iretp, iretm
    real(kind=8) :: valre1(4), valre2(2), valre3(1), valre4(5), valre5(5)
    real(kind=8) :: valpar(2), irram, irrap
    real(kind=8) :: fno, h1
    real(kind=8) :: dux, duy, dph, dth
    real(kind=8) :: uxm, phm, thm
    real(kind=8) :: kxx, kyy, kzz, kpp, ktt
    real(kind=8) :: fl(12)
    real(kind=8) :: temp
    real(kind=8) :: ktax, muax, etax, knax
    real(kind=8) :: ktrig, dp, pm, rtm, rtp, rte
    real(kind=8) :: sieleq, beta, seuil, rm
    real(kind=8) :: phipl, ppm, fphi, mophi, dkh, khm
    real(kind=8) :: phitan, thetan, kkk, ecro
    real(kind=8) :: phic, kphi, thetac, ktheta, ephi
    real(kind=8) :: dpp, dphipl, seurot, kthet2
    real(kind=8) :: tempp, tempm, sgne
!
    data nomre1/'KN_AX','KT_AX','ET_AX','ET_ROT'/
    data nomre2/'F_SER','COUL_AX'/
    data nomre3/'F_SER_FO'/
    data nomre4/'ANG1','ANG2','PEN1','PEN2','PEN3'/
    data nomre5/'ANG1_FO','ANG2_FO','PEN1_FO','PEN2_FO','PEN3_FO'/
!
!
!
! TYPE D'ELEMENT: SEG2
    npg = 2
!
!---procedure pour la liaison grille crayon
!
! on travaille ici dans le repère local
!
!
!  recuperer les deplacements et rotation
!  recuperer les variables internes  (moins)
!  recuperer les parametres de defi_materiau
!
    call r8inir(neq, 0.d0, fl, 1)
!
! recuperation des donnees materiau pour le discret
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'DIS_GRICRA', 0, ' ', [0.d0],&
                4, nomre1, valre1, codre1, 0)
!
    knax=valre1(1)
    ktax=valre1(2)/4.d0
    etax=valre1(3)*ktax
!
!     ON RECUPERE L'INCREMENT D'IRRADIATION SUR LE 1ER PG :
    call rcvarc(' ', 'IRRA', '-', 'RIGI', 1,&
                1, irram, iret2)
    if (iret2 .gt. 0) irram=0.d0
    call rcvarc(' ', 'IRRA', '+', 'RIGI', 1,&
                1, irrap, iret2)
    if (iret2 .gt. 0) irrap=0.d0
    irrap = irrap - irram + varim(6)
!
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'DIS_GRICRA', 0, ' ', [0.d0],&
                2, nomre2, valre2, codre2, 0)
!
    if (codre2(1) .eq. 0) then
        fno=valre2(1)/4.d0
        muax=valre2(2)
    else
        call moytem(fami, npg, 1, '+', tempp,&
                    iretp)
        call moytem(fami, npg, 1, '-', tempm,&
                    iretm)
        if ((iretp+iretm) .ge. 1) then
            call utmess('F', 'CALCULEL_31')
        endif
        temp = (tempp+tempm)/2.d0
!
        nbpar=2
        nompar(2)='IRRA'
        nompar(1)='TEMP'
        valpar(2)=irrap
        valpar(1)=temp
!
        call rcvalb(fami, 1, 1, '+', icodma,&
                    ' ', 'DIS_GRICRA', nbpar, nompar, valpar,&
                    1, nomre3, valre3, codre3, 0)
        if (codre3(1) .eq. 0) then
            fno=valre3(1)/4.d0
            muax=valre2(2)
        endif
    endif
!
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'DIS_GRICRA', 0, ' ', [0.d0],&
                5, nomre4, valre4, codre4, 0)
!
    if (codre4(1) .eq. 0) then
        phic=valre4(1)
        thetac=valre4(2)
        ktheta=valre4(4)/2.d0
        kphi=valre4(3)/2.d0-ktheta
        kthet2=valre4(5)/2.d0
    else
        call moytem(fami, npg, 1, '+', tempp,&
                    iretp)
        call moytem(fami, npg, 1, '-', tempm,&
                    iretm)
        if ((iretp+iretm) .ge. 1) then
            call utmess('F', 'CALCULEL_31')
        endif
        temp = (tempp + tempm)/2.d0
        nbpar=2
        nompar(2)='IRRA'
        nompar(1)='TEMP'
        valpar(2)=irrap
        valpar(1)=temp
        call rcvalb(fami, 1, 1, '+', icodma,&
                    ' ', 'DIS_GRICRA', nbpar, nompar, valpar,&
                    5, nomre5, valre5, codre5, 0)
        phic=valre5(1)
        thetac=valre5(2)
        ktheta=valre5(4)/2.d0
        kphi=valre5(3)/2.d0-ktheta
        kthet2=valre5(5)/2.d0
    endif
    ephi=valre1(4)*kphi
!
!
! Variables internes de contact au temps moins
    h1=1.d0-varim(3)
!
!
!
!---calcul de l'evolution des variables internes et des forces
!---pour FULL_MECA et RAPH_MECA
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
!
!  extension de l'element
!
!  extension au pas de temps precedent
        uxm = ulm(1+nc) - ulm(1)
        phm = ulm(4+nc) - ulm(4)
        khm = ulm(5+nc) - ulm(5)
        thm = ulm(6+nc) - ulm(6)
!  variation d'extension
        dux = dul(1+nc) - dul(1)
        duy = dul(2+nc) - dul(2)
        dph = dul(4+nc) - dul(4)
        dkh = dul(5+nc) - dul(5)
        dth = dul(6+nc) - dul(6)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  CALCUL DES FORCES EN TRANSLATION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  calcul de la force (translation)
!
! on garde -FN0 dans la direction du discret
! possibilité de frottement dans la direction 2 (verticalement)
! force nulle dans la 3e direction
!
        rtm = (sim(2+nc)+sim(2))/2.d0
        seuil = muax*fno
        pm = varim(1)
        rte = rtm + ktax*duy
        sieleq = abs(rte)
        beta = ktax*etax/ (ktax-etax)
        rm = beta*pm + seuil
        if (sieleq .le. rm) then
            dp = 0.d0
            rtp = rte
            ktrig = ktax
            varip(1) = pm
            varip(2) = 0.d0
        else
            varip(2) = 1.d0
            dp = abs(rte) - (seuil+beta*pm)
            dp = dp/ (beta+ktax)
            rtp = (seuil + beta* (pm+dp))*rte/abs(rte)
            varip(1) = pm + dp
            ktrig = etax
        endif
        sip(1)=-fno+knax*(uxm+dux)
        sip(2)=rtp
        sip(3)=0.d0
        sip(1+nc)=-fno+knax*(uxm+dux)
        sip(2+nc)=rtp
        sip(3+nc)=0.d0
!
        fl(2)=-sip(2)
        fl(1)=-sip(1)
        fl(3)=-sip(3)
        fl(2+nc)=sip(2)
        fl(1+nc)=sip(1)
        fl(3+nc)=sip(3)
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  CALCUL DES FORCES EN ROTATION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  Calcul des forces et evolution des variables internes
!   sur chacun des sous éléments du système de liaison
!
!
!  angle theta (DRZ): possibilite de decollement
!  cela equivaut a une loi bilineaire
!
        if ((thetac-thm-dth) .lt. 0.d0) then
            h1=0.d0
            sip(6)=ktheta*thetac+kthet2*(thm+dth-thetac)
        else if ((thetac+thm+dth).lt.0.d0) then
            h1=0.d0
            sip(6)=-ktheta*thetac+kthet2*(thm+dth+thetac)
        else
            h1=1.d0
            sip(6)=ktheta*(thm+dth)
        endif
!
        sip(6+nc)=sip(6)
        fl(6)=-sip(6)
        fl(6+nc)=sip(6)
        thetan=h1*ktheta+(1.d0-h1)*kthet2
        varip(3)=1.d0-h1
!
!
!  angle phi (DRX): loi de frottement
!
        phipl=varim(4)
        ppm=varim(5)
        fphi=phm+dph-phipl
        ecro=ephi/kphi
!
        seurot=abs(fphi)-phic-ecro*ppm
        if (seurot .lt. 0.d0) then
            mophi=kphi*fphi
            varip(4)=varim(4)
            varip(5)=varim(5)
            phitan=kphi
        else
            sgne=(fphi)/abs(fphi)
            dpp=-(phic+ecro*ppm-abs(phm-phipl+dph))/(1.d0+ecro*sgne)
            dphipl=dpp*sgne
            varip(4)=varim(4)+dphipl
            varip(5)=varim(5)+dpp
            mophi=kphi*(phm+dph-phipl-dphipl)
            phitan=ephi
        endif
!
        sip(4)=mophi
        sip(4+nc)=mophi
        fl(4)=-mophi
        fl(4+nc)=mophi
!
! pour le dernier angle, on met quand meme une rigidite, meme si
!les conditions limites doivent imposer que ça ne tourne pas
        sip(5)=kphi*(khm+dkh)
        sip(5+nc)=kphi*(khm+dkh)
        fl(5)=-kphi*(khm+dkh)
        fl(5+nc)=kphi*(khm+dkh)
!
        varip(6)=irrap
    endif
!
!
!
!---Matrice tangente pour FULL_MECA(_ELAS) et RIGI_MECA(_ELAS)
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RIGI_MECA')) then
!
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(10:14).eq.'_ELAS')) then
!
            kxx=knax
            kyy=ktax
            kzz=0.d0
            kpp=kphi
            kkk=kphi
            ktt=ktheta
        else
            kxx=knax
            kyy=ktrig
            kzz=0.d0
            kpp=phitan
            kkk=kphi
            ktt=thetan
        endif
!
!
        call r8inir(78, 0.d0, klv, 1)
        klv(1)=kxx
        klv(3)=kyy
        klv(6)=kzz
        klv(10)=kpp
        klv(15)=kkk
        klv(21)=ktt
        klv(28)=kxx
        klv(36)=kyy
        klv(45)=kzz
        klv(55)=kpp
        klv(66)=kkk
        klv(78)=ktt
        klv(22)=-kxx
        klv(30)=-kyy
        klv(39)=-kzz
        klv(49)=-kpp
        klv(60)=-kkk
        klv(72)=-ktt
!
    endif
!
!---Calcul des forces nodales dans le repère global
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        nno = 2
        call utpvlg(nno, nc, pgl, fl, fono)
    endif
end subroutine
