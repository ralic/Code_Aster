subroutine lcmmfi(coeft, ifa, nmat, nbcomm, necris,&
                  is, nbsys, vind, nsfv, dy,&
                  nfs, nsg, hsr, iexp, expbp,&
                  rp)
    implicit none
    include 'asterfort/lcmmdc.h'
    include 'asterfort/u2mess.h'
    integer :: ifa, nmat, nbcomm(nmat, 3), nbsys, is, iexp, nfs, nsg, nsfv
    real(kind=8) :: coeft(nmat), dy(*), vind(*), hsr(nsg, nsg), sq, expbp(*)
    character(len=16) :: necris
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
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
! person_in_charge: jean-michel.proix at edf.fr
!  COMPORTEMENT MONOCRISTALLIN : ECROUISSAGE ISOTROPE
!     IN  COEFT   :  PARAMETRES MATERIAU
!         IFA     :  NUMERO DE FAMILLE
!         NMAT    :  NOMBRE MAXI DE MATERIAUX
!         NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!         NECRIS  :  NOM DE LA LOI D'ECROUISSAGE ISTROPE
!         IS      :  NUMERO DU SYSTEME DE GLISSEMET EN COURS
!         NBCOMM  :  INCIDES DES COEF MATERIAU
!         NBSYS   :  NOMBRE DE SYSTEMES DE GLISSEMENT DE LA FAMILLE
!         VIND    :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!         DY      :  SOLUTION
!         HSR     :  MATRICE D'INTERACTION
!         IEXP    :  Indice pour recalculer EXPBP (0 si deja calcule)
!         EXPBP   :  TERMES 1.-EXP(-BPr)  pour tous les systemes Ir
!     OUT:
!         RP      :  R(P)
! ======================================================================
!
!     ----------------------------------------------------------------
    real(kind=8) :: p, r0, q, b, rp, b1, b2, q1, q2
    real(kind=8) :: pr, mu, ceff, alpham(12), alphas(12), r8b
    real(kind=8) :: alloop, alpvid, filoop, rhovid
    integer :: iei, ir, nueiso
!     ----------------------------------------------------------------
!
    iei=nbcomm(ifa,3)
    nueiso=nint(coeft(iei))
!
!--------------------------------------------------------------------
!     POUR UN NOUVEAU TYPE D'ECROUISSAGE ISOTROPE, AJOUTER UN BLOC IF
!--------------------------------------------------------------------
!      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
    if (nueiso .eq. 1) then
!
        r0 =coeft(iei+1)
        q =coeft(iei+2)
        b =coeft(iei+3)
!
        if (iexp .eq. 1) then
            do 10 ir = 1, nbsys
                pr=vind(nsfv+3*(ir-1)+3)+abs(dy(ir))
                expbp(ir) = (1.d0-exp(-b*pr))
10          continue
        endif
!
!       VIND commence en fait au début de systemes de glissement
!      de LA famille courante;
        sq=0.d0
        do 11 ir = 1, nbsys
            pr=vind(nsfv+3*(ir-1)+3)+abs(dy(ir))
            sq = sq + hsr(is,ir)*expbp(ir)
11      continue
        rp=r0+q*sq
!
!      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
    else if (nueiso.eq.2) then
!
        r0=coeft(iei+1)
        q1=coeft(iei+2)
        b1=coeft(iei+3)
        q2=coeft(iei+4)
        b2=coeft(iei+5)
!
!        VIND COMMENCE EN FAIT AU DÉBUT DE SYSTEMES DE GLISSEMENT
!        DE LA FAMILLE COURANTE;
!
        sq=0.d0
        do 12 ir = 1, nbsys
            pr=vind(nsfv+3*(ir-1)+3)+abs(dy(ir))
            sq = sq + hsr(is,ir)*(1.d0-exp(-b1*pr))
12      continue
        p=vind(nsfv+3*(is-1)+3)+abs(dy(is))
        rp=r0+q1*sq+q2*(1.d0-exp(-b2*p))
!
!      ELSEIF ((NECRIS.EQ.'ECRO_DD_CFC').OR.
!             (NECRIS.EQ.'ECRO_ECP_CFC')) THEN
    else if ((nueiso.eq.3).or.(nueiso.eq.4)) then
!
        if (nueiso .eq. 3) then
            mu =coeft(iei+4)
!           NUMHSR=NINT(COEFT(IEI+5))
        else
!          CAS NUEISO = 4 C'EST A DIRE NECRIS = 'ECRO_ECP_CFC'
            mu =coeft(iei+1)
!           NUMHSR=NINT(COEFT(IEI+2))
        endif
!
!        VIND COMMENCE EN FAIT AU DÉBUT DE SYSTEMES DE GLISSEMENT
!        DE LA FAMILLE COURANTE;
!        VARIABLE INTERNE PRINCIPALE : ALPHA=RHO*B**2
!
        do 55 ir = 1, nbsys
            alpham(ir)=vind(nsfv+3*(ir-1)+1)
            alphas(ir)= alpham(ir)+dy(ir)
55      continue
!
        rp=0.d0
        do 23 ir = 1, nbsys
            if (alphas(ir) .gt. 0.d0) then
                rp=rp+alphas(ir)*hsr(is,ir)
            endif
23      continue
!
        if (nueiso .eq. 3) then
            call lcmmdc(coeft, ifa, nmat, nbcomm, alphas,&
                        is, ceff, r8b)
        else
!          CAS NUEISO = 4 C'EST A DIRE NECRIS = 'ECRO_ECP_CFC'
            ceff = 1.d0
        endif
!
!        CE QUE L'ON APPELLE RP CORRESPOND ICI A TAU_S_FOREST
        rp=mu*sqrt(rp)*ceff
!
!        DD_CFC_IRRA
    else if (nueiso.eq.8) then
!
        rhovid =coeft(iei+4)
        filoop =coeft(iei+5)
        alpvid =coeft(iei+6)
        alloop =coeft(iei+7)
        mu =coeft(iei+12)
!
!        VIND COMMENCE EN FAIT AU DÉBUT DE SYSTEMES DE GLISSEMENT
!        DE LA FAMILLE COURANTE;
!        VARIABLE INTERNE PRINCIPALE : ALPHA=RHO*B**2
        do 56 ir = 1, nbsys
            alpham(ir)=vind(nsfv+3*(ir-1)+1)
            alphas(ir)= alpham(ir)+dy(ir)
56      continue
!
        rp=0.d0
        do 24 ir = 1, nbsys
            if (alphas(ir) .gt. 0.d0) then
                rp=rp+alphas(ir)*hsr(is,ir)
            endif
24      continue
        call lcmmdc(coeft, ifa, nmat, nbcomm, alphas,&
                    is, ceff, r8b)
!
        rp=rp*ceff*ceff
        rp=rp+alloop*filoop*vind(decirr+(is-1)+1)
        rp=rp+alpvid*rhovid*vind(decirr+12+(is-1)+1)
!
!        CE QUE L'ON APPELLE RP CORRESPOND ICI A TAU_S_FOREST
        rp=mu*sqrt(rp)
!
!        DD_CC : ON SORT UNIQUEMENT TAU_F
    else if (nueiso.eq.7) then
        rp=coeft(iei+1)
!
    else
        call u2mess('F', 'COMPOR1_21')
    endif
!
!
end subroutine
