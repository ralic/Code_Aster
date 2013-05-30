subroutine rsingu(ndim, nelem, nbr, nalpha, degre,&
                  prec, erreur, alpha, types, re)
    implicit none
    include 'asterfort/u2mess.h'
    integer :: ndim, nelem, nbr(nelem), nalpha, degre
    real(kind=8) :: prec, erreur(nelem), alpha(nelem), re(nelem)
    character(len=16) :: types
! ----------------------------------------------------------------------
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT:
!         CALCUL DU RAPPORT ENTRE L ANCIENNE ET LA NOUVELLE TAILLE
!         OPTION : 'SING_ELEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NELEM         : NOMBRE D ELEMENTS FINIS
! IN   NBR(NELEM)    : NOMBRE DE COMPOSANTES A STOCKER PAR EF
!      3 SI EF SURFACIQUES EN 2D OU VOLUMIQUES EN 3D
!      0 SINON
! IN   NALPHA        : NOMBRE DE CPE PAR ELEMENT DIFFERENTS
!                      1 PAR DEFAUT SI PAS DE SINGULARITE
! IN   DEGRE         : DEGRE DES EF 1 EF P1 2 POUR EF P2
! IN   PREC          : % DE L ERREUR TOTALE SOUHAITE POUR CALCULER
!                      LA NOUVELLE CARTE DE TAILLE DES EF
! IN   ERREUR(NELEM) : ERREUR DE CHAQUE EF
! IN   ALPHA(NELEM)  : DEGRE DE LA SINGULARITE PAR ELEMENT
!
!      SORTIE :
!-------------
! OUT  RE(NELEM)     : RAPPORT ENTRE ANCIENNE ET NOUVELLE TAILLE
!
! ......................................................................
!
    integer :: inel, iter
    real(kind=8) :: errtot, prec0, cumm, ordre, ae(nelem), be(nelem)
    real(kind=8) :: d, mu, fonc, dfonc
    logical :: lqi
!
! 0 - ERREUR EN NORME DE L ENERGIE OU EN QUANTITE D INTERET
!
    lqi = .false.
    if (types(1:2) .eq. 'QI') lqi = .true.
!
! 1 - CALCUL DE L ERREUR TOTALE
!
    errtot=0.d0
    do 10 inel = 1, nelem
        errtot=errtot+erreur(inel)**2
! ------POUR L ERREUR EN QUANTITE D INTERET QUI PEUT ETRE NEGATIVE
        erreur(inel)=abs(erreur(inel))
10  end do
    errtot=sqrt(errtot)
!
! 2 - CALCUL DE RE
!
    ordre=degre
    d=ndim
    prec0=prec*errtot
!
! 2.1 - CAS OU AUCUN ELEMENT N EST SINGULIER
!
    if (nalpha .eq. 1) then
!
        cumm=0.d0
!
        do 20 inel = 1, nelem
            if (nbr(inel) .eq. 3) then
                if (lqi) then
                    cumm=cumm+(erreur(inel)**(d/(2.d0*ordre+d)))
                else
                    cumm=cumm+(erreur(inel)**(2.d0*d/(2.d0*ordre+d)))
                endif
            endif
20      continue
!
        cumm=cumm**(1.d0/(2.d0*ordre))
!
        do 30 inel = 1, nelem
            if (nbr(inel) .eq. 3) then
                if (lqi) then
                    re(inel)=erreur(inel)**(1.d0/(2.d0*ordre+d))
                    re(inel)=1.d0/(re(inel)*cumm)
                    re(inel)=(prec0**(1.d0/ordre))*re(inel)
                else
                    re(inel)=erreur(inel)**(2.d0/(2.d0*ordre+d))
                    re(inel)=1.d0/(re(inel)*cumm)
                    re(inel)=(prec0**(1.d0/2.d0*ordre))*re(inel)
                endif
            endif
30      continue
!
! 2.2 - CAS OU CERTAINS ELEMENTS SONT SINGULIERS
! 2.2.1 - CALCUL DES COEFFICIENTS AE ET BE POUR SIMPLIFIER EXPRESSION
!
    else
        mu=0.d0
        do 40 inel = 1, nelem
            if (nbr(inel) .eq. 3) then
                ae(inel)=2.d0*alpha(inel)/(2.d0*alpha(inel)+d)
                if (lqi) then
                    be(inel)=erreur(inel)**(d/(2.d0*alpha(inel)))
                else
                    be(inel)=erreur(inel)**(d/alpha(inel))
                endif
                be(inel)=d*be(inel)/(2.d0*alpha(inel))
                be(inel)=be(inel)**ae(inel)
                mu=mu+(erreur(inel)**(2.d0*ordre/(2.d0*ordre+d)))
            endif
40      continue
!
! 2.2.2 - RECHERCHE DU LAGRANGIEN MU PAR MEHODE DE NEWTON
!         OU DICHOTOMIE
!
        mu=mu/(prec0**2.d0)
        mu=mu**((2.d0*ordre+d)/(2.d0*ordre))
        mu=d*mu/(2.d0*ordre)
        iter=1
!
70      continue
!
        if (iter .le. 15) then
            if (lqi) then
                fonc=-prec0
            else
                fonc=-(prec0**2.d0)
            endif
            dfonc=0.d0
            do 50 inel = 1, nelem
                if (nbr(inel) .eq. 3) then
                    fonc=fonc+be(inel)/(mu**ae(inel))
                    dfonc=dfonc-ae(inel)*be(inel)/(mu**(ae(inel)+1.d0)&
                    )
                endif
50          continue
!
            if (abs(fonc) .le. 1.d-06) goto 60
            if (fonc .ge. 0.d0) then
                mu=mu-fonc/dfonc
                iter=iter+1
                goto 70
            else
                mu=mu/2.d0
                iter=iter+1
                goto 70
            endif
!
60          continue
!
            do 80 inel = 1, nelem
                if (nbr(inel) .eq. 3) then
                    if (lqi) then
                        re(inel)=d/(2.d0*mu*alpha(inel)*erreur(inel))
                    else
                        re(inel)=d/(2.d0*mu*alpha(inel)*(erreur(inel)&
                        **2.d0))
                    endif
                    re(inel)=re(inel)**(1.d0/(2.d0*alpha(inel)+d))
                endif
80          continue
!
        else
!
            call u2mess('F', 'CALCULEL3_99')
!
        endif
!
!
    endif
!
end subroutine
