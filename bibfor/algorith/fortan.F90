subroutine fortan(fn, xlocal, vitloc, cfrotd, cfrots,&
                  ktang, ctang, iadher, oldvt, oldft,&
                  oldxlo, cost, sint, ftange, flocal,&
                  vt)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA FORCE TANGENTIELLE DE CONTACT
! -----------
!               APPELANTS : CALREF, CALRES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    real(kind=8) :: fn, xlocal(*), vitloc(*), cfrotd, cfrots, ktang, ctang
    integer :: iadher
    real(kind=8) :: oldvt(*), oldft(*), oldxlo(*), cost, sint, ftange(*)
    real(kind=8) :: flocal(*), vt(*)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: xscal, xnorvt, dxt(3), xnftan
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  SQRT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
! 1.  CALCUL DE LA VITESSE TANGENTIELLE
!-----------------------------------------------------------------------
!
    vt(1) = -sint*vitloc(2) + cost*vitloc(3)
    vt(2) = vitloc(1)
    xnorvt = sqrt(vt(1)*vt(1)+vt(2)*vt(2))
!
! ... PRODUIT SCALAIRE DE LA VITESSE TANGENTIELLE A L'INSTANT COURANT
! ... PAR LA VITESSE TANGENTIELLE A L'INSTANT PRECEDENT POUR DETECTER
! ... UN EVENTUEL CHANGEMENT DE SIGNE
!
    xscal = vt(1)*oldvt(1) + vt(2)*oldvt(2)
!
!-----------------------------------------------------------------------
! 2.  ESTIMATION DE LA FORCE TANGENTIELLE DE FROTTEMENT
!-----------------------------------------------------------------------
! 2.1 CAS DU GLISSEMENT
!     -----------------
    if ((xscal.ge.0.0d0) .and. (iadher.eq.0) .and. (xnorvt.gt.1.0d-06)) then
!
        ftange(1) = -cfrotd * fn * vt(1) / xnorvt
        ftange(2) = -cfrotd * fn * vt(2) / xnorvt
!
        oldft(1) = ftange(1)
        oldft(2) = ftange(2)
        oldxlo(1) = xlocal(1)
        oldxlo(2) = xlocal(2)
        oldxlo(3) = xlocal(3)
!
! 2.2 CAS DE L'ADHERENCE
!     ------------------
    else
!
! ...... DISTANCE DE GLISSEMENT
!
        dxt(1)=(-(xlocal(2)-oldxlo(2))*sint+(xlocal(3)-oldxlo(3))*&
        cost)
        dxt(2)= xlocal(1)-oldxlo(1)
!
        iadher = 1
!
        ftange(1) = oldft(1) - ktang * dxt(1) - ctang * vt(1)
        ftange(2) = oldft(2) - ktang * dxt(2) - ctang * vt(2)
!
        xnftan = sqrt(ftange(1)*ftange(1)+ftange(2)*ftange(2))
        if (xnftan .gt. (cfrots*fn)) then
            iadher = 0
            if (xnorvt .eq. 0.0d0) then
                ftange(1) = 0.0d0
                ftange(2) = 0.0d0
            else
                ftange(1) = -cfrotd * fn * vt(1) / xnorvt
                ftange(2) = -cfrotd * fn * vt(2) / xnorvt
            endif
        endif
        oldft(1) = ftange(1)
        oldft(2) = ftange(2)
        oldxlo(1) = xlocal(1)
        oldxlo(2) = xlocal(2)
        oldxlo(3) = xlocal(3)
    endif
!
!
! 2.3 MISE A JOUR DE OLDVT POUR L'INSTANT SUIVANT
!     -------------------------------------------
    oldvt(1) = vt(1)
    oldvt(2) = vt(2)
!
!-----------------------------------------------------------------------
! 3.  ACCUMULATION DE LA FORCE TANGENTIELLE ET DE LA FORCE NORMALE
!     DE FROTTEMENT (REPERE LOCAL)
!-----------------------------------------------------------------------
!
    flocal(1) = flocal(1) + ftange(2)
    flocal(2) = flocal(2) - ftange(1)*sint
    flocal(3) = flocal(3) + ftange(1)*cost
!
! --- FIN DE FORTAN.
end subroutine
