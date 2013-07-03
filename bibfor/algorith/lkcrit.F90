function lkcrit(amat, mmat, smat, gamcjs, sigc,&
                h0ext, rcos3t, invar, sii)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/lkhlod.h"
    real(kind=8) :: lkcrit, amat, mmat, smat, gamcjs, sigc, h0ext, rcos3t, invar
    real(kind=8) :: sii
! =================================================================
! --- FONCTION ADAPTEE AU POST-TRAITEMENT DE LA LOI LETK, QUI -----
! --- CALCULE LA POSITION D'UN ETAT DE CONTRAINTE PAR -------------
! --- RAPPORT A UN SEUIL VISCOPLASTIQUE ---------------------------
! =================================================================
    real(kind=8) :: un, deux, trois, six, kseuil, h0c, h0e, aseuil, bseuil
    real(kind=8) :: dseuil, fact1, fact2, fact3, ucrit, htheta, hlode
    parameter( un     =  1.0d0   )
    parameter( deux   =  2.0d0   )
    parameter( trois  =  3.0d0   )
    parameter( six    =  6.0d0   )
! =================================================================
! --- CALCUL DES CRITERES D'ECROUISSAGE ---------------------------
! =================================================================
    kseuil = (deux/trois)**(un/deux/amat)
    h0c = (un - gamcjs)**(un/six)
    h0e = (un + gamcjs)**(un/six)
    aseuil = - mmat * kseuil/sqrt(six)/sigc/h0c
    bseuil = mmat * kseuil/trois/sigc
    dseuil = smat * kseuil
! =================================================================
    fact1 = (h0c + h0ext)/deux
    fact2 = (h0c - h0ext)/deux
    hlode = lkhlod(gamcjs,rcos3t)
    fact3 = (deux*hlode-(h0c+h0e))/(h0c-h0e)
    htheta = fact1+fact2*fact3
! =================================================================
    ucrit = aseuil*sii*htheta + bseuil*invar+dseuil
    if (ucrit .lt. 0.0d0) then
        ucrit=0.0d0
    endif
    lkcrit = sii*htheta - sigc*h0c*(ucrit)**amat
end function
