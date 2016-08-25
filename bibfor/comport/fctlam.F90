subroutine fctlam(x,finalW)
! ====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/assert.h"
!
    real(kind=8) :: x, finalW
! ----------------------------------------------------------------------
!
! RESOUT LA FONCTION DE LAMBERT W : 
!              
!                 w*EXP(w)=x avec comme solution w=W(x)
! 
! ON UTILISE LA METHODE ITERATIVE DE NEWTON
! DOC DE REFERENCE (INTERNET juin 2016) :
! https://fr.wikipedia.org/wiki/Fonction_W_de_Lambert#M.C3.A9thodes_de_calcul_de_W0
! ----------------------------------------------------------------------
!
! IN    x        VALEUR D'EVALUATION DE LA FONCTION
!
! OUT   finalW   VALEUR DE SORTIE w = W(x)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: lastW, currentW, expW, eps
    integer :: n, nmax
!
    eps = 1.d-9
    currentW=0.d0
    n = 0
    nmax = 1000
! La valeur w0, premier terme de la suite, a un impact fort sur la convergence 
! lors de l'utilisation de valeur 'x élevées'
! On lui associe des valeurs precalculees  par le biais d'une régression lineraire
! pour assurer cette convergence
    
    if (x.lt.8) then
        lastW=1.d0
    elseif (x.ge.8) then
        lastW = 0.7988*log(x) - 0.1091
    endif
    
    
! ON S'ASSURE DE RESTER SUR LES BRANCHES AUX VALEURS REELLES
    if (x .le. -1.d0/exp(1.d0)) then
        ASSERT(.false.)
    end if
    
    loop:do
        expW = exp(lastW)
        currentW = lastW - (lastW*expW-x)/((1+lastW)*expW)
        if(lastW - currentW < eps) exit loop
        lastW = currentW
        
        n=n+1
        if(n>nmax) then
            ASSERT(.false.)
        end if
        
    end do loop
    
    finalW = currentW

end subroutine
