subroutine analybar(x3d1, x3d2, x3d3, x3dp,&
                    xbar, excent, iproj, inoeu, icote)
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
!  DESCRIPTION : 
!  -----------   
!       APRES ECHEC DE TSTBAR SUR UNE FACE, ANALYSE A PARTIR D'UN
!       EXCENTREMENT DONNE SI LE POINT PROJETE EST SUFFISAMMENT PRES DE
!       LA FACE POUR POUVOIR TENTER UNE PROJECTION SUR COTE OU SUR NOEUD
!       DANS LE CADRE DE LA LIAISON CABLE/COQUE DE DEFI_CABLE_BP
!
!       LA VALEUR DE ALPHA_MAX EST FIXEE A 45°, CELA EXPRIME LE FAIT
!       QUE L'ON CONSIDERE QUE DEUX MAILLES ADJACENTES NE DOIVENT PAS 
!       AVOIR UN ANGLE DE PLUS DE 45° ENTRE ELLES (LE PLAT ETANT 0°)
!
!       OUT : IPROJ = -1 si noeud trop loin de la maille
!                   = 20 si projection sur côté possible
!                   = 30 si projection sur noeud possible
!       OUT : INOEU = 1, 2 ou 3 (numéro du noeud)
!       OUT : ICOTE = 1, 2 ou 3 (numéro du côté)
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
#include "asterfort/assert.h"
#include "asterfort/calc_h_tria.h"
#include "asterfort/tstbar.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
! ARGUMENTS
! ---------
    integer :: iproj, inoeu, icote
    real(kind=8) :: xbar(3), excent, x3d1(3), x3d2(3), x3d3(3), x3dp(3)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: h, d, alpha, alpha_max, r8bid3(3), xbar2(2)
    integer :: j, ino, jno, kno, lno_neg(2), iproj2
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!
    alpha_max = r8pi()/4.d0
    if (excent.lt.r8prem())then
        iproj =-1
        goto 77
    endif
!
    j = 0
    do ino = 1,3
        if (xbar(ino) .lt. 0.d0)then
!           calcul de la hauteur
            call calc_h_tria(ino, x3d1, x3d2, x3d3, h)
!           distance au côté opposé au noeud
            d = -h*xbar(ino)
!           calcul de l'angle
            alpha = atan(d/excent)
            if (alpha .le. alpha_max) then
                j = j+1
                lno_neg(j) = ino
            else
!           pas de projection segment ou noeud possible
                iproj = -1
                goto 77
            endif
        endif
    enddo
    if (j.eq.2)then
!           projection possible sur le noeud pas dans la liste lno_neg
        iproj = 30
        do 20 ino = 1,3
            do jno =1, 2
                if (ino .eq. lno_neg(jno)) goto 20
            enddo
            exit
20      continue
        inoeu = ino
    elseif (j.eq.1)then
!           projection possible sur le côté opposé au noeud ou sur un
!           des deux autres noeuds
        ino = lno_neg(1)
        jno = ino+1
        if (jno.gt.3) jno = 1
        kno = jno+1
        if (kno.gt.3) kno = 1
        if (jno.eq.1)then
            call tstbar(2, x3d1, x3d2, r8bid3, r8bid3,&
                x3dp, xbar2, iproj2)
        elseif (jno.eq.2)then
            call tstbar(2, x3d2, x3d3, r8bid3, r8bid3,&
                x3dp, xbar2, iproj2)
        else
            call tstbar(2, x3d3, x3d1, r8bid3, r8bid3,&
                x3dp, xbar2, iproj2)
        endif
        if (iproj2 .eq. 0)then
!               projection possible sur un côté
            iproj = 20
            icote = jno
        else
            iproj = 30
            if (xbar2(1).le.0.01d0)then
                inoeu = kno
            elseif (xbar2(2).le.0.01d0)then
                inoeu = jno
            else
                ASSERT(.false.)
            endif
        endif
    endif
77      continue         
    
end subroutine
