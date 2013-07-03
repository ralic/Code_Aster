subroutine vicin2(ndim, g, npg, lgpg, vim,&
                  rpt, nbcin, numcin)
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
    implicit none
#include "asterfort/utbtab.h"
    integer :: ndim, g, npg, lgpg, i, nbcin, numcin(2)
    real(kind=8) :: vim(lgpg, npg), rpt(3, 3), vimm(3, 3)
    real(kind=8) :: vari(3, 3), esp(9)
!
! ---------------------------------
!   G       : NUMERO DU POINT DE GAUSS COURANT
!   NDIM    : DIMENSION DE L'ESPACE
!   NPG     : NOMBRE DE POINTS DE GAUSS
!   LGPG    : TAILLE DES VARIABLES INTERNES DEPEND DU COMPORTEMENT
!   VIM     : VARIABLE INTERNE EN T-
!   RPT     : TENSEUR ROTATION
!   NBCIN   : NOMBRE DE VARIABLES CINEMATIQUES
!   NUMCIN  : NUMEROS DES VARIABLES CINEMATIQUES
!
!   SORTIE:
!   VIM     : VECTEUR VIM MODIFIE
! ----------------------------
!
!   CALCUL DES ENTREES DE NMCOMP POUR GDEF_HYPO_ELAS
!
! -------------------------
!
!     PREMIERE VARIABLE CINEMATIQUE
    do 1 i = 1, 3
        vimm(i,i)=vim(numcin(1)-1+i,g)
 1  end do
    vimm(1,2)=vim(numcin(1)-1+4,g)
    vimm(2,1)=vim(numcin(1)-1+4,g)
    vimm(1,3)=vim(numcin(1)-1+5,g)
    vimm(3,1)=vim(numcin(1)-1+5,g)
    vimm(2,3)=vim(numcin(1)-1+6,g)
    vimm(3,2)=vim(numcin(1)-1+6,g)
!
!     MULTIPLICATION PAR f^T_{n+alpha}
    call utbtab('ZERO', 3, 3, vimm, rpt,&
                esp, vari)
!
!     MODIFICATION DES VARIABLES INTERNES POUR NMCOMP
    do 2 i = 1, 3
        vim(numcin(1)-1+i,g)=vari(i,i)
 2  end do
    vim(numcin(1)-1+4,g)=vari(1,2)
    vim(numcin(1)-1+5,g)=vari(1,3)
    vim(numcin(1)-1+6,g)=vari(2,3)
!
    if (nbcin .eq. 2) then
!
!        DEUXIEME VARIABLE CINEMATIQUE
!
        do 11 i = 1, 3
            vimm(i,i)=vim(numcin(2)-1+i,g)
!
11      continue
        vimm(1,2)=vim(numcin(2)-1+4,g)
        vimm(2,1)=vim(numcin(2)-1+4,g)
        vimm(1,3)=vim(numcin(2)-1+5,g)
        vimm(3,1)=vim(numcin(2)-1+5,g)
        vimm(2,3)=vim(numcin(2)-1+6,g)
        vimm(3,2)=vim(numcin(2)-1+6,g)
!
!        MULTIPLICATION PAR f^T_{n+alpha}
        call utbtab('ZERO', 3, 3, vimm, rpt,&
                    esp, vari)
!
!        MODIFICATION DES VARIABLES INTERNES POUR NMCOMP
        do 12 i = 1, 3
            vim(numcin(2)-1+i,g)=vari(i,i)
12      continue
        vim(numcin(2)-1+4,g)=vari(1,2)
        vim(numcin(2)-1+5,g)=vari(1,3)
        vim(numcin(2)-1+6,g)=vari(2,3)
    endif
end subroutine
