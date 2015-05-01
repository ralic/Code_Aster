subroutine imprel(titre, nbterm, coef, lisddl, lisno,&
                  beta)
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
    implicit none
#include "asterfort/infniv.h"
    character(len=*) :: titre
    integer :: info, nbterm, ifm, i
    character(len=8) :: lisddl(nbterm), lisno(nbterm)
    character(len=24) :: titr24
    real(kind=8) :: coef(nbterm), beta
!
    call infniv(ifm, info)
    if (info .lt. 2) goto 9999
!
    titr24=titre
!
    10 format(2x,'    COEF    ','*','   DDL  ','(',' NOEUD  ',')')
    20 format(2x,1pe12.5,' * ',a8,'(',a8,')','+')
    30 format(2x,'=',1pe12.5)
    40 format(2x,'______________________________________')
!
    write(ifm,*) 'RELATION LINEAIRE AFFECTEE PAR '//titr24
    write(ifm,10)
    do 100 i = 1, nbterm
        write(ifm,20) coef(i),lisddl(i),lisno(i)
100  end do
    write(ifm,30) beta
    write(ifm,40)
9999  continue
end subroutine
