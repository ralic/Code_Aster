subroutine te0208(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 2007 NECS - BRUNO ZUBER   WWW.NECS.FR
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/pipef3.h'
    include 'asterfort/tecach.h'
    character(len=16) :: nomte, option
!
!-----------------------------------------------------------------------
!     PILOTAGE POUR LES ELEMENTS DE JOINT 3D
!     OPTION : PILO_PRED_ELAS
!-----------------------------------------------------------------------
!
!
!
    integer :: igeom, imater, ideplm, ivarim, npg, jtab(7), iret, lgpg
    integer :: iddepl, idepl0, idepl1, ictau, icopil, ndim, nno, nnos, nddl
    integer :: ipoids, ivf, idfde, jgano
    character(len=8) :: typmod(2)
!
!    PARAMETRES DE L'ELEMENT FINI
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    nddl = 6*nno
!
! - PARAMETRES EN ENTREE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imater)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDDEPLR', 'L', iddepl)
    call jevech('PDEPL0R', 'L', idepl0)
    call jevech('PDEPL1R', 'L', idepl1)
    call jevech('PCDTAU', 'L', ictau)
!
    typmod(1) = '3D'
    typmod(2) = 'ELEMJOIN'
!
! RECUPERATION DU NOMBRE DE VARIABLES INTERNES PAR POINTS DE GAUSS :
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    lgpg = max(jtab(6),1)*jtab(7)
!
! PARAMETRE EN SORTIE
    call jevech('PCOPILO', 'E', icopil)
!
    call pipef3(ndim, nno, nddl, npg, lgpg,&
                zr(ipoids), zr(ivf), zr(idfde), zi(imater), zr(igeom),&
                zr(ivarim), zr(iddepl), zr(ideplm), zr(idepl0), zr(idepl1),&
                zr(ictau), zr(icopil), typmod)
!
!
end subroutine
