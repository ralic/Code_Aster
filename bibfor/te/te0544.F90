subroutine te0544(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lcegeo.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/pipepe.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES COEFFICIENTS A0 ET A1
!                          POUR LE PILOTAGE PAR CRITERE ELASTIQUE
!                          POUR LES ELEMENTS A VARIABLES DELOCALISEES
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: typmod(2), lielrf(10)
    character(len=16) :: compor, pilo
    integer :: jgano, ndim, nno, nnos, npg, lgpg, jtab(7), ntrou
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim, icopil, iborne, ictau, iret
    integer :: ideplm, iddepl, idepl0, idepl1, icompo, itype
    real(kind=8) :: dfdi(2187), elgeom(10, 27)
!
!
!
! - TYPE DE MODELISATION
    typmod(2) = 'GRADEPSI'
!
    if (nomte(1:5) .eq. 'MGCA_') then
        typmod(1) = '3D'
    else if (lteatt(' ','C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN'
    else if (lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN'
    else
!       NOM D'ELEMENT ILLICITE
        call assert(nomte(1:5).eq.'MGCA_')
    endif
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS POUR LES DEFO GENERALISEES
    call elref2(nomte, 10, lielrf, ntrou)
    call assert(ntrou.ge.2)
    call elref4(lielrf(2), 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    if (nno .gt. 27) call u2mess('F', 'ELEMENTS4_31')
    if (npg .gt. 27) call u2mess('F', 'ELEMENTS4_31')
!
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDDEPLR', 'L', iddepl)
    call jevech('PDEPL0R', 'L', idepl0)
    call jevech('PDEPL1R', 'L', idepl1)
    call jevech('PTYPEPI', 'L', itype)
!
    pilo = zk16(itype)
    compor = zk16(icompo)
    call jevech('PCDTAU', 'L', ictau)
    call jevech('PBORNPI', 'L', iborne)
!
!
! -- NOMBRE DE VARIABLES INTERNES
!
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    lgpg = max(jtab(6),1)*jtab(7)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
!
    if (compor .eq. 'BETON_DOUBLE_DP') then
        call lcegeo(nno, npg, ipoids, ivf, idfde,&
                    zr(igeom), typmod, compor, ndim, dfdi,&
                    zr(ideplm), zr(iddepl), elgeom)
    endif
!
! PARAMETRES EN SORTIE
!
    call jevech('PCOPILO', 'E', icopil)
!
!
    call pipepe(pilo, ndim, nno, npg, ipoids,&
                ivf, idfde, zr(igeom), typmod, zi(imate),&
                zk16(icompo), lgpg, zr(ideplm), zr(icontm), zr(ivarim),&
                zr(iddepl), zr(idepl0), zr(idepl1), zr(icopil), dfdi,&
                elgeom, iborne, ictau)
!
end subroutine
