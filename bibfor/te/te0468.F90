subroutine te0468(option, nomte)
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
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vff2dn.h'
    character(len=16) :: option, nomte
! ......................................................................
!
!     BUT: CALCUL DU FLUX HYDRAULIQUE NORMAL
!          SUR DES SEG2
!          OPTION : 'FLHN_ELGA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: poids, nx, ny, flx(4), fly(4), flun
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: iflux, ivectu, k, i, iad
    character(len=24) :: valkm(3)
!
!
!-----------------------------------------------------------------------
    integer :: ifl, jgano, nbflux, ndim, nnos
    real(kind=8) :: s, t
!-----------------------------------------------------------------------
    call elref4('SE2', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTR', 'L', iflux)
    call jevech('PFLHN', 'E', ivectu)
!
!  CALCUL DU NBRE DE CMP CALCULEES DU FLUX
    if (nomte .eq. 'HM_D_PLAN_SE3' .or. nomte .eq. 'THM_D_PLAN_SE3' .or. nomte .eq.&
        'HM_AXIS_SE3' .or. nomte .eq. 'THM_AXIS_SE3' .or. nomte .eq. 'H_D_PLAN_SE3') then
        nbflux=1
    else if (nomte.eq.'THV_D_PLAN_SE3'.or.nomte.eq.'THV_AXIS_SE3') then
        nbflux=2
        elseif(nomte.eq.'HHM_D_PLAN_SE3'.or.nomte.eq.'THH_D_PLAN_SE3'&
    .or.nomte.eq.'THHM_D_PLAN_SE3'.or.nomte.eq.'HH_D_PLAN_SE3'&
    .or.nomte.eq.'HHM_AXIS_SE3'.or.nomte.eq.'THH_AXIS_SE3' .or.nomte&
    .eq.'THHM_AXIS_SE3'.or.nomte.eq.'HH_AXIS_SE3') then
        nbflux=3
        elseif(nomte.eq.'HH2M_D_PLAN_SE3'.or.nomte.eq.'THH2_D_PLAN_SE3'&
    .or.nomte.eq.'THH2M_D_PLAN_SE3'.or.nomte.eq.'HH2_D_PLAN_SE3'&
    .or.nomte.eq.'HH2M_AXIS_SE3'.or.nomte.eq.'THH2_AXIS_SE3' .or.nomte&
    .eq.'THH2M_AXIS_SE3'.or.nomte.eq.'HH2_AXIS_SE3') then
        nbflux=4
    else
        valkm(1)=option
        valkm(2)=nomte
        valkm(3)='TE0468'
        call u2mesk('F', 'CALCULEL7_2', 3, valkm)
    endif
!    BOUCLE SUR LES CMP
    do 100 ifl = 1, nbflux
!
!    BOUCLE SUR LES POINTS DE GAUSS
        do 40 kp = 1, npg
            k = (kp-1)*nno
! CALCUL DES FLUX AU POINT DE GAUSS KP A PARTIR DES FLUX AUX NOEUDS
            s = 0.d0
            t = 0.d0
            do 10 i = 1, nno
                iad = iflux+2*(ifl-1)+2*nbflux*(i-1)
                s = s + zr(iad )*zr(ivf+k+i-1)
                t = t + zr(iad+1)*zr(ivf+k+i-1)
10          continue
            flx(kp) = s
            fly(kp) = t
! CALCUL DE LA NORMALE
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        zr(igeom), nx, ny, poids)
            flun = nx*flx(kp) + ny*fly(kp)
            zr(ivectu+nbflux*(kp-1)+ifl-1) = flun
40      continue
100  end do
end subroutine
