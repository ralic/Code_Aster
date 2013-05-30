subroutine porea1(nno, nc, deplm, deplp, geom,&
                  gamma, vecteu, pgl, xl, angp)
    implicit    none
    include 'jeveux.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/angvx.h'
    include 'asterfort/assert.h'
    include 'asterfort/gareac.h'
    include 'asterfort/matrot.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/vdiff.h'
    include 'blas/ddot.h'
    integer :: nno, nc
    real(kind=8) :: deplm(nno*nc), deplp(nno*nc), geom(3, nno), gamma
!
    real(kind=8) :: pgl(3, 3), xl, angp(3)
    logical :: vecteu
!
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
!
!     CALCUL DE LA MATRICE DE PASSAGE GLOBALE/LOCALE EN TENANT COMPTE
!     DE LA GEOMETRIE REACTUALISEE POUR LES POUTRES AINSI QUE LA
!     LONGUEUR DE LA POUTRE
!     POUR LES OPTIONS FULL_MECA RAPH_MECA ET RIGI_MECA_TANG
!
! --- ------------------------------------------------------------------
!
! IN  NNO    : NOMBRE DE NOEUDS
! IN  NC     : NOMBRE DE COMPOSANTE DU CHAMP DE DEPLACEMENTS
! IN  DEPLM  : DEPLACEMENT AU TEMPS -
! IN  DEPLP  : INCREMENT DE DEPLACEMENT AU TEMPS +
! IN  GEOM   : COORDONNEES DES NOEUDS
! IN  GAMMA  : ANGLE DE VRILLE AU TEMPS -
! IN  VECTEU : TRUE SI FULL_MECA OU RAPH_MECA
! OUT PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
! OUT XL     : LONGUEUR DE L'ELEMENT
! OUT ANGP   : ANGLES NAUTIQUES ACTUALISEE
!              ATTENTION ANGP(3) EST DIFFERENT DE GAMMA1 QUI A SERVIT
!              POUR CALCUL PGL
! --- ------------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: i, ibid
    real(kind=8) :: utg(14), xug(6), xd(3), xl2, alfa1, beta1, alfa0, beta0
    real(kind=8) :: xugm(6), dgamma, tet1, tet2, gamma1, xdm(3)
    real(kind=8) :: ang1(3)
!
    integer :: iadzi, iazk24
    character(len=24) :: valkm(2)
    real(kind=8) :: valrm
!
    call assert(nno.eq.2)
!
!     CALCUL DU VECTEUR XLOCAL AU TEMPS T-
    do 20 i = 1, 3
        xug(i) = deplm(i) + geom(i,1)
        xug(i+3) = deplm(i+nc) + geom(i,2)
20  end do
    call vdiff(3, xug(4), xug(1), xd)
!     CALCUL DES DEUX PREMIERS ANGLES NAUTIQUES AU TEMPS T-
    call angvx(xd, alfa0, beta0)
!
!     DEPLACEMENT TOTAL A T+
    do 110 i = 1, nno*nc
        utg(i) = deplm(i) + deplp(i)
110  continue
!     CALCUL DU VECTEUR XLOCAL AU TEMPS T+
    do 120 i = 1, 3
        xug(i) = utg(i) + geom(i,1)
        xug(i+3) = utg(i+nc) + geom(i,2)
120  end do
    call vdiff(3, xug(4), xug(1), xd)
!     CALCUL DES DEUX PREMIERS ANGLES NAUTIQUES AU TEMPS T+
    call angvx(xd, alfa1, beta1)
!     SI DIFF(ANGLE) > PI/8
    if (abs(alfa0 - alfa1) .gt. 0.3927d+00) then
        call tecael(iadzi, iazk24)
        valkm(1) = zk24(iazk24+3-1)
        valkm(2) = 'ALPHA'
        valrm = (alfa0 - alfa1)*r8rddg()
        call u2mesg('A', 'ELEMENTS_38', 2, valkm, 0,&
                    ibid, 1, valrm)
    endif
    if (abs(beta0 - beta1) .gt. 0.3927d+00) then
        call tecael(iadzi, iazk24)
        valkm(1) = zk24(iazk24+3-1)
        valkm(2) = 'BETA'
        valrm = (beta0 - beta1)*r8rddg()
        call u2mesg('A', 'ELEMENTS_38', 2, valkm, 0,&
                    ibid, 1, valrm)
    endif
!
!     LONGUEUR DE L'ELEMENT AU TEMPS T+
    xl2=ddot(3,xd,1,xd,1)
    xl = sqrt(xl2)
    if (vecteu) then
!        CALCUL DU VECTEUR XLOCAL AU TEMPS T-
        do 130 i = 1, 3
            xugm(i ) = geom(i,1) + deplm(i)
            xugm(i+3) = geom(i,2) + deplm(nc+i)
130      continue
        call vdiff(3, xugm(4), xugm(1), xdm)
!        MISE A JOUR DU 3EME ANGLE NAUTIQUE AU TEMPS T+
        call gareac(xdm, xd, dgamma)
!        SI DGAMMA > PI/8
        if (abs(dgamma) .gt. 0.3927d+00) then
            call tecael(iadzi, iazk24)
            valkm(1) = zk24(iazk24+3-1)
            valkm(2) = 'GAMMA'
            valrm = dgamma*r8rddg()
            call u2mesg('A', 'ELEMENTS_38', 2, valkm, 0,&
                        ibid, 1, valrm)
        endif
    else
        dgamma = 0.d0
    endif
!
    tet1 = ddot(3,utg(4),1,xd,1)
    tet2 = ddot(3,utg(nc+4),1,xd,1)
    tet1 = tet1/xl
    tet2 = tet2/xl
    gamma1 = gamma + dgamma + (tet1+tet2)/2.d0
!     SAUVEGARDE DES ANGLES NAUTIQUES
    angp(1) = alfa1
    angp(2) = beta1
    angp(3) = gamma + dgamma
!     MATRICE DE PASSAGE GLOBAL -> LOCAL
    ang1(1) = alfa1
    ang1(2) = beta1
    ang1(3) = gamma1
    call matrot(ang1, pgl)
end subroutine
