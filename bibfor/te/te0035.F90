subroutine te0035(option, nomte)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/dxbsig.h'
    include 'asterfort/dxefgi.h'
    include 'asterfort/dxefgt.h'
    include 'asterfort/dxqpgl.h'
    include 'asterfort/dxtpgl.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/utpvgl.h'
    character(len=16) :: option, nomte
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          POUR LES ELEMENTS DKT, DST, DKQ, DSQ ET Q4G
!                          OPTIONS : 'CHAR_MECA_TEMP_R'
!                                    'CHAR_MECA_EPSI_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i, jgeom, jcaco, jvecg, idefi, iret
    real(kind=8) :: pgl(3, 3), xyzl(3, 4)
    real(kind=8) :: epsini(6)
    real(kind=8) :: bsigma(24), sigt(32)
! ----------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcaco)
    call jevech('PVECTUR', 'E', jvecg)
!
! --- DETERMINATION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
! --- AU REPERE LOCAL A L'ELEMENT
!     ---------------------------
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno .eq. 4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
! --- DETERMINATION DES COORDONNEES DES CONNECTIVITES DE L'ELEMENT
! --- DANS SON REPERE LOCAL
!     ---------------------
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
!
! --- CALCUL DES EFFORTS GENERALISES D'ORIGINE THERMIQUE
! --- AUX POINTS D'INTEGRATION
!     ------------------------
    if (option .eq. 'CHAR_MECA_TEMP_R') then
!
        call dxefgt(pgl, sigt)
!
    else if (option .eq.'CHAR_MECA_EPSI_R') then
!
        call jevech('PEPSINR', 'L', idefi)
!
        epsini(1) = zr(idefi+1-1)
        epsini(2) = zr(idefi+2-1)
        epsini(3) = zr(idefi+3-1)
        epsini(4) = zr(idefi+4-1)
        epsini(5) = zr(idefi+5-1)
        epsini(6) = zr(idefi+6-1)
!
        call dxefgi(nomte, xyzl, pgl, epsini, sigt)
!
    endif
!
! --- CALCUL DES EFFORTS INTERNES D'ORIGINE THERMIQUE
! --- (I.E. SOMME_VOL(BT_SIG))
!     ------------------------
    call dxbsig(nomte, xyzl, pgl, sigt, bsigma)
!
! --- AFFECTATION DU VECTEUR DES FORCES ELEMENTAIRES EN SORTIE DU TE
!     --------------------------------------------------------------
    do 20 i = 1, nno*6
        zr(jvecg+i-1) = bsigma(i)
20  end do
!
end subroutine
