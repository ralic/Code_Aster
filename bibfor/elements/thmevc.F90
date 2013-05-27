subroutine thmevc(option, nomte, axi, nno, npg,&
                  ipoids, ivf, idfde, nddls, nnos,&
                  nddlm, nnom)
    implicit none
    include 'jeveux.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tefrep.h'
    logical :: axi
    integer :: nno, npg, ipoids, ivf, idfde
    integer :: nnos, nddls, nnom, nddlm
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.meunier at edf.fr
! =====================================================================
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENTS THHM, HM ET HH
! =====================================================================
!  THERMO-HYDRO-MECANIQUE - CALCUL DES EVOL_CHAR
!  *      *     *                      **   *
! =====================================================================
! OPTION    NOM DE L'OPTION
! NOMTE     NOM DE L'OPTION
! NNO       NOMBRE DE NOEUDS TOTAL SUR L'ELEMENT
! NPG       NOMBRE DE POINTS DE GAUSS
! IPOIDS    ADRESSE DU VECTEUR POIDS DES POINTS DE GAUSS
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IDFDE     ADRESSE DES DERIVEES DES FONCTIONS DE FORME
! NDDLS     NB DE DDL SUR LES SOMMETS
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NDDLM     NB DE DDL SUR LES MILIEUX
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! =====================================================================
!
!
!
!
! =====================================================================
    real(kind=8) :: poids
    integer :: i, iforc, igeom, ivectu, ii, kp, k
    real(kind=8) :: dfdbid(27), fx, fy, fz
    real(kind=8) :: rx
! =====================================================================
! --- 1. OPTION : CHAR_MECA_FR3D3D ------------------------------------
! =====================================================================
    if (option .eq. 'CHAR_MECA_FR3D3D') then
        call jevech('PGEOMER', 'L', igeom)
        call tefrep(option, nomte, 'PFR3D3D', iforc)
        call jevech('PVECTUR', 'E', ivectu)
!
        do 10 kp = 1, npg
            k = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, dfdbid, poids)
!
!      --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
!
            fx = 0.d0
            fy = 0.d0
            fz = 0.d0
!
            do 20 i = 1, nno
                ii = 3 * (i-1)
                fx = fx + zr(ivf+k+i-1) * zr(iforc+ii )
                fy = fy + zr(ivf+k+i-1) * zr(iforc+ii+1)
                fz = fz + zr(ivf+k+i-1) * zr(iforc+ii+2)
20          continue
!
            do 30 i = 1, nnos
                ii = nddls * (i-1)
                zr(ivectu+ii) = zr(ivectu+ii) + poids * fx * zr(ivf+k+ i-1)
                zr(ivectu+ii+1) = zr(ivectu+ii+1) + poids * fy * zr( ivf+k+i-1)
                zr(ivectu+ii+2) = zr(ivectu+ii+2) + poids * fz * zr( ivf+k+i-1)
30          continue
!
            do 40 i = 1, nnom
                ii = nnos*nddls+nddlm*(i-1)
                zr(ivectu+ii)= zr(ivectu+ii) + poids * fx * zr(ivf+k+&
                i+nnos-1)
                zr(ivectu+ii+1)= zr(ivectu+ii+1) + poids * fy * zr(&
                ivf+k+i+nnos-1)
                zr(ivectu+ii+2)= zr(ivectu+ii+2) + poids * fz * zr(&
                ivf+k+i+nnos-1)
40          continue
10      continue
    endif
! ======================================================================
! --- 2. OPTION : CHAR_MECA_FR2D2D -------------------------------------
! ======================================================================
    if (option .eq. 'CHAR_MECA_FR2D2D') then
        call jevech('PGEOMER', 'L', igeom)
        call tefrep(option, nomte, 'PFR2D2D', iforc)
        call jevech('PVECTUR', 'E', ivectu)
!
        do 100 kp = 1, npg
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdbid, dfdbid, poids)
!
!      --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
            fx = 0.d0
            fy = 0.d0
            do 110 i = 1, nno
                ii = 2 * (i-1)
                fx = fx + zr(ivf+k+i-1) * zr(iforc+ii )
                fy = fy + zr(ivf+k+i-1) * zr(iforc+ii+1)
110          continue
!
            if (axi) then
                rx = 0.d0
                do 120 i = 1, nno
                    rx = rx + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
120              continue
                poids = poids*rx
            endif
!
            do 130 i = 1, nnos
                ii = nddls* (i-1)
                zr(ivectu+ii) = zr(ivectu+ii) + poids * fx * zr(ivf+k+ i-1)
                zr(ivectu+ii+1) = zr(ivectu+ii+1) + poids * fy * zr( ivf+k+i-1)
130          continue
!
            do 140 i = 1, nnom
                ii = nnos*nddls+nddlm*(i-1)
                zr(ivectu+ii)= zr(ivectu+ii) + poids * fx * zr(ivf+k+&
                i+nnos-1)
                zr(ivectu+ii+1)= zr(ivectu+ii+1) + poids * fy * zr(&
                ivf+k+i+nnos-1)
140          continue
100      continue
    endif
!
end subroutine
