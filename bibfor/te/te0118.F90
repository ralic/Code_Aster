subroutine te0118(option, nomte)
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
! ----------------------------------------------------------------------
!  FONCTION REALISEE:
!     DANS LE CADRE X-FEM, PROPAGATION :
!    * CALCUL DE LA LONGUEUR DU PLUS PETIT ARETE DU MAILLAGE, C'EST A
!      DIRE LA VALEUR DE LA CONDITION CFL POUR LES PHASES DE
!      REINITIALISATION ET REORTHOGONALISATION
!    * CALCULS ELEMENTAIRES NECESSAIRES AUX PHASES DE REINITIALISATION
!     ET REORTHOGONALISATION (CF DOC R7.02.12)
!
!  OPTIONS
!  -------
!     'CFL_XFEM'
!      --------
!     CALCULE LA CONDITION CFL POUR LES PHASES DE REINITIALISATION ET
!     REORTHOGONALISATION
!        IN :  'PGEOMR'    GEOMETRIE
!        OUT : 'PLONCAR'   CHAM_ELEM
!
!     'MOY_NOEU_S'
!      ----------
!     CALCULE LE CHAM_ELEM CONTENANT LA MOYENNE ARITHMETIQUE AUX NOEUDS
!     SOMMETS D'UN CHAM_NO
!        IN :  'PNEUTR'    CHAM_NO
!        OUT : 'PMOYEL'    CHAM_ELEM
!
!     'XFEM_SMPLX_INIT'
!      ---------------
!     METHODE 'SIMPLEXE' : CALCULE LE CHAM_ELEM CONTENANT |T| DE
!     L'ELEMENT, ET LE CHAM_ELNO DES DIRECTIONS NI (CF DOC R)
!        IN :  'PGEOMR'    GEOMETRIE
!        OUT : 'PMEAST'    CHAM_ELEM
!              'PNIELNO'   CHAM_ELNO
!
!     'XFEM_SMPLX_REAC'
!      ---------------
!     METHODE 'SIMPLEXE' :  CALCULE LE CHAM_ELEM DELTA_PHI,
!                           ET LE CHAM_ELNO ALPHA (CF DOC R)
!        IN :  'PLSNO'     CHAM_NO
!              'PGRLS'     CHAM_NO
!              'PGRANDF'   CHAM_ELEM
!              'PNIELNO'   CHAM_ELNO
!        OUT : 'PDPHI'     CHAM_ELEM
!              'PALPHA'    CHAM_ELNO
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/teattr.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!
    integer :: iadzi, iazk24, igeom, ndim, nno, nnos, npg, ipoids, ivf, idfde
    integer :: jgano, i, j, ino, imeast, ini, ilsno, igrls, igdf, idphi, ialpha
    integer :: ineutr, imoyel, ilc, ibid, ndime
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), meast, nx, ny, nz, neutr, jac
    real(kind=8) :: gradx, grady, gradz, normgr, k(8), delphi, dphi(8), sigmak
    real(kind=8) :: sigkfi, alpha(8), smxdfi, dmin, distij, xi, yi, zpti, xj, yj
    real(kind=8) :: zj, sigmni(3), norm12, norm14, norm15, toleni
    character(len=8) :: typma, nomail, kdime
    character(len=24) :: valk(3)
!
    parameter   (toleni=1.d-6)
!
! DEBUT ----------------------------------------------------------------
    call jemarq()
!
!  VERIFICATION DU TYPE D'ELEMENT
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
    nomail= zk24(iazk24-1+3)(1:8)
!
!
!  CALCUL DE NDIME (2 OU 3) DIMENSION DE L'ESPACE :
    call teattr('S', 'DIM_COOR_MODELI', kdime, ibid)
    read(kdime,'(I8)')  ndime
    ASSERT(ndime.eq.2.or.ndime.eq.3)
!
!
!-----------------------------------------------------------------------
    if (option .eq. 'CFL_XFEM') then
!-----------------------------------------------------------------------
!  --------------------------------
!  RECUPERATION DES ENTREES/SORTIES
!  --------------------------------
!
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PLONCAR', 'E', ilc)
!
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
!
        if (ndime .eq. 3) then
!  ----------------------------------------------------------
!  CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
!  ----------------------------------------------------------
            dmin = sqrt(&
                   (&
                   zr(&
                   igeom-1+ndim*(2-1)+1)-zr(igeom-1+1))**2 +(zr(igeom-1+ndim*(2-1)+2)-zr(igeom-1+&
                   &2))**2 +(zr(igeom-1+ ndim*(2-1)+3)-zr(igeom-1+3&
                   )&
                   )**2&
                   )
!
!     BOUCLE SUR LES NOEUDS SOMMETS
            do 10 i = 1, nnos-1
                xi = zr(igeom-1+ndim*(i-1)+1)
                yi = zr(igeom-1+ndim*(i-1)+2)
                zpti = zr(igeom-1+ndim*(i-1)+3)
!
!     ON CHERCHE LE NOEUDS SOMMET LE PLUS PROCHE
                do 20 j = i+1, nnos
                    xj = zr(igeom-1+ndim*(j-1)+1)
                    yj = zr(igeom-1+ndim*(j-1)+2)
                    zj = zr(igeom-1+ndim*(j-1)+3)
!
                    distij = sqrt((xj-xi)**2+(yj-yi)**2+(zj-zpti)**2)
                    if ((distij.le.dmin) .and. (distij.ne.0)) dmin = distij
!
20              continue
!
10          continue
!
        else if (ndime.eq.2) then
!
!  ----------------------------------------------------------
!  CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
!  ----------------------------------------------------------
            dmin = sqrt(&
                   (&
                   zr(&
                   igeom-1+ndim*(2-1)+1)-zr(igeom-1+1))**2 +(zr(igeom-1+ndim*(2-1)+2)-zr(igeom-1+&
                   &2&
                   )&
                   )**2&
                   )
!
!     BOUCLE SUR LES NOEUDS SOMMETS
            do 30 i = 1, nnos-1
                xi = zr(igeom-1+ndim*(i-1)+1)
                yi = zr(igeom-1+ndim*(i-1)+2)
!
!     ON CHERCHE LE NOEUDS SOMMET LE PLUS PROCHE
                do 40 j = i+1, nnos
                    xj = zr(igeom-1+ndim*(j-1)+1)
                    yj = zr(igeom-1+ndim*(j-1)+2)
                    distij = sqrt((xj-xi)**2+(yj-yi)**2)
                    if ((distij.le.dmin) .and. (distij.ne.0)) dmin = distij
!
40              continue
!
30          continue
        endif
!
        zr(ilc) = dmin
!
    else
        if (typma(1:5) .ne. 'TETRA' .and. typma(1:4) .ne. 'HEXA' .and. typma(1:3) .ne.&
            'TRI' .and. typma(1:3) .ne. 'QUA') then
            valk(1) = option
            valk(2) = nomail
            valk(3) = typma
            call utmess('F', 'ELEMENTS3_19', nk=3, valk=valk)
        endif
    endif
!
!-----------------------------------------------------------------------
    if (option .eq. 'MOY_NOEU_S') then
!-----------------------------------------------------------------------
!
!  --------------------------------
!  RECUPERATION DES ENTREES/SORTIES
!  --------------------------------
        call jevech('PNEUTR', 'L', ineutr)
        call jevech('PMOYEL', 'E', imoyel)
!
!  --------------------------
!  CALCUL DE GRANDF ET PETITF
!  --------------------------
        call elref4(' ', 'NOEU', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
        neutr = 0.d0
        do 50 ino = 1, nno
            neutr = neutr + zr(ineutr-1+ino)
50      continue
!
        neutr = neutr / nno
        zr(imoyel) = neutr
!
!-----------------------------------------------------------------------
    else if (option.eq.'XFEM_SMPLX_INIT') then
!-----------------------------------------------------------------------
!
!  --------------------------------
!  RECUPERATION DES ENTREES/SORTIES
!  --------------------------------
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMEAST', 'E', imeast)
        call jevech('PNIELNO', 'E', ini)
!
        call elref4(' ', 'NOEU', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
        ASSERT(nno.le.8)
!
!
!  --------------
!  CALCUL DE |T|
!  --------------
!
        if (ndime .eq. 3) then
!
            if (typma(1:5) .eq. 'TETRA') then
                call elref4(' ', 'RIGI', ndim, nno, nnos,&
                            npg, ipoids, ivf, idfde, jgano)
                ASSERT(npg.eq.1)
                call dfdm3d(nno, 1, ipoids, idfde, zr(igeom),&
                            meast, dfdx, dfdy, dfdz)
                call elref4(' ', 'NOEU', ndim, nno, nnos,&
                            npg, ipoids, ivf, idfde, jgano)
!
            else if (typma(1:4).eq.'HEXA') then
                norm12=0.d0
                norm14=0.d0
                norm15=0.d0
                do 115 i = 1, 3
                    norm12=norm12+(zr(igeom-1+ndim*(2-1)+i) -zr(igeom-&
                    1+ndim*(1-1)+i))**2.d0
                    norm14=norm14+(zr(igeom-1+ndim*(4-1)+i) -zr(igeom-&
                    1+ndim*(1-1)+i))**2.d0
                    norm15=norm15+(zr(igeom-1+ndim*(5-1)+i) -zr(igeom-&
                    1+ndim*(1-1)+i))**2.d0
115              continue
                norm12=norm12**.5d0
                norm14=norm14**.5d0
                norm15=norm15**.5d0
!       Volume elementaire
                meast = norm12 * norm14 * norm15
            endif
!
        else if (ndime.eq.2) then
            if (typma(1:3) .eq. 'QUA') then
                norm12=0.d0
                norm14=0.d0
                do 116 i = 1, 2
                    norm12=norm12+(zr(igeom-1+ndim*(2-1)+i) -zr(igeom-&
                    1+ndim*(1-1)+i))**2.d0
                    norm14=norm14+(zr(igeom-1+ndim*(4-1)+i) -zr(igeom-&
                    1+ndim*(1-1)+i))**2.d0
116              continue
                norm12=norm12**.5d0
                norm14=norm14**.5d0
!       Surface elementaire
                meast = norm12 * norm14
            else if (typma(1:3).eq.'TRI') then
                call elref4(' ', 'RIGI', ndim, nno, nnos,&
                            npg, ipoids, ivf, idfde, jgano)
!               ASSERT(NPG.EQ.1)
                call dfdm2d(nno, 1, ipoids, idfde, zr(igeom),&
                            meast, dfdx, dfdy)
                call elref4(' ', 'NOEU', ndim, nno, nnos,&
                            npg, ipoids, ivf, idfde, jgano)
            endif
        endif
!  -----------------------
!  CALCUL DE NI AUX NOEUDS
!  -----------------------
        do 100 ino = 1, nno
!
            if (ndime .eq. 3) then
                call dfdm3d(nno, ino, ipoids, idfde, zr(igeom),&
                            jac, dfdx, dfdy, dfdz)
            else if (ndime.eq.2) then
                call dfdm2d(nno, ino, ipoids, idfde, zr(igeom),&
                            jac, dfdx, dfdy)
            endif
!
            if (typma(1:5) .eq. 'TETRA') then
                zr(ini-1+(ino-1)*3+1) = ndim * meast * dfdx(ino)
                zr(ini-1+(ino-1)*3+2) = ndim * meast * dfdy(ino)
                zr(ini-1+(ino-1)*3+3) = ndim * meast * dfdz(ino)
!
            else if (typma(1:4).eq.'HEXA') then
                zr(ini-1+(ino-1)*3+1) = ndim * meast * dfdx(ino)/4.d0
                zr(ini-1+(ino-1)*3+2) = ndim * meast * dfdy(ino)/4.d0
                zr(ini-1+(ino-1)*3+3) = ndim * meast * dfdz(ino)/4.d0
!
            else if (typma(1:3).eq.'QUA') then
!   Rq : Pur les QUAD la derivee des FF en 0 vaut la moitie de celle au
!   Noeud considere. Il faudra penser a s'occuper des FF d'ordre 2!
                zr(ini-1+(ino-1)*2+1) = ndim * meast * dfdx(ino)/2.d0
                zr(ini-1+(ino-1)*2+2) = ndim * meast * dfdy(ino)/2.d0
!
            else if (typma(1:3).eq.'TRI') then
                zr(ini-1+(ino-1)*2+1) = ndim * meast * dfdx(ino)
                zr(ini-1+(ino-1)*2+2) = ndim * meast * dfdy(ino)
!
            endif
100      continue
!
!  VERIFICATION SIGMA(NI)=0
        if (typma(1:4) .eq. 'HEXA') then
            do 120 i = 1, 3
                sigmni(i)=0
                do 130 ino = 1, nno
                    sigmni(i)=sigmni(i)+zr(ini-1+(ino-1)*3+i)
130              continue
                if (abs(sigmni(i)) .gt. toleni) then
                    call utmess('F', 'ELEMENTS3_20', sk=nomail)
                endif
120          continue
        endif
!
        zr(imeast) = meast
!
!-----------------------------------------------------------------------
    else if (option.eq.'XFEM_SMPLX_CALC') then
!-----------------------------------------------------------------------
!
!  ------------------------
!  RECUPERATION DES ENTREES
!  ------------------------
        call jevech('PLSNO', 'L', ilsno)
        call jevech('PGRLS', 'L', igrls)
        call jevech('PGRANDF', 'L', igdf)
        call jevech('PNIELNO', 'L', ini)
!
        call elref4(' ', 'NOEU', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
        ASSERT(nno.le.8)
!
!  ------------------------------------------
!  INITIALISATION DE DELTAPHI ET ALPHA(I) A 0
!  ------------------------------------------
        delphi=0.d0
        do 200 ino = 1, nno
            alpha(ino)=0.d0
200      continue
!
!  ---------------------
!  TRAITEMENT DU CAS F=0
!  ---------------------
!  CETTE PARTIE EST UTILE EN REINITIALISATION 3D, SI L'ON A DES ELEMENTS
!  AYANT AUTANT DE NOEUDS DE PART ET D'AUTRE DE L'ISOZERO EN L'ABSENCE
!  DE L'ALGORITHME DE CALCUL DIRECT DES DISTANCES
!
        if (zr(igdf) .eq. 0.d0) goto 850
!  --------------------------------------
!  CALCUL DU GRADIENT DE LS SUR L'ELEMENT
!  --------------------------------------
        gradx = 0.d0
        grady = 0.d0
        gradz = 0.d0
        do 300 ino = 1, nno
            gradx = gradx + zr(igrls-1+(ino-1)*ndim+1)/(nno*4.d0)
            grady = grady + zr(igrls-1+(ino-1)*ndim+2)/(nno*4.d0)
            if (ndim .eq. 3) gradz = gradz+zr( igrls-1+(ino-1)*ndim+3)/( nno*4.d0)
300      continue
!
!  CALCUL DE LA NORME DU GRADIENT
        normgr = (gradx**2.d0 + grady**2.d0 + gradz**2.d0 )**0.5d0
!
        if (abs(normgr) .lt. r8prem()) goto 850
!
!  -----------------------
!  CALCUL DE KI AUX NOEUDS
!  -----------------------
        do 400 ino = 1, nno
!
!  RECUPERATION DE LA DIRECTION NI AU NOEUD
            nx = zr(ini-1+(ino-1)*ndime+1)
            ny = zr(ini-1+(ino-1)*ndime+2)
            if (ndime .eq. 3) nz = zr(ini-1+(ino-1)*3+3)
            if (ndime .eq. 2) nz = 0.d0
!
!
!  CALCUL DE KI AU NOEUD
            k(ino) = zr(igdf)*(gradx*nx+grady*ny+gradz*nz)/(normgr* ndim)
!
400      continue
!
!  ----------------------
!  CALCUL DE SIGMA(KI(-))
!  ----------------------
        sigmak = 0.d0
        do 500 ino = 1, nno
            sigmak = sigmak + min(0.d0,k(ino))
500      continue
!
        if (abs(sigmak) .lt. r8prem()) goto 850
!
!  ------------------------------------------------------
!  CALCUL DE DELTAPHI AUX NOEUDS ET DELTAPHI DE L'ELEMENT
!  ------------------------------------------------------
        delphi = 0.d0
!
        do 600 ino = 1, nno
!
!  CALCUL DE SIGKFI = SIGMA( K(I)(-)*(LS(INO)-LS(I)) ) AUX NOEUDS
            sigkfi = 0.d0
            do 610 i = 1, nno
                sigkfi = sigkfi + min( 0.d0,k(i)) * ( zr(ilsno+ino-1) - zr(ilsno+i-1))
610          continue
!
            dphi(ino) = max(0.d0,k(ino)) * sigkfi / sigmak
            delphi = delphi + k(ino)*zr(ilsno+ino-1)
!
600      continue
!
        if (abs(delphi) .lt. r8prem()) goto 850
!
!  --------------------------------------------------------
!  CALCUL DE SIGMA( MAX(0,DPHI(I)/DELPHI) ) SUR LES NOEUDS
!  --------------------------------------------------------
        smxdfi = 0.d0
        do 700 ino = 1, nno
            smxdfi = smxdfi + max(0.d0,dphi(ino)/delphi)
700      continue
!
        if (abs(smxdfi) .lt. r8prem()) goto 850
!
!  --------------------------
!  CALCUL DE ALPHA AUX NOEUDS
!  --------------------------
        do 800 ino = 1, nno
            alpha(ino) = max(0.d0,dphi(ino)/delphi) / smxdfi
800      continue
!
850      continue
!
!  --------------------
!  STOCKAGE DES SORTIES
!  --------------------
        call jevech('PDPHI', 'E', idphi)
        call jevech('PALPHA', 'E', ialpha)
!
        zr(idphi) = delphi
!
        do 900 ino = 1, nno
            zr(ialpha-1+ino) = alpha(ino)
!
900      continue
!
!-----------------------------------------------------------------------
    endif
!-----------------------------------------------------------------------
    call jedema()
end subroutine
