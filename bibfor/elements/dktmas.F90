subroutine dktmas(xyzl, option, pgl, mas, ener)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/diaexp.h"
#include "asterfort/dialum.h"
#include "asterfort/dktnib.h"
#include "asterfort/dktniw.h"
#include "asterfort/dxroep.h"
#include "asterfort/dxtloc.h"
#include "asterfort/dxtloe.h"
#include "asterfort/dxtnim.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
!
    real(kind=8) :: xyzl(3, *), pgl(*), mas(*), ener(*)
    character(len=16) :: option
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
!     ------------------------------------------------------------------
!     MATRICE MASSE DE L'ELEMENT DE PLAQUE DKT
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT MAS    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: i, j, k, i1, i2, i3, jcoqu, jdepg, m1, m2, m3
    integer :: jvitg, iret
    real(kind=8) :: detj, wgt, wkt(9), depl(18), nfx(9), nfy(9), nmi(3)
    real(kind=8) :: vite(18)
    real(kind=8) :: flex(9, 9), memb(6, 6), mefl(6, 9), masloc(171), masglo(171)
    real(kind=8) :: rho, epais, roe, rof, ctor, excent, xinert
    real(kind=8) :: zero, un, six, huit, douze, wgtf, wgtmf
    real(kind=8) :: qsi, eta, carat3(21), coef1, coef2
    character(len=3) :: stopz
    aster_logical :: exce, iner
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    zero = 0.0d0
    un = 1.0d0
    six = 6.0d0
    huit = 8.0d0
    douze = 12.0d0
!
    call dxroep(rho, epais)
    roe = rho*epais
    rof = rho*epais*epais*epais/douze
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
    xinert = zr(jcoqu+5)
!
    exce = .false.
    iner = .false.
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
    if (abs(xinert) .gt. un/r8gaem()) iner = .true.
    if (.not. iner) rof = zero
!
! --- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE :
!     -------------------------------------------------
    call gtria3(xyzl, carat3)
!
! --- INITIALISATIONS :
!     ---------------
    call r8inir(81, zero, flex, 1)
    call r8inir(54, zero, mefl, 1)
    call r8inir(36, zero, memb, 1)
!
    detj = carat3(7)
!
!======================================
! ---  CALCUL DE LA MATRICE DE MASSE  =
!======================================
!=====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE CLASSIQUE DE LA MATRICE DE MASSE =
! ---  LES TERMES SONT EN NK*NP                                      =
!=====================================================================
!
    memb(1,1) = carat3(8)*roe/six
    memb(1,3) = carat3(8)*roe/douze
    memb(1,5) = memb(1,3)
    memb(2,2) = memb(1,1)
    memb(2,4) = memb(1,3)
    memb(2,6) = memb(1,3)
    memb(3,1) = memb(1,3)
    memb(3,3) = memb(1,1)
    memb(3,5) = memb(1,3)
    memb(4,2) = memb(1,3)
    memb(4,4) = memb(1,1)
    memb(4,6) = memb(1,3)
    memb(5,1) = memb(1,3)
    memb(5,3) = memb(1,3)
    memb(5,5) = memb(1,1)
    memb(6,2) = memb(1,3)
    memb(6,4) = memb(1,3)
    memb(6,6) = memb(1,1)
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     ===================================
    do i = 1, npg
!
        qsi = zr(icoopg-1+ndim*(i-1)+1)
        eta = zr(icoopg-1+ndim*(i-1)+2)
!
!===========================================================
! ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE  =
!===========================================================
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
!       -------------------------------------------------
        call dktniw(qsi, eta, carat3, wkt)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
! ---   EST EGALE A RHO_E = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+i-1)*detj*roe
!
! ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
! ---   DUE AUX SEULS TERMES DE LA FLECHE W :
!       -----------------------------------
        do k = 1, 9
            do j = 1, 9
                flex(k,j) = flex(k,j) + wkt(k)*wkt(j)*wgt
            end do
        end do
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
!       --------------------------------------------------
        call dktnib(qsi, eta, carat3, nfx, nfy)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
! ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
!       ----------------------------------------------------
        wgtf = zr(ipoids+i-1)*detj*(rof+excent*excent*roe)
!
! ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
!       -------------------------------------------------------
        do k = 1, 9
            do j = 1, 9
                flex(k,j) = flex(k,j)+(nfx(k)*nfx(j)+nfy(k)*nfy(j))* wgtf
            end do
        end do
!
!====================================================================
! ---  CAS OU L'ELEMENT EST EXCENTRE                                =
! ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE  =
!====================================================================
!
        if (exce) then
!
! ---     FONCTIONS D'INTERPOLATION MEMBRANE :
!         ----------------------------------
            call dxtnim(qsi, eta, nmi)
!
! ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
! ---     DE LA MASSE VOLUMIQUE
! ---     RHO_MF = D*EPAIS*RHO
!         --------------------
            wgtmf = zr(ipoids+i-1)*detj*excent*roe
!
! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
!         ------------------------------------------
            do k = 1, 3
                do j = 1, 9
                    i1 = 2*(k-1)+1
                    i2 = i1 +1
                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
                end do
            end do
        endif
!
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
    end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
!
!
! --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
! --- DE MASSE A LA MATRICE ELLE MEME :
!     ===============================
    if (( option .eq. 'MASS_MECA' ) .or. (option.eq.'M_GAMMA')) then
        call dxtloc(flex, memb, mefl, ctor, mas)
!
    else if (option.eq.'MASS_MECA_DIAG') then
        call dxtloc(flex, memb, mefl, ctor, masloc)
        wgt = carat3(8)*roe
        wgtf= carat3(8)*rof
        call utpslg(3, 6, pgl, masloc, masglo)
        call dialum(3, 6, 18, wgt, masglo,&
                    mas)
!
    else if (option.eq.'MASS_MECA_EXPLI') then
        call dxtloc(flex, memb, mefl, ctor, masloc)
        wgt = carat3(8)*roe
        wgtf= carat3(8)*rof
        call utpslg(3, 6, pgl, masloc, masglo)
        call diaexp(3, 6, 18, masglo, mas)
!
        coef1 = epais*epais/douze
        coef2 = carat3(8)/huit
        if (coef2 .gt. coef1) then
            coef1 = coef2
        endif
        do j = 1, nno
            k = 6*(j-1) + 1
            m2 = 6*(j-1) + 2
            m3 = 6*(j-1) + 3
            i1 = 6*(j-1) + 5
            i2 = 6*(j-1) + 4
            i3 = 6*j
!
            m1 = (k + 1)*k/2
            m2 = (m2 + 1)*m2/2
            m3 = (m3 + 1)*m3/2
            i1 = (i1 + 1)*i1/2
            i2 = (i2 + 1)*i2/2
            i3 = (i3 + 1)*i3/2
!
            mas(m2) = mas(m1)
            mas(m3) = mas(m1)
            mas(i1) = mas(m1)*coef1
            mas(i2) = mas(i1)
            mas(i3) = mas(i1)
        end do
!
    else if (option.eq.'ECIN_ELEM') then
        stopz='ONO'
! IRET NE PEUT VALOIR QUE 0 (TOUT VA BIEN) OU 2 (CHAMP NON FOURNI)
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvitg)
        if (iret .eq. 0) then
            call utpvgl(3, 6, pgl, zr(jvitg), vite)
            call dxtloe(flex, memb, mefl, ctor, .false._1,&
                        vite, ener)
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepg)
            if (iret .eq. 0) then
                call utpvgl(3, 6, pgl, zr(jdepg), depl)
                call dxtloe(flex, memb, mefl, ctor, .false._1,&
                            depl, ener)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
end subroutine
